import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  statSync,
  unlinkSync,
  writeFileSync,
} from 'fs';
import {execFileSync} from 'child_process';
import {createHash} from 'crypto';
import {resolve} from 'path';
import type {ApiData, StoryAssetType} from './types';

const STORY_REGEX =
  /\{\{story (?:(\w+)\.)?(png|gif|cast|static) (.+?)\}\}/g;

const COMMON_VHS = `
Set FontSize 18
Set Width 1300
Set Height 650
Set Padding 0
`;

export function resolveStoryFilename(
  name: string | undefined,
  type: StoryAssetType,
  command: string,
): string {
  if (name) return `${name}.${type}`;
  const hash = createHash('sha256')
    .update(command)
    .digest('hex')
    .slice(0, 12);
  return `${hash}.${type}`;
}

export function ensureStoryAsset(
  projectDir: string,
  name: string | undefined,
  type: StoryAssetType,
  command: string,
): string {
  const imagesDir = resolve(projectDir, 'static', 'images');
  mkdirSync(imagesDir, {recursive: true});

  const filename = resolveStoryFilename(name, type, command);
  const absPath = resolve(imagesDir, filename);

  if (existsSync(absPath)) return filename;

  console.error(`~> building ${filename} (${command})`);

  // The command string may contain extra flags (e.g.
  // "cy/palette --width 120 --height 26") that need to be
  // separate argv entries.
  const commandParts = command.split(/\s+/);

  try {
    if (type === 'cast') {
      const storybook = resolve(projectDir, 'storybook');
      execFileSync(
        storybook,
        ['--cast', absPath, '-s', ...commandParts],
        {
          env: {
            ...process.env,
            TERM: 'xterm-256color',
            EDITOR: '/usr/bin/vim',
          },
          stdio: ['ignore', 'ignore', 'inherit'],
          timeout: 60_000,
        },
      );
      return filename;
    }

    const storybookCmd = process.env.CI
      ? resolve(projectDir, 'storybook')
      : './storybook';

    let script: string;
    if (type === 'gif') {
      script = `${COMMON_VHS}
Output ${absPath}
Set Framerate 23
Set PlaybackSpeed 0.5
Hide
Type "${storybookCmd} -s ${command} && clear"
Enter
Sleep 500ms
Show
Sleep 8s
`;
    } else {
      script = `${COMMON_VHS}
Hide
Type "${storybookCmd} -s ${command} && clear"
Enter
Sleep 2s
Show
Sleep 1s
Screenshot ${absPath}
`;
    }

    const tapePath = absPath.replace(/\.(png|gif)$/, '.tape');
    writeFileSync(tapePath, script);

    const vhsBin = process.env.CI
      ? resolve(projectDir, 'vhs')
      : 'vhs';

    execFileSync(vhsBin, ['-q', tapePath], {
      stdio: ['ignore', 'ignore', 'inherit'],
    });

    if (existsSync(tapePath)) unlinkSync(tapePath);

    if (!existsSync(absPath)) {
      console.error(
        `[cy-docs] warning: VHS did not produce ${filename}`,
      );
    }
  } catch (err: unknown) {
    const msg =
      err instanceof Error ? err.message : String(err);
    console.error(
      `[cy-docs] warning: failed to build ${filename}: ${msg}`,
    );
  }

  return filename;
}

export function ensureStorybookBinary(
  projectDir: string,
  repoDir: string,
): void {
  const storybook = resolve(projectDir, 'storybook');
  if (existsSync(storybook)) return;
  if (process.env.CI) return;
  if (process.env.CY_SKIP_ASSETS === '1') return;

  console.error('Building storybook binary...');
  execFileSync(
    'go',
    [
      'build',
      '-o',
      storybook,
      resolve(repoDir, 'cmd', 'stories', '...'),
    ],
    {cwd: repoDir, stdio: 'inherit'},
  );
}

function collectStoriesFrom(
  content: string,
  imagesDir: string,
  jobs: Map<string, string>,
): void {
  let match: RegExpExecArray | null;
  STORY_REGEX.lastIndex = 0;
  while ((match = STORY_REGEX.exec(content)) !== null) {
    let type = match[2] as StoryAssetType;
    const name = match[1];
    const command = match[3];
    if (!command) continue;

    if (type === ('static' as string)) type = 'cast';

    const filename = resolveStoryFilename(
      name,
      type,
      command,
    );
    const absPath = resolve(imagesDir, filename);
    jobs.set(absPath, command);
  }
}

export function collectAllStoryJobs(
  docsDir: string,
  imagesDir: string,
  apiData: ApiData | null,
): Map<string, string> {
  const jobs = new Map<string, string>();

  function walkDocs(dir: string) {
    let entries: string[];
    try {
      entries = readdirSync(dir);
    } catch {
      return;
    }
    for (const entry of entries) {
      const full = resolve(dir, entry);
      try {
        if (statSync(full).isDirectory()) {
          walkDocs(full);
          continue;
        }
      } catch {
        continue;
      }
      if (!entry.endsWith('.md')) continue;
      const content = readFileSync(full, 'utf-8');
      collectStoriesFrom(content, imagesDir, jobs);
    }
  }

  walkDocs(docsDir);

  if (apiData) {
    for (const frame of apiData.frames) {
      collectStoriesFrom(
        `{{story png frame/${frame}}}`,
        imagesDir,
        jobs,
      );
    }
    for (const anim of apiData.animations) {
      collectStoriesFrom(
        `{{story gif animation/${anim}}}`,
        imagesDir,
        jobs,
      );
    }
  }

  return jobs;
}

export function pregenerateAllAssets(
  projectDir: string,
  docsDir: string,
  apiData: ApiData | null,
): void {
  const imagesDir = resolve(projectDir, 'static', 'images');
  mkdirSync(imagesDir, {recursive: true});

  const allJobs = collectAllStoryJobs(docsDir, imagesDir, apiData);
  const missing = [...allJobs.entries()].filter(
    ([path]) => !existsSync(path),
  );

  if (missing.length === 0) {
    console.error('All story assets up to date.');
    return;
  }

  console.error(
    `\nBuilding ${missing.length} story assets...`,
  );
  for (const [absPath, command] of missing) {
    const ext = absPath.split('.').pop() as StoryAssetType;
    const base = absPath
      .slice(imagesDir.length + 1)
      .replace(/\.\w+$/, '');

    // Determine if this was a named or hashed file
    const hashCheck = createHash('sha256')
      .update(command)
      .digest('hex')
      .slice(0, 12);
    const name = base === hashCheck ? undefined : base;

    ensureStoryAsset(projectDir, name, ext, command);
  }
}

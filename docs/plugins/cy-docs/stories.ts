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
import {tmpdir} from 'os';
import {resolve, join} from 'path';
import type {ApiData, StoryAssetType} from './types';

const STORY_REGEX =
  /\{\{story (?:(\w+)\.)?(png|gif|cast|static) (.+?)\}\}/g;

const AGG_FONT_FAMILY = 'JetBrains Mono';
const AGG_FONT_SIZE = '18';
const AGG_LINE_HEIGHT = '1.4';
const AGG_SPEED = '0.5';
const AGG_FPS_CAP = '23';
const AGG_IDLE_TIME_LIMIT = '30';
const AGG_LAST_FRAME_DURATION = '1';

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

function generateCast(
  projectDir: string,
  commandParts: string[],
  outputPath: string,
): void {
  const storybook = resolve(projectDir, 'storybook');
  execFileSync(
    storybook,
    ['--cast', outputPath, '-s', ...commandParts],
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
}

function castHasCursorEvents(castFile: string): boolean {
  const content = readFileSync(castFile, 'utf-8');
  for (const line of content.split('\n')) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    try {
      const entry = JSON.parse(trimmed);
      if (
        Array.isArray(entry) &&
        entry.length >= 2 &&
        entry[1] === 'c'
      ) {
        return true;
      }
    } catch {
      continue;
    }
  }
  return false;
}

function generateGif(
  projectDir: string,
  castFile: string,
  outputPath: string,
): void {
  const hasCursor = castHasCursorEvents(castFile);
  const aggBin = 'agg';
  const targetGif = hasCursor ? outputPath + '.tmp.gif' : outputPath;

  const fontsDir = resolve(projectDir, 'static', 'fonts');

  execFileSync(
    aggBin,
    [
      castFile,
      targetGif,
      '--font-family',
      AGG_FONT_FAMILY,
      '--font-dir',
      fontsDir,
      '--font-size',
      AGG_FONT_SIZE,
      '--line-height',
      AGG_LINE_HEIGHT,
      '--speed',
      AGG_SPEED,
      '--fps-cap',
      AGG_FPS_CAP,
      '--idle-time-limit',
      AGG_IDLE_TIME_LIMIT,
      '--last-frame-duration',
      AGG_LAST_FRAME_DURATION,
      '--theme',
      'asciinema',
    ],
    {stdio: ['ignore', 'ignore', 'inherit']},
  );

  if (hasCursor) {
    const storybook = resolve(projectDir, 'storybook');
    execFileSync(
      storybook,
      [
        '--overlay',
        '--cast',
        castFile,
        '--gif',
        targetGif,
        '--output',
        outputPath,
        '--font-size',
        AGG_FONT_SIZE,
        '--line-height',
        AGG_LINE_HEIGHT,
        '--speed',
        AGG_SPEED,
      ],
      {stdio: ['ignore', 'ignore', 'inherit']},
    );
    if (existsSync(targetGif)) unlinkSync(targetGif);
  }
}

function makeTempPath(suffix: string): string {
  const name =
    'cy-story-' +
    createHash('sha256')
      .update(String(Date.now()) + String(Math.random()))
      .digest('hex')
      .slice(0, 8) +
    suffix;
  return join(tmpdir(), name);
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

  const commandParts = command.split(/\s+/);

  try {
    if (type === 'cast') {
      generateCast(projectDir, commandParts, absPath);
      return filename;
    }

    if (type === 'gif') {
      const castFile = makeTempPath('.cast');
      try {
        generateCast(projectDir, commandParts, castFile);
        generateGif(projectDir, castFile, absPath);
      } finally {
        if (existsSync(castFile)) unlinkSync(castFile);
      }
      return filename;
    }

    // PNG: generate GIF then extract last frame
    const castFile = makeTempPath('.cast');
    const gifFile = makeTempPath('.gif');
    try {
      generateCast(projectDir, commandParts, castFile);
      generateGif(projectDir, castFile, gifFile);
      const storybook = resolve(projectDir, 'storybook');
      execFileSync(
        storybook,
        [
          '--extract-frame',
          '--gif',
          gifFile,
          '--output',
          absPath,
        ],
        {stdio: ['ignore', 'ignore', 'inherit']},
      );
    } finally {
      if (existsSync(castFile)) unlinkSync(castFile);
      if (existsSync(gifFile)) unlinkSync(gifFile);
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
  if (process.env.CY_SKIP_ASSETS === '1') return;

  const storybook = resolve(projectDir, 'storybook');

  // Always run go build — it's a no-op when sources haven't
  // changed and avoids stale binary issues.
  console.error('Building storybook binary...');
  execFileSync(
    'go',
    [
      'build',
      '-o',
      storybook,
      resolve(repoDir, 'cmd', 'stories'),
    ],
    {cwd: repoDir, stdio: 'inherit'},
  );
}

export function ensureExampleBinary(
  projectDir: string,
  repoDir: string,
): void {
  if (process.env.CY_SKIP_EXAMPLES === '1') return;

  const example = resolve(projectDir, 'example');

  console.error('Building example binary...');
  execFileSync(
    'go',
    [
      'build',
      '-o',
      example,
      resolve(repoDir, 'cmd', 'example'),
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
        `{{story gif animation/${anim} --width 160 --height 48}}`,
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

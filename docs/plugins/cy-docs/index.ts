import {execFileSync, spawn} from 'child_process';
import {writeFileSync, mkdirSync} from 'fs';
import {resolve} from 'path';
import type {LoadContext, Plugin} from '@docusaurus/types';
import {reloadApiData} from './api';
import {
  ensureStorybookBinary,
  ensureExampleBinary,
  pregenerateAllAssets,
} from './stories';
import {
  getPreprocessorErrors,
  getJanetBlocks,
} from './preprocessor';

function validateJanetBlocks(
  exampleBin: string,
  blocks: Array<{file: string; line: number; code: string}>,
): Promise<string[]> {
  if (blocks.length === 0) return Promise.resolve([]);

  const errors: string[] = [];

  return new Promise((resolvePromise, rejectPromise) => {
    const proc = spawn(exampleBin, [], {
      stdio: ['pipe', 'pipe', 'inherit'],
    });

    let buf = '';
    let blockIdx = 0;
    let errorLines: string[] = [];

    function sendNext() {
      if (blockIdx < blocks.length) {
        proc.stdin.write(blocks[blockIdx].code + '\0');
      } else {
        proc.stdin.end();
      }
    }

    function processLine(line: string) {
      if (line === 'ready') {
        sendNext();
        return;
      }

      if (line === 'ok') {
        errorLines = [];
        blockIdx++;
        sendNext();
        return;
      }

      if (line === 'error') {
        const block = blocks[blockIdx];
        const msg = errorLines.join('\n');
        errors.push(
          `${block.file}:${block.line}: failed executing Janet code: ${msg}`,
        );
        errorLines = [];
        blockIdx++;
        sendNext();
        return;
      }

      errorLines.push(line);
    }

    proc.stdout.on('data', (data: Buffer) => {
      buf += data.toString();
      let nlIdx: number;
      while ((nlIdx = buf.indexOf('\n')) !== -1) {
        const line = buf.slice(0, nlIdx).trim();
        buf = buf.slice(nlIdx + 1);
        if (line) processLine(line);
      }
    });

    proc.on('close', () => {
      resolvePromise(errors);
    });

    proc.on('error', (err) => {
      rejectPromise(err);
    });
  });
}

export default async function cyDocsPlugin(
  context: LoadContext,
): Promise<Plugin> {
  const projectDir = context.siteDir;
  const repoDir = resolve(projectDir, '..');

  // Generate API data JSON (unless CY_SKIP_API=1)
  if (process.env.CY_SKIP_API !== '1') {
    console.error('Generating API data...');
    const result = execFileSync(
      'go',
      ['run', './cmd/docs/main.go'],
      {cwd: repoDir},
    );
    const staticDir = resolve(projectDir, 'static');
    mkdirSync(staticDir, {recursive: true});
    writeFileSync(
      resolve(staticDir, 'api-data.json'),
      result,
    );
    console.error('  Wrote static/api-data.json');
  }

  // Build storybook binary if missing (dev only)
  ensureStorybookBinary(projectDir, repoDir);

  // Build example binary for Janet code validation (dev only)
  ensureExampleBinary(projectDir, repoDir);

  // Reload API data cache so preprocessor picks it up
  const apiData = reloadApiData(projectDir);

  return {
    name: 'cy-docs',

    async loadContent() {
      // In CI, pre-generate all story assets before docs are
      // processed, so we don't rely on the synchronous
      // preprocessor path for bulk generation.
      if (process.env.CI && !process.env.CY_SKIP_ASSETS) {
        pregenerateAllAssets(
          projectDir,
          resolve(projectDir, 'docs'),
          apiData,
        );
      }
    },

    async postBuild() {
      const errors = [...getPreprocessorErrors()];

      // Validate Janet code examples
      const blocks = getJanetBlocks();
      if (
        blocks.length > 0 &&
        process.env.CY_SKIP_EXAMPLES !== '1'
      ) {
        console.error(
          `\nValidating ${blocks.length} Janet code examples...`,
        );
        const exampleBin = process.env.CI
          ? resolve(projectDir, 'example')
          : resolve(projectDir, 'example');
        const exampleErrors = await validateJanetBlocks(
          exampleBin,
          blocks,
        );
        errors.push(...exampleErrors);
      }

      if (errors.length > 0) {
        console.error(
          `\n${errors.length} error(s) during documentation build:`,
        );
        for (const err of errors) {
          console.error(`  ${err}`);
        }
        throw new Error(
          `${errors.length} documentation error(s)`,
        );
      }
    },

    getPathsToWatch() {
      return [resolve(projectDir, 'docs', '**', '*.md')];
    },
  };
}

export {createPreprocessor} from './preprocessor';

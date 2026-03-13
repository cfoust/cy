import {execFileSync} from 'child_process';
import {writeFileSync, mkdirSync} from 'fs';
import {resolve} from 'path';
import type {LoadContext, Plugin} from '@docusaurus/types';
import {reloadApiData} from './api';
import {
  ensureStorybookBinary,
  pregenerateAllAssets,
} from './stories';

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

    getPathsToWatch() {
      return [resolve(projectDir, 'docs', '**', '*.md')];
    },
  };
}

export {createPreprocessor} from './preprocessor';

import {createHash} from 'crypto';
import {
  loadApiData,
  renderKeys,
  renderFrames,
  renderAnimations,
  renderApi,
  renderParams,
  renderSymbolLink,
  renderParamLink,
  renderKeySequence,
  symbolToUrl,
  getPackages,
} from './api';
import {ensureStoryAsset} from './stories';
import type {StoryAssetType} from './types';

type Replacement = [number, number, string];

function applyPattern(
  content: string,
  regex: RegExp,
  handler: (m: RegExpExecArray) => Replacement | null,
): string {
  const replacements: Replacement[] = [];
  let match: RegExpExecArray | null;
  regex.lastIndex = 0;
  while ((match = regex.exec(content)) !== null) {
    const result = handler(match);
    if (result) replacements.push(result);
  }
  replacements.sort((a, b) => b[0] - a[0]);
  for (const [start, end, text] of replacements) {
    content = content.slice(0, start) + text + content.slice(end);
  }
  return content;
}

function escapeStrayBraces(content: string): string {
  const lines = content.split('\n');
  const result: string[] = [];
  let inFencedCode = false;

  for (const line of lines) {
    if (line.startsWith('```')) {
      inFencedCode = !inFencedCode;
      result.push(line);
      continue;
    }

    if (inFencedCode) {
      result.push(line);
      continue;
    }

    if (/^import\s/.test(line) || /^export\s/.test(line)) {
      result.push(line);
      continue;
    }

    if (/^\s*<\w/.test(line)) {
      result.push(line);
      continue;
    }

    let escaped = '';
    let inInline = false;
    for (let i = 0; i < line.length; i++) {
      const ch = line[i];
      if (ch === '`') {
        inInline = !inInline;
        escaped += ch;
      } else if (!inInline && (ch === '{' || ch === '}')) {
        escaped += '\\' + ch;
      } else {
        escaped += ch;
      }
    }
    result.push(escaped);
  }

  return result.join('\n');
}

export function createPreprocessor(
  projectDir: string,
): (args: {filePath: string; fileContent: string}) => string {
  const skipAssets = process.env.CY_SKIP_ASSETS === '1';

  return ({filePath, fileContent}) => {
    const data = loadApiData(projectDir);
    if (!data) return fileContent;

    let content = fileContent;
    const {
      symbols,
      params,
      bindings,
      frames,
      animations,
      symbolLookup,
      paramLookup,
    } = data;

    // {{packages}}
    content = applyPattern(
      content,
      /\{\{packages\}\}/g,
      (m) => [
        m.index,
        m.index + m[0].length,
        getPackages(projectDir),
      ],
    );

    // {{keys source group [--skip N]}}
    content = applyPattern(
      content,
      /\{\{keys (.+?)\}\}/g,
      (m) => [
        m.index,
        m.index + m[0].length,
        renderKeys(bindings, m[1]),
      ],
    );

    // {{gendoc type}} — may produce {{story ...}} directives
    content = applyPattern(
      content,
      /\{\{gendoc (.+?)\}\}/g,
      (m) => {
        const cmd = m[1];
        let output = '';
        if (cmd === 'frames') output = renderFrames(frames);
        else if (cmd === 'animations')
          output = renderAnimations(animations);
        else if (cmd === 'api') output = renderApi(symbols);
        else if (cmd === 'params') output = renderParams(params);
        return [m.index, m.index + m[0].length, output];
      },
    );

    // {{api symbol/name}}
    content = applyPattern(
      content,
      /\{\{api ([a-z0-9/-]+)\}\}/g,
      (m) => {
        const name = m[1];
        if (!(name in symbolLookup)) {
          console.warn(
            `[cy-docs] ${filePath}: missing symbol: ${name}`,
          );
          return null;
        }
        return [
          m.index,
          m.index + m[0].length,
          renderSymbolLink(symbolLookup[name]),
        ];
      },
    );

    // {{param name}}
    content = applyPattern(
      content,
      /\{\{param ([a-zA-Z0-9_-]+)\}\}/g,
      (m) => {
        const name = m[1];
        if (!(name in paramLookup)) {
          console.warn(
            `[cy-docs] ${filePath}: missing parameter: ${name}`,
          );
          return null;
        }
        return [
          m.index,
          m.index + m[0].length,
          renderParamLink(paramLookup[name]),
        ];
      },
    );

    // {{bind :scope keys}}
    content = applyPattern(
      content,
      /\{\{bind :(\w+) ((?:[0-9a-zA-Z+\-?:;|\\\[\]]+\s*)+)\}\}/g,
      (m) => {
        const source = m[1];
        const sequence = m[2].trim().split(/\s+/);
        const binding = bindings.find(
          (b) =>
            b.Source === source &&
            b.Sequence.length === sequence.length &&
            b.Sequence.every((k, i) => k === sequence[i]),
        );
        if (!binding?.Function) {
          console.warn(
            `[cy-docs] ${filePath}: missing binding: :${source} ${sequence.join(' ')}`,
          );
          return null;
        }
        const keySeq = renderKeySequence(binding.Sequence);
        return [
          m.index,
          m.index + m[0].length,
          `${keySeq} [<sup>[?]</sup>](${symbolToUrl(binding.Function)})`,
        ];
      },
    );

    // {{story ...}} directives (including those produced by {{gendoc}})
    content = applyPattern(
      content,
      /\{\{story (?:(\w+)\.)?(png|gif|cast|static) (.+?)\}\}/g,
      (m) => {
        const type: StoryAssetType =
          m[2] === 'static' ? 'cast' : (m[2] as StoryAssetType);
        const isStatic = m[2] === 'static';
        const command = m[3];
        if (!command) return null;

        let name: string | undefined = m[1];
        if (!name) {
          name = createHash('sha256')
            .update(command)
            .digest('hex')
            .slice(0, 12);
        }

        if (!skipAssets) {
          ensureStoryAsset(projectDir, m[1], type, command);
        }

        const imagePath = `/images/${name}.${type}`;
        if (type === 'png' || type === 'gif') {
          return [
            m.index,
            m.index + m[0].length,
            `![${command}](${imagePath})`,
          ];
        }
        const staticProp = isStatic ? ' isStatic' : '';
        return [
          m.index,
          m.index + m[0].length,
          `<AsciinemaPlayer cast="${name}"${staticProp} />`,
        ];
      },
    );

    // Janet code blocks: strip # ignore, # {, # }
    content = applyPattern(
      content,
      /```janet([^`]+)```/gm,
      (m) => {
        const lines = m[1].trim().split('\n');
        const filtered: string[] = [];
        let hiding = false;
        for (const line of lines) {
          if (hiding) {
            if (line === '# }') hiding = false;
            continue;
          }
          if (line === '# ignore') continue;
          if (line === '# {') {
            hiding = true;
            continue;
          }
          filtered.push(line);
        }
        return [
          m.index,
          m.index + m[0].length,
          '```janet\n' + filtered.join('\n') + '\n```',
        ];
      },
    );

    // Rewrite .md links to clean URLs
    content = applyPattern(
      content,
      /(\[[^\]]+\])\(([^)]+)\)/g,
      (m) => {
        const target = m[2];
        if (target.startsWith('http') || target.startsWith('#'))
          return null;
        if (target.includes('/images/')) return null;
        if (!target.includes('.md')) return null;

        let newTarget = target;
        if (newTarget.startsWith('./'))
          newTarget = newTarget.slice(2);
        newTarget = newTarget.replace(/\.md(#|$)/g, '$1');

        if (newTarget !== target) {
          return [
            m.index,
            m.index + m[0].length,
            `${m[1]}(${newTarget})`,
          ];
        }
        return null;
      },
    );

    // Escape stray braces for MDX
    content = escapeStrayBraces(content);

    // Add AsciinemaPlayer import if needed
    if (content.includes('<AsciinemaPlayer')) {
      const importLine =
        "import AsciinemaPlayer from '@site/src/components/AsciinemaPlayer';\n\n";
      if (content.startsWith('---')) {
        const endIdx = content.indexOf('---', 3);
        const nlIdx = content.indexOf('\n', endIdx);
        content =
          content.slice(0, nlIdx + 1) +
          '\n' +
          importLine +
          content.slice(nlIdx + 1);
      } else {
        content = importLine + content;
      }
    }

    return content;
  };
}

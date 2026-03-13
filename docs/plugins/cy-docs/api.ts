import {
  readFileSync,
  existsSync,
  readdirSync,
  statSync,
} from 'fs';
import {resolve} from 'path';
import type {
  ApiData,
  RawApiData,
  Symbol,
  Param,
  ResolvedBinding,
} from './types';

const SITE_URL = '/cy';

let apiData: ApiData | null = null;

export function loadApiData(projectDir: string): ApiData | null {
  if (apiData !== null) return apiData;

  const dataPath = resolve(projectDir, 'static', 'api-data.json');
  if (!existsSync(dataPath)) {
    console.warn(
      '[cy-docs] static/api-data.json not found — ' +
        'API directives will pass through unchanged',
    );
    return null;
  }

  const raw: RawApiData = JSON.parse(readFileSync(dataPath, 'utf-8'));
  apiData = buildApiData(raw);
  return apiData;
}

export function reloadApiData(projectDir: string): ApiData | null {
  apiData = null;
  return loadApiData(projectDir);
}

function buildApiData(raw: RawApiData): ApiData {
  const symbols = raw.Symbols || [];
  const params = raw.Parameters || [];
  const frames = raw.Frames || [];
  const animations = raw.Animations || [];

  const symbolLookup: Record<string, Symbol> = Object.fromEntries(
    symbols.map((s) => [s.Name, s]),
  );
  const paramLookup: Record<string, Param> = Object.fromEntries(
    params.map((p) => [p.Name, p]),
  );

  const bindings: ResolvedBinding[] = (raw.Binds || [])
    .filter((b) => b.Function in symbolLookup)
    .map((b) => ({...b, Function: symbolLookup[b.Function]}));

  return {
    symbols,
    params,
    bindings,
    frames,
    animations,
    symbolLookup,
    paramLookup,
  };
}

export function symbolToUrl(symbol: Symbol): string {
  const slug = symbol.Name.replace(/\?/g, '').replace(/\//g, '');
  return `${SITE_URL}/api#${slug}`;
}

export function renderSymbolLink(symbol: Symbol): string {
  return `[${symbol.Name}](${symbolToUrl(symbol)})`;
}

export function paramToUrl(param: Param): string {
  return `${SITE_URL}/default-parameters#${param.Name}`;
}

export function renderParamLink(param: Param): string {
  return `[\`:${param.Name}\`](${paramToUrl(param)})`;
}

function renderKey(key: string): string {
  if (key === ' ') return 'space';
  if (key === '|') return '&#124;';
  return key;
}

export function renderKeySequence(sequence: string[]): string {
  return sequence
    .map((k) => (k === ' ' ? 'space' : k))
    .map((k) => `<kbd>${renderKey(k)}</kbd>`)
    .join(' ');
}

export function renderKeys(
  bindings: ResolvedBinding[],
  argsStr: string,
): string {
  const parts = argsStr.split(/\s+/);
  const source = parts[0];
  const group = parts[1];
  let skip = 0;
  const skipIdx = parts.indexOf('--skip');
  if (skipIdx !== -1 && parts[skipIdx + 1]) {
    skip = parseInt(parts[skipIdx + 1], 10) || 0;
  }

  let output = `
| Sequence    | Action  | Description |
| ----------- | ------- | ----------- |
`;

  const filtered = bindings
    .filter((b) => b.Source === source && b.Tag === group)
    .sort((a, b) =>
      (a.Function?.Name || '').localeCompare(b.Function?.Name || ''),
    );

  for (const bind of filtered) {
    const symbol = bind.Function;
    if (!symbol) continue;
    const link = renderSymbolLink(symbol);
    const seq = renderKeySequence(bind.Sequence.slice(skip));
    const description = symbol.Docstring.split('\n')[2] || '';
    output += `| ${seq} | ${link} | ${description} |\n`;
  }

  return output;
}

export function renderFrames(frames: string[]): string {
  let output = '\n---\n';
  for (const frame of frames) {
    output += `
#### ${frame}

\`\`\`janet
# ignore
(viewport/set-frame "${frame}")
\`\`\`

{{story png frame/${frame}}}

---
`;
  }
  return output;
}

export function renderAnimations(animations: string[]): string {
  let output = '\n---\n';
  for (const anim of animations) {
    output += `
#### ${anim}

{{story gif animation/${anim}}}

---
`;
  }
  return output;
}

export function renderApi(symbols: Symbol[]): string {
  let output = '## Symbols\n\n';
  for (const symbol of symbols) {
    output += renderSymbolLink(symbol) + ' ';
  }
  output += '\n\n---\n';

  for (const symbol of symbols) {
    const type = symbol.Macro ? 'macro' : 'function';
    const source = symbol.Link ? `[source](${symbol.Link})` : '';
    const lines = symbol.Docstring.split('\n');
    if (!lines.length) continue;
    const rest =
      lines.length > 1 ? '\n' + lines.slice(1).join('\n') : '';

    output += `
### ${symbol.Name}

${type}

\`\`\`janet
# ignore
${lines[0]}
\`\`\`${rest}

${source}

`;
  }
  return output;
}

export function renderParams(params: Param[]): string {
  let output = '';
  for (const param of params) {
    output += `[${param.Name}](${SITE_URL}/default-parameters#${param.Name}) `;
  }
  output += '\n\n---\n';

  for (const param of params) {
    output += `
### ${param.Name}

**Type:** \`:${param.Type}\`

**Default:** \`${param.Default}\`

${param.Docstring}

`;
  }
  return output;
}

let _packagesCache: string | null = null;

export function getPackages(projectDir: string): string {
  if (_packagesCache !== null) return _packagesCache;

  const pkgDir = resolve(projectDir, '..', 'pkg');
  const packages: [string, string][] = [];

  function walk(dir: string) {
    let entries: string[];
    try {
      entries = readdirSync(dir);
    } catch {
      return;
    }
    for (const entry of entries) {
      const full = resolve(dir, entry);
      try {
        if (!statSync(full).isDirectory()) continue;
      } catch {
        continue;
      }
      const readme = resolve(full, 'README.md');
      if (existsSync(readme)) {
        packages.push([
          full.slice(pkgDir.length + 1),
          readFileSync(readme, 'utf-8'),
        ]);
      }
      walk(full);
    }
  }

  walk(pkgDir);
  packages.sort((a, b) => a[0].localeCompare(b[0]));

  let docs = '';
  for (const [name, readme] of packages) {
    const lines = readme.split('\n').slice(1);
    const bumped = lines.map((l) =>
      l.startsWith('#') ? '#' + l : l,
    );
    docs += `## ${name}\n\n[source](https://github.com/cfoust/cy/tree/main/pkg/${name})\n\n${bumped.join('\n')}`;
  }

  _packagesCache = docs;
  return docs;
}

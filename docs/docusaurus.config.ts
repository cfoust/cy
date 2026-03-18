import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';
import cyDocsPlugin, {createPreprocessor} from './plugins/cy-docs';

const config: Config = {
  title: 'cy documentation',
  tagline: 'A next-generation terminal multiplexer that records everything you do.',
  favicon: 'img/favicon.svg',

  future: {
    v4: true,
  },

  url: 'https://cfoust.github.io',
  baseUrl: '/cy/',

  organizationName: 'cfoust',
  projectName: 'cy',

  onBrokenLinks: 'throw',
  onBrokenAnchors: 'throw',
  trailingSlash: false,

  plugins: [cyDocsPlugin],

  markdown: {
    format: 'mdx',
    preprocessor: createPreprocessor(__dirname),
  },

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          routeBasePath: '/',
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/cfoust/cy/tree/main/docs/docs/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    colorMode: {
      defaultMode: 'dark',
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'cy',
      logo: {
        alt: 'cy logo',
        src: 'img/favicon.svg',
      },
      items: [
        {
          href: 'https://github.com/cfoust/cy',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      copyright: `Copyright © ${new Date().getFullYear()} Caleb Foust. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['clojure'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;

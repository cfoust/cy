import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docs: [
    'index',
    'why-not-tmux',
    'installation',
    'about-docs',
    {
      type: 'category',
      label: 'Quick start',
      items: [
        'quick-start/starting-cy',
        'quick-start/new-shells',
        'quick-start/replay-mode',
        'quick-start/switching',
        'quick-start/new-projects',
        'quick-start/command-palette',
        'quick-start/the-layout',
      ],
    },
    {
      type: 'category',
      label: 'User guide',
      items: [
        'cli',
        'configuration',
        'keybindings',
        'groups-and-panes',
        {
          type: 'category',
          label: 'Parameters',
          link: {type: 'doc', id: 'parameters'},
          items: [
            'parameters/colors',
          ],
        },
        {
          type: 'category',
          label: 'Replay mode',
          link: {type: 'doc', id: 'replay-mode'},
          items: [
            'replay-mode/modes',
          ],
        },
        {
          type: 'category',
          label: 'Command history',
          link: {type: 'doc', id: 'command-history'},
          items: [
            'command-history/ctrl+r',
            'command-history/switching-panes',
            'command-history/replay-mode',
          ],
        },
        'search-mode',
        {
          type: 'category',
          label: 'Layouts',
          link: {type: 'doc', id: 'layouts'},
          items: [
            {
              type: 'category',
              label: 'Nodes',
              link: {type: 'doc', id: 'layouts/nodes'},
              items: [
                'layouts/nodes/bar',
                'layouts/nodes/borders',
                'layouts/nodes/color-map',
                'layouts/nodes/margins',
                'layouts/nodes/view',
                'layouts/nodes/split',
                'layouts/nodes/stack',
                'layouts/nodes/tabs',
              ],
            },
            'layouts/appearance',
          ],
        },
        {
          type: 'category',
          label: 'User input',
          link: {type: 'doc', id: 'user-input'},
          items: [
            'user-input/fuzzy-finding',
            'user-input/text',
            'user-input/thumbs',
          ],
        },
        'notifications',
      ],
    },
    {
      type: 'category',
      label: 'Reference',
      items: [
        'default-keys',
        'default-parameters',
        'preset-keys',
        'frames',
        'animations',
        'api',
        'emulator-features',
      ],
    },
    {
      type: 'category',
      label: 'Developer guide',
      items: [
        'contributing',
        'roadmap',
        'architecture',
        'packages',
        'stories',
        'documentation',
      ],
    },
    'acknowledgements',
  ],
};

export default sidebars;

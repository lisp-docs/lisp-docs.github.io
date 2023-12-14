// @ts-check
// `@type` JSDoc annotations allow editor autocompletion and type checking
// (when paired with `@ts-check`).
// There are various equivalent ways to declare your Docusaurus config.
// See: https://docusaurus.io/docs/api/docusaurus-config

import {themes as prismThemes} from 'prism-react-renderer';
// import remarkTok from "remark-toc";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Lisp Docs',
  tagline: 'The (Un)Official LISP Documentation ',
  favicon: 'img/favicon.ico',

  // Set the production url of your site here
  url: 'https://lisp-docs.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'lisp-docs', // Usually your GitHub org/user name.
  projectName: 'lisp-docs.github.io', // Usually your repo name.
  trailingSlash: false,
  deploymentBranch: "gh-pages",
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  markdown: {
    // format: 'detect',
  },

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: './sidebars.js',
          // rehypePlugins: [
          //   [remarkTok, {strict: false}],
          // ],
          editUrl:
            'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      metadata: [
        {name:"google-site-verification", content:"Vzaw013_bfdKeUVG89Ch3W1zC9_vH9ID2dPB9Dz0vr0"},
      ],
      // Replace with your project's social card
      // image: 'img/docusaurus-social-card.jpg',
      image: 'img/1024px-Lisp_logo.svg.png',
      navbar: {
        title: 'LISP Docs',
        logo: {
          alt: 'LISP Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'tutorialSidebar',
            position: 'left',
            label: 'Tutorial',
          },
          {to: 'https://lisp-docs.github.io/cl-language-reference/', label: 'Technical Reference', position: 'left'},
          {to: '/getting-started', label: 'Getting Started', position: 'left'},
          {to: '/contribue', label: 'Contribute!', position: 'left'},
          {to: '/about', label: 'About', position: 'right'},
          {to: '/blog', label: 'Blog', position: 'right'},
          {to: '/faq', label: 'FAQ', position: 'right'},
          {to: '/help', label: 'Getting Help', position: 'right'},
          {
            href: 'https://github.com/lisp-docs/lisp-docs.github.io',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Tutorial',
                to: '/docs/intro',
              },
              {
                label: 'Cl Technical Reference',
                to: 'https://github.com/lisp-docs/cl-language-reference/',
              },
              {
                label: 'Help',
                to: '/help',
              }
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Stack Overflow',
                href: 'https://stackoverflow.com/questions/tagged/common-lisp',
              },
              {
                label: 'Discord',
                href: 'https://discord.gg/hhk46CE',
              },
              {
                label: 'Reddit',
                href: 'https://www.reddit.com/r/common_lisp',
              },
              {
                label: 'IRC',
                href: 'https://irclog.tymoon.eu/libera/commonlisp',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Blog',
                to: '/blog',
              },
              {
                label: 'GitHub',
                href: 'https://github.com/lisp-docs/lisp-docs.github.io',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} LISP Docs, MIT License (see GitHub Repo).`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
        // https://prismjs.com/#supported-languages
        additionalLanguages: ['powershell', 'python',  'lisp', 'bash',],
      },
    }),
};

export default config;

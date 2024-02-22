// @ts-check
// `@type` JSDoc annotations allow editor autocompletion and type checking
// (when paired with `@ts-check`).
// There are various equivalent ways to declare your Docusaurus config.
// See: https://docusaurus.io/docs/api/docusaurus-config

import { themes as prismThemes } from "prism-react-renderer";
// import remarkTok from "remark-toc";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Common Lisp Docs",
  tagline: "The (Un)Official Common Lisp Documentation ",
  favicon: "img/favicon.ico",

  // Set the production url of your site here
  url: "https://lisp-docs.github.io",
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: "/",

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: "lisp-docs", // Usually your GitHub org/user name.
  projectName: "lisp-docs.github.io", // Usually your repo name.
  trailingSlash: false,
  deploymentBranch: "gh-pages",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",

  markdown: {
    // format: 'detect',
  },

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: "./sidebars.js",
          showLastUpdateAuthor: true,
          // rehypePlugins: [
          //   [remarkTok, {strict: false}],
          // ],
          editUrl:
            "https://github.com/lisp-docs/lisp-docs.github.io/tree/main/",
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            "https://github.com/lisp-docs/lisp-docs.github.io/",
        },
        theme: {
          customCss: "./src/css/custom.css",
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      metadata: [
        {
          name: "google-site-verification",
          content: "Vzaw013_bfdKeUVG89Ch3W1zC9_vH9ID2dPB9Dz0vr0",
        },
      ],
      algolia: {
        appId: "C1F2Q5VM6X",
        apiKey: "43b7e710ac70357f752ad4ebea87c4d3",
        indexName: "cl-language-reference--category-1-introduction",
        contextualSearch: true,

        // Optional: Specify domains where the navigation should occur through window.location instead on history.push. Useful when our Algolia config crawls multiple documentation sites and we want to navigate with window.location.href to them.
        // externalUrlRegex: "external\\.com|domain\\.com",
        externalUrlRegex: "localhost|lisp-docs.github.io",

        // Optional: Replace parts of the item URLs from Algolia. Useful when using the same search index for multiple deployments using a different baseUrl. You can use regexp or string in the `from` param. For example: localhost:3000 vs myCompany.com/docs
        // replaceSearchResultPathname: {
        //   from: "/docs/", // or as RegExp: /\/docs\//
        //   to: "/",
        // },
      },
      // Replace with your project's social card
      // image: 'img/docusaurus-social-card.jpg',
      image: "img/1024px-Lisp_logo.svg.png",
      navbar: {
        title: "Common Lisp Docs",
        logo: {
          alt: "Lisp Logo",
          src: "img/logo.svg",
        },
        items: [
          {
            type: "docSidebar",
            sidebarId: "tutorialSidebar",
            position: "left",
            label: "Tutorial",
          },
          {
            to: "https://lisp-docs.github.io/cl-language-reference/",
            label: "Technical Reference",
            position: "left",
          },
          // {to: '/docs/getting-started', label: 'Getting Started', position: 'left'},
          { to: "/docs/whylisp", label: "Why Lisp?", position: "left" },
          { to: "/docs/howto", label: "Guides", position: "left" },

          { to: "/docs/contribute", label: "Contribute!", position: "right" },
          // {to: '/docs/faq', label: 'FAQ', position: 'right'},
          { to: "/docs/help", label: "Getting Help", position: "right" },
          { to: "/docs/about", label: "About", position: "right" },
          { to: "/blog", label: "Blog", position: "right" },
          {
            href: "https://github.com/lisp-docs",
            label: "GitHub",
            position: "right",
          },
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Docs",
            items: [
              {
                label: "Common Lisp Tutorial",
                to: "/docs/tutorial",
              },
              {
                label: "Common Lisp Technical Reference",
                to: "https://lisp-docs.github.io/cl-language-reference/",
              },
              {
                label: "Help",
                to: "/docs/help",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "Stack Overflow",
                href: "https://stackoverflow.com/questions/tagged/common-lisp",
              },
              {
                label: "Discord",
                href: "https://discord.gg/hhk46CE",
              },
              {
                label: "Reddit",
                href: "https://www.reddit.com/r/common_lisp",
              },
              {
                label: "IRC",
                href: "https://irclog.tymoon.eu/libera/commonlisp",
              },
            ],
          },
          {
            title: "More",
            items: [
              {
                label: "Blog",
                to: "/blog",
              },
              {
                label: "Tutorial and General Content GitHub",
                href: "https://github.com/lisp-docs/lisp-docs.github.io",
              },
              {
                label: "Technical Reference GitHub",
                href: "https://github.com/lisp-docs/cl-language-reference/",
              },
              { label: "Contribute to the Docs", to: "/docs/contribute" },
            ],
          },
          {
            title: "Common Lisp Docs",
            items: [
              { label: "About", to: "/docs/about" },
              { label: "Why Lisp", to: "/docs/whylisp" },
              { label: "Getting Started", to: "/docs/getting-started" },
              { label: "FAQ", to: "/docs/faq" },
              { label: "Getting Help", to: "/docs/help" },
              { label: "How To Guides", to: "/docs/howto" },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Common Lisp Docs, MIT License (see GitHub Repo).`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
        // https://prismjs.com/#supported-languages
        additionalLanguages: ["powershell", "python", "lisp", "bash"],
      },
    }),
};

export default config;

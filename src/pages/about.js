import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import HomepageFeatures from '@site/src/components/HomepageFeatures';

import Heading from '@theme/Heading';
import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title}
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/intro">
            Quickstart - 5min ⏱️
          </Link>
        </div>
      </div>
    </header>
  );
}

export default function About() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="The (Un)Official Lisp Documentation">
      {/* <HomepageHeader /> */}
      <br />
      <main>
      <h2 className="text--center">About LISP Docs</h2> 
      <br />
<p>The goal of this page is to make it easier for new (and experienced) lisp users to navigate through LISP Documentation by both manking the already existing documentation easier to navigate and by developing new documentation.</p>
<p>Based on [Documentation System](https://documentation.divio.com/) there are 4 types of documentaion:</p>

<ul>
<li> Tutorials </li>
<li> How To Guides </li>
<li> Explanations </li>
<li> References </li>

</ul>
<p>We want to categorize and provied all types of LISP Documentation.</p>
<p>Please help us and contribute to our [GitHub Repo](https://github.com/lisp-docs/lisp-docs.github.io)</p>

      </main>
    </Layout>
  );
}

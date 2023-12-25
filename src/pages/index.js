import clsx from "clsx";
import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Layout from "@theme/Layout";
import HomepageFeatures from "@site/src/components/HomepageFeatures";

import Heading from "@theme/Heading";
import styles from "./index.module.css";
import "react-tooltip/dist/react-tooltip.css";
import ReferenceFeatures from "../components/ReferenceFeatures";

function HomepageHeader() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <header className={clsx("hero hero--primary", styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title}
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/getting-started"
          >
            Quickstart - 5min ⏱️
          </Link>
        </div>
        <br />
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/tutorial"
          >
            Read the Tutorial
          </Link>
        </div>
        <br />
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="https://lisp-docs.github.io/cl-language-reference/"
          >
            Read the Technical Reference
          </Link>
        </div>
        <br />
      </div>
    </header>
  );
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="The (Un)Official Lisp Documentation"
    >
      <HomepageHeader />
      <main>
        <HomepageFeatures />
        <hr />
        <div className="text--center padding-horiz--md margin-bottom--lg">
          <Heading as="h1">About The Tehcnical Reference</Heading>
          {/* {description} */}
        </div>
        {/* <h1>The Tehcnical Reference</h1> */}
        <ReferenceFeatures />
      </main>
    </Layout>
  );
}

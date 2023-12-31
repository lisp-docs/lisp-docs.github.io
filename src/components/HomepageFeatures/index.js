import clsx from "clsx";
import Heading from "@theme/Heading";
import styles from "./styles.module.css";
import { useNavigate } from "react-router-dom";
import { useHistory } from "@docusaurus/router";
import Link from "@docusaurus/Link";
// import { useNavigate } from "react-router-dom";

// https://undraw.co/illustrations

const FeatureList = [
  {
    title: "Lisp Tutorial",
    link: "/docs/tutorial",
    // Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        A complete Lisp Tutorial for beginners to be able to jump right into the
        language.
      </>
    ),
  },
  {
    title: "Technical Reference",
    link: "https://lisp-docs.github.io/cl-language-reference/",
    // Svg: require('@site/static/img/undraw_docusaurus_tree.svg').default,
    description: (
      <>
        Right now we are building a new Open Source, MIT Licensed, Technical Reference from
        the last ANSI Draft. An improved standard with many examples.
      </>
    ),
  },
  {
    title: "How To Guides",
    link: "/docs/howto",
    // Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>Follow hands-on guides on how to use Common Lisp for what you need.</>
    ),
  },
  {
    title: "Common Lisp ANSI Standard",
    link: "https://github.com/lisp-docs/cl-standard/blob/master/new-spec/spec-source-pdf/cl-ansi-standard-draft-w-sidebar.pdf",
    // Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>
        The original Lisp Standard Draft dpANS3R+ version, now freely available. This is
        meant mainly for people who write Common Lisp Systems and Compilers. For the
        Application Programmer please see the Technical Reference.{" "}
      </>
    ),
  },
];

function SimpleCard({ title, description, link, buttonText }) {
  // const navigate = useNavigate();
  const history = useHistory();
  // https://infima.dev/docs/components/card#simple-card

  function conditionalNavigate(link) {
    if (link.startsWith("http")) {
      window.open(link);
    } else {
      history.push(link);
    }
  }

  return (
    <div
      className={clsx(
        "col col--3 card-demo padding-horiz--md padding-vert--md"
      ) + " " + styles["clickable"]}
      onClick={() => conditionalNavigate(link)}
    >
      <div className="card">
        <div className="card__header">
          <h3>{title}</h3>
        </div>
        <div className="card__body">
          <p>{description}</p>
        </div>
        <div className="card__footer">
          <button 
            onClick={() => conditionalNavigate(link)}
            className="button button--secondary button--block"
          >
            {buttonText && buttonText}
            {!buttonText && "Read Now"}
          </button>
        </div>
      </div>
    </div>
  );
}

function Feature({ Svg, title, description, link }) {
  return (
    <div className={clsx("col col--3")}>
      <div className="text--center">
        {/* <Svg className={styles.featureSvg} role="img" /> */}
      </div>
      <a href={link}>
        <div className="text--center padding-horiz--md">
          <Heading as="h3">{title}</Heading>
          <p>{description}</p>
        </div>
      </a>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        {/* <div className="row">
          {FeatureList.map((props, idx) => (
            <SimpleCard key={idx} {...props} />
          ))}
        </div> */}
        <div className={"row " + styles["cardsRow"]}>
          {FeatureList.slice(0, 2).map((props, idx) => (
            <SimpleCard key={idx} {...props} />
          ))}
        </div>
        <div className={"row " + styles["cardsRow"]}>
          {FeatureList.slice(2, 4).map((props, idx) => (
            <SimpleCard key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}

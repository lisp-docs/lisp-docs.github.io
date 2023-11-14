import clsx from "clsx";
import Heading from "@theme/Heading";
import styles from "./styles.module.css";
// import { useNavigate } from "react-router-dom";

// https://undraw.co/illustrations

const FeatureList = [
  {
    title: "Lisp Tutorial",
    link: "/docs/intro",
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
    link: "",
    // Svg: require('@site/static/img/undraw_docusaurus_tree.svg').default,
    description: (
      <>
        Right now we are building a new Open Source, MIT Licensed HyperSpec from
        the last ANSI Draft.
      </>
    ),
  },
  {
    title: "How To Guides",
    link: "",
    // Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>Follow hands-on guides on how to use Common Lisp for what you need.</>
    ),
  },
  {
    title: "Lisp Standard",
    link: "",
    // Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>The original Lisp Standard Draft version, now freely available. 
      This is meant mainly for people who write Lisp Systems and Compilers. 
      For the Application Programmer please see the Technical Reference. </>
    ),
  },
];

function SimpleCard({title, description, link, buttonText}) {
  // const navigate = useNavigate();
  // https://infima.dev/docs/components/card#simple-card
  // onClick={() => navigate(link)}
  return (
    <div className={clsx("col col--3 card-demo padding-horiz--md padding-vert--md")}>
      <div className="card">
        <div className="card__header">
          <h3>{title}</h3>
        </div>
        <div className="card__body">
          <p>
            {description}
          </p>
        </div>
        <div className="card__footer">
          <button className="button button--secondary button--block">
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
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
          {FeatureList.map((props, idx) => (
            <SimpleCard key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}

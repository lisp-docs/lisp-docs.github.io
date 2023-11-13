import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

// https://undraw.co/illustrations

const FeatureList = [
  {
    title: 'Lisp Tutorial',
    // Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        A complete Lisp Tutorial for beginners to be able to jump right into the language.
      </>
    ),
  },
  {
    title: 'Technical Reference',
    // Svg: require('@site/static/img/undraw_docusaurus_tree.svg').default,
    description: (
      <>
        Right now we are building a new Open Source, MIT Licensed HyperSpec from the last ANSI Draft.
      </>
    ),
  },
  {
    title: 'How To Guides',
    // Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>
        Follow hands-on guides on how to use Common Lisp for what you need.
      </>
    ),
  },
];

function Feature({Svg, title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        {/* <Svg className={styles.featureSvg} role="img" /> */}
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
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
        </div>
      </div>
    </section>
  );
}

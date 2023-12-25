import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Complete',
    Svg: require('@site/static/img/svg-icons/complete-svgrepo-com.svg').default,
    description: (
      <>
        Complete Reference of Common Lisp <strong>all in one place</strong>. No need to search around
        the web for hours until you find an explanation for how the API works.
      </>
    ),
  },
  {
    title: 'Community Driven, MIT License',
    Svg: require('@site/static/img/svg-icons/people-teamwork-svgrepo-com.svg').default,
    description: (
      <>
        Finally a joint effort where <strong>anyone</strong> can contribute. No longer are we stuck
        to individual vendors or people locking down documentation. Finally we can 
        build together all the docs in one place and it can <strong>evolve</strong> even if the
        original contributors are no longer around!
      </>
    ),
  },
  {
    title: 'Fully Featured',
    Svg: require('@site/static/img/svg-icons/website-program-svgrepo-com.svg').default,
    description: (
      <>
        {/* <p>Easy to read documentation that is fully featured:</p> */}
        <ul>
          <li><strong>Easy to read</strong></li>
          <li><strong>Search</strong> <code>Ctrl+K</code> or <code>Cmd+K</code></li>
          <li><strong>Side Panel</strong> Table of Contents</li>
          <li><strong>Code Highlighting</strong></li>
        </ul>
      </>
    ),
  },
  // "/static/img/svg-icons/programming-svgrepo-com.svg"
  // {
  //   title: 'Fully Featured',
  //   Svg: require('@site/static/img/svg-icons/website-program-svgrepo-com.svg').default,
  //   description: (
  //     <>
  //       Easy to read documentation that is fully featured:
  //       <ol>
  //         <li>Search `Ctrl+K` or `Cmd+K`</li>
  //       </ol>
  //     </>
  //   ),
  // },
];

function Feature({Svg, title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        {description}
      </div>
    </div>
  );
}

export default function ReferenceFeatures() {
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

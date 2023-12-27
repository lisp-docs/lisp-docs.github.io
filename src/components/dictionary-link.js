import Link from "@docusaurus/Link";
import { ReferenceAid, getDictionaryLink, DefinitionTooltips, isDefinition, isDictionaryItem } from "@lisp-docs/utils";
import BrowserOnly from "@docusaurus/BrowserOnly";
import 'tippy.js/dist/tippy.css'; // optional

export function DictionaryLinkAsync({ children, styled, term }) {
  
  function getLispDocsLink() {
    if (term !== null && term !== undefined && typeof term === "string") {
      const givenLink = getDictionaryLink(term);
      if (givenLink !== null && givenLink !== undefined) {
        return givenLink;
      }
    }
    if (typeof children === "string" && children !== null) {
      if (children.endsWith("s")) {
        const trimmedTerm = children.trim();
        const singularTerm = trimmedTerm.substring(0, trimmedTerm.length - 1);
        return getDictionaryLink(trimmedTerm) || getDictionaryLink(singularTerm);
      } else {
        const trimmedTerm = children.trim();
        return getDictionaryLink(trimmedTerm);
      }
    } else return null;
  }

  function getTerm() {
    if (term !== null && term !== undefined && typeof term === "string") {
      return term.trim();
    }
    if (typeof children === "string" && children !== null) {
      if (children.endsWith("s")) {
        const trimmedTerm = children.trim();
        const singularTerm = trimmedTerm.substring(0, trimmedTerm.length - 1);
        if (getDictionaryLink(trimmedTerm)) {
          return trimmedTerm;
        } else if (getDictionaryLink(singularTerm)) {
          return singularTerm;
        } else return null;
      } else {
        const trimmedTerm = children.trim();
        return trimmedTerm;
      }
    } else return null;
  }

  function getClLink() {
    const term = getTerm();
    const link = getLispDocsLink();
    // console.debug(term);
    // console.debug(link);
    // {/* <DefinitionTooltips term={term}>{children}</DefinitionTooltips> */}
    if (link === null) {
      return <span>{children}</span>;
    } else {
      return (
        <Link to={link}>
          {children}
        </Link>
      );
    }
  }

  function getDisplay() {
    const term = getTerm();
    if (styled != undefined && term !== null) {
      if (isDictionaryItem(term)) {
        return <b>{getClLink()}</b>;
      }
    }
    return getClLink();
  }
  return getDisplay();
}

export default function DictionaryLink({ children, styled, term }) {
  return (
    <BrowserOnly fallback={<span>{children}</span>}>
      {() => <DictionaryLinkAsync term={term} styled={styled}>{children}</DictionaryLinkAsync>}
      </BrowserOnly>
  );
}

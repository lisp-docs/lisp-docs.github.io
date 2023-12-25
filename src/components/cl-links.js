import Link from "@docusaurus/Link";
import { ReferenceAid, getLink, DefinitionTooltips, isDefinition, isDictionaryItem } from "@lisp-docs/utils";
import BrowserOnly from "@docusaurus/BrowserOnly";
import 'tippy.js/dist/tippy.css'; // optional

export function AsyncClLinks({ children, styled, term }) {
  // const LispDocsUtils = require("@lisp-docs/utils");
  // const getLink = LispDocsUtils.getLink;
  // const DefinitionTooltips = LispDocsUtils.DefinitionTooltips;
  // const isDefinition = LispDocsUtils.isDefinition;
  // const isDictionaryItem = LispDocsUtils.isDictionaryItem;
  
  function getLispDocsLink() {
    if (term !== null && term !== undefined && typeof term === "string") {
      const givenLink = getLink(term);
      if (givenLink !== null && givenLink !== undefined) {
        return givenLink;
      }
    }
    if (typeof children === "string" && children !== null) {
      if (children.endsWith("s")) {
        const trimmedTerm = children.trim();
        const singularTerm = trimmedTerm.substring(0, trimmedTerm.length - 1);
        return getLink(trimmedTerm) || getLink(singularTerm);
      } else {
        const trimmedTerm = children.trim();
        return getLink(trimmedTerm);
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
        if (getLink(trimmedTerm)) {
          return trimmedTerm;
        } else if (getLink(singularTerm)) {
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
    if (link === null) {
      return <span>{children}</span>;
    } else {
      return (
        <Link to={link}>
          <DefinitionTooltips term={term}>{children}</DefinitionTooltips>
        </Link>
      );
    }
  }

  function getDisplay() {
    const term = getTerm();
    if (styled != undefined && term !== null) {
      if (isDefinition(term) && isDictionaryItem(term)) {
        return (
          <i>
            <b>{getClLink()}</b>
          </i>
        );
      } else if (isDefinition(term)) {
        return <i>{getClLink()}</i>;
      } else if (isDictionaryItem(term)) {
        return <b>{getClLink()}</b>;
      }
    }
    return getClLink();
  }
  return getDisplay();
}

export default function ClLinks({ children, styled, term }) {
  return (
    <BrowserOnly fallback={<span>{children}</span>}>
      {() => <AsyncClLinks term={term} styled={styled}>{children}</AsyncClLinks>}
    </BrowserOnly>
  );
}

import Link from "@docusaurus/Link";
import { ReferenceAid, getLink, DefinitionTooltips, isDefinition, isDictionaryItem, getDefinitionLink } from "@lisp-docs/utils";
import BrowserOnly from "@docusaurus/BrowserOnly";
import 'tippy.js/dist/tippy.css'; // optional

export function GlossaryTermAsync({ children, styled, term }) {
  
  function getLispDocsLink() {
    if (term !== null && term !== undefined && typeof term === "string") {
      const givenLink = getDefinitionLink(term);
      if (givenLink !== null && givenLink !== undefined) {
        return givenLink;
      }
    }
    if (typeof children === "string" && children !== null) {
      if (children.endsWith("s")) {
        const trimmedTerm = children.trim();
        const singularTerm = trimmedTerm.substring(0, trimmedTerm.length - 1);
        return getDefinitionLink(trimmedTerm) || getDefinitionLink(singularTerm);
      } else {
        const trimmedTerm = children.trim();
        return getDefinitionLink(trimmedTerm);
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
        if (getDefinitionLink(trimmedTerm)) {
          return trimmedTerm;
        } else if (getDefinitionLink(singularTerm)) {
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
      if (isDefinition(term)) {
        return <i>{getClLink()}</i>;
      }
    }
    return getClLink();
  }
    
  return getDisplay();
}

export default function GlossaryTerm({ children, styled, term }) {
  return (
    <BrowserOnly fallback={<span>{children}</span>}>
      {() => <GlossaryTermAsync term={term} styled={styled}>{children}</GlossaryTermAsync>}
    </BrowserOnly>
  );
}

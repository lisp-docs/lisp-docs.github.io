import React from 'react';
// Import the original mapper
import MDXComponents from '@theme-original/MDXComponents';
import GlossaryTerm from '../components/glossary-term';
import DictionaryLink from '../components/dictionary-link';

export default {
  // Re-use the default mapping
  ...MDXComponents,
  DictionaryLink, GlossaryTerm,
};
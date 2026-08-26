/**
 * @file Lumina grammar for tree-sitter
 * @author simvux
 * @license MPL-2
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: "lumina",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});

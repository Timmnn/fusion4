/**
 * @file Fusion grammar for tree-sitter
 * @author Timm Nicolaizik
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'fusion',

  rules: {
    source_file: $ => repeat($.identifier),

    identifier: $ => token(/[a-zA-Z]+/)
  }
});

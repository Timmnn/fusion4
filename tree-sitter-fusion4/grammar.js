/**
 * @file Parser for my Fusion Lang :)
 * @author Timm Nicolaizik <timmmmnn@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "fusion4",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});

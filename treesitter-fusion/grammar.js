/**
 * @file Fusion grammar for tree-sitter
 * @author Timm Nicolaizik
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'fusion',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  rules: {
    // Program structure
    program: $ => seq(
      optional($.expression),
      repeat(seq(
        $._expression_separator,
        $.expression
      )),
      optional($._expression_separator)
    ),

    block: $ => seq(
      '{',
      optional($.expression),
      repeat(seq(
        $._expression_separator,
        $.expression
      )),
      optional($._expression_separator),
      '}'
    ),

    // Expressions
    expression: $ => choice(
      $.c_import,
      $.import,
      $.func_def,
      $.if_stat,
      $.return_expr,
      $.while_loop,
      $.var_decl,
      prec(888, $.var_access),
      $.struct_def,
      $.assignment,
      $.reference,
      $.deref,
      $.bool_expr
    ),

    reference: $ => seq('&', $.expression),
    deref: $ => seq('*', $.expression),

    // Boolean and arithmetic expressions
    bool_expr: $ => prec.left(1, choice(
      seq(
        $.add_expr,
        repeat1(seq(
          choice($.equal, $.less_than),
          $.add_expr
        ))
      ),
      $.add_expr
    )),

    add_expr: $ => prec.left(2, choice(
      seq(
        $.mul_expr,
        repeat1(seq(
          choice($.add, $.subtract),
          $.mul_expr
        ))
      ),
      $.mul_expr
    )),

    mul_expr: $ => prec.left(3, choice(
      seq(
        $.primary,
        repeat1(seq(
          choice($.multiply, $.divide),
          $.primary
        ))
      ),
      $.primary
    )),

    primary: $ => choice(
      $.float_lit,
      $.int_lit,
      $.str_lit,
      $.func_call,
      $.struct_init,
      $.struct_field_access,
      $.block,
      $.var_access,
      seq('(', $.expression, ')')
    ),

    // Control flow
    while_loop: $ => seq('while', $.expression, $.block),
    if_stat: $ => seq('if', $.expression, $.block),
    return_expr: $ => seq('return', $.expression),

    // Imports
    import: $ => seq(
      'import',
      '{',
      $.ident,
      repeat(seq(',', $.ident)),
      '}',
      'from',
      $.str_lit
    ),

    c_import: $ => seq(
      '_c_import',
      $.str_lit,
      '(',
      repeat1(seq($.ident, ':', $.ident)),
      ')'
    ),

    // Variables
    var_access: $ => $.ident,
var_decl: $ => prec.right(seq(
  $.ident,
  ':=',
  $.expression,
  optional(seq('as', $.ident))
)),
    assignment: $ => seq($.ident, '=', $.expression),

    // Functions
    func_def: $ => seq(
      optional('pub'),
      'fn',
      $.ident,
      optional($.generic_typing),
      '(',
      optional($.param_def_list),
      ')',
      optional($.return_type),
      $.block
    ),

    generic_typing: $ => seq('<', repeat1($.ident), '>'),
    return_type: $ => $.ident,
    param_def_list: $ => seq(
      $.field_def,
      repeat(seq(',', $.field_def))
    ),

func_call: $ => prec(4, choice(
  seq(
    $.ident,
    $.generic_params,
    '(',
    optional($.param_list),
    ')'
  ),
  seq(
    $.ident,
    '(',
    optional($.param_list),
    ')'
  )
)),

    generic_params: $ => seq(
      '<',
      $.ident,
      repeat(seq(',', $.ident)),
      '>'
    ),

    param_list: $ => seq(
      $.expression,
      repeat(seq(',', $.expression))
    ),

    // Structs
    struct_def: $ => seq(
      'struct',
      $.ident,
      '=',
      '{',
      optional($.struct_def_content),
      '}'
    ),

    struct_def_content: $ => seq(
      $._newline,
      $.field_def,
      repeat(seq(
        choice($._newline, ','),
        $.field_def
      )),
      repeat($._newline)
    ),

    field_def: $ => seq($.field_ident, ':', $.ident),

    struct_init: $ => prec(999, seq(
      $.ident,
      '{',
      repeat($._newline),
      repeat($.struct_field_init),
      repeat(seq(
        $._parameter_separator,
        $.struct_field_init
      )),
      repeat($._newline),
      '}'
    )),

    struct_field_init: $ => seq($.ident, ':', $.expression),
    struct_field_access: $ => seq($.ident, '.', $.ident),

    // Type alias
    type_alias: $ => seq('type', $.ident, '=', $.ident),

    // Literals
    int_lit: $ => seq(optional('-'), /\d+/),
    
    str_lit: $ => seq(
      '"',
      $.inner_string,
      '"'
    ),

    inner_string: $ => /([^"\\]|\\["\\\/bfnrt]|\\u[0-9a-fA-F]{4})*/,
    
    float_lit: $ => /\d*\.\d+/,

    // Operators
    add: $ => '+',
    subtract: $ => '-',
    multiply: $ => '*',
    divide: $ => '/',
    and: $ => 'and',
    or: $ => 'or',
    equal: $ => '==',
    greater_than: $ => '>',
    less_than: $ => '<',
    unequal: $ => '!=',
    greater_or_equal: $ => '>=',
    less_or_equal: $ => '<=',

    // Identifiers
    ident: $ => /[a-zA-Z*#~^$][a-zA-Z0-9_*#~^$]*|[0-9][a-zA-Z][a-zA-Z0-9_*#~^$]*|_[a-zA-Z][a-zA-Z0-9_*#~^$]*/,
    field_ident: $ => /[a-zA-Z0-9*#~^$]+/,

    // Comments
    comment: $ => token(seq('//', /.*/)),

    // Hidden rules (prefixed with _)
    _expression_separator: $ => choice($._newline, ';'),
    _parameter_separator: $ => prec(100, choice($._newline, ',')),
    _newline: $ => choice('\n', '\r\n'),
  }
});

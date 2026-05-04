[
  (if_statement)
  (guard_statement)
  (while_statement)
  (for_statement)
  (switch_statement)
  (case_item)
  (function_declaration)
  (init_declaration)
  (deinit_declaration)
  (subscript_declaration)
  (class_declaration)
  (struct_declaration)
  (enum_declaration)
  (protocol_declaration)
  (extension_declaration)
  (closure_expression)
  (computed_property)
  (getter)
  (setter)
  (willSet_block)
  (didSet_block)
  (do_statement)
  (catch_block)
  (defer_statement)
] @indent

[
  "}"
  "]"
  ")"
] @outdent

[
  (comment)
  (multiline_comment)
] @auto

(switch_statement
  (switch_entry) @indent)

(enum_declaration
  (enum_case_declaration) @indent)

(protocol_declaration
  (function_declaration) @indent)

(extension_declaration
  (function_declaration) @indent)

; Continuation indents
(call_expression
  (argument_list) @indent)

(array_literal) @indent
(dictionary_literal) @indent

; String interpolation
(string_literal
  (interpolation) @indent)
open Parser_raw

module Default = struct

  open Parsetree
  open Ast_helper

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc Ast_helper.hole_txt !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))

  let default_pattern () = Pat.any ~loc:!default_loc ()

  let default_module_expr () = Mod.structure ~loc:!default_loc []
  let default_module_type () = Mty.signature ~loc:!default_loc []

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SLASHGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_ITEM -> ("", Location.none, "", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_EXPR -> ("", Location.none, "", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!+"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_ESCAPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_CLOSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_LET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSSLASH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_JSX_UIDENT_E -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_JSX_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_JSX_LIDENT_E -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_JSX_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_HASH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EFFECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> ("", Location.none)
    | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> '_'
    | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ANDOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_extra_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_use_file -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tuple_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_param_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_concat_fun_param_as_list_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_jsx_prop_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr_content -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_module_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_any_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_atomic_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_object_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_val_ident_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_ident_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_43_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list_jsx -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_local_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body_no_punning -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_jsx_prop -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_jsx_longident_JSX_UIDENT_E_JSX_LIDENT_E_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_jsx_longident_JSX_UIDENT_JSX_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_jsx_element -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_index_mod -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_params -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_param_as_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_fun_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_colon_package_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_delimited_type_supporting_local_open -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_delimited_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_extra_nonprefix_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_any_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_alias_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found
end

let default_value = Default.value

open MenhirInterpreter

type action =
  | Abort
  | R of int
  | S : 'a symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

let depth =
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;1;2;1;2;3;1;1;2;3;4;5;6;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;2;1;1;2;1;2;3;4;5;6;7;8;1;2;3;4;1;1;1;2;1;1;2;3;4;5;6;7;8;1;2;1;2;3;1;2;3;1;1;1;2;3;4;1;1;1;2;1;2;1;1;1;1;1;2;3;1;1;1;2;3;4;1;1;2;1;2;2;1;1;2;1;1;1;2;3;4;2;3;1;2;3;1;2;1;1;2;1;2;2;3;4;5;4;1;2;1;1;2;1;1;2;1;1;1;1;1;2;3;2;1;2;1;2;3;2;3;2;3;2;3;4;5;3;1;1;2;3;4;3;3;3;2;3;4;5;6;7;8;2;2;3;2;3;4;3;1;2;3;3;4;5;6;1;2;3;4;5;6;1;7;1;2;3;1;2;1;7;2;1;2;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;5;6;7;8;9;10;11;12;13;9;1;2;2;1;2;2;1;1;2;3;4;1;5;6;6;1;2;1;2;3;1;2;1;4;2;1;2;1;1;2;3;3;1;1;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;2;3;2;3;1;1;4;5;2;3;4;2;3;4;1;3;2;3;5;3;4;5;7;8;1;1;1;2;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;4;5;6;7;8;9;5;4;5;1;1;1;1;2;3;1;1;2;3;4;1;1;1;2;2;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;1;2;3;1;2;4;5;6;2;3;2;3;2;3;4;5;6;7;8;4;3;4;3;3;3;4;5;2;3;2;3;2;4;4;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;8;9;8;8;2;3;4;5;6;7;8;9;5;4;5;4;4;2;3;3;4;5;4;1;1;2;1;3;4;5;1;1;1;2;3;1;4;1;1;1;1;1;2;3;1;1;1;1;2;1;1;2;3;4;5;6;7;8;9;10;11;12;13;9;8;9;8;1;8;2;3;2;1;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;4;5;6;7;8;9;5;4;5;4;4;1;2;3;4;5;6;7;8;9;5;4;5;4;4;1;1;2;1;2;1;2;3;3;1;3;4;2;1;2;3;1;1;2;3;1;1;2;3;4;1;2;3;1;1;1;1;1;1;1;2;3;1;3;2;3;1;1;1;2;3;1;2;3;1;1;2;1;1;2;3;4;1;1;4;5;6;1;2;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;7;8;9;10;1;1;1;1;2;3;4;1;2;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;1;1;2;1;2;3;3;4;5;1;2;1;2;3;4;2;3;4;5;6;7;1;2;3;4;5;6;7;8;2;1;1;1;2;4;1;2;5;6;1;2;3;4;5;6;7;8;1;2;3;4;9;10;7;6;7;2;3;2;3;1;2;3;4;5;1;2;3;4;1;2;3;1;2;3;4;1;1;1;1;1;2;3;3;4;1;2;3;3;1;2;5;6;2;1;2;3;4;5;1;1;1;2;3;4;5;2;1;2;1;2;1;2;2;3;1;2;3;4;5;6;1;2;3;4;5;6;7;4;3;4;3;4;5;6;2;3;1;2;1;2;3;1;1;2;3;4;5;6;3;2;3;4;5;6;3;2;1;2;1;2;3;4;5;2;2;3;4;5;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;7;4;3;4;3;4;5;6;3;2;3;4;5;6;3;1;2;1;1;2;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;1;2;3;1;4;1;1;2;3;4;5;1;2;3;2;3;4;5;6;7;8;4;3;4;3;3;2;3;2;3;1;3;1;2;4;2;3;1;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;3;4;5;6;7;8;5;1;2;2;1;2;6;1;1;7;8;9;10;11;4;5;3;4;5;3;4;5;3;4;5;6;7;5;6;7;5;6;7;3;6;7;8;9;6;7;3;4;5;2;3;3;2;4;4;5;6;7;8;9;10;11;12;13;14;11;6;7;8;9;10;11;8;4;5;3;4;5;3;4;5;3;4;5;6;7;8;5;4;5;4;5;6;7;4;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;3;4;4;5;3;4;5;6;3;1;2;3;1;2;3;4;5;1;4;5;1;2;3;4;4;4;5;2;3;2;3;4;5;2;2;3;4;2;3;2;3;4;2;3;1;2;3;4;5;6;5;6;7;8;1;2;3;2;3;4;5;4;5;5;6;2;3;4;5;1;2;3;4;5;1;2;6;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;1;1;2;3;1;4;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;1;1;2;3;1;2;1;1;2;3;4;1;1;2;6;7;8;9;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;1;2;3;1;1;2;1;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;1;1;2;1;2;1;1;1;1;2;3;1;1;1;3;4;3;4;4;5;6;2;4;5;2;2;3;4;5;6;3;2;2;3;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;2;3;2;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;5;6;2;3;4;2;3;4;2;3;5;6;2;2;3;2;4;5;6;7;8;9;10;11;8;7;8;7;8;9;10;7;2;3;4;5;6;7;8;5;4;5;4;5;6;7;4;4;5;6;3;4;9;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;6;7;8;5;6;7;8;9;3;4;5;9;10;11;12;4;5;6;7;8;9;3;4;5;3;4;5;6;7;2;3;4;5;6;7;2;3;4;2;2;2;2;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;3;4;7;8;9;10;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY -> true
  | T_TRUE -> true
  | T_TO -> true
  | T_TILDE -> true
  | T_THEN -> true
  | T_STRUCT -> true
  | T_STAR -> true
  | T_SLASHGREATER -> true
  | T_SIG -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
  | T_OR -> true
  | T_OPEN -> true
  | T_OF -> true
  | T_OBJECT -> true
  | T_NONREC -> true
  | T_NEW -> true
  | T_MUTABLE -> true
  | T_MODULE -> true
  | T_MINUSGREATER -> true
  | T_MINUSDOT -> true
  | T_MINUS -> true
  | T_METHOD -> true
  | T_METAOCAML_ESCAPE -> true
  | T_METAOCAML_BRACKET_OPEN -> true
  | T_METAOCAML_BRACKET_CLOSE -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LET -> true
  | T_LESSSLASH -> true
  | T_LESSMINUS -> true
  | T_LESS -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LBRACKETPERCENT -> true
  | T_LBRACKETLESS -> true
  | T_LBRACKETGREATER -> true
  | T_LBRACKETBAR -> true
  | T_LBRACKETATATAT -> true
  | T_LBRACKETATAT -> true
  | T_LBRACKETAT -> true
  | T_LBRACKET -> true
  | T_LBRACELESS -> true
  | T_LBRACE -> true
  | T_LAZY -> true
  | T_INITIALIZER -> true
  | T_INHERIT -> true
  | T_INCLUDE -> true
  | T_IN -> true
  | T_IF -> true
  | T_HASH -> true
  | T_GREATERRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_EFFECT -> true
  | T_DOWNTO -> true
  | T_DOTDOT -> true
  | T_DOT -> true
  | T_DONE -> true
  | T_DO -> true
  | T_CONSTRAINT -> true
  | T_COMMA -> true
  | T_COLONGREATER -> true
  | T_COLONEQUAL -> true
  | T_COLONCOLON -> true
  | T_COLON -> true
  | T_CLASS -> true
  | T_BEGIN -> true
  | T_BARRBRACKET -> true
  | T_BARBAR -> true
  | T_BAR -> true
  | T_BANG -> true
  | T_BACKQUOTE -> true
  | T_ASSERT -> true
  | T_AS -> true
  | T_AND -> true
  | T_AMPERSAND -> true
  | T_AMPERAMPER -> true
  | _ -> false

let recover =
  let r0 = [R 237] in
  let r1 = S (N N_fun_expr) :: r0 in
  let r2 = [R 747] in
  let r3 = Sub (r1) :: r2 in
  let r4 = [R 155] in
  let r5 = S (T T_DONE) :: r4 in
  let r6 = Sub (r3) :: r5 in
  let r7 = S (T T_DO) :: r6 in
  let r8 = Sub (r3) :: r7 in
  let r9 = R 376 :: r8 in
  let r10 = [R 855] in
  let r11 = S (T T_AND) :: r10 in
  let r12 = [R 32] in
  let r13 = Sub (r11) :: r12 in
  let r14 = [R 133] in
  let r15 = [R 33] in
  let r16 = [R 617] in
  let r17 = S (N N_structure) :: r16 in
  let r18 = [R 34] in
  let r19 = Sub (r17) :: r18 in
  let r20 = [R 35] in
  let r21 = S (T T_RBRACKET) :: r20 in
  let r22 = Sub (r19) :: r21 in
  let r23 = [R 955] in
  let r24 = S (T T_LIDENT) :: r23 in
  let r25 = [R 31] in
  let r26 = S (T T_UNDERSCORE) :: r25 in
  let r27 = [R 928] in
  let r28 = Sub (r26) :: r27 in
  let r29 = [R 241] in
  let r30 = Sub (r28) :: r29 in
  let r31 = [R 17] in
  let r32 = Sub (r30) :: r31 in
  let r33 = [R 108] in
  let r34 = Sub (r32) :: r33 in
  let r35 = [R 622] in
  let r36 = Sub (r34) :: r35 in
  let r37 = [R 963] in
  let r38 = R 382 :: r37 in
  let r39 = Sub (r36) :: r38 in
  let r40 = S (T T_COLON) :: r39 in
  let r41 = Sub (r24) :: r40 in
  let r42 = R 376 :: r41 in
  let r43 = [R 555] in
  let r44 = S (T T_AMPERAMPER) :: r43 in
  let r45 = [R 954] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = Sub (r44) :: r46 in
  let r48 = [R 529] in
  let r49 = S (T T_RPAREN) :: r48 in
  let r50 = R 270 :: r49 in
  let r51 = [R 271] in
  let r52 = [R 531] in
  let r53 = S (T T_RBRACKET) :: r52 in
  let r54 = [R 533] in
  let r55 = S (T T_RBRACE) :: r54 in
  let r56 = [R 443] in
  let r57 = [R 135] in
  let r58 = [R 268] in
  let r59 = S (T T_LIDENT) :: r58 in
  let r60 = [R 707] in
  let r61 = [R 30] in
  let r62 = Sub (r59) :: r61 in
  let r63 = [R 581] in
  let r64 = S (T T_COLON) :: r63 in
  let r65 = [R 114] in
  let r66 = S (T T_RPAREN) :: r65 in
  let r67 = S (N N_module_type) :: r66 in
  let r68 = R 376 :: r67 in
  let r69 = R 132 :: r68 in
  let r70 = S (T T_MODULE) :: r69 in
  let r71 = [R 251] in
  let r72 = Sub (r30) :: r71 in
  let r73 = S (T T_MINUSGREATER) :: r72 in
  let r74 = S (T T_RPAREN) :: r73 in
  let r75 = S (N N_module_type) :: r74 in
  let r76 = S (T T_COLON) :: r75 in
  let r77 = S (T T_UIDENT) :: r76 in
  let r78 = R 376 :: r77 in
  let r79 = R 132 :: r78 in
  let r80 = [R 750] in
  let r81 = R 384 :: r80 in
  let r82 = [R 479] in
  let r83 = S (T T_END) :: r82 in
  let r84 = Sub (r81) :: r83 in
  let r85 = [R 265] in
  let r86 = R 382 :: r85 in
  let r87 = R 695 :: r86 in
  let r88 = R 933 :: r87 in
  let r89 = S (T T_LIDENT) :: r88 in
  let r90 = R 937 :: r89 in
  let r91 = R 376 :: r90 in
  let r92 = R 132 :: r91 in
  let r93 = [R 441] in
  let r94 = S (T T_LIDENT) :: r93 in
  let r95 = [R 935] in
  let r96 = Sub (r94) :: r95 in
  let r97 = [R 93] in
  let r98 = S (T T_FALSE) :: r97 in
  let r99 = [R 97] in
  let r100 = Sub (r98) :: r99 in
  let r101 = [R 262] in
  let r102 = R 376 :: r101 in
  let r103 = R 255 :: r102 in
  let r104 = Sub (r100) :: r103 in
  let r105 = [R 648] in
  let r106 = Sub (r104) :: r105 in
  let r107 = [R 757] in
  let r108 = R 382 :: r107 in
  let r109 = Sub (r106) :: r108 in
  let r110 = R 628 :: r109 in
  let r111 = S (T T_PLUSEQ) :: r110 in
  let r112 = Sub (r96) :: r111 in
  let r113 = R 937 :: r112 in
  let r114 = R 376 :: r113 in
  let r115 = [R 266] in
  let r116 = R 382 :: r115 in
  let r117 = R 695 :: r116 in
  let r118 = R 933 :: r117 in
  let r119 = S (T T_LIDENT) :: r118 in
  let r120 = R 937 :: r119 in
  let r121 = [R 758] in
  let r122 = R 382 :: r121 in
  let r123 = Sub (r106) :: r122 in
  let r124 = R 628 :: r123 in
  let r125 = S (T T_PLUSEQ) :: r124 in
  let r126 = Sub (r96) :: r125 in
  let r127 = [R 941] in
  let r128 = S (T T_UNDERSCORE) :: r127 in
  let r129 = [R 936] in
  let r130 = Sub (r128) :: r129 in
  let r131 = R 942 :: r130 in
  let r132 = [R 720] in
  let r133 = Sub (r131) :: r132 in
  let r134 = [R 939] in
  let r135 = S (T T_RPAREN) :: r134 in
  let r136 = [R 940] in
  let r137 = [R 721] in
  let r138 = [R 510] in
  let r139 = S (T T_DOTDOT) :: r138 in
  let r140 = [R 934] in
  let r141 = [R 511] in
  let r142 = [R 96] in
  let r143 = S (T T_RPAREN) :: r142 in
  let r144 = [R 92] in
  let r145 = [R 724] in
  let r146 = Sub (r26) :: r145 in
  let r147 = [R 249] in
  let r148 = Sub (r146) :: r147 in
  let r149 = S (T T_STAR) :: r148 in
  let r150 = Sub (r26) :: r149 in
  let r151 = [R 250] in
  let r152 = Sub (r30) :: r151 in
  let r153 = S (T T_MINUSGREATER) :: r152 in
  let r154 = S (T T_RPAREN) :: r153 in
  let r155 = S (N N_module_type) :: r154 in
  let r156 = [R 482] in
  let r157 = S (N N_module_expr) :: r156 in
  let r158 = R 376 :: r157 in
  let r159 = S (T T_OF) :: r158 in
  let r160 = [R 455] in
  let r161 = [R 467] in
  let r162 = S (T T_END) :: r161 in
  let r163 = S (N N_structure) :: r162 in
  let r164 = [R 813] in
  let r165 = [R 642] in
  let r166 = Sub (r104) :: r165 in
  let r167 = [R 411] in
  let r168 = R 382 :: r167 in
  let r169 = Sub (r166) :: r168 in
  let r170 = R 628 :: r169 in
  let r171 = S (T T_PLUSEQ) :: r170 in
  let r172 = Sub (r96) :: r171 in
  let r173 = R 937 :: r172 in
  let r174 = R 376 :: r173 in
  let r175 = [R 412] in
  let r176 = R 382 :: r175 in
  let r177 = Sub (r166) :: r176 in
  let r178 = R 628 :: r177 in
  let r179 = S (T T_PLUSEQ) :: r178 in
  let r180 = Sub (r96) :: r179 in
  let r181 = [R 626] in
  let r182 = S (T T_RBRACKET) :: r181 in
  let r183 = Sub (r19) :: r182 in
  let r184 = [R 421] in
  let r185 = Sub (r3) :: r184 in
  let r186 = S (T T_MINUSGREATER) :: r185 in
  let r187 = S (N N_pattern) :: r186 in
  let r188 = [R 709] in
  let r189 = Sub (r187) :: r188 in
  let r190 = [R 148] in
  let r191 = Sub (r189) :: r190 in
  let r192 = S (T T_WITH) :: r191 in
  let r193 = Sub (r3) :: r192 in
  let r194 = R 376 :: r193 in
  let r195 = [R 671] in
  let r196 = S (N N_fun_expr) :: r195 in
  let r197 = S (T T_COMMA) :: r196 in
  let r198 = [R 930] in
  let r199 = Sub (r34) :: r198 in
  let r200 = S (T T_COLON) :: r199 in
  let r201 = [R 676] in
  let r202 = S (N N_fun_expr) :: r201 in
  let r203 = S (T T_COMMA) :: r202 in
  let r204 = S (T T_RPAREN) :: r203 in
  let r205 = Sub (r200) :: r204 in
  let r206 = [R 932] in
  let r207 = [R 520] in
  let r208 = [R 252] in
  let r209 = [R 483] in
  let r210 = S (T T_RPAREN) :: r209 in
  let r211 = [R 477] in
  let r212 = [R 134] in
  let r213 = S (T T_RBRACKET) :: r212 in
  let r214 = Sub (r17) :: r213 in
  let r215 = [R 388] in
  let r216 = [R 274] in
  let r217 = S (T T_UNDERSCORE) :: r164 in
  let r218 = [R 803] in
  let r219 = [R 797] in
  let r220 = S (T T_END) :: r219 in
  let r221 = R 393 :: r220 in
  let r222 = R 60 :: r221 in
  let r223 = R 376 :: r222 in
  let r224 = [R 58] in
  let r225 = S (T T_RPAREN) :: r224 in
  let r226 = [R 841] in
  let r227 = [R 685] in
  let r228 = S (T T_DOTDOT) :: r227 in
  let r229 = S (T T_COMMA) :: r228 in
  let r230 = [R 686] in
  let r231 = S (T T_DOTDOT) :: r230 in
  let r232 = S (T T_COMMA) :: r231 in
  let r233 = S (T T_RPAREN) :: r232 in
  let r234 = Sub (r34) :: r233 in
  let r235 = S (T T_COLON) :: r234 in
  let r236 = [R 731] in
  let r237 = Sub (r34) :: r236 in
  let r238 = [R 716] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 120] in
  let r241 = S (T T_RBRACKET) :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = [R 119] in
  let r244 = S (T T_RBRACKET) :: r243 in
  let r245 = [R 118] in
  let r246 = S (T T_RBRACKET) :: r245 in
  let r247 = [R 499] in
  let r248 = Sub (r59) :: r247 in
  let r249 = S (T T_BACKQUOTE) :: r248 in
  let r250 = [R 916] in
  let r251 = R 376 :: r250 in
  let r252 = Sub (r249) :: r251 in
  let r253 = [R 115] in
  let r254 = S (T T_RBRACKET) :: r253 in
  let r255 = [R 624] in
  let r256 = Sub (r32) :: r255 in
  let r257 = [R 433] in
  let r258 = R 376 :: r257 in
  let r259 = Sub (r256) :: r258 in
  let r260 = [R 86] in
  let r261 = Sub (r94) :: r260 in
  let r262 = [R 26] in
  let r263 = [R 442] in
  let r264 = S (T T_LIDENT) :: r263 in
  let r265 = S (T T_DOT) :: r264 in
  let r266 = S (T T_UIDENT) :: r56 in
  let r267 = [R 459] in
  let r268 = Sub (r266) :: r267 in
  let r269 = [R 460] in
  let r270 = S (T T_RPAREN) :: r269 in
  let r271 = [R 444] in
  let r272 = S (T T_UIDENT) :: r271 in
  let r273 = [R 247] in
  let r274 = [R 243] in
  let r275 = Sub (r30) :: r274 in
  let r276 = S (T T_MINUSGREATER) :: r275 in
  let r277 = [R 25] in
  let r278 = Sub (r96) :: r277 in
  let r279 = [R 28] in
  let r280 = [R 728] in
  let r281 = S (T T_DOT) :: r272 in
  let r282 = S (T T_LBRACKETGREATER) :: r244 in
  let r283 = [R 29] in
  let r284 = Sub (r282) :: r283 in
  let r285 = [R 521] in
  let r286 = [R 113] in
  let r287 = [R 929] in
  let r288 = [R 725] in
  let r289 = Sub (r26) :: r288 in
  let r290 = [R 27] in
  let r291 = [R 726] in
  let r292 = [R 727] in
  let r293 = [R 18] in
  let r294 = Sub (r59) :: r293 in
  let r295 = [R 242] in
  let r296 = Sub (r30) :: r295 in
  let r297 = S (T T_MINUSGREATER) :: r296 in
  let r298 = S (T T_RPAREN) :: r297 in
  let r299 = Sub (r34) :: r298 in
  let r300 = [R 708] in
  let r301 = [R 729] in
  let r302 = [R 625] in
  let r303 = Sub (r32) :: r302 in
  let r304 = [R 432] in
  let r305 = [R 428] in
  let r306 = R 376 :: r305 in
  let r307 = Sub (r256) :: r306 in
  let r308 = [R 426] in
  let r309 = [R 377] in
  let r310 = [R 116] in
  let r311 = S (T T_RBRACKET) :: r310 in
  let r312 = [R 717] in
  let r313 = [R 712] in
  let r314 = Sub (r32) :: r313 in
  let r315 = [R 915] in
  let r316 = R 376 :: r315 in
  let r317 = Sub (r314) :: r316 in
  let r318 = [R 713] in
  let r319 = [R 117] in
  let r320 = S (T T_RBRACKET) :: r319 in
  let r321 = Sub (r239) :: r320 in
  let r322 = [R 705] in
  let r323 = Sub (r249) :: r322 in
  let r324 = [R 121] in
  let r325 = S (T T_RBRACKET) :: r324 in
  let r326 = [R 321] in
  let r327 = [R 322] in
  let r328 = S (T T_RPAREN) :: r327 in
  let r329 = Sub (r34) :: r328 in
  let r330 = S (T T_COLON) :: r329 in
  let r331 = [R 773] in
  let r332 = [R 771] in
  let r333 = [R 837] in
  let r334 = S (T T_RPAREN) :: r333 in
  let r335 = S (N N_pattern) :: r334 in
  let r336 = S (T T_UNDERSCORE) :: r211 in
  let r337 = [R 839] in
  let r338 = S (T T_RPAREN) :: r337 in
  let r339 = Sub (r336) :: r338 in
  let r340 = R 376 :: r339 in
  let r341 = [R 840] in
  let r342 = S (T T_RPAREN) :: r341 in
  let r343 = [R 480] in
  let r344 = S (N N_module_type) :: r343 in
  let r345 = S (T T_MINUSGREATER) :: r344 in
  let r346 = S (N N_functor_args) :: r345 in
  let r347 = [R 253] in
  let r348 = S (T T_RPAREN) :: r347 in
  let r349 = S (N N_module_type) :: r348 in
  let r350 = [R 451] in
  let r351 = Sub (r59) :: r350 in
  let r352 = [R 491] in
  let r353 = Sub (r351) :: r352 in
  let r354 = [R 976] in
  let r355 = S (N N_module_type) :: r354 in
  let r356 = S (T T_EQUAL) :: r355 in
  let r357 = Sub (r353) :: r356 in
  let r358 = S (T T_TYPE) :: r357 in
  let r359 = S (T T_MODULE) :: r358 in
  let r360 = [R 714] in
  let r361 = Sub (r359) :: r360 in
  let r362 = [R 487] in
  let r363 = [R 453] in
  let r364 = S (T T_LIDENT) :: r363 in
  let r365 = [R 296] in
  let r366 = Sub (r364) :: r365 in
  let r367 = [R 973] in
  let r368 = Sub (r32) :: r367 in
  let r369 = S (T T_COLONEQUAL) :: r368 in
  let r370 = Sub (r366) :: r369 in
  let r371 = [R 454] in
  let r372 = S (T T_LIDENT) :: r371 in
  let r373 = [R 456] in
  let r374 = [R 461] in
  let r375 = [R 972] in
  let r376 = R 695 :: r375 in
  let r377 = [R 696] in
  let r378 = Sub (r34) :: r377 in
  let r379 = S (T T_EQUAL) :: r378 in
  let r380 = [R 452] in
  let r381 = Sub (r59) :: r380 in
  let r382 = [R 481] in
  let r383 = S (N N_module_type) :: r382 in
  let r384 = [R 486] in
  let r385 = [R 977] in
  let r386 = [R 974] in
  let r387 = Sub (r268) :: r386 in
  let r388 = S (T T_UIDENT) :: r373 in
  let r389 = [R 975] in
  let r390 = [R 715] in
  let r391 = [R 778] in
  let r392 = [R 91] in
  let r393 = [R 741] in
  let r394 = S (N N_pattern) :: r393 in
  let r395 = [R 776] in
  let r396 = S (T T_RBRACKET) :: r395 in
  let r397 = [R 402] in
  let r398 = R 574 :: r397 in
  let r399 = R 567 :: r398 in
  let r400 = Sub (r366) :: r399 in
  let r401 = [R 775] in
  let r402 = S (T T_RBRACE) :: r401 in
  let r403 = [R 568] in
  let r404 = [R 575] in
  let r405 = S (T T_UNDERSCORE) :: r226 in
  let r406 = [R 836] in
  let r407 = Sub (r405) :: r406 in
  let r408 = [R 608] in
  let r409 = Sub (r407) :: r408 in
  let r410 = R 376 :: r409 in
  let r411 = [R 87] in
  let r412 = [R 846] in
  let r413 = S (T T_INT) :: r411 in
  let r414 = [R 770] in
  let r415 = Sub (r413) :: r414 in
  let r416 = [R 843] in
  let r417 = [R 848] in
  let r418 = S (T T_RBRACKET) :: r417 in
  let r419 = S (T T_LBRACKET) :: r418 in
  let r420 = [R 849] in
  let r421 = [R 684] in
  let r422 = S (T T_DOTDOT) :: r421 in
  let r423 = S (T T_COMMA) :: r422 in
  let r424 = [R 313] in
  let r425 = [R 314] in
  let r426 = S (T T_RPAREN) :: r425 in
  let r427 = Sub (r34) :: r426 in
  let r428 = S (T T_COLON) :: r427 in
  let r429 = [R 312] in
  let r430 = [R 101] in
  let r431 = [R 602] in
  let r432 = S (N N_pattern) :: r431 in
  let r433 = R 376 :: r432 in
  let r434 = [R 604] in
  let r435 = Sub (r407) :: r434 in
  let r436 = [R 603] in
  let r437 = Sub (r407) :: r436 in
  let r438 = S (T T_COMMA) :: r437 in
  let r439 = [R 607] in
  let r440 = [R 682] in
  let r441 = [R 305] in
  let r442 = [R 306] in
  let r443 = S (T T_RPAREN) :: r442 in
  let r444 = Sub (r34) :: r443 in
  let r445 = S (T T_COLON) :: r444 in
  let r446 = [R 304] in
  let r447 = [R 596] in
  let r448 = [R 605] in
  let r449 = [R 500] in
  let r450 = S (T T_LIDENT) :: r449 in
  let r451 = [R 606] in
  let r452 = Sub (r407) :: r451 in
  let r453 = S (T T_RPAREN) :: r452 in
  let r454 = [R 100] in
  let r455 = S (T T_RPAREN) :: r454 in
  let r456 = [R 683] in
  let r457 = [R 309] in
  let r458 = [R 310] in
  let r459 = S (T T_RPAREN) :: r458 in
  let r460 = Sub (r34) :: r459 in
  let r461 = S (T T_COLON) :: r460 in
  let r462 = [R 308] in
  let r463 = [R 851] in
  let r464 = S (T T_RPAREN) :: r463 in
  let r465 = Sub (r34) :: r464 in
  let r466 = [R 601] in
  let r467 = [R 599] in
  let r468 = [R 99] in
  let r469 = S (T T_RPAREN) :: r468 in
  let r470 = [R 850] in
  let r471 = [R 404] in
  let r472 = [R 777] in
  let r473 = [R 320] in
  let r474 = [R 317] in
  let r475 = [R 318] in
  let r476 = S (T T_RPAREN) :: r475 in
  let r477 = Sub (r34) :: r476 in
  let r478 = S (T T_COLON) :: r477 in
  let r479 = [R 316] in
  let r480 = [R 59] in
  let r481 = S (T T_RPAREN) :: r480 in
  let r482 = [R 959] in
  let r483 = Sub (r3) :: r482 in
  let r484 = S (T T_EQUAL) :: r483 in
  let r485 = S (T T_LIDENT) :: r484 in
  let r486 = R 492 :: r485 in
  let r487 = R 376 :: r486 in
  let r488 = [R 46] in
  let r489 = R 382 :: r488 in
  let r490 = [R 960] in
  let r491 = Sub (r3) :: r490 in
  let r492 = S (T T_EQUAL) :: r491 in
  let r493 = S (T T_LIDENT) :: r492 in
  let r494 = R 492 :: r493 in
  let r495 = [R 57] in
  let r496 = Sub (r364) :: r495 in
  let r497 = [R 794] in
  let r498 = Sub (r496) :: r497 in
  let r499 = R 376 :: r498 in
  let r500 = [R 790] in
  let r501 = [R 791] in
  let r502 = S (T T_METAOCAML_BRACKET_CLOSE) :: r501 in
  let r503 = [R 147] in
  let r504 = Sub (r189) :: r503 in
  let r505 = S (T T_WITH) :: r504 in
  let r506 = Sub (r3) :: r505 in
  let r507 = R 376 :: r506 in
  let r508 = [R 779] in
  let r509 = S (T T_RPAREN) :: r508 in
  let r510 = [R 818] in
  let r511 = [R 211] in
  let r512 = [R 361] in
  let r513 = Sub (r24) :: r512 in
  let r514 = [R 364] in
  let r515 = Sub (r513) :: r514 in
  let r516 = [R 208] in
  let r517 = Sub (r3) :: r516 in
  let r518 = S (T T_IN) :: r517 in
  let r519 = [R 691] in
  let r520 = S (T T_DOTDOT) :: r519 in
  let r521 = S (T T_COMMA) :: r520 in
  let r522 = [R 692] in
  let r523 = S (T T_DOTDOT) :: r522 in
  let r524 = S (T T_COMMA) :: r523 in
  let r525 = S (T T_RPAREN) :: r524 in
  let r526 = Sub (r34) :: r525 in
  let r527 = S (T T_COLON) :: r526 in
  let r528 = [R 341] in
  let r529 = [R 342] in
  let r530 = S (T T_RPAREN) :: r529 in
  let r531 = Sub (r34) :: r530 in
  let r532 = S (T T_COLON) :: r531 in
  let r533 = [R 340] in
  let r534 = [R 609] in
  let r535 = [R 688] in
  let r536 = [R 325] in
  let r537 = [R 326] in
  let r538 = S (T T_RPAREN) :: r537 in
  let r539 = Sub (r34) :: r538 in
  let r540 = S (T T_COLON) :: r539 in
  let r541 = [R 324] in
  let r542 = [R 337] in
  let r543 = [R 338] in
  let r544 = S (T T_RPAREN) :: r543 in
  let r545 = Sub (r34) :: r544 in
  let r546 = S (T T_COLON) :: r545 in
  let r547 = [R 336] in
  let r548 = [R 690] in
  let r549 = S (T T_DOTDOT) :: r548 in
  let r550 = S (T T_COMMA) :: r549 in
  let r551 = [R 333] in
  let r552 = [R 334] in
  let r553 = S (T T_RPAREN) :: r552 in
  let r554 = Sub (r34) :: r553 in
  let r555 = S (T T_COLON) :: r554 in
  let r556 = [R 332] in
  let r557 = [R 830] in
  let r558 = [R 294] in
  let r559 = S (T T_LIDENT) :: r558 in
  let r560 = [R 829] in
  let r561 = S (T T_RPAREN) :: r560 in
  let r562 = [R 295] in
  let r563 = [R 623] in
  let r564 = Sub (r34) :: r563 in
  let r565 = [R 826] in
  let r566 = [R 825] in
  let r567 = S (T T_RPAREN) :: r566 in
  let r568 = R 576 :: r567 in
  let r569 = [R 577] in
  let r570 = [R 346] in
  let r571 = Sub (r24) :: r570 in
  let r572 = [R 353] in
  let r573 = R 382 :: r572 in
  let r574 = Sub (r571) :: r573 in
  let r575 = R 635 :: r574 in
  let r576 = R 376 :: r575 in
  let r577 = R 132 :: r576 in
  let r578 = S (T T_QUOTED_STRING_ITEM) :: r216 in
  let r579 = [R 406] in
  let r580 = R 382 :: r579 in
  let r581 = Sub (r578) :: r580 in
  let r582 = [R 145] in
  let r583 = Sub (r3) :: r582 in
  let r584 = S (T T_IN) :: r583 in
  let r585 = Sub (r581) :: r584 in
  let r586 = R 376 :: r585 in
  let r587 = [R 522] in
  let r588 = R 382 :: r587 in
  let r589 = S (N N_module_expr) :: r588 in
  let r590 = R 376 :: r589 in
  let r591 = [R 523] in
  let r592 = R 382 :: r591 in
  let r593 = S (N N_module_expr) :: r592 in
  let r594 = R 376 :: r593 in
  let r595 = [R 583] in
  let r596 = S (T T_RPAREN) :: r595 in
  let r597 = [R 124] in
  let r598 = S (N N_fun_expr) :: r597 in
  let r599 = [R 584] in
  let r600 = S (T T_RPAREN) :: r599 in
  let r601 = Sub (r598) :: r600 in
  let r602 = [R 732] in
  let r603 = S (N N_fun_expr) :: r602 in
  let r604 = [R 821] in
  let r605 = S (T T_RBRACKET) :: r604 in
  let r606 = [R 806] in
  let r607 = S (T T_RBRACE) :: r606 in
  let r608 = [R 738] in
  let r609 = R 569 :: r608 in
  let r610 = [R 570] in
  let r611 = [R 744] in
  let r612 = R 569 :: r611 in
  let r613 = R 578 :: r612 in
  let r614 = Sub (r366) :: r613 in
  let r615 = [R 637] in
  let r616 = Sub (r614) :: r615 in
  let r617 = [R 815] in
  let r618 = S (T T_RBRACE) :: r617 in
  let r619 = S (T T_UIDENT) :: r160 in
  let r620 = Sub (r619) :: r374 in
  let r621 = [R 279] in
  let r622 = [R 793] in
  let r623 = S (T T_END) :: r622 in
  let r624 = R 376 :: r623 in
  let r625 = [R 158] in
  let r626 = Sub (r217) :: r625 in
  let r627 = R 376 :: r626 in
  let r628 = [R 804] in
  let r629 = [R 814] in
  let r630 = S (T T_RPAREN) :: r629 in
  let r631 = S (T T_LPAREN) :: r630 in
  let r632 = S (T T_DOT) :: r631 in
  let r633 = [R 824] in
  let r634 = S (T T_RPAREN) :: r633 in
  let r635 = S (N N_module_type) :: r634 in
  let r636 = S (T T_COLON) :: r635 in
  let r637 = S (N N_module_expr) :: r636 in
  let r638 = R 376 :: r637 in
  let r639 = [R 468] in
  let r640 = S (N N_module_expr) :: r639 in
  let r641 = S (T T_MINUSGREATER) :: r640 in
  let r642 = S (N N_functor_args) :: r641 in
  let r643 = [R 473] in
  let r644 = [R 582] in
  let r645 = S (T T_RPAREN) :: r644 in
  let r646 = [R 362] in
  let r647 = Sub (r3) :: r646 in
  let r648 = S (T T_EQUAL) :: r647 in
  let r649 = [R 666] in
  let r650 = S (N N_fun_expr) :: r649 in
  let r651 = S (T T_COMMA) :: r650 in
  let r652 = [R 811] in
  let r653 = [R 784] in
  let r654 = S (T T_RPAREN) :: r653 in
  let r655 = Sub (r603) :: r654 in
  let r656 = S (T T_LPAREN) :: r655 in
  let r657 = [R 153] in
  let r658 = S (N N_fun_expr) :: r657 in
  let r659 = S (T T_THEN) :: r658 in
  let r660 = Sub (r3) :: r659 in
  let r661 = R 376 :: r660 in
  let r662 = [R 748] in
  let r663 = Sub (r189) :: r662 in
  let r664 = R 376 :: r663 in
  let r665 = [R 710] in
  let r666 = [R 422] in
  let r667 = Sub (r3) :: r666 in
  let r668 = S (T T_MINUSGREATER) :: r667 in
  let r669 = [R 832] in
  let r670 = Sub (r407) :: r669 in
  let r671 = [R 235] in
  let r672 = Sub (r670) :: r671 in
  let r673 = [R 699] in
  let r674 = Sub (r672) :: r673 in
  let r675 = [R 236] in
  let r676 = Sub (r674) :: r675 in
  let r677 = [R 143] in
  let r678 = Sub (r1) :: r677 in
  let r679 = [R 146] in
  let r680 = Sub (r678) :: r679 in
  let r681 = S (T T_MINUSGREATER) :: r680 in
  let r682 = R 565 :: r681 in
  let r683 = Sub (r676) :: r682 in
  let r684 = R 376 :: r683 in
  let r685 = [R 616] in
  let r686 = S (T T_UNDERSCORE) :: r685 in
  let r687 = [R 828] in
  let r688 = [R 827] in
  let r689 = S (T T_RPAREN) :: r688 in
  let r690 = R 576 :: r689 in
  let r691 = [R 359] in
  let r692 = [R 234] in
  let r693 = S (T T_RPAREN) :: r692 in
  let r694 = [R 834] in
  let r695 = S (T T_RPAREN) :: r694 in
  let r696 = Sub (r34) :: r695 in
  let r697 = [R 831] in
  let r698 = [R 833] in
  let r699 = S (T T_RPAREN) :: r698 in
  let r700 = Sub (r34) :: r699 in
  let r701 = [R 566] in
  let r702 = [R 142] in
  let r703 = Sub (r189) :: r702 in
  let r704 = R 376 :: r703 in
  let r705 = [R 661] in
  let r706 = [R 664] in
  let r707 = [R 665] in
  let r708 = S (T T_RPAREN) :: r707 in
  let r709 = Sub (r200) :: r708 in
  let r710 = [R 931] in
  let r711 = [R 663] in
  let r712 = [R 810] in
  let r713 = [R 781] in
  let r714 = S (T T_RPAREN) :: r713 in
  let r715 = Sub (r3) :: r714 in
  let r716 = S (T T_LPAREN) :: r715 in
  let r717 = [R 123] in
  let r718 = S (T T_DOWNTO) :: r717 in
  let r719 = [R 156] in
  let r720 = S (T T_DONE) :: r719 in
  let r721 = Sub (r3) :: r720 in
  let r722 = S (T T_DO) :: r721 in
  let r723 = Sub (r3) :: r722 in
  let r724 = Sub (r718) :: r723 in
  let r725 = Sub (r3) :: r724 in
  let r726 = S (T T_EQUAL) :: r725 in
  let r727 = S (N N_pattern) :: r726 in
  let r728 = R 376 :: r727 in
  let r729 = [R 157] in
  let r730 = Sub (r217) :: r729 in
  let r731 = R 376 :: r730 in
  let r732 = [R 275] in
  let r733 = S (T T_SLASHGREATER) :: r732 in
  let r734 = [R 286] in
  let r735 = [R 288] in
  let r736 = [R 287] in
  let r737 = [R 282] in
  let r738 = S (T T_JSX_LIDENT_E) :: r737 in
  let r739 = [R 276] in
  let r740 = S (T T_GREATER) :: r739 in
  let r741 = Sub (r738) :: r740 in
  let r742 = [R 283] in
  let r743 = [R 203] in
  let r744 = [R 204] in
  let r745 = Sub (r189) :: r744 in
  let r746 = R 376 :: r745 in
  let r747 = [R 299] in
  let r748 = [R 300] in
  let r749 = S (T T_RPAREN) :: r748 in
  let r750 = Sub (r200) :: r749 in
  let r751 = [R 301] in
  let r752 = [R 302] in
  let r753 = [R 298] in
  let r754 = [R 734] in
  let r755 = Sub (r189) :: r754 in
  let r756 = R 376 :: r755 in
  let r757 = [R 651] in
  let r758 = [R 654] in
  let r759 = [R 655] in
  let r760 = S (T T_RPAREN) :: r759 in
  let r761 = Sub (r200) :: r760 in
  let r762 = [R 653] in
  let r763 = [R 652] in
  let r764 = Sub (r189) :: r763 in
  let r765 = R 376 :: r764 in
  let r766 = [R 711] in
  let r767 = [R 207] in
  let r768 = Sub (r3) :: r767 in
  let r769 = [R 183] in
  let r770 = [R 184] in
  let r771 = Sub (r189) :: r770 in
  let r772 = R 376 :: r771 in
  let r773 = [R 171] in
  let r774 = [R 172] in
  let r775 = Sub (r189) :: r774 in
  let r776 = R 376 :: r775 in
  let r777 = [R 205] in
  let r778 = [R 206] in
  let r779 = Sub (r189) :: r778 in
  let r780 = R 376 :: r779 in
  let r781 = [R 240] in
  let r782 = Sub (r3) :: r781 in
  let r783 = [R 177] in
  let r784 = [R 178] in
  let r785 = Sub (r189) :: r784 in
  let r786 = R 376 :: r785 in
  let r787 = [R 185] in
  let r788 = [R 186] in
  let r789 = Sub (r189) :: r788 in
  let r790 = R 376 :: r789 in
  let r791 = [R 169] in
  let r792 = [R 170] in
  let r793 = Sub (r189) :: r792 in
  let r794 = R 376 :: r793 in
  let r795 = [R 175] in
  let r796 = [R 176] in
  let r797 = Sub (r189) :: r796 in
  let r798 = R 376 :: r797 in
  let r799 = [R 173] in
  let r800 = [R 174] in
  let r801 = Sub (r189) :: r800 in
  let r802 = R 376 :: r801 in
  let r803 = [R 193] in
  let r804 = [R 194] in
  let r805 = Sub (r189) :: r804 in
  let r806 = R 376 :: r805 in
  let r807 = [R 181] in
  let r808 = [R 182] in
  let r809 = Sub (r189) :: r808 in
  let r810 = R 376 :: r809 in
  let r811 = [R 179] in
  let r812 = [R 180] in
  let r813 = Sub (r189) :: r812 in
  let r814 = R 376 :: r813 in
  let r815 = [R 189] in
  let r816 = [R 190] in
  let r817 = Sub (r189) :: r816 in
  let r818 = R 376 :: r817 in
  let r819 = [R 167] in
  let r820 = [R 168] in
  let r821 = Sub (r189) :: r820 in
  let r822 = R 376 :: r821 in
  let r823 = [R 165] in
  let r824 = [R 166] in
  let r825 = Sub (r189) :: r824 in
  let r826 = R 376 :: r825 in
  let r827 = [R 209] in
  let r828 = [R 210] in
  let r829 = Sub (r189) :: r828 in
  let r830 = R 376 :: r829 in
  let r831 = [R 163] in
  let r832 = [R 164] in
  let r833 = Sub (r189) :: r832 in
  let r834 = R 376 :: r833 in
  let r835 = [R 191] in
  let r836 = [R 192] in
  let r837 = Sub (r189) :: r836 in
  let r838 = R 376 :: r837 in
  let r839 = [R 187] in
  let r840 = [R 188] in
  let r841 = Sub (r189) :: r840 in
  let r842 = R 376 :: r841 in
  let r843 = [R 195] in
  let r844 = [R 196] in
  let r845 = Sub (r189) :: r844 in
  let r846 = R 376 :: r845 in
  let r847 = [R 197] in
  let r848 = [R 198] in
  let r849 = Sub (r189) :: r848 in
  let r850 = R 376 :: r849 in
  let r851 = [R 199] in
  let r852 = [R 200] in
  let r853 = Sub (r189) :: r852 in
  let r854 = R 376 :: r853 in
  let r855 = [R 656] in
  let r856 = [R 659] in
  let r857 = [R 660] in
  let r858 = S (T T_RPAREN) :: r857 in
  let r859 = Sub (r200) :: r858 in
  let r860 = [R 658] in
  let r861 = [R 657] in
  let r862 = Sub (r189) :: r861 in
  let r863 = R 376 :: r862 in
  let r864 = [R 201] in
  let r865 = [R 202] in
  let r866 = Sub (r189) :: r865 in
  let r867 = R 376 :: r866 in
  let r868 = [R 19] in
  let r869 = R 382 :: r868 in
  let r870 = Sub (r571) :: r869 in
  let r871 = [R 906] in
  let r872 = Sub (r3) :: r871 in
  let r873 = [R 350] in
  let r874 = Sub (r3) :: r873 in
  let r875 = S (T T_EQUAL) :: r874 in
  let r876 = Sub (r34) :: r875 in
  let r877 = S (T T_DOT) :: r876 in
  let r878 = [R 349] in
  let r879 = Sub (r3) :: r878 in
  let r880 = S (T T_EQUAL) :: r879 in
  let r881 = Sub (r34) :: r880 in
  let r882 = [R 348] in
  let r883 = Sub (r3) :: r882 in
  let r884 = [R 907] in
  let r885 = Sub (r678) :: r884 in
  let r886 = S (T T_EQUAL) :: r885 in
  let r887 = [R 352] in
  let r888 = Sub (r3) :: r887 in
  let r889 = S (T T_EQUAL) :: r888 in
  let r890 = [R 351] in
  let r891 = Sub (r3) :: r890 in
  let r892 = [R 689] in
  let r893 = [R 329] in
  let r894 = [R 330] in
  let r895 = S (T T_RPAREN) :: r894 in
  let r896 = Sub (r34) :: r895 in
  let r897 = S (T T_COLON) :: r896 in
  let r898 = [R 328] in
  let r899 = [R 614] in
  let r900 = [R 612] in
  let r901 = [R 383] in
  let r902 = [R 221] in
  let r903 = [R 222] in
  let r904 = Sub (r189) :: r903 in
  let r905 = R 376 :: r904 in
  let r906 = [R 788] in
  let r907 = S (T T_RBRACKET) :: r906 in
  let r908 = Sub (r603) :: r907 in
  let r909 = [R 229] in
  let r910 = [R 230] in
  let r911 = Sub (r189) :: r910 in
  let r912 = R 376 :: r911 in
  let r913 = [R 786] in
  let r914 = S (T T_RBRACE) :: r913 in
  let r915 = Sub (r603) :: r914 in
  let r916 = [R 225] in
  let r917 = [R 226] in
  let r918 = Sub (r189) :: r917 in
  let r919 = R 376 :: r918 in
  let r920 = [R 215] in
  let r921 = [R 216] in
  let r922 = Sub (r189) :: r921 in
  let r923 = R 376 :: r922 in
  let r924 = [R 783] in
  let r925 = S (T T_RBRACKET) :: r924 in
  let r926 = Sub (r3) :: r925 in
  let r927 = [R 219] in
  let r928 = [R 220] in
  let r929 = Sub (r189) :: r928 in
  let r930 = R 376 :: r929 in
  let r931 = [R 782] in
  let r932 = S (T T_RBRACE) :: r931 in
  let r933 = Sub (r3) :: r932 in
  let r934 = [R 217] in
  let r935 = [R 218] in
  let r936 = Sub (r189) :: r935 in
  let r937 = R 376 :: r936 in
  let r938 = [R 785] in
  let r939 = S (T T_RPAREN) :: r938 in
  let r940 = Sub (r603) :: r939 in
  let r941 = S (T T_LPAREN) :: r940 in
  let r942 = [R 223] in
  let r943 = [R 224] in
  let r944 = Sub (r189) :: r943 in
  let r945 = R 376 :: r944 in
  let r946 = [R 789] in
  let r947 = S (T T_RBRACKET) :: r946 in
  let r948 = Sub (r603) :: r947 in
  let r949 = [R 231] in
  let r950 = [R 232] in
  let r951 = Sub (r189) :: r950 in
  let r952 = R 376 :: r951 in
  let r953 = [R 787] in
  let r954 = S (T T_RBRACE) :: r953 in
  let r955 = Sub (r603) :: r954 in
  let r956 = [R 227] in
  let r957 = [R 228] in
  let r958 = Sub (r189) :: r957 in
  let r959 = R 376 :: r958 in
  let r960 = [R 213] in
  let r961 = [R 214] in
  let r962 = Sub (r189) :: r961 in
  let r963 = R 376 :: r962 in
  let r964 = [R 662] in
  let r965 = Sub (r189) :: r964 in
  let r966 = R 376 :: r965 in
  let r967 = [R 154] in
  let r968 = Sub (r189) :: r967 in
  let r969 = R 376 :: r968 in
  let r970 = [R 151] in
  let r971 = [R 152] in
  let r972 = Sub (r189) :: r971 in
  let r973 = R 376 :: r972 in
  let r974 = [R 149] in
  let r975 = [R 150] in
  let r976 = Sub (r189) :: r975 in
  let r977 = R 376 :: r976 in
  let r978 = [R 669] in
  let r979 = [R 670] in
  let r980 = S (T T_RPAREN) :: r979 in
  let r981 = Sub (r200) :: r980 in
  let r982 = [R 668] in
  let r983 = [R 667] in
  let r984 = Sub (r189) :: r983 in
  let r985 = R 376 :: r984 in
  let r986 = [R 363] in
  let r987 = Sub (r3) :: r986 in
  let r988 = [R 365] in
  let r989 = [R 808] in
  let r990 = [R 820] in
  let r991 = [R 819] in
  let r992 = [R 823] in
  let r993 = [R 822] in
  let r994 = S (T T_LIDENT) :: r609 in
  let r995 = [R 809] in
  let r996 = S (T T_RBRACE) :: r995 in
  let r997 = S (T T_GREATER) :: r996 in
  let r998 = [R 816] in
  let r999 = S (T T_RBRACE) :: r998 in
  let r1000 = [R 638] in
  let r1001 = Sub (r614) :: r1000 in
  let r1002 = [R 792] in
  let r1003 = [R 571] in
  let r1004 = Sub (r189) :: r1003 in
  let r1005 = R 376 :: r1004 in
  let r1006 = [R 805] in
  let r1007 = S (T T_RBRACE) :: r1006 in
  let r1008 = [R 125] in
  let r1009 = Sub (r189) :: r1008 in
  let r1010 = R 376 :: r1009 in
  let r1011 = [R 131] in
  let r1012 = [R 127] in
  let r1013 = [R 129] in
  let r1014 = [R 130] in
  let r1015 = [R 126] in
  let r1016 = [R 128] in
  let r1017 = [R 462] in
  let r1018 = S (N N_module_expr) :: r1017 in
  let r1019 = S (T T_EQUAL) :: r1018 in
  let r1020 = [R 419] in
  let r1021 = R 382 :: r1020 in
  let r1022 = Sub (r1019) :: r1021 in
  let r1023 = Sub (r336) :: r1022 in
  let r1024 = R 376 :: r1023 in
  let r1025 = [R 489] in
  let r1026 = R 382 :: r1025 in
  let r1027 = R 572 :: r1026 in
  let r1028 = Sub (r59) :: r1027 in
  let r1029 = R 376 :: r1028 in
  let r1030 = R 132 :: r1029 in
  let r1031 = [R 573] in
  let r1032 = [R 414] in
  let r1033 = R 372 :: r1032 in
  let r1034 = R 382 :: r1033 in
  let r1035 = Sub (r1019) :: r1034 in
  let r1036 = [R 463] in
  let r1037 = S (N N_module_expr) :: r1036 in
  let r1038 = S (T T_EQUAL) :: r1037 in
  let r1039 = [R 373] in
  let r1040 = R 372 :: r1039 in
  let r1041 = R 382 :: r1040 in
  let r1042 = Sub (r1019) :: r1041 in
  let r1043 = Sub (r336) :: r1042 in
  let r1044 = [R 464] in
  let r1045 = [R 273] in
  let r1046 = S (T T_RBRACKET) :: r1045 in
  let r1047 = Sub (r17) :: r1046 in
  let r1048 = [R 620] in
  let r1049 = [R 621] in
  let r1050 = [R 139] in
  let r1051 = S (T T_RBRACKET) :: r1050 in
  let r1052 = Sub (r19) :: r1051 in
  let r1053 = [R 911] in
  let r1054 = R 382 :: r1053 in
  let r1055 = S (N N_module_expr) :: r1054 in
  let r1056 = R 376 :: r1055 in
  let r1057 = [R 502] in
  let r1058 = S (T T_STRING) :: r1057 in
  let r1059 = [R 627] in
  let r1060 = R 382 :: r1059 in
  let r1061 = Sub (r1058) :: r1060 in
  let r1062 = S (T T_EQUAL) :: r1061 in
  let r1063 = Sub (r36) :: r1062 in
  let r1064 = S (T T_COLON) :: r1063 in
  let r1065 = Sub (r24) :: r1064 in
  let r1066 = R 376 :: r1065 in
  let r1067 = [R 749] in
  let r1068 = R 382 :: r1067 in
  let r1069 = R 376 :: r1068 in
  let r1070 = R 255 :: r1069 in
  let r1071 = Sub (r100) :: r1070 in
  let r1072 = R 376 :: r1071 in
  let r1073 = R 132 :: r1072 in
  let r1074 = [R 103] in
  let r1075 = Sub (r26) :: r1074 in
  let r1076 = [R 256] in
  let r1077 = [R 289] in
  let r1078 = R 376 :: r1077 in
  let r1079 = Sub (r256) :: r1078 in
  let r1080 = S (T T_COLON) :: r1079 in
  let r1081 = S (T T_LIDENT) :: r1080 in
  let r1082 = R 492 :: r1081 in
  let r1083 = [R 291] in
  let r1084 = Sub (r1082) :: r1083 in
  let r1085 = [R 105] in
  let r1086 = S (T T_RBRACE) :: r1085 in
  let r1087 = [R 290] in
  let r1088 = R 376 :: r1087 in
  let r1089 = S (T T_SEMI) :: r1088 in
  let r1090 = R 376 :: r1089 in
  let r1091 = Sub (r256) :: r1090 in
  let r1092 = S (T T_COLON) :: r1091 in
  let r1093 = [R 104] in
  let r1094 = Sub (r26) :: r1093 in
  let r1095 = Sub (r98) :: r430 in
  let r1096 = [R 905] in
  let r1097 = R 382 :: r1096 in
  let r1098 = R 376 :: r1097 in
  let r1099 = S (T T_COLONCOLON) :: r469 in
  let r1100 = [R 259] in
  let r1101 = [R 260] in
  let r1102 = Sub (r26) :: r1101 in
  let r1103 = [R 258] in
  let r1104 = Sub (r26) :: r1103 in
  let r1105 = [R 257] in
  let r1106 = Sub (r26) :: r1105 in
  let r1107 = [R 618] in
  let r1108 = [R 385] in
  let r1109 = [R 524] in
  let r1110 = R 382 :: r1109 in
  let r1111 = Sub (r268) :: r1110 in
  let r1112 = R 376 :: r1111 in
  let r1113 = [R 525] in
  let r1114 = R 382 :: r1113 in
  let r1115 = Sub (r268) :: r1114 in
  let r1116 = R 376 :: r1115 in
  let r1117 = [R 465] in
  let r1118 = S (N N_module_type) :: r1117 in
  let r1119 = S (T T_COLON) :: r1118 in
  let r1120 = [R 760] in
  let r1121 = R 382 :: r1120 in
  let r1122 = Sub (r1119) :: r1121 in
  let r1123 = Sub (r336) :: r1122 in
  let r1124 = R 376 :: r1123 in
  let r1125 = [R 490] in
  let r1126 = R 382 :: r1125 in
  let r1127 = S (N N_module_type) :: r1126 in
  let r1128 = S (T T_COLONEQUAL) :: r1127 in
  let r1129 = Sub (r59) :: r1128 in
  let r1130 = R 376 :: r1129 in
  let r1131 = [R 478] in
  let r1132 = R 382 :: r1131 in
  let r1133 = [R 763] in
  let r1134 = R 374 :: r1133 in
  let r1135 = R 382 :: r1134 in
  let r1136 = S (N N_module_type) :: r1135 in
  let r1137 = S (T T_COLON) :: r1136 in
  let r1138 = [R 375] in
  let r1139 = R 374 :: r1138 in
  let r1140 = R 382 :: r1139 in
  let r1141 = S (N N_module_type) :: r1140 in
  let r1142 = S (T T_COLON) :: r1141 in
  let r1143 = Sub (r336) :: r1142 in
  let r1144 = [R 761] in
  let r1145 = R 382 :: r1144 in
  let r1146 = [R 466] in
  let r1147 = [R 767] in
  let r1148 = R 382 :: r1147 in
  let r1149 = S (N N_module_type) :: r1148 in
  let r1150 = R 376 :: r1149 in
  let r1151 = S (T T_QUOTED_STRING_EXPR) :: r57 in
  let r1152 = [R 71] in
  let r1153 = Sub (r1151) :: r1152 in
  let r1154 = [R 81] in
  let r1155 = Sub (r1153) :: r1154 in
  let r1156 = [R 768] in
  let r1157 = R 368 :: r1156 in
  let r1158 = R 382 :: r1157 in
  let r1159 = Sub (r1155) :: r1158 in
  let r1160 = S (T T_COLON) :: r1159 in
  let r1161 = S (T T_LIDENT) :: r1160 in
  let r1162 = R 140 :: r1161 in
  let r1163 = R 964 :: r1162 in
  let r1164 = R 376 :: r1163 in
  let r1165 = [R 85] in
  let r1166 = R 370 :: r1165 in
  let r1167 = R 382 :: r1166 in
  let r1168 = Sub (r1153) :: r1167 in
  let r1169 = S (T T_EQUAL) :: r1168 in
  let r1170 = S (T T_LIDENT) :: r1169 in
  let r1171 = R 140 :: r1170 in
  let r1172 = R 964 :: r1171 in
  let r1173 = R 376 :: r1172 in
  let r1174 = [R 141] in
  let r1175 = S (T T_RBRACKET) :: r1174 in
  let r1176 = [R 72] in
  let r1177 = S (T T_END) :: r1176 in
  let r1178 = R 391 :: r1177 in
  let r1179 = R 62 :: r1178 in
  let r1180 = [R 61] in
  let r1181 = S (T T_RPAREN) :: r1180 in
  let r1182 = [R 64] in
  let r1183 = R 382 :: r1182 in
  let r1184 = Sub (r34) :: r1183 in
  let r1185 = S (T T_COLON) :: r1184 in
  let r1186 = S (T T_LIDENT) :: r1185 in
  let r1187 = R 494 :: r1186 in
  let r1188 = [R 65] in
  let r1189 = R 382 :: r1188 in
  let r1190 = Sub (r36) :: r1189 in
  let r1191 = S (T T_COLON) :: r1190 in
  let r1192 = S (T T_LIDENT) :: r1191 in
  let r1193 = R 630 :: r1192 in
  let r1194 = [R 63] in
  let r1195 = R 382 :: r1194 in
  let r1196 = Sub (r1153) :: r1195 in
  let r1197 = [R 74] in
  let r1198 = Sub (r1153) :: r1197 in
  let r1199 = S (T T_IN) :: r1198 in
  let r1200 = Sub (r620) :: r1199 in
  let r1201 = R 376 :: r1200 in
  let r1202 = [R 75] in
  let r1203 = Sub (r1153) :: r1202 in
  let r1204 = S (T T_IN) :: r1203 in
  let r1205 = Sub (r620) :: r1204 in
  let r1206 = [R 718] in
  let r1207 = Sub (r34) :: r1206 in
  let r1208 = [R 70] in
  let r1209 = Sub (r261) :: r1208 in
  let r1210 = S (T T_RBRACKET) :: r1209 in
  let r1211 = Sub (r1207) :: r1210 in
  let r1212 = [R 719] in
  let r1213 = [R 102] in
  let r1214 = Sub (r34) :: r1213 in
  let r1215 = S (T T_EQUAL) :: r1214 in
  let r1216 = Sub (r34) :: r1215 in
  let r1217 = [R 66] in
  let r1218 = R 382 :: r1217 in
  let r1219 = Sub (r1216) :: r1218 in
  let r1220 = [R 67] in
  let r1221 = [R 392] in
  let r1222 = [R 371] in
  let r1223 = R 370 :: r1222 in
  let r1224 = R 382 :: r1223 in
  let r1225 = Sub (r1153) :: r1224 in
  let r1226 = S (T T_EQUAL) :: r1225 in
  let r1227 = S (T T_LIDENT) :: r1226 in
  let r1228 = R 140 :: r1227 in
  let r1229 = R 964 :: r1228 in
  let r1230 = [R 83] in
  let r1231 = Sub (r1155) :: r1230 in
  let r1232 = S (T T_MINUSGREATER) :: r1231 in
  let r1233 = Sub (r28) :: r1232 in
  let r1234 = [R 84] in
  let r1235 = Sub (r1155) :: r1234 in
  let r1236 = [R 82] in
  let r1237 = Sub (r1155) :: r1236 in
  let r1238 = S (T T_MINUSGREATER) :: r1237 in
  let r1239 = [R 369] in
  let r1240 = R 368 :: r1239 in
  let r1241 = R 382 :: r1240 in
  let r1242 = Sub (r1155) :: r1241 in
  let r1243 = S (T T_COLON) :: r1242 in
  let r1244 = S (T T_LIDENT) :: r1243 in
  let r1245 = R 140 :: r1244 in
  let r1246 = R 964 :: r1245 in
  let r1247 = [R 386] in
  let r1248 = [R 751] in
  let r1249 = [R 755] in
  let r1250 = [R 379] in
  let r1251 = R 378 :: r1250 in
  let r1252 = R 382 :: r1251 in
  let r1253 = R 695 :: r1252 in
  let r1254 = R 933 :: r1253 in
  let r1255 = S (T T_LIDENT) :: r1254 in
  let r1256 = R 937 :: r1255 in
  let r1257 = [R 756] in
  let r1258 = [R 381] in
  let r1259 = R 380 :: r1258 in
  let r1260 = R 382 :: r1259 in
  let r1261 = R 695 :: r1260 in
  let r1262 = Sub (r139) :: r1261 in
  let r1263 = S (T T_COLONEQUAL) :: r1262 in
  let r1264 = S (T T_LIDENT) :: r1263 in
  let r1265 = R 937 :: r1264 in
  let r1266 = [R 514] in
  let r1267 = S (T T_RBRACE) :: r1266 in
  let r1268 = [R 518] in
  let r1269 = [R 261] in
  let r1270 = R 376 :: r1269 in
  let r1271 = R 255 :: r1270 in
  let r1272 = Sub (r100) :: r1271 in
  let r1273 = [R 512] in
  let r1274 = [R 513] in
  let r1275 = [R 517] in
  let r1276 = S (T T_RBRACE) :: r1275 in
  let r1277 = [R 516] in
  let r1278 = S (T T_RBRACE) :: r1277 in
  let r1279 = [R 43] in
  let r1280 = Sub (r1151) :: r1279 in
  let r1281 = [R 52] in
  let r1282 = Sub (r1280) :: r1281 in
  let r1283 = S (T T_EQUAL) :: r1282 in
  let r1284 = [R 416] in
  let r1285 = R 366 :: r1284 in
  let r1286 = R 382 :: r1285 in
  let r1287 = Sub (r1283) :: r1286 in
  let r1288 = S (T T_LIDENT) :: r1287 in
  let r1289 = R 140 :: r1288 in
  let r1290 = R 964 :: r1289 in
  let r1291 = R 376 :: r1290 in
  let r1292 = [R 80] in
  let r1293 = S (T T_END) :: r1292 in
  let r1294 = R 393 :: r1293 in
  let r1295 = R 60 :: r1294 in
  let r1296 = S (T T_EQUAL) :: r872 in
  let r1297 = [R 435] in
  let r1298 = Sub (r1296) :: r1297 in
  let r1299 = S (T T_LIDENT) :: r1298 in
  let r1300 = R 628 :: r1299 in
  let r1301 = R 376 :: r1300 in
  let r1302 = [R 47] in
  let r1303 = R 382 :: r1302 in
  let r1304 = [R 436] in
  let r1305 = Sub (r1296) :: r1304 in
  let r1306 = S (T T_LIDENT) :: r1305 in
  let r1307 = R 628 :: r1306 in
  let r1308 = [R 438] in
  let r1309 = Sub (r3) :: r1308 in
  let r1310 = S (T T_EQUAL) :: r1309 in
  let r1311 = [R 440] in
  let r1312 = Sub (r3) :: r1311 in
  let r1313 = S (T T_EQUAL) :: r1312 in
  let r1314 = Sub (r34) :: r1313 in
  let r1315 = S (T T_DOT) :: r1314 in
  let r1316 = [R 434] in
  let r1317 = Sub (r36) :: r1316 in
  let r1318 = S (T T_COLON) :: r1317 in
  let r1319 = [R 437] in
  let r1320 = Sub (r3) :: r1319 in
  let r1321 = S (T T_EQUAL) :: r1320 in
  let r1322 = [R 439] in
  let r1323 = Sub (r3) :: r1322 in
  let r1324 = S (T T_EQUAL) :: r1323 in
  let r1325 = Sub (r34) :: r1324 in
  let r1326 = S (T T_DOT) :: r1325 in
  let r1327 = [R 49] in
  let r1328 = R 382 :: r1327 in
  let r1329 = Sub (r3) :: r1328 in
  let r1330 = [R 44] in
  let r1331 = R 382 :: r1330 in
  let r1332 = R 563 :: r1331 in
  let r1333 = Sub (r1280) :: r1332 in
  let r1334 = [R 45] in
  let r1335 = R 382 :: r1334 in
  let r1336 = R 563 :: r1335 in
  let r1337 = Sub (r1280) :: r1336 in
  let r1338 = [R 76] in
  let r1339 = S (T T_RPAREN) :: r1338 in
  let r1340 = [R 39] in
  let r1341 = Sub (r1280) :: r1340 in
  let r1342 = S (T T_IN) :: r1341 in
  let r1343 = Sub (r620) :: r1342 in
  let r1344 = R 376 :: r1343 in
  let r1345 = [R 356] in
  let r1346 = R 382 :: r1345 in
  let r1347 = Sub (r571) :: r1346 in
  let r1348 = R 635 :: r1347 in
  let r1349 = R 376 :: r1348 in
  let r1350 = [R 40] in
  let r1351 = Sub (r1280) :: r1350 in
  let r1352 = S (T T_IN) :: r1351 in
  let r1353 = Sub (r620) :: r1352 in
  let r1354 = [R 78] in
  let r1355 = Sub (r496) :: r1354 in
  let r1356 = S (T T_RBRACKET) :: r1355 in
  let r1357 = [R 55] in
  let r1358 = Sub (r1280) :: r1357 in
  let r1359 = S (T T_MINUSGREATER) :: r1358 in
  let r1360 = Sub (r670) :: r1359 in
  let r1361 = [R 37] in
  let r1362 = Sub (r1360) :: r1361 in
  let r1363 = [R 38] in
  let r1364 = Sub (r1280) :: r1363 in
  let r1365 = [R 355] in
  let r1366 = R 382 :: r1365 in
  let r1367 = Sub (r571) :: r1366 in
  let r1368 = [R 79] in
  let r1369 = S (T T_RPAREN) :: r1368 in
  let r1370 = [R 564] in
  let r1371 = [R 48] in
  let r1372 = R 382 :: r1371 in
  let r1373 = Sub (r1216) :: r1372 in
  let r1374 = [R 50] in
  let r1375 = [R 394] in
  let r1376 = [R 53] in
  let r1377 = Sub (r1280) :: r1376 in
  let r1378 = S (T T_EQUAL) :: r1377 in
  let r1379 = [R 54] in
  let r1380 = [R 367] in
  let r1381 = R 366 :: r1380 in
  let r1382 = R 382 :: r1381 in
  let r1383 = Sub (r1283) :: r1382 in
  let r1384 = S (T T_LIDENT) :: r1383 in
  let r1385 = R 140 :: r1384 in
  let r1386 = R 964 :: r1385 in
  let r1387 = [R 390] in
  let r1388 = [R 410] in
  let r1389 = [R 909] in
  let r1390 = R 387 :: r1389 in
  let r1391 = [R 212] in
  let r1392 = Sub (r189) :: r1391 in
  let r1393 = R 376 :: r1392 in
  let r1394 = [R 817] in
  let r1395 = [R 795] in
  let r1396 = S (T T_RPAREN) :: r1395 in
  let r1397 = S (N N_module_expr) :: r1396 in
  let r1398 = R 376 :: r1397 in
  let r1399 = [R 796] in
  let r1400 = S (T T_RPAREN) :: r1399 in
  let r1401 = [R 780] in
  let r1402 = [R 962] in
  let r1403 = Sub (r3) :: r1402 in
  let r1404 = [R 958] in
  let r1405 = Sub (r34) :: r1404 in
  let r1406 = S (T T_COLON) :: r1405 in
  let r1407 = [R 961] in
  let r1408 = Sub (r3) :: r1407 in
  let r1409 = [R 389] in
  let r1410 = R 387 :: r1409 in
  let r1411 = [R 519] in
  let r1412 = [R 679] in
  let r1413 = [R 680] in
  let r1414 = S (T T_RPAREN) :: r1413 in
  let r1415 = Sub (r200) :: r1414 in
  let r1416 = [R 678] in
  let r1417 = [R 677] in
  let r1418 = Sub (r189) :: r1417 in
  let r1419 = R 376 :: r1418 in
  let r1420 = [R 674] in
  let r1421 = [R 675] in
  let r1422 = S (T T_RPAREN) :: r1421 in
  let r1423 = Sub (r200) :: r1422 in
  let r1424 = [R 673] in
  let r1425 = [R 672] in
  let r1426 = Sub (r189) :: r1425 in
  let r1427 = R 376 :: r1426 in
  let r1428 = [R 136] in
  let r1429 = R 376 :: r1428 in
  let r1430 = [R 137] in
  let r1431 = R 376 :: r1430 in
  let r1432 = [R 244] in
  let r1433 = Sub (r30) :: r1432 in
  let r1434 = S (T T_MINUSGREATER) :: r1433 in
  let r1435 = S (T T_RPAREN) :: r1434 in
  let r1436 = Sub (r34) :: r1435 in
  let r1437 = [R 245] in
  let r1438 = Sub (r30) :: r1437 in
  let r1439 = [R 248] in
  let r1440 = [R 246] in
  let r1441 = Sub (r30) :: r1440 in
  let r1442 = S (T T_MINUSGREATER) :: r1441 in
  let r1443 = S (T T_RPAREN) :: r1442 in
  let r1444 = Sub (r34) :: r1443 in
  let r1445 = [R 515] in
  let r1446 = S (T T_RBRACE) :: r1445 in
  let r1447 = [R 264] in
  let r1448 = R 382 :: r1447 in
  let r1449 = R 695 :: r1448 in
  let r1450 = [R 263] in
  let r1451 = R 382 :: r1450 in
  let r1452 = R 695 :: r1451 in
  let r1453 = [R 269] in
  let r1454 = [R 272] in
  let r1455 = [R 446] in
  let r1456 = [R 449] in
  let r1457 = S (T T_RPAREN) :: r1456 in
  let r1458 = S (T T_COLONCOLON) :: r1457 in
  let r1459 = S (T T_LPAREN) :: r1458 in
  let r1460 = [R 585] in
  let r1461 = [R 586] in
  let r1462 = [R 587] in
  let r1463 = [R 588] in
  let r1464 = [R 589] in
  let r1465 = [R 590] in
  let r1466 = [R 591] in
  let r1467 = [R 592] in
  let r1468 = [R 593] in
  let r1469 = [R 594] in
  let r1470 = [R 595] in
  let r1471 = [R 917] in
  let r1472 = [R 926] in
  let r1473 = [R 396] in
  let r1474 = [R 924] in
  let r1475 = S (T T_SEMISEMI) :: r1474 in
  let r1476 = [R 925] in
  let r1477 = [R 398] in
  let r1478 = [R 401] in
  let r1479 = [R 400] in
  let r1480 = [R 399] in
  let r1481 = R 397 :: r1480 in
  let r1482 = [R 953] in
  let r1483 = S (T T_EOF) :: r1482 in
  let r1484 = R 397 :: r1483 in
  let r1485 = [R 952] in
  function
  | 0 | 2195 | 2199 | 2217 | 2221 | 2225 | 2229 | 2233 | 2237 | 2241 | 2245 | 2249 | 2253 | 2258 | 2278 -> Nothing
  | 2194 -> One ([R 0])
  | 2198 -> One ([R 1])
  | 2204 -> One ([R 2])
  | 2218 -> One ([R 3])
  | 2222 -> One ([R 4])
  | 2228 -> One ([R 5])
  | 2230 -> One ([R 6])
  | 2234 -> One ([R 7])
  | 2238 -> One ([R 8])
  | 2242 -> One ([R 9])
  | 2246 -> One ([R 10])
  | 2252 -> One ([R 11])
  | 2256 -> One ([R 12])
  | 2268 -> One ([R 13])
  | 2288 -> One ([R 14])
  | 625 -> One ([R 15])
  | 624 -> One ([R 16])
  | 2212 -> One ([R 20])
  | 2214 -> One ([R 21])
  | 267 -> One ([R 22])
  | 245 -> One ([R 23])
  | 278 -> One ([R 24])
  | 1886 -> One ([R 36])
  | 1890 -> One ([R 41])
  | 1887 -> One ([R 42])
  | 1926 -> One ([R 51])
  | 1893 -> One ([R 56])
  | 1682 -> One ([R 68])
  | 1662 -> One ([R 69])
  | 1664 -> One ([R 73])
  | 1888 -> One ([R 77])
  | 478 -> One ([R 88])
  | 210 -> One ([R 89])
  | 476 -> One ([R 90])
  | 159 -> One ([R 94])
  | 158 | 1500 -> One ([R 95])
  | 1527 -> One ([R 98])
  | 1766 -> One ([R 106])
  | 1770 -> One ([R 107])
  | 270 -> One ([R 109])
  | 257 -> One ([R 110])
  | 264 -> One ([R 111])
  | 266 -> One ([R 112])
  | 1272 -> One ([R 122])
  | 1 -> One (R 132 :: r9)
  | 62 -> One (R 132 :: r42)
  | 192 -> One (R 132 :: r194)
  | 214 -> One (R 132 :: r223)
  | 382 -> One (R 132 :: r340)
  | 470 -> One (R 132 :: r410)
  | 508 -> One (R 132 :: r433)
  | 626 -> One (R 132 :: r499)
  | 635 -> One (R 132 :: r507)
  | 729 -> One (R 132 :: r590)
  | 730 -> One (R 132 :: r594)
  | 751 -> One (R 132 :: r624)
  | 754 -> One (R 132 :: r627)
  | 767 -> One (R 132 :: r638)
  | 804 -> One (R 132 :: r661)
  | 807 -> One (R 132 :: r664)
  | 813 -> One (R 132 :: r684)
  | 855 -> One (R 132 :: r704)
  | 876 -> One (R 132 :: r728)
  | 881 -> One (R 132 :: r731)
  | 911 -> One (R 132 :: r746)
  | 931 -> One (R 132 :: r756)
  | 947 -> One (R 132 :: r765)
  | 961 -> One (R 132 :: r772)
  | 967 -> One (R 132 :: r776)
  | 976 -> One (R 132 :: r780)
  | 987 -> One (R 132 :: r786)
  | 993 -> One (R 132 :: r790)
  | 999 -> One (R 132 :: r794)
  | 1005 -> One (R 132 :: r798)
  | 1011 -> One (R 132 :: r802)
  | 1017 -> One (R 132 :: r806)
  | 1023 -> One (R 132 :: r810)
  | 1029 -> One (R 132 :: r814)
  | 1035 -> One (R 132 :: r818)
  | 1041 -> One (R 132 :: r822)
  | 1047 -> One (R 132 :: r826)
  | 1053 -> One (R 132 :: r830)
  | 1059 -> One (R 132 :: r834)
  | 1065 -> One (R 132 :: r838)
  | 1071 -> One (R 132 :: r842)
  | 1077 -> One (R 132 :: r846)
  | 1083 -> One (R 132 :: r850)
  | 1089 -> One (R 132 :: r854)
  | 1103 -> One (R 132 :: r863)
  | 1109 -> One (R 132 :: r867)
  | 1179 -> One (R 132 :: r905)
  | 1188 -> One (R 132 :: r912)
  | 1197 -> One (R 132 :: r919)
  | 1207 -> One (R 132 :: r923)
  | 1216 -> One (R 132 :: r930)
  | 1225 -> One (R 132 :: r937)
  | 1236 -> One (R 132 :: r945)
  | 1245 -> One (R 132 :: r952)
  | 1254 -> One (R 132 :: r959)
  | 1261 -> One (R 132 :: r963)
  | 1299 -> One (R 132 :: r966)
  | 1315 -> One (R 132 :: r969)
  | 1320 -> One (R 132 :: r973)
  | 1327 -> One (R 132 :: r977)
  | 1349 -> One (R 132 :: r985)
  | 1400 -> One (R 132 :: r1005)
  | 1415 -> One (R 132 :: r1010)
  | 1440 -> One (R 132 :: r1024)
  | 1481 -> One (R 132 :: r1056)
  | 1486 -> One (R 132 :: r1066)
  | 1550 -> One (R 132 :: r1112)
  | 1551 -> One (R 132 :: r1116)
  | 1560 -> One (R 132 :: r1124)
  | 1597 -> One (R 132 :: r1150)
  | 1606 -> One (R 132 :: r1164)
  | 1607 -> One (R 132 :: r1173)
  | 1803 -> One (R 132 :: r1291)
  | 1988 -> One (R 132 :: r1393)
  | 1997 -> One (R 132 :: r1398)
  | 2064 -> One (R 132 :: r1419)
  | 2079 -> One (R 132 :: r1427)
  | 265 -> One ([R 138])
  | 916 -> One ([R 144])
  | 1267 -> One ([R 159])
  | 937 -> One ([R 160])
  | 974 -> One ([R 161])
  | 954 -> One ([R 162])
  | 972 -> One ([R 233])
  | 981 -> One ([R 238])
  | 985 -> One ([R 239])
  | 394 -> One ([R 254])
  | 115 -> One ([R 267])
  | 92 -> One (R 270 :: r53)
  | 96 -> One (R 270 :: r55)
  | 742 -> One ([R 277])
  | 750 -> One ([R 278])
  | 744 -> One ([R 280])
  | 901 -> One ([R 281])
  | 903 -> One ([R 284])
  | 896 -> One ([R 285])
  | 1517 -> One ([R 292])
  | 1518 -> One ([R 293])
  | 1266 -> One ([R 297])
  | 534 -> One ([R 303])
  | 560 -> One ([R 307])
  | 571 -> One ([R 311])
  | 610 -> One ([R 315])
  | 597 -> One ([R 319])
  | 680 -> One ([R 323])
  | 1161 -> One ([R 327])
  | 707 -> One ([R 331])
  | 693 -> One ([R 335])
  | 662 -> One ([R 339])
  | 517 -> One ([R 343])
  | 661 -> One ([R 344])
  | 1166 -> One ([R 345])
  | 1134 -> One ([R 347])
  | 1171 -> One ([R 354])
  | 1891 -> One ([R 357])
  | 819 -> One ([R 358])
  | 1987 -> One ([R 360])
  | 129 -> One (R 376 :: r84)
  | 179 -> One (R 376 :: r163)
  | 312 -> One (R 376 :: r304)
  | 318 -> One (R 376 :: r308)
  | 325 -> One (R 376 :: r309)
  | 389 -> One (R 376 :: r346)
  | 618 -> One (R 376 :: r494)
  | 734 -> One (R 376 :: r601)
  | 770 -> One (R 376 :: r642)
  | 1114 -> One (R 376 :: r870)
  | 1461 -> One (R 376 :: r1043)
  | 1579 -> One (R 376 :: r1143)
  | 1618 -> One (R 376 :: r1179)
  | 1624 -> One (R 376 :: r1187)
  | 1635 -> One (R 376 :: r1193)
  | 1646 -> One (R 376 :: r1196)
  | 1650 -> One (R 376 :: r1205)
  | 1671 -> One (R 376 :: r1219)
  | 1687 -> One (R 376 :: r1229)
  | 1722 -> One (R 376 :: r1246)
  | 1744 -> One (R 376 :: r1256)
  | 1754 -> One (R 376 :: r1265)
  | 1811 -> One (R 376 :: r1295)
  | 1815 -> One (R 376 :: r1307)
  | 1855 -> One (R 376 :: r1329)
  | 1859 -> One (R 376 :: r1333)
  | 1860 -> One (R 376 :: r1337)
  | 1871 -> One (R 376 :: r1353)
  | 1879 -> One (R 376 :: r1362)
  | 1918 -> One (R 376 :: r1373)
  | 1938 -> One (R 376 :: r1386)
  | 1743 -> One (R 378 :: r1249)
  | 1965 -> One (R 378 :: r1388)
  | 1753 -> One (R 380 :: r1257)
  | 1168 -> One (R 382 :: r901)
  | 1680 -> One (R 382 :: r1220)
  | 1741 -> One (R 382 :: r1248)
  | 1924 -> One (R 382 :: r1374)
  | 1970 -> One (R 382 :: r1390)
  | 2039 -> One (R 382 :: r1410)
  | 2273 -> One (R 382 :: r1475)
  | 2284 -> One (R 382 :: r1481)
  | 2289 -> One (R 382 :: r1484)
  | 1549 -> One (R 384 :: r1108)
  | 1733 -> One (R 384 :: r1247)
  | 211 -> One (R 387 :: r215)
  | 1948 -> One (R 387 :: r1387)
  | 1683 -> One (R 391 :: r1221)
  | 1927 -> One (R 393 :: r1375)
  | 2271 -> One (R 395 :: r1473)
  | 2279 -> One (R 397 :: r1477)
  | 2280 -> One (R 397 :: r1478)
  | 2281 -> One (R 397 :: r1479)
  | 586 -> One ([R 403])
  | 590 -> One ([R 405])
  | 1967 -> One ([R 407])
  | 1957 -> One ([R 408])
  | 1947 -> One ([R 409])
  | 1955 -> One ([R 413])
  | 1959 -> One ([R 415])
  | 1968 -> One ([R 417])
  | 1956 -> One ([R 418])
  | 1958 -> One ([R 420])
  | 1309 -> One ([R 423])
  | 321 -> One ([R 424])
  | 324 -> One ([R 425])
  | 323 -> One ([R 427])
  | 322 -> One ([R 429])
  | 320 -> One ([R 430])
  | 328 -> One ([R 431])
  | 2213 -> One ([R 445])
  | 2203 -> One ([R 447])
  | 2211 -> One ([R 448])
  | 2210 -> One ([R 450])
  | 745 -> One ([R 457])
  | 748 -> One ([R 458])
  | 774 -> One ([R 469])
  | 784 -> One ([R 470])
  | 785 -> One ([R 471])
  | 783 -> One ([R 472])
  | 786 -> One ([R 474])
  | 177 -> One ([R 475])
  | 206 | 385 | 1570 -> One ([R 476])
  | 426 -> One ([R 484])
  | 396 -> One ([R 485])
  | 439 -> One ([R 488])
  | 620 | 2024 -> One ([R 493])
  | 1628 -> One ([R 495])
  | 1626 -> One ([R 496])
  | 1629 -> One ([R 497])
  | 1627 -> One ([R 498])
  | 541 -> One ([R 501])
  | 1494 -> One ([R 503])
  | 1779 -> One ([R 504])
  | 2151 -> One ([R 505])
  | 1795 -> One ([R 506])
  | 2152 -> One ([R 507])
  | 1794 -> One ([R 508])
  | 1786 -> One ([R 509])
  | 67 | 639 -> One ([R 526])
  | 75 | 793 -> One ([R 527])
  | 103 -> One ([R 528])
  | 91 -> One ([R 530])
  | 95 -> One ([R 532])
  | 99 -> One ([R 534])
  | 82 -> One ([R 535])
  | 102 | 1364 -> One ([R 536])
  | 81 -> One ([R 537])
  | 80 -> One ([R 538])
  | 79 -> One ([R 539])
  | 78 -> One ([R 540])
  | 77 -> One ([R 541])
  | 70 | 381 | 766 -> One ([R 542])
  | 69 | 765 -> One ([R 543])
  | 68 -> One ([R 544])
  | 74 | 458 | 792 -> One ([R 545])
  | 73 | 791 -> One ([R 546])
  | 66 -> One ([R 547])
  | 71 -> One ([R 548])
  | 84 -> One ([R 549])
  | 76 -> One ([R 550])
  | 83 -> One ([R 551])
  | 72 -> One ([R 552])
  | 101 -> One ([R 553])
  | 104 -> One ([R 554])
  | 100 -> One ([R 556])
  | 340 -> One ([R 557])
  | 339 -> One (R 558 :: r317)
  | 223 -> One (R 559 :: r242)
  | 224 -> One ([R 560])
  | 587 -> One (R 561 :: r471)
  | 588 -> One ([R 562])
  | 1135 -> One (R 578 :: r886)
  | 1136 -> One ([R 579])
  | 121 -> One ([R 580])
  | 520 -> One ([R 597])
  | 518 -> One ([R 598])
  | 521 -> One ([R 600])
  | 665 -> One ([R 610])
  | 666 -> One ([R 611])
  | 667 -> One ([R 613])
  | 825 -> One ([R 615])
  | 1802 -> One ([R 619])
  | 1817 | 1836 -> One ([R 629])
  | 1639 -> One ([R 631])
  | 1637 -> One ([R 632])
  | 1640 -> One ([R 633])
  | 1638 -> One ([R 634])
  | 1900 -> One (R 635 :: r1367)
  | 728 -> One ([R 636])
  | 1777 -> One ([R 639])
  | 1778 -> One ([R 640])
  | 1772 -> One ([R 641])
  | 2104 -> One ([R 643])
  | 2103 -> One ([R 644])
  | 2105 -> One ([R 645])
  | 2100 -> One ([R 646])
  | 2101 -> One ([R 647])
  | 2165 -> One ([R 649])
  | 2163 -> One ([R 650])
  | 522 -> One ([R 681])
  | 668 -> One ([R 687])
  | 885 -> One (R 693 :: r733)
  | 909 -> One ([R 694])
  | 899 -> One (R 697 :: r741)
  | 906 -> One ([R 698])
  | 849 -> One ([R 700])
  | 438 -> One ([R 701])
  | 395 -> One ([R 702])
  | 1269 -> One ([R 703])
  | 1268 -> One ([R 704])
  | 362 -> One ([R 706])
  | 332 -> One ([R 730])
  | 1174 -> One ([R 733])
  | 935 -> One ([R 735])
  | 1175 -> One ([R 736])
  | 936 -> One ([R 737])
  | 1406 -> One ([R 739])
  | 1407 -> One ([R 740])
  | 581 -> One ([R 742])
  | 582 -> One ([R 743])
  | 1386 -> One ([R 745])
  | 1387 -> One ([R 746])
  | 1797 -> One ([R 752])
  | 1732 -> One ([R 753])
  | 1735 -> One ([R 754])
  | 1734 -> One ([R 759])
  | 1739 -> One ([R 762])
  | 1738 -> One ([R 764])
  | 1737 -> One ([R 765])
  | 1736 -> One ([R 766])
  | 1798 -> One ([R 769])
  | 379 -> One ([R 772])
  | 376 -> One ([R 774])
  | 892 -> One ([R 798])
  | 758 -> One ([R 799])
  | 895 -> One ([R 800])
  | 894 | 973 -> One ([R 801])
  | 760 | 953 -> One ([R 802])
  | 1259 | 1298 -> One ([R 807])
  | 893 -> One ([R 812])
  | 479 -> One ([R 835])
  | 483 -> One ([R 838])
  | 484 -> One ([R 842])
  | 506 -> One ([R 844])
  | 488 -> One ([R 845])
  | 583 -> One ([R 847])
  | 505 -> One ([R 852])
  | 28 -> One ([R 853])
  | 8 -> One ([R 854])
  | 53 -> One ([R 856])
  | 52 -> One ([R 857])
  | 51 -> One ([R 858])
  | 50 -> One ([R 859])
  | 49 -> One ([R 860])
  | 48 -> One ([R 861])
  | 47 -> One ([R 862])
  | 46 -> One ([R 863])
  | 45 -> One ([R 864])
  | 44 -> One ([R 865])
  | 43 -> One ([R 866])
  | 42 -> One ([R 867])
  | 41 -> One ([R 868])
  | 40 -> One ([R 869])
  | 39 -> One ([R 870])
  | 38 -> One ([R 871])
  | 37 -> One ([R 872])
  | 36 -> One ([R 873])
  | 35 -> One ([R 874])
  | 34 -> One ([R 875])
  | 33 -> One ([R 876])
  | 32 -> One ([R 877])
  | 31 -> One ([R 878])
  | 30 -> One ([R 879])
  | 29 -> One ([R 880])
  | 27 -> One ([R 881])
  | 26 -> One ([R 882])
  | 25 -> One ([R 883])
  | 24 -> One ([R 884])
  | 23 -> One ([R 885])
  | 22 -> One ([R 886])
  | 21 -> One ([R 887])
  | 20 -> One ([R 888])
  | 19 -> One ([R 889])
  | 18 -> One ([R 890])
  | 17 -> One ([R 891])
  | 16 -> One ([R 892])
  | 15 -> One ([R 893])
  | 14 -> One ([R 894])
  | 13 -> One ([R 895])
  | 12 -> One ([R 896])
  | 11 -> One ([R 897])
  | 10 -> One ([R 898])
  | 9 -> One ([R 899])
  | 7 -> One ([R 900])
  | 6 -> One ([R 901])
  | 5 -> One ([R 902])
  | 4 -> One ([R 903])
  | 3 -> One ([R 904])
  | 1974 -> One ([R 908])
  | 1962 | 1975 -> One ([R 910])
  | 1960 -> One ([R 912])
  | 632 -> One ([R 913])
  | 631 -> One ([R 914])
  | 2262 -> One ([R 918])
  | 2263 -> One ([R 919])
  | 2265 -> One ([R 920])
  | 2266 -> One ([R 921])
  | 2264 -> One ([R 922])
  | 2261 -> One ([R 923])
  | 2267 -> One ([R 927])
  | 399 -> One (R 937 :: r370)
  | 420 -> One ([R 938])
  | 135 -> One ([R 943])
  | 138 -> One ([R 944])
  | 142 -> One ([R 945])
  | 136 -> One ([R 946])
  | 143 -> One ([R 947])
  | 139 -> One ([R 948])
  | 144 -> One ([R 949])
  | 141 -> One ([R 950])
  | 134 -> One ([R 951])
  | 480 -> One ([R 956])
  | 749 -> One ([R 957])
  | 1610 -> One ([R 965])
  | 2022 -> One ([R 966])
  | 2025 -> One ([R 967])
  | 2023 -> One ([R 968])
  | 1834 -> One ([R 969])
  | 1837 -> One ([R 970])
  | 1835 -> One ([R 971])
  | 409 -> One ([R 978])
  | 410 -> One ([R 979])
  | 1380 -> One (S (T T_WITH) :: r1001)
  | 173 -> One (S (T T_TYPE) :: r159)
  | 1763 -> One (S (T T_STRING) :: r1268)
  | 1520 -> One (S (T T_STAR) :: r1094)
  | 2269 -> One (S (T T_SEMISEMI) :: r1472)
  | 2276 -> One (S (T T_SEMISEMI) :: r1476)
  | 2200 -> One (S (T T_RPAREN) :: r144)
  | 391 -> One (S (T T_RPAREN) :: r208)
  | 250 -> One (S (T T_RPAREN) :: r278)
  | 268 | 300 -> One (S (T T_RPAREN) :: r286)
  | 491 -> One (S (T T_RPAREN) :: r420)
  | 574 -> One (S (T T_RPAREN) :: r470)
  | 776 -> One (S (T T_RPAREN) :: r643)
  | 1365 -> One (S (T T_RPAREN) :: r989)
  | 2007 -> One (S (T T_RPAREN) :: r1401)
  | 2201 -> One (S (T T_RPAREN) :: r1455)
  | 1504 | 1759 -> One (S (T T_RBRACKET) :: r392)
  | 1371 -> One (S (T T_RBRACKET) :: r992)
  | 1373 -> One (S (T T_RBRACKET) :: r993)
  | 287 -> One (S (T T_QUOTE) :: r294)
  | 1648 -> One (S (T T_OPEN) :: r1201)
  | 1863 -> One (S (T T_OPEN) :: r1344)
  | 433 -> One (S (T T_MINUSGREATER) :: r383)
  | 1536 -> One (S (T T_MINUSGREATER) :: r1104)
  | 1540 -> One (S (T T_MINUSGREATER) :: r1106)
  | 1709 -> One (S (T T_MINUSGREATER) :: r1235)
  | 2133 -> One (S (T T_MINUSGREATER) :: r1438)
  | 85 -> One (S (T T_LPAREN) :: r50)
  | 118 -> One (S (T T_LIDENT) :: r64)
  | 195 -> One (S (T T_LIDENT) :: r197)
  | 196 -> One (S (T T_LIDENT) :: r205)
  | 219 -> One (S (T T_LIDENT) :: r229)
  | 220 -> One (S (T T_LIDENT) :: r235)
  | 367 -> One (S (T T_LIDENT) :: r326)
  | 368 -> One (S (T T_LIDENT) :: r330)
  | 496 -> One (S (T T_LIDENT) :: r424)
  | 497 -> One (S (T T_LIDENT) :: r428)
  | 524 -> One (S (T T_LIDENT) :: r441)
  | 525 -> One (S (T T_LIDENT) :: r445)
  | 550 -> One (S (T T_LIDENT) :: r457)
  | 551 -> One (S (T T_LIDENT) :: r461)
  | 600 -> One (S (T T_LIDENT) :: r474)
  | 601 -> One (S (T T_LIDENT) :: r478)
  | 644 -> One (S (T T_LIDENT) :: r521)
  | 645 -> One (S (T T_LIDENT) :: r527)
  | 651 -> One (S (T T_LIDENT) :: r528)
  | 652 -> One (S (T T_LIDENT) :: r532)
  | 670 -> One (S (T T_LIDENT) :: r536)
  | 671 -> One (S (T T_LIDENT) :: r540)
  | 683 -> One (S (T T_LIDENT) :: r542)
  | 684 -> One (S (T T_LIDENT) :: r546)
  | 697 -> One (S (T T_LIDENT) :: r551)
  | 698 -> One (S (T T_LIDENT) :: r555)
  | 709 -> One (S (T T_LIDENT) :: r557)
  | 721 -> One (S (T T_LIDENT) :: r565)
  | 860 -> One (S (T T_LIDENT) :: r706)
  | 861 -> One (S (T T_LIDENT) :: r709)
  | 872 -> One (S (T T_LIDENT) :: r712)
  | 888 -> One (S (T T_LIDENT) :: r734)
  | 917 -> One (S (T T_LIDENT) :: r747)
  | 918 -> One (S (T T_LIDENT) :: r750)
  | 923 -> One (S (T T_LIDENT) :: r751)
  | 939 -> One (S (T T_LIDENT) :: r758)
  | 940 -> One (S (T T_LIDENT) :: r761)
  | 1095 -> One (S (T T_LIDENT) :: r856)
  | 1096 -> One (S (T T_LIDENT) :: r859)
  | 1151 -> One (S (T T_LIDENT) :: r893)
  | 1152 -> One (S (T T_LIDENT) :: r897)
  | 1341 -> One (S (T T_LIDENT) :: r978)
  | 1342 -> One (S (T T_LIDENT) :: r981)
  | 1508 -> One (S (T T_LIDENT) :: r1092)
  | 1838 -> One (S (T T_LIDENT) :: r1318)
  | 1910 -> One (S (T T_LIDENT) :: r1370)
  | 2026 -> One (S (T T_LIDENT) :: r1406)
  | 2056 -> One (S (T T_LIDENT) :: r1412)
  | 2057 -> One (S (T T_LIDENT) :: r1415)
  | 2071 -> One (S (T T_LIDENT) :: r1420)
  | 2072 -> One (S (T T_LIDENT) :: r1423)
  | 374 -> One (S (T T_INT) :: r331)
  | 377 -> One (S (T T_INT) :: r332)
  | 955 -> One (S (T T_IN) :: r768)
  | 1883 -> One (S (T T_IN) :: r1364)
  | 199 -> One (S (T T_GREATER) :: r207)
  | 261 -> One (S (T T_GREATER) :: r285)
  | 737 -> One (S (T T_GREATER) :: r607)
  | 1410 -> One (S (T T_GREATER) :: r1007)
  | 2050 -> One (S (T T_GREATER) :: r1411)
  | 442 -> One (S (T T_EQUAL) :: r387)
  | 1131 -> One (S (T T_EQUAL) :: r883)
  | 1147 -> One (S (T T_EQUAL) :: r891)
  | 1355 -> One (S (T T_EQUAL) :: r987)
  | 2016 -> One (S (T T_EQUAL) :: r1403)
  | 2034 -> One (S (T T_EQUAL) :: r1408)
  | 2192 -> One (S (T T_EOF) :: r1453)
  | 2196 -> One (S (T T_EOF) :: r1454)
  | 2215 -> One (S (T T_EOF) :: r1460)
  | 2219 -> One (S (T T_EOF) :: r1461)
  | 2223 -> One (S (T T_EOF) :: r1462)
  | 2226 -> One (S (T T_EOF) :: r1463)
  | 2231 -> One (S (T T_EOF) :: r1464)
  | 2235 -> One (S (T T_EOF) :: r1465)
  | 2239 -> One (S (T T_EOF) :: r1466)
  | 2243 -> One (S (T T_EOF) :: r1467)
  | 2247 -> One (S (T T_EOF) :: r1468)
  | 2250 -> One (S (T T_EOF) :: r1469)
  | 2254 -> One (S (T T_EOF) :: r1470)
  | 2293 -> One (S (T T_EOF) :: r1485)
  | 1396 -> One (S (T T_END) :: r1002)
  | 87 -> One (S (T T_DOTDOT) :: r51)
  | 162 -> One (S (T T_DOTDOT) :: r141)
  | 523 -> One (S (T T_DOTDOT) :: r440)
  | 549 -> One (S (T T_DOTDOT) :: r456)
  | 669 -> One (S (T T_DOTDOT) :: r535)
  | 1150 -> One (S (T T_DOTDOT) :: r892)
  | 1780 -> One (S (T T_DOTDOT) :: r1273)
  | 1781 -> One (S (T T_DOTDOT) :: r1274)
  | 292 -> One (S (T T_DOT) :: r299)
  | 307 -> One (S (T T_DOT) :: r303)
  | 402 | 1230 | 1287 -> One (S (T T_DOT) :: r372)
  | 2257 -> One (S (T T_DOT) :: r388)
  | 713 -> One (S (T T_DOT) :: r564)
  | 833 -> One (S (T T_DOT) :: r696)
  | 841 -> One (S (T T_DOT) :: r700)
  | 1126 -> One (S (T T_DOT) :: r881)
  | 1534 -> One (S (T T_DOT) :: r1102)
  | 2127 -> One (S (T T_DOT) :: r1436)
  | 2141 -> One (S (T T_DOT) :: r1444)
  | 2205 -> One (S (T T_DOT) :: r1459)
  | 163 | 1501 -> One (S (T T_COLONCOLON) :: r143)
  | 171 -> One (S (T T_COLON) :: r155)
  | 230 -> One (S (T T_COLON) :: r259)
  | 273 -> One (S (T T_COLON) :: r289)
  | 314 -> One (S (T T_COLON) :: r307)
  | 392 -> One (S (T T_COLON) :: r349)
  | 1703 -> One (S (T T_COLON) :: r1233)
  | 459 -> One (S (T T_BARRBRACKET) :: r391)
  | 592 -> One (S (T T_BARRBRACKET) :: r472)
  | 640 -> One (S (T T_BARRBRACKET) :: r510)
  | 1367 -> One (S (T T_BARRBRACKET) :: r990)
  | 1369 -> One (S (T T_BARRBRACKET) :: r991)
  | 1994 -> One (S (T T_BARRBRACKET) :: r1394)
  | 351 -> One (S (T T_BAR) :: r321)
  | 217 -> One (S (N N_pattern) :: r225)
  | 469 -> One (S (N N_pattern) :: r404)
  | 535 -> One (S (N N_pattern) :: r447)
  | 564 -> One (S (N N_pattern) :: r466)
  | 663 -> One (S (N N_pattern) :: r534)
  | 1162 -> One (S (N N_pattern) :: r899)
  | 1475 -> One (S (N N_pattern) :: r1048)
  | 388 -> One (S (N N_module_type) :: r342)
  | 436 -> One (S (N N_module_type) :: r384)
  | 440 -> One (S (N N_module_type) :: r385)
  | 780 -> One (S (N N_module_type) :: r645)
  | 1419 -> One (S (N N_module_type) :: r1011)
  | 1421 -> One (S (N N_module_type) :: r1012)
  | 1423 -> One (S (N N_module_type) :: r1013)
  | 1426 -> One (S (N N_module_type) :: r1014)
  | 1428 -> One (S (N N_module_type) :: r1015)
  | 1430 -> One (S (N N_module_type) :: r1016)
  | 1445 -> One (S (N N_module_type) :: r1031)
  | 1455 -> One (S (N N_module_type) :: r1038)
  | 2002 -> One (S (N N_module_type) :: r1400)
  | 733 -> One (S (N N_module_expr) :: r596)
  | 818 -> One (S (N N_let_pattern) :: r690)
  | 642 -> One (S (N N_fun_expr) :: r511)
  | 739 -> One (S (N N_fun_expr) :: r610)
  | 859 -> One (S (N N_fun_expr) :: r705)
  | 910 -> One (S (N N_fun_expr) :: r743)
  | 938 -> One (S (N N_fun_expr) :: r757)
  | 960 -> One (S (N N_fun_expr) :: r769)
  | 966 -> One (S (N N_fun_expr) :: r773)
  | 975 -> One (S (N N_fun_expr) :: r777)
  | 986 -> One (S (N N_fun_expr) :: r783)
  | 992 -> One (S (N N_fun_expr) :: r787)
  | 998 -> One (S (N N_fun_expr) :: r791)
  | 1004 -> One (S (N N_fun_expr) :: r795)
  | 1010 -> One (S (N N_fun_expr) :: r799)
  | 1016 -> One (S (N N_fun_expr) :: r803)
  | 1022 -> One (S (N N_fun_expr) :: r807)
  | 1028 -> One (S (N N_fun_expr) :: r811)
  | 1034 -> One (S (N N_fun_expr) :: r815)
  | 1040 -> One (S (N N_fun_expr) :: r819)
  | 1046 -> One (S (N N_fun_expr) :: r823)
  | 1052 -> One (S (N N_fun_expr) :: r827)
  | 1058 -> One (S (N N_fun_expr) :: r831)
  | 1064 -> One (S (N N_fun_expr) :: r835)
  | 1070 -> One (S (N N_fun_expr) :: r839)
  | 1076 -> One (S (N N_fun_expr) :: r843)
  | 1082 -> One (S (N N_fun_expr) :: r847)
  | 1088 -> One (S (N N_fun_expr) :: r851)
  | 1094 -> One (S (N N_fun_expr) :: r855)
  | 1108 -> One (S (N N_fun_expr) :: r864)
  | 1178 -> One (S (N N_fun_expr) :: r902)
  | 1187 -> One (S (N N_fun_expr) :: r909)
  | 1196 -> One (S (N N_fun_expr) :: r916)
  | 1206 -> One (S (N N_fun_expr) :: r920)
  | 1215 -> One (S (N N_fun_expr) :: r927)
  | 1224 -> One (S (N N_fun_expr) :: r934)
  | 1235 -> One (S (N N_fun_expr) :: r942)
  | 1244 -> One (S (N N_fun_expr) :: r949)
  | 1253 -> One (S (N N_fun_expr) :: r956)
  | 1260 -> One (S (N N_fun_expr) :: r960)
  | 1319 -> One (S (N N_fun_expr) :: r970)
  | 1326 -> One (S (N N_fun_expr) :: r974)
  | 634 -> One (Sub (r3) :: r502)
  | 724 -> One (Sub (r3) :: r569)
  | 812 -> One (Sub (r3) :: r668)
  | 1477 -> One (Sub (r3) :: r1049)
  | 2 -> One (Sub (r13) :: r14)
  | 56 -> One (Sub (r13) :: r15)
  | 60 -> One (Sub (r13) :: r22)
  | 190 -> One (Sub (r13) :: r183)
  | 208 -> One (Sub (r13) :: r214)
  | 982 -> One (Sub (r13) :: r782)
  | 1473 -> One (Sub (r13) :: r1047)
  | 1479 -> One (Sub (r13) :: r1052)
  | 1864 -> One (Sub (r13) :: r1349)
  | 566 -> One (Sub (r24) :: r467)
  | 1164 -> One (Sub (r24) :: r900)
  | 280 -> One (Sub (r26) :: r291)
  | 282 -> One (Sub (r26) :: r292)
  | 851 -> One (Sub (r26) :: r701)
  | 1533 -> One (Sub (r26) :: r1100)
  | 248 -> One (Sub (r28) :: r276)
  | 1711 -> One (Sub (r28) :: r1238)
  | 247 -> One (Sub (r30) :: r273)
  | 2139 -> One (Sub (r30) :: r1439)
  | 343 -> One (Sub (r32) :: r318)
  | 413 -> One (Sub (r32) :: r376)
  | 198 -> One (Sub (r34) :: r206)
  | 256 -> One (Sub (r34) :: r280)
  | 301 -> One (Sub (r34) :: r301)
  | 416 -> One (Sub (r34) :: r379)
  | 466 -> One (Sub (r34) :: r403)
  | 613 -> One (Sub (r34) :: r481)
  | 795 -> One (Sub (r34) :: r648)
  | 865 -> One (Sub (r34) :: r710)
  | 1143 -> One (Sub (r34) :: r889)
  | 1620 -> One (Sub (r34) :: r1181)
  | 1658 -> One (Sub (r34) :: r1212)
  | 712 -> One (Sub (r36) :: r562)
  | 820 -> One (Sub (r36) :: r691)
  | 1820 -> One (Sub (r36) :: r1310)
  | 1844 -> One (Sub (r36) :: r1321)
  | 147 -> One (Sub (r59) :: r136)
  | 293 -> One (Sub (r59) :: r300)
  | 2259 -> One (Sub (r59) :: r1471)
  | 1548 -> One (Sub (r81) :: r1107)
  | 474 -> One (Sub (r96) :: r412)
  | 153 -> One (Sub (r131) :: r137)
  | 140 -> One (Sub (r133) :: r135)
  | 1612 -> One (Sub (r133) :: r1175)
  | 157 -> One (Sub (r139) :: r140)
  | 2154 -> One (Sub (r139) :: r1449)
  | 2168 -> One (Sub (r139) :: r1452)
  | 272 -> One (Sub (r146) :: r287)
  | 810 -> One (Sub (r187) :: r665)
  | 951 -> One (Sub (r187) :: r766)
  | 213 -> One (Sub (r217) :: r218)
  | 633 -> One (Sub (r217) :: r500)
  | 757 -> One (Sub (r217) :: r628)
  | 798 -> One (Sub (r217) :: r651)
  | 800 -> One (Sub (r217) :: r652)
  | 870 -> One (Sub (r217) :: r711)
  | 890 -> One (Sub (r217) :: r735)
  | 897 -> One (Sub (r217) :: r736)
  | 925 -> One (Sub (r217) :: r752)
  | 927 -> One (Sub (r217) :: r753)
  | 945 -> One (Sub (r217) :: r762)
  | 1101 -> One (Sub (r217) :: r860)
  | 1347 -> One (Sub (r217) :: r982)
  | 2062 -> One (Sub (r217) :: r1416)
  | 2077 -> One (Sub (r217) :: r1424)
  | 336 -> One (Sub (r237) :: r312)
  | 228 -> One (Sub (r239) :: r246)
  | 329 -> One (Sub (r239) :: r311)
  | 229 -> One (Sub (r252) :: r254)
  | 232 -> One (Sub (r261) :: r262)
  | 252 -> One (Sub (r261) :: r279)
  | 276 -> One (Sub (r261) :: r290)
  | 235 -> One (Sub (r268) :: r270)
  | 446 -> One (Sub (r268) :: r389)
  | 1571 -> One (Sub (r268) :: r1132)
  | 359 -> One (Sub (r323) :: r325)
  | 1451 -> One (Sub (r336) :: r1035)
  | 1574 -> One (Sub (r336) :: r1137)
  | 450 -> One (Sub (r359) :: r390)
  | 398 -> One (Sub (r361) :: r362)
  | 462 -> One (Sub (r400) :: r402)
  | 493 -> One (Sub (r407) :: r423)
  | 503 -> One (Sub (r407) :: r429)
  | 531 -> One (Sub (r407) :: r446)
  | 557 -> One (Sub (r407) :: r462)
  | 594 -> One (Sub (r407) :: r473)
  | 607 -> One (Sub (r407) :: r479)
  | 658 -> One (Sub (r407) :: r533)
  | 677 -> One (Sub (r407) :: r541)
  | 690 -> One (Sub (r407) :: r547)
  | 694 -> One (Sub (r407) :: r550)
  | 704 -> One (Sub (r407) :: r556)
  | 837 -> One (Sub (r407) :: r697)
  | 1158 -> One (Sub (r407) :: r898)
  | 485 -> One (Sub (r415) :: r416)
  | 511 -> One (Sub (r435) :: r438)
  | 539 -> One (Sub (r450) :: r453)
  | 828 -> One (Sub (r450) :: r693)
  | 1120 -> One (Sub (r450) :: r877)
  | 1821 -> One (Sub (r450) :: r1315)
  | 1845 -> One (Sub (r450) :: r1326)
  | 617 -> One (Sub (r487) :: r489)
  | 1361 -> One (Sub (r513) :: r988)
  | 643 -> One (Sub (r515) :: r518)
  | 710 -> One (Sub (r559) :: r561)
  | 722 -> One (Sub (r559) :: r568)
  | 740 -> One (Sub (r616) :: r618)
  | 1379 -> One (Sub (r616) :: r999)
  | 743 -> One (Sub (r620) :: r621)
  | 902 -> One (Sub (r620) :: r742)
  | 1588 -> One (Sub (r620) :: r1145)
  | 816 -> One (Sub (r686) :: r687)
  | 1375 -> One (Sub (r994) :: r997)
  | 1467 -> One (Sub (r1019) :: r1044)
  | 1506 -> One (Sub (r1075) :: r1076)
  | 1507 -> One (Sub (r1084) :: r1086)
  | 1760 -> One (Sub (r1084) :: r1267)
  | 1782 -> One (Sub (r1084) :: r1276)
  | 1790 -> One (Sub (r1084) :: r1278)
  | 2147 -> One (Sub (r1084) :: r1446)
  | 1525 -> One (Sub (r1095) :: r1098)
  | 2095 -> One (Sub (r1095) :: r1429)
  | 2107 -> One (Sub (r1095) :: r1431)
  | 1595 -> One (Sub (r1119) :: r1146)
  | 1906 -> One (Sub (r1155) :: r1369)
  | 1930 -> One (Sub (r1155) :: r1378)
  | 1875 -> One (Sub (r1207) :: r1356)
  | 1862 -> One (Sub (r1280) :: r1339)
  | 1934 -> One (Sub (r1283) :: r1379)
  | 1814 -> One (Sub (r1301) :: r1303)
  | 959 -> One (r0)
  | 958 -> One (r2)
  | 2191 -> One (r4)
  | 2190 -> One (r5)
  | 2189 -> One (r6)
  | 2188 -> One (r7)
  | 2187 -> One (r8)
  | 59 -> One (r9)
  | 54 -> One (r10)
  | 55 -> One (r12)
  | 58 -> One (r14)
  | 57 -> One (r15)
  | 1969 -> One (r16)
  | 1973 -> One (r18)
  | 2186 -> One (r20)
  | 2185 -> One (r21)
  | 61 -> One (r22)
  | 108 | 641 | 741 | 1393 -> One (r23)
  | 111 -> One (r25)
  | 271 -> One (r27)
  | 246 -> One (r29)
  | 263 -> One (r31)
  | 286 -> One (r33)
  | 717 -> One (r35)
  | 2184 -> One (r37)
  | 2183 -> One (r38)
  | 110 -> One (r39)
  | 109 -> One (r40)
  | 64 -> One (r41)
  | 63 -> One (r42)
  | 105 -> One (r43)
  | 107 -> One (r45)
  | 106 -> One (r46)
  | 65 -> One (r47)
  | 90 -> One (r48)
  | 89 -> One (r49)
  | 86 -> One (r50)
  | 88 -> One (r51)
  | 94 -> One (r52)
  | 93 -> One (r53)
  | 98 -> One (r54)
  | 97 -> One (r55)
  | 112 | 128 -> One (r56)
  | 113 -> One (r57)
  | 116 -> One (r58)
  | 161 -> One (r61)
  | 160 -> One (r62)
  | 120 -> One (r63)
  | 119 -> One (r64)
  | 2048 -> One (r65)
  | 2047 -> One (r66)
  | 170 | 203 -> One (r67)
  | 169 | 202 -> One (r68)
  | 168 | 201 -> One (r69)
  | 167 | 200 | 249 | 260 -> One (r70)
  | 2182 -> One (r71)
  | 2181 -> One (r72)
  | 2180 -> One (r73)
  | 2179 -> One (r74)
  | 127 -> One (r75)
  | 126 -> One (r76)
  | 1801 -> One (r80)
  | 2178 -> One (r82)
  | 2177 -> One (r83)
  | 130 -> One (r84)
  | 2114 -> One (r85)
  | 2113 -> One (r86)
  | 2112 -> One (r87)
  | 233 | 281 -> One (r93)
  | 255 -> One (r95)
  | 477 -> One (r97)
  | 1547 -> One (r99)
  | 1789 -> One (r101)
  | 1788 -> One (r102)
  | 1787 | 2106 -> One (r103)
  | 2164 -> One (r105)
  | 2176 -> One (r107)
  | 2175 -> One (r108)
  | 2174 -> One (r109)
  | 2173 -> One (r110)
  | 2172 -> One (r111)
  | 2089 -> One (r115)
  | 189 -> One (r116)
  | 188 -> One (r117)
  | 2162 -> One (r121)
  | 2161 -> One (r122)
  | 2160 -> One (r123)
  | 2159 -> One (r124)
  | 2158 -> One (r125)
  | 146 -> One (r127)
  | 149 -> One (r129)
  | 145 -> One (r130)
  | 150 -> One (r132)
  | 152 -> One (r134)
  | 151 -> One (r135)
  | 148 -> One (r136)
  | 154 -> One (r137)
  | 1765 -> One (r138)
  | 2153 -> One (r140)
  | 2150 -> One (r141)
  | 1503 -> One (r142)
  | 1502 -> One (r143)
  | 164 -> One (r144)
  | 285 -> One (r145)
  | 2138 -> One (r147)
  | 2137 -> One (r148)
  | 2136 -> One (r149)
  | 166 -> One (r150)
  | 2126 -> One (r151)
  | 2125 -> One (r152)
  | 2124 -> One (r153)
  | 2123 -> One (r154)
  | 172 -> One (r155)
  | 2122 -> One (r156)
  | 176 -> One (r157)
  | 175 -> One (r158)
  | 174 -> One (r159)
  | 178 -> One (r160)
  | 2121 -> One (r161)
  | 2120 -> One (r162)
  | 180 -> One (r163)
  | 181 -> One (r164)
  | 2102 -> One (r165)
  | 2119 -> One (r167)
  | 2118 -> One (r168)
  | 2117 -> One (r169)
  | 2116 -> One (r170)
  | 2115 -> One (r171)
  | 2099 -> One (r175)
  | 2098 -> One (r176)
  | 2092 -> One (r177)
  | 2091 -> One (r178)
  | 2090 -> One (r179)
  | 2088 -> One (r181)
  | 2087 -> One (r182)
  | 191 -> One (r183)
  | 1310 -> One (r184)
  | 1308 -> One (r185)
  | 811 -> One (r186)
  | 915 -> One (r188)
  | 2086 -> One (r190)
  | 2085 -> One (r191)
  | 2084 -> One (r192)
  | 194 -> One (r193)
  | 193 -> One (r194)
  | 2083 -> One (r195)
  | 2070 -> One (r196)
  | 2069 -> One (r197)
  | 864 -> One (r198)
  | 863 | 1119 -> One (r199)
  | 2068 -> One (r201)
  | 2055 -> One (r202)
  | 2054 -> One (r203)
  | 2053 -> One (r204)
  | 197 -> One (r205)
  | 2052 -> One (r206)
  | 2049 -> One (r207)
  | 207 -> One (r208)
  | 2046 -> One (r209)
  | 2045 -> One (r210)
  | 205 -> One (r211)
  | 2044 -> One (r212)
  | 2043 -> One (r213)
  | 209 -> One (r214)
  | 2042 -> One (r215)
  | 212 -> One (r216)
  | 1996 -> One (r218)
  | 2038 -> One (r219)
  | 2037 -> One (r220)
  | 616 -> One (r221)
  | 216 -> One (r222)
  | 215 -> One (r223)
  | 612 -> One (r224)
  | 611 -> One (r225)
  | 218 -> One (r226)
  | 609 -> One (r227)
  | 599 -> One (r228)
  | 598 -> One (r229)
  | 596 -> One (r230)
  | 366 -> One (r231)
  | 365 -> One (r232)
  | 364 -> One (r233)
  | 222 -> One (r234)
  | 221 -> One (r235)
  | 348 -> One (r236)
  | 333 -> One (r238)
  | 358 -> One (r240)
  | 357 -> One (r241)
  | 225 -> One (r242)
  | 227 -> One (r243)
  | 226 -> One (r244)
  | 356 -> One (r245)
  | 355 -> One (r246)
  | 331 -> One (r247)
  | 330 -> One (r248)
  | 347 -> One (r250)
  | 338 -> One (r251)
  | 350 -> One (r253)
  | 349 -> One (r254)
  | 327 -> One (r255)
  | 311 -> One (r257)
  | 310 -> One (r258)
  | 231 -> One (r259)
  | 243 | 1714 -> One (r260)
  | 244 -> One (r262)
  | 242 -> One (r263)
  | 241 -> One (r264)
  | 234 -> One (r265)
  | 240 -> One (r267)
  | 237 -> One (r269)
  | 236 -> One (r270)
  | 239 -> One (r271)
  | 238 -> One (r272)
  | 306 -> One (r273)
  | 305 -> One (r274)
  | 304 -> One (r275)
  | 303 -> One (r276)
  | 254 -> One (r277)
  | 251 -> One (r278)
  | 253 -> One (r279)
  | 291 -> One (r280)
  | 290 -> One (r283)
  | 262 -> One (r285)
  | 269 -> One (r286)
  | 279 -> One (r287)
  | 275 -> One (r288)
  | 274 -> One (r289)
  | 277 -> One (r290)
  | 284 -> One (r291)
  | 283 -> One (r292)
  | 289 -> One (r293)
  | 288 -> One (r294)
  | 299 -> One (r295)
  | 298 -> One (r296)
  | 297 -> One (r297)
  | 296 -> One (r298)
  | 295 -> One (r299)
  | 294 -> One (r300)
  | 302 -> One (r301)
  | 309 -> One (r302)
  | 308 -> One (r303)
  | 313 -> One (r304)
  | 317 -> One (r305)
  | 316 -> One (r306)
  | 315 -> One (r307)
  | 319 -> One (r308)
  | 326 -> One (r309)
  | 335 -> One (r310)
  | 334 -> One (r311)
  | 337 -> One (r312)
  | 346 -> One (r313)
  | 345 -> One (r315)
  | 342 -> One (r316)
  | 341 -> One (r317)
  | 344 -> One (r318)
  | 354 -> One (r319)
  | 353 -> One (r320)
  | 352 -> One (r321)
  | 363 -> One (r322)
  | 361 -> One (r324)
  | 360 -> One (r325)
  | 373 -> One (r326)
  | 372 -> One (r327)
  | 371 -> One (r328)
  | 370 -> One (r329)
  | 369 -> One (r330)
  | 375 -> One (r331)
  | 378 -> One (r332)
  | 548 -> One (r333)
  | 547 | 831 | 839 -> One (r334)
  | 538 | 827 | 838 | 1809 -> One (r335)
  | 387 -> One (r337)
  | 386 -> One (r338)
  | 384 -> One (r339)
  | 383 -> One (r340)
  | 457 -> One (r341)
  | 456 -> One (r342)
  | 455 -> One (r343)
  | 454 -> One (r344)
  | 453 -> One (r345)
  | 390 -> One (r346)
  | 452 -> One (r347)
  | 397 -> One (r348)
  | 393 -> One (r349)
  | 432 -> One (r350)
  | 431 -> One (r352)
  | 425 -> One (r354)
  | 424 -> One (r355)
  | 423 -> One (r356)
  | 422 -> One (r357)
  | 421 -> One (r358)
  | 448 -> One (r360)
  | 449 -> One (r362)
  | 401 -> One (r363)
  | 407 -> One (r365)
  | 412 -> One (r367)
  | 411 -> One (r368)
  | 408 -> One (r369)
  | 400 -> One (r370)
  | 405 -> One (r371)
  | 403 -> One (r372)
  | 404 -> One (r373)
  | 406 -> One (r374)
  | 415 -> One (r375)
  | 414 -> One (r376)
  | 419 -> One (r377)
  | 418 -> One (r378)
  | 417 -> One (r379)
  | 430 -> One (r380)
  | 435 -> One (r382)
  | 434 -> One (r383)
  | 437 -> One (r384)
  | 441 -> One (r385)
  | 444 -> One (r386)
  | 443 -> One (r387)
  | 445 | 747 -> One (r388)
  | 447 -> One (r389)
  | 451 -> One (r390)
  | 591 -> One (r391)
  | 461 -> One (r392)
  | 580 -> One (r393)
  | 579 -> One (r395)
  | 578 -> One (r396)
  | 585 -> One (r397)
  | 468 -> One (r398)
  | 465 -> One (r399)
  | 464 -> One (r401)
  | 463 -> One (r402)
  | 467 -> One (r403)
  | 584 -> One (r404)
  | 481 | 1142 -> One (r406)
  | 482 -> One (r408)
  | 472 -> One (r409)
  | 471 -> One (r410)
  | 473 -> One (r411)
  | 475 -> One (r412)
  | 487 -> One (r414)
  | 486 -> One (r416)
  | 577 -> One (r417)
  | 576 -> One (r418)
  | 490 -> One (r419)
  | 492 -> One (r420)
  | 570 -> One (r421)
  | 495 -> One (r422)
  | 494 -> One (r423)
  | 502 -> One (r424)
  | 501 -> One (r425)
  | 500 -> One (r426)
  | 499 -> One (r427)
  | 498 -> One (r428)
  | 504 -> One (r429)
  | 507 -> One (r430)
  | 569 -> One (r431)
  | 510 -> One (r432)
  | 509 -> One (r433)
  | 512 | 794 -> One (r434)
  | 515 -> One (r436)
  | 514 -> One (r437)
  | 513 -> One (r438)
  | 519 -> One (r439)
  | 533 -> One (r440)
  | 530 -> One (r441)
  | 529 -> One (r442)
  | 528 -> One (r443)
  | 527 -> One (r444)
  | 526 -> One (r445)
  | 532 -> One (r446)
  | 536 -> One (r447)
  | 568 -> One (r448)
  | 540 -> One (r449)
  | 544 -> One (r451)
  | 543 -> One (r452)
  | 542 -> One (r453)
  | 546 -> One (r454)
  | 545 -> One (r455)
  | 559 -> One (r456)
  | 556 -> One (r457)
  | 555 -> One (r458)
  | 554 -> One (r459)
  | 553 -> One (r460)
  | 552 -> One (r461)
  | 558 -> One (r462)
  | 563 -> One (r463)
  | 562 -> One (r464)
  | 561 | 832 | 840 -> One (r465)
  | 565 -> One (r466)
  | 567 -> One (r467)
  | 573 -> One (r468)
  | 572 -> One (r469)
  | 575 -> One (r470)
  | 589 -> One (r471)
  | 593 -> One (r472)
  | 595 -> One (r473)
  | 606 -> One (r474)
  | 605 -> One (r475)
  | 604 -> One (r476)
  | 603 -> One (r477)
  | 602 -> One (r478)
  | 608 -> One (r479)
  | 615 -> One (r480)
  | 614 -> One (r481)
  | 2033 -> One (r482)
  | 2032 -> One (r483)
  | 2031 -> One (r484)
  | 2030 -> One (r485)
  | 2021 -> One (r486)
  | 2020 -> One (r488)
  | 2019 -> One (r489)
  | 2015 -> One (r490)
  | 623 -> One (r491)
  | 622 -> One (r492)
  | 621 -> One (r493)
  | 619 -> One (r494)
  | 629 -> One (r495)
  | 630 -> One (r497)
  | 628 -> One (r498)
  | 627 -> One (r499)
  | 2014 -> One (r500)
  | 2013 -> One (r501)
  | 2012 -> One (r502)
  | 2011 -> One (r503)
  | 2010 -> One (r504)
  | 2009 -> One (r505)
  | 637 -> One (r506)
  | 636 -> One (r507)
  | 2006 -> One (r508)
  | 2005 -> One (r509)
  | 1993 -> One (r510)
  | 1992 -> One (r511)
  | 708 -> One (r512)
  | 1363 -> One (r514)
  | 1360 -> One (r516)
  | 1359 -> One (r517)
  | 1358 -> One (r518)
  | 692 -> One (r519)
  | 682 -> One (r520)
  | 681 -> One (r521)
  | 660 -> One (r522)
  | 650 -> One (r523)
  | 649 -> One (r524)
  | 648 -> One (r525)
  | 647 -> One (r526)
  | 646 -> One (r527)
  | 657 -> One (r528)
  | 656 -> One (r529)
  | 655 -> One (r530)
  | 654 -> One (r531)
  | 653 -> One (r532)
  | 659 -> One (r533)
  | 664 -> One (r534)
  | 679 -> One (r535)
  | 676 -> One (r536)
  | 675 -> One (r537)
  | 674 -> One (r538)
  | 673 -> One (r539)
  | 672 -> One (r540)
  | 678 -> One (r541)
  | 689 -> One (r542)
  | 688 -> One (r543)
  | 687 -> One (r544)
  | 686 -> One (r545)
  | 685 -> One (r546)
  | 691 -> One (r547)
  | 706 -> One (r548)
  | 696 -> One (r549)
  | 695 -> One (r550)
  | 703 -> One (r551)
  | 702 -> One (r552)
  | 701 -> One (r553)
  | 700 -> One (r554)
  | 699 -> One (r555)
  | 705 -> One (r556)
  | 720 -> One (r557)
  | 711 -> One (r558)
  | 719 -> One (r560)
  | 718 -> One (r561)
  | 716 -> One (r562)
  | 715 -> One (r563)
  | 714 -> One (r564)
  | 1986 -> One (r565)
  | 1985 -> One (r566)
  | 1984 -> One (r567)
  | 723 -> One (r568)
  | 1983 -> One (r569)
  | 1116 -> One (r570)
  | 1954 -> One (r572)
  | 1953 -> One (r573)
  | 1952 -> One (r574)
  | 1951 -> One (r575)
  | 1950 -> One (r576)
  | 1949 -> One (r577)
  | 1964 -> One (r579)
  | 1963 -> One (r580)
  | 1982 -> One (r582)
  | 1981 -> One (r583)
  | 1980 -> One (r584)
  | 1439 -> One (r587)
  | 1438 -> One (r588)
  | 1437 -> One (r589)
  | 1436 -> One (r590)
  | 1435 -> One (r591)
  | 1434 -> One (r592)
  | 732 -> One (r593)
  | 731 -> One (r594)
  | 779 -> One (r595)
  | 778 -> One (r596)
  | 1425 -> One (r597)
  | 1433 -> One (r599)
  | 1432 -> One (r600)
  | 735 -> One (r601)
  | 1173 -> One (r602)
  | 1414 -> One (r604)
  | 1413 -> One (r605)
  | 1409 -> One (r606)
  | 1408 -> One (r607)
  | 1405 -> One (r608)
  | 738 -> One (r609)
  | 1404 -> One (r610)
  | 1385 -> One (r611)
  | 1384 -> One (r612)
  | 1383 -> One (r613)
  | 1388 -> One (r615)
  | 1399 -> One (r617)
  | 1398 -> One (r618)
  | 746 -> One (r621)
  | 1395 -> One (r622)
  | 753 -> One (r623)
  | 752 -> One (r624)
  | 1394 -> One (r625)
  | 756 -> One (r626)
  | 755 -> One (r627)
  | 759 -> One (r628)
  | 764 -> One (r629)
  | 763 -> One (r630)
  | 762 | 1392 -> One (r631)
  | 1391 -> One (r632)
  | 790 -> One (r633)
  | 789 -> One (r634)
  | 788 -> One (r635)
  | 787 -> One (r636)
  | 769 -> One (r637)
  | 768 -> One (r638)
  | 775 -> One (r639)
  | 773 -> One (r640)
  | 772 -> One (r641)
  | 771 -> One (r642)
  | 777 -> One (r643)
  | 782 -> One (r644)
  | 781 -> One (r645)
  | 1354 -> One (r646)
  | 797 -> One (r647)
  | 796 -> One (r648)
  | 1353 -> One (r649)
  | 1340 -> One (r650)
  | 799 -> One (r651)
  | 801 -> One (r652)
  | 1177 | 1333 -> One (r653)
  | 1176 | 1332 -> One (r654)
  | 803 | 930 -> One (r655)
  | 802 | 929 -> One (r656)
  | 1325 -> One (r657)
  | 1314 -> One (r658)
  | 1313 -> One (r659)
  | 806 -> One (r660)
  | 805 -> One (r661)
  | 1312 -> One (r662)
  | 809 -> One (r663)
  | 808 -> One (r664)
  | 1311 -> One (r665)
  | 1307 -> One (r666)
  | 1306 -> One (r667)
  | 1305 -> One (r668)
  | 846 -> One (r669)
  | 847 -> One (r671)
  | 1141 -> One (r673)
  | 848 -> One (r675)
  | 1139 -> One (r677)
  | 1304 -> One (r679)
  | 854 -> One (r680)
  | 853 -> One (r681)
  | 850 -> One (r682)
  | 815 -> One (r683)
  | 814 -> One (r684)
  | 817 -> One (r685)
  | 826 -> One (r687)
  | 824 -> One (r688)
  | 823 -> One (r689)
  | 822 -> One (r690)
  | 821 -> One (r691)
  | 830 -> One (r692)
  | 829 -> One (r693)
  | 836 -> One (r694)
  | 835 -> One (r695)
  | 834 -> One (r696)
  | 845 -> One (r697)
  | 844 -> One (r698)
  | 843 -> One (r699)
  | 842 -> One (r700)
  | 852 -> One (r701)
  | 858 -> One (r702)
  | 857 -> One (r703)
  | 856 -> One (r704)
  | 1303 -> One (r705)
  | 869 -> One (r706)
  | 868 -> One (r707)
  | 867 -> One (r708)
  | 862 -> One (r709)
  | 866 -> One (r710)
  | 871 -> One (r711)
  | 873 -> One (r712)
  | 1205 | 1280 -> One (r713)
  | 1204 | 1279 -> One (r714)
  | 875 | 1203 -> One (r715)
  | 874 | 1202 -> One (r716)
  | 1273 -> One (r717)
  | 1278 -> One (r719)
  | 1277 -> One (r720)
  | 1276 -> One (r721)
  | 1275 -> One (r722)
  | 1274 -> One (r723)
  | 1271 -> One (r724)
  | 880 -> One (r725)
  | 879 -> One (r726)
  | 878 -> One (r727)
  | 877 -> One (r728)
  | 884 -> One (r729)
  | 883 -> One (r730)
  | 882 -> One (r731)
  | 887 -> One (r732)
  | 886 -> One (r733)
  | 889 -> One (r734)
  | 891 -> One (r735)
  | 898 -> One (r736)
  | 905 -> One (r737)
  | 908 -> One (r739)
  | 907 -> One (r740)
  | 900 -> One (r741)
  | 904 -> One (r742)
  | 1270 -> One (r743)
  | 914 -> One (r744)
  | 913 -> One (r745)
  | 912 -> One (r746)
  | 922 -> One (r747)
  | 921 -> One (r748)
  | 920 -> One (r749)
  | 919 -> One (r750)
  | 924 -> One (r751)
  | 926 -> One (r752)
  | 928 -> One (r753)
  | 934 -> One (r754)
  | 933 -> One (r755)
  | 932 -> One (r756)
  | 1172 -> One (r757)
  | 944 -> One (r758)
  | 943 -> One (r759)
  | 942 -> One (r760)
  | 941 -> One (r761)
  | 946 -> One (r762)
  | 950 -> One (r763)
  | 949 -> One (r764)
  | 948 -> One (r765)
  | 952 -> One (r766)
  | 957 -> One (r767)
  | 956 -> One (r768)
  | 965 -> One (r769)
  | 964 -> One (r770)
  | 963 -> One (r771)
  | 962 -> One (r772)
  | 971 -> One (r773)
  | 970 -> One (r774)
  | 969 -> One (r775)
  | 968 -> One (r776)
  | 980 -> One (r777)
  | 979 -> One (r778)
  | 978 -> One (r779)
  | 977 -> One (r780)
  | 984 -> One (r781)
  | 983 -> One (r782)
  | 991 -> One (r783)
  | 990 -> One (r784)
  | 989 -> One (r785)
  | 988 -> One (r786)
  | 997 -> One (r787)
  | 996 -> One (r788)
  | 995 -> One (r789)
  | 994 -> One (r790)
  | 1003 -> One (r791)
  | 1002 -> One (r792)
  | 1001 -> One (r793)
  | 1000 -> One (r794)
  | 1009 -> One (r795)
  | 1008 -> One (r796)
  | 1007 -> One (r797)
  | 1006 -> One (r798)
  | 1015 -> One (r799)
  | 1014 -> One (r800)
  | 1013 -> One (r801)
  | 1012 -> One (r802)
  | 1021 -> One (r803)
  | 1020 -> One (r804)
  | 1019 -> One (r805)
  | 1018 -> One (r806)
  | 1027 -> One (r807)
  | 1026 -> One (r808)
  | 1025 -> One (r809)
  | 1024 -> One (r810)
  | 1033 -> One (r811)
  | 1032 -> One (r812)
  | 1031 -> One (r813)
  | 1030 -> One (r814)
  | 1039 -> One (r815)
  | 1038 -> One (r816)
  | 1037 -> One (r817)
  | 1036 -> One (r818)
  | 1045 -> One (r819)
  | 1044 -> One (r820)
  | 1043 -> One (r821)
  | 1042 -> One (r822)
  | 1051 -> One (r823)
  | 1050 -> One (r824)
  | 1049 -> One (r825)
  | 1048 -> One (r826)
  | 1057 -> One (r827)
  | 1056 -> One (r828)
  | 1055 -> One (r829)
  | 1054 -> One (r830)
  | 1063 -> One (r831)
  | 1062 -> One (r832)
  | 1061 -> One (r833)
  | 1060 -> One (r834)
  | 1069 -> One (r835)
  | 1068 -> One (r836)
  | 1067 -> One (r837)
  | 1066 -> One (r838)
  | 1075 -> One (r839)
  | 1074 -> One (r840)
  | 1073 -> One (r841)
  | 1072 -> One (r842)
  | 1081 -> One (r843)
  | 1080 -> One (r844)
  | 1079 -> One (r845)
  | 1078 -> One (r846)
  | 1087 -> One (r847)
  | 1086 -> One (r848)
  | 1085 -> One (r849)
  | 1084 -> One (r850)
  | 1093 -> One (r851)
  | 1092 -> One (r852)
  | 1091 -> One (r853)
  | 1090 -> One (r854)
  | 1107 -> One (r855)
  | 1100 -> One (r856)
  | 1099 -> One (r857)
  | 1098 -> One (r858)
  | 1097 -> One (r859)
  | 1102 -> One (r860)
  | 1106 -> One (r861)
  | 1105 -> One (r862)
  | 1104 -> One (r863)
  | 1113 -> One (r864)
  | 1112 -> One (r865)
  | 1111 -> One (r866)
  | 1110 -> One (r867)
  | 1170 -> One (r868)
  | 1167 -> One (r869)
  | 1115 -> One (r870)
  | 1118 -> One (r871)
  | 1117 -> One (r872)
  | 1125 -> One (r873)
  | 1124 -> One (r874)
  | 1123 -> One (r875)
  | 1122 -> One (r876)
  | 1121 -> One (r877)
  | 1130 -> One (r878)
  | 1129 -> One (r879)
  | 1128 -> One (r880)
  | 1127 -> One (r881)
  | 1133 -> One (r882)
  | 1132 -> One (r883)
  | 1140 -> One (r884)
  | 1138 -> One (r885)
  | 1137 -> One (r886)
  | 1146 -> One (r887)
  | 1145 -> One (r888)
  | 1144 -> One (r889)
  | 1149 -> One (r890)
  | 1148 -> One (r891)
  | 1160 -> One (r892)
  | 1157 -> One (r893)
  | 1156 -> One (r894)
  | 1155 -> One (r895)
  | 1154 -> One (r896)
  | 1153 -> One (r897)
  | 1159 -> One (r898)
  | 1163 -> One (r899)
  | 1165 -> One (r900)
  | 1169 -> One (r901)
  | 1183 -> One (r902)
  | 1182 -> One (r903)
  | 1181 -> One (r904)
  | 1180 -> One (r905)
  | 1186 | 1336 -> One (r906)
  | 1185 | 1335 -> One (r907)
  | 1184 | 1334 -> One (r908)
  | 1192 -> One (r909)
  | 1191 -> One (r910)
  | 1190 -> One (r911)
  | 1189 -> One (r912)
  | 1195 | 1339 -> One (r913)
  | 1194 | 1338 -> One (r914)
  | 1193 | 1337 -> One (r915)
  | 1201 -> One (r916)
  | 1200 -> One (r917)
  | 1199 -> One (r918)
  | 1198 -> One (r919)
  | 1211 -> One (r920)
  | 1210 -> One (r921)
  | 1209 -> One (r922)
  | 1208 -> One (r923)
  | 1214 | 1283 -> One (r924)
  | 1213 | 1282 -> One (r925)
  | 1212 | 1281 -> One (r926)
  | 1220 -> One (r927)
  | 1219 -> One (r928)
  | 1218 -> One (r929)
  | 1217 -> One (r930)
  | 1223 | 1286 -> One (r931)
  | 1222 | 1285 -> One (r932)
  | 1221 | 1284 -> One (r933)
  | 1229 -> One (r934)
  | 1228 -> One (r935)
  | 1227 -> One (r936)
  | 1226 -> One (r937)
  | 1234 | 1291 -> One (r938)
  | 1233 | 1290 -> One (r939)
  | 1232 | 1289 -> One (r940)
  | 1231 | 1288 -> One (r941)
  | 1240 -> One (r942)
  | 1239 -> One (r943)
  | 1238 -> One (r944)
  | 1237 -> One (r945)
  | 1243 | 1294 -> One (r946)
  | 1242 | 1293 -> One (r947)
  | 1241 | 1292 -> One (r948)
  | 1249 -> One (r949)
  | 1248 -> One (r950)
  | 1247 -> One (r951)
  | 1246 -> One (r952)
  | 1252 | 1297 -> One (r953)
  | 1251 | 1296 -> One (r954)
  | 1250 | 1295 -> One (r955)
  | 1258 -> One (r956)
  | 1257 -> One (r957)
  | 1256 -> One (r958)
  | 1255 -> One (r959)
  | 1265 -> One (r960)
  | 1264 -> One (r961)
  | 1263 -> One (r962)
  | 1262 -> One (r963)
  | 1302 -> One (r964)
  | 1301 -> One (r965)
  | 1300 -> One (r966)
  | 1318 -> One (r967)
  | 1317 -> One (r968)
  | 1316 -> One (r969)
  | 1324 -> One (r970)
  | 1323 -> One (r971)
  | 1322 -> One (r972)
  | 1321 -> One (r973)
  | 1331 -> One (r974)
  | 1330 -> One (r975)
  | 1329 -> One (r976)
  | 1328 -> One (r977)
  | 1346 -> One (r978)
  | 1345 -> One (r979)
  | 1344 -> One (r980)
  | 1343 -> One (r981)
  | 1348 -> One (r982)
  | 1352 -> One (r983)
  | 1351 -> One (r984)
  | 1350 -> One (r985)
  | 1357 -> One (r986)
  | 1356 -> One (r987)
  | 1362 -> One (r988)
  | 1366 -> One (r989)
  | 1368 -> One (r990)
  | 1370 -> One (r991)
  | 1372 -> One (r992)
  | 1374 -> One (r993)
  | 1378 -> One (r995)
  | 1377 -> One (r996)
  | 1376 -> One (r997)
  | 1390 -> One (r998)
  | 1389 -> One (r999)
  | 1382 -> One (r1000)
  | 1381 -> One (r1001)
  | 1397 -> One (r1002)
  | 1403 -> One (r1003)
  | 1402 -> One (r1004)
  | 1401 -> One (r1005)
  | 1412 -> One (r1006)
  | 1411 -> One (r1007)
  | 1418 -> One (r1008)
  | 1417 -> One (r1009)
  | 1416 -> One (r1010)
  | 1420 -> One (r1011)
  | 1422 -> One (r1012)
  | 1424 -> One (r1013)
  | 1427 -> One (r1014)
  | 1429 -> One (r1015)
  | 1431 -> One (r1016)
  | 1454 -> One (r1017)
  | 1453 -> One (r1018)
  | 1472 -> One (r1020)
  | 1471 -> One (r1021)
  | 1470 -> One (r1022)
  | 1450 -> One (r1023)
  | 1449 -> One (r1024)
  | 1448 -> One (r1025)
  | 1447 -> One (r1026)
  | 1444 -> One (r1027)
  | 1443 -> One (r1028)
  | 1442 -> One (r1029)
  | 1441 -> One (r1030)
  | 1446 -> One (r1031)
  | 1469 -> One (r1032)
  | 1460 -> One (r1033)
  | 1459 -> One (r1034)
  | 1452 -> One (r1035)
  | 1458 -> One (r1036)
  | 1457 -> One (r1037)
  | 1456 -> One (r1038)
  | 1466 -> One (r1039)
  | 1465 -> One (r1040)
  | 1464 -> One (r1041)
  | 1463 -> One (r1042)
  | 1462 -> One (r1043)
  | 1468 -> One (r1044)
  | 1979 -> One (r1045)
  | 1978 -> One (r1046)
  | 1474 -> One (r1047)
  | 1476 -> One (r1048)
  | 1478 -> One (r1049)
  | 1977 -> One (r1050)
  | 1976 -> One (r1051)
  | 1480 -> One (r1052)
  | 1485 -> One (r1053)
  | 1484 -> One (r1054)
  | 1483 -> One (r1055)
  | 1482 -> One (r1056)
  | 1493 -> One (r1057)
  | 1496 -> One (r1059)
  | 1495 -> One (r1060)
  | 1492 -> One (r1061)
  | 1491 -> One (r1062)
  | 1490 -> One (r1063)
  | 1489 -> One (r1064)
  | 1488 -> One (r1065)
  | 1487 -> One (r1066)
  | 1546 -> One (r1067)
  | 1545 -> One (r1068)
  | 1544 -> One (r1069)
  | 1505 | 1605 -> One (r1070)
  | 1499 | 1604 -> One (r1071)
  | 1498 | 1603 -> One (r1072)
  | 1497 | 1602 -> One (r1073)
  | 1524 -> One (r1074)
  | 1523 -> One (r1076)
  | 1519 -> One (r1083)
  | 1516 -> One (r1085)
  | 1515 -> One (r1086)
  | 1514 -> One (r1087)
  | 1513 -> One (r1088)
  | 1512 -> One (r1089)
  | 1511 -> One (r1090)
  | 1510 -> One (r1091)
  | 1509 -> One (r1092)
  | 1522 -> One (r1093)
  | 1521 -> One (r1094)
  | 1532 -> One (r1096)
  | 1531 -> One (r1097)
  | 1530 -> One (r1098)
  | 1529 -> One (r1099)
  | 1543 -> One (r1100)
  | 1539 -> One (r1101)
  | 1535 -> One (r1102)
  | 1538 -> One (r1103)
  | 1537 -> One (r1104)
  | 1542 -> One (r1105)
  | 1541 -> One (r1106)
  | 1800 -> One (r1107)
  | 1799 -> One (r1108)
  | 1559 -> One (r1109)
  | 1558 -> One (r1110)
  | 1557 -> One (r1111)
  | 1556 -> One (r1112)
  | 1555 -> One (r1113)
  | 1554 -> One (r1114)
  | 1553 -> One (r1115)
  | 1552 -> One (r1116)
  | 1592 -> One (r1117)
  | 1591 -> One (r1118)
  | 1594 -> One (r1120)
  | 1593 -> One (r1121)
  | 1587 -> One (r1122)
  | 1569 -> One (r1123)
  | 1568 -> One (r1124)
  | 1567 -> One (r1125)
  | 1566 -> One (r1126)
  | 1565 -> One (r1127)
  | 1573 -> One (r1131)
  | 1572 -> One (r1132)
  | 1586 -> One (r1133)
  | 1578 -> One (r1134)
  | 1577 -> One (r1135)
  | 1576 -> One (r1136)
  | 1575 -> One (r1137)
  | 1585 -> One (r1138)
  | 1584 -> One (r1139)
  | 1583 -> One (r1140)
  | 1582 -> One (r1141)
  | 1581 -> One (r1142)
  | 1580 -> One (r1143)
  | 1590 -> One (r1144)
  | 1589 -> One (r1145)
  | 1596 -> One (r1146)
  | 1601 -> One (r1147)
  | 1600 -> One (r1148)
  | 1599 -> One (r1149)
  | 1598 -> One (r1150)
  | 1661 | 1715 -> One (r1152)
  | 1717 -> One (r1154)
  | 1731 -> One (r1156)
  | 1721 -> One (r1157)
  | 1720 -> One (r1158)
  | 1702 -> One (r1159)
  | 1701 -> One (r1160)
  | 1700 -> One (r1161)
  | 1699 -> One (r1162)
  | 1698 -> One (r1163)
  | 1697 -> One (r1164)
  | 1696 -> One (r1165)
  | 1686 -> One (r1166)
  | 1685 -> One (r1167)
  | 1617 -> One (r1168)
  | 1616 -> One (r1169)
  | 1615 -> One (r1170)
  | 1611 -> One (r1171)
  | 1609 -> One (r1172)
  | 1608 -> One (r1173)
  | 1614 -> One (r1174)
  | 1613 -> One (r1175)
  | 1679 -> One (r1176)
  | 1678 -> One (r1177)
  | 1623 -> One (r1178)
  | 1619 -> One (r1179)
  | 1622 -> One (r1180)
  | 1621 -> One (r1181)
  | 1634 -> One (r1182)
  | 1633 -> One (r1183)
  | 1632 -> One (r1184)
  | 1631 -> One (r1185)
  | 1630 -> One (r1186)
  | 1625 -> One (r1187)
  | 1645 -> One (r1188)
  | 1644 -> One (r1189)
  | 1643 -> One (r1190)
  | 1642 -> One (r1191)
  | 1641 -> One (r1192)
  | 1636 -> One (r1193)
  | 1670 -> One (r1194)
  | 1669 -> One (r1195)
  | 1647 -> One (r1196)
  | 1668 -> One (r1197)
  | 1667 -> One (r1198)
  | 1666 -> One (r1199)
  | 1665 -> One (r1200)
  | 1649 -> One (r1201)
  | 1663 -> One (r1202)
  | 1653 -> One (r1203)
  | 1652 -> One (r1204)
  | 1651 -> One (r1205)
  | 1660 | 1708 -> One (r1206)
  | 1657 -> One (r1208)
  | 1656 -> One (r1209)
  | 1655 -> One (r1210)
  | 1654 | 1707 -> One (r1211)
  | 1659 -> One (r1212)
  | 1675 -> One (r1213)
  | 1674 -> One (r1214)
  | 1673 -> One (r1215)
  | 1677 -> One (r1217)
  | 1676 -> One (r1218)
  | 1672 -> One (r1219)
  | 1681 -> One (r1220)
  | 1684 -> One (r1221)
  | 1695 -> One (r1222)
  | 1694 -> One (r1223)
  | 1693 -> One (r1224)
  | 1692 -> One (r1225)
  | 1691 -> One (r1226)
  | 1690 -> One (r1227)
  | 1689 -> One (r1228)
  | 1688 -> One (r1229)
  | 1719 -> One (r1230)
  | 1706 -> One (r1231)
  | 1705 -> One (r1232)
  | 1704 -> One (r1233)
  | 1718 -> One (r1234)
  | 1710 -> One (r1235)
  | 1716 -> One (r1236)
  | 1713 -> One (r1237)
  | 1712 -> One (r1238)
  | 1730 -> One (r1239)
  | 1729 -> One (r1240)
  | 1728 -> One (r1241)
  | 1727 -> One (r1242)
  | 1726 -> One (r1243)
  | 1725 -> One (r1244)
  | 1724 -> One (r1245)
  | 1723 -> One (r1246)
  | 1740 -> One (r1247)
  | 1742 -> One (r1248)
  | 1752 -> One (r1249)
  | 1751 -> One (r1250)
  | 1750 -> One (r1251)
  | 1749 -> One (r1252)
  | 1748 -> One (r1253)
  | 1747 -> One (r1254)
  | 1746 -> One (r1255)
  | 1745 -> One (r1256)
  | 1796 -> One (r1257)
  | 1776 -> One (r1258)
  | 1775 -> One (r1259)
  | 1774 -> One (r1260)
  | 1773 -> One (r1261)
  | 1758 -> One (r1262)
  | 1757 -> One (r1263)
  | 1756 -> One (r1264)
  | 1755 -> One (r1265)
  | 1762 -> One (r1266)
  | 1761 -> One (r1267)
  | 1764 -> One (r1268)
  | 1769 -> One (r1269)
  | 1768 -> One (r1270)
  | 1767 | 2094 -> One (r1271)
  | 1771 | 2093 -> One (r1272)
  | 1793 -> One (r1273)
  | 1785 -> One (r1274)
  | 1784 -> One (r1275)
  | 1783 -> One (r1276)
  | 1792 -> One (r1277)
  | 1791 -> One (r1278)
  | 1885 -> One (r1279)
  | 1929 -> One (r1281)
  | 1810 -> One (r1282)
  | 1946 -> One (r1284)
  | 1937 -> One (r1285)
  | 1936 -> One (r1286)
  | 1808 -> One (r1287)
  | 1807 -> One (r1288)
  | 1806 -> One (r1289)
  | 1805 -> One (r1290)
  | 1804 -> One (r1291)
  | 1923 -> One (r1292)
  | 1922 -> One (r1293)
  | 1813 -> One (r1294)
  | 1812 -> One (r1295)
  | 1854 -> One (r1297)
  | 1843 -> One (r1298)
  | 1842 -> One (r1299)
  | 1833 -> One (r1300)
  | 1832 -> One (r1302)
  | 1831 -> One (r1303)
  | 1830 -> One (r1304)
  | 1819 -> One (r1305)
  | 1818 -> One (r1306)
  | 1816 -> One (r1307)
  | 1829 -> One (r1308)
  | 1828 -> One (r1309)
  | 1827 -> One (r1310)
  | 1826 -> One (r1311)
  | 1825 -> One (r1312)
  | 1824 -> One (r1313)
  | 1823 -> One (r1314)
  | 1822 -> One (r1315)
  | 1841 -> One (r1316)
  | 1840 -> One (r1317)
  | 1839 -> One (r1318)
  | 1853 -> One (r1319)
  | 1852 -> One (r1320)
  | 1851 -> One (r1321)
  | 1850 -> One (r1322)
  | 1849 -> One (r1323)
  | 1848 -> One (r1324)
  | 1847 -> One (r1325)
  | 1846 -> One (r1326)
  | 1858 -> One (r1327)
  | 1857 -> One (r1328)
  | 1856 -> One (r1329)
  | 1917 -> One (r1330)
  | 1916 -> One (r1331)
  | 1915 -> One (r1332)
  | 1914 -> One (r1333)
  | 1913 -> One (r1334)
  | 1912 -> One (r1335)
  | 1909 -> One (r1336)
  | 1861 -> One (r1337)
  | 1905 -> One (r1338)
  | 1904 -> One (r1339)
  | 1899 -> One (r1340)
  | 1898 -> One (r1341)
  | 1897 -> One (r1342)
  | 1896 -> One (r1343)
  | 1870 -> One (r1344)
  | 1869 -> One (r1345)
  | 1868 -> One (r1346)
  | 1867 -> One (r1347)
  | 1866 -> One (r1348)
  | 1865 -> One (r1349)
  | 1895 -> One (r1350)
  | 1874 -> One (r1351)
  | 1873 -> One (r1352)
  | 1872 -> One (r1353)
  | 1878 -> One (r1354)
  | 1877 -> One (r1355)
  | 1876 -> One (r1356)
  | 1892 -> One (r1357)
  | 1882 -> One (r1358)
  | 1881 -> One (r1359)
  | 1894 -> One (r1361)
  | 1880 -> One (r1362)
  | 1889 -> One (r1363)
  | 1884 -> One (r1364)
  | 1903 -> One (r1365)
  | 1902 -> One (r1366)
  | 1901 -> One (r1367)
  | 1908 -> One (r1368)
  | 1907 -> One (r1369)
  | 1911 -> One (r1370)
  | 1921 -> One (r1371)
  | 1920 -> One (r1372)
  | 1919 -> One (r1373)
  | 1925 -> One (r1374)
  | 1928 -> One (r1375)
  | 1933 -> One (r1376)
  | 1932 -> One (r1377)
  | 1931 -> One (r1378)
  | 1935 -> One (r1379)
  | 1945 -> One (r1380)
  | 1944 -> One (r1381)
  | 1943 -> One (r1382)
  | 1942 -> One (r1383)
  | 1941 -> One (r1384)
  | 1940 -> One (r1385)
  | 1939 -> One (r1386)
  | 1961 -> One (r1387)
  | 1966 -> One (r1388)
  | 1972 -> One (r1389)
  | 1971 -> One (r1390)
  | 1991 -> One (r1391)
  | 1990 -> One (r1392)
  | 1989 -> One (r1393)
  | 1995 -> One (r1394)
  | 2001 -> One (r1395)
  | 2000 -> One (r1396)
  | 1999 -> One (r1397)
  | 1998 -> One (r1398)
  | 2004 -> One (r1399)
  | 2003 -> One (r1400)
  | 2008 -> One (r1401)
  | 2018 -> One (r1402)
  | 2017 -> One (r1403)
  | 2029 -> One (r1404)
  | 2028 -> One (r1405)
  | 2027 -> One (r1406)
  | 2036 -> One (r1407)
  | 2035 -> One (r1408)
  | 2041 -> One (r1409)
  | 2040 -> One (r1410)
  | 2051 -> One (r1411)
  | 2061 -> One (r1412)
  | 2060 -> One (r1413)
  | 2059 -> One (r1414)
  | 2058 -> One (r1415)
  | 2063 -> One (r1416)
  | 2067 -> One (r1417)
  | 2066 -> One (r1418)
  | 2065 -> One (r1419)
  | 2076 -> One (r1420)
  | 2075 -> One (r1421)
  | 2074 -> One (r1422)
  | 2073 -> One (r1423)
  | 2078 -> One (r1424)
  | 2082 -> One (r1425)
  | 2081 -> One (r1426)
  | 2080 -> One (r1427)
  | 2097 -> One (r1428)
  | 2096 -> One (r1429)
  | 2109 -> One (r1430)
  | 2108 -> One (r1431)
  | 2132 -> One (r1432)
  | 2131 -> One (r1433)
  | 2130 -> One (r1434)
  | 2129 -> One (r1435)
  | 2128 -> One (r1436)
  | 2135 -> One (r1437)
  | 2134 -> One (r1438)
  | 2140 -> One (r1439)
  | 2146 -> One (r1440)
  | 2145 -> One (r1441)
  | 2144 -> One (r1442)
  | 2143 -> One (r1443)
  | 2142 -> One (r1444)
  | 2149 -> One (r1445)
  | 2148 -> One (r1446)
  | 2157 -> One (r1447)
  | 2156 -> One (r1448)
  | 2155 -> One (r1449)
  | 2171 -> One (r1450)
  | 2170 -> One (r1451)
  | 2169 -> One (r1452)
  | 2193 -> One (r1453)
  | 2197 -> One (r1454)
  | 2202 -> One (r1455)
  | 2209 -> One (r1456)
  | 2208 -> One (r1457)
  | 2207 -> One (r1458)
  | 2206 -> One (r1459)
  | 2216 -> One (r1460)
  | 2220 -> One (r1461)
  | 2224 -> One (r1462)
  | 2227 -> One (r1463)
  | 2232 -> One (r1464)
  | 2236 -> One (r1465)
  | 2240 -> One (r1466)
  | 2244 -> One (r1467)
  | 2248 -> One (r1468)
  | 2251 -> One (r1469)
  | 2255 -> One (r1470)
  | 2260 -> One (r1471)
  | 2270 -> One (r1472)
  | 2272 -> One (r1473)
  | 2275 -> One (r1474)
  | 2274 -> One (r1475)
  | 2277 -> One (r1476)
  | 2287 -> One (r1477)
  | 2283 -> One (r1478)
  | 2282 -> One (r1479)
  | 2286 -> One (r1480)
  | 2285 -> One (r1481)
  | 2292 -> One (r1482)
  | 2291 -> One (r1483)
  | 2290 -> One (r1484)
  | 2294 -> One (r1485)
  | 489 -> Select (function
    | -1 -> [R 98]
    | _ -> S (T T_DOT) :: r419)
  | 761 -> Select (function
    | -1 -> [R 98]
    | _ -> r632)
  | 131 -> Select (function
    | -1 -> r92
    | _ -> R 132 :: r114)
  | 182 -> Select (function
    | -1 -> r92
    | _ -> R 132 :: r174)
  | 725 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1474 | 1480 | 2279 -> r577
    | _ -> R 132 :: r586)
  | 1561 -> Select (function
    | -1 -> r1030
    | _ -> R 132 :: r1130)
  | 429 -> Select (function
    | -1 -> r271
    | _ -> [R 267])
  | 537 -> Select (function
    | -1 -> [R 844]
    | _ -> S (N N_pattern) :: r448)
  | 516 -> Select (function
    | -1 -> [R 845]
    | _ -> S (N N_pattern) :: r439)
  | 137 -> Select (function
    | -1 -> r120
    | _ -> R 937 :: r126)
  | 185 -> Select (function
    | -1 -> r120
    | _ -> R 937 :: r180)
  | 1526 -> Select (function
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> S (T T_COLONCOLON) :: r455)
  | 638 -> Select (function
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> Sub (r3) :: r509)
  | 380 -> Select (function
    | 643 | 793 | 1115 | 1361 | 1867 | 1901 | 1952 -> r47
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> r335)
  | 204 -> Select (function
    | -1 -> S (T T_RPAREN) :: r208
    | _ -> S (N N_module_type) :: r210)
  | 460 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r392
    | _ -> Sub (r394) :: r396)
  | 736 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r392
    | _ -> Sub (r603) :: r605)
  | 122 -> Select (function
    | -1 -> r70
    | _ -> S (T T_MODULE) :: r79)
  | 1528 -> Select (function
    | -1 -> r388
    | _ -> S (T T_LPAREN) :: r1099)
  | 258 -> Select (function
    | 1702 | 1706 | 1710 | 1713 | 1727 | 1906 | 1930 -> r265
    | -1 -> r281
    | _ -> S (T T_DOT) :: r284)
  | 427 -> Select (function
    | -1 -> r281
    | _ -> S (T T_DOT) :: r381)
  | 165 -> Select (function
    | -1 -> r93
    | _ -> S (T T_COLON) :: r150)
  | 114 -> Select (function
    | 122 | 163 | 167 | 249 | 832 | 840 | 1119 | 1533 -> r62
    | _ -> Sub (r59) :: r60)
  | 117 -> Select (function
    | 122 | 163 | 167 | 249 | 832 | 840 | 1119 | 1533 -> r61
    | _ -> r60)
  | 125 -> Select (function
    | -1 -> r67
    | _ -> r77)
  | 124 -> Select (function
    | -1 -> r68
    | _ -> r78)
  | 123 -> Select (function
    | -1 -> r69
    | _ -> r79)
  | 2111 -> Select (function
    | -1 -> r88
    | _ -> r93)
  | 2167 -> Select (function
    | -1 -> r88
    | _ -> r93)
  | 2166 -> Select (function
    | -1 -> r89
    | _ -> r112)
  | 2110 -> Select (function
    | -1 -> r89
    | _ -> r172)
  | 133 -> Select (function
    | -1 -> r90
    | _ -> r113)
  | 184 -> Select (function
    | -1 -> r90
    | _ -> r173)
  | 132 -> Select (function
    | -1 -> r91
    | _ -> r114)
  | 183 -> Select (function
    | -1 -> r91
    | _ -> r174)
  | 187 -> Select (function
    | -1 -> r118
    | _ -> r93)
  | 156 -> Select (function
    | -1 -> r118
    | _ -> r93)
  | 155 -> Select (function
    | -1 -> r119
    | _ -> r126)
  | 186 -> Select (function
    | -1 -> r119
    | _ -> r180)
  | 259 -> Select (function
    | 1702 | 1706 | 1710 | 1713 | 1727 | 1906 | 1930 -> r264
    | -1 -> r272
    | _ -> r284)
  | 428 -> Select (function
    | -1 -> r272
    | _ -> r381)
  | 727 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1474 | 1480 | 2279 -> r575
    | _ -> r585)
  | 726 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1474 | 1480 | 2279 -> r576
    | _ -> r586)
  | 1564 -> Select (function
    | -1 -> r1027
    | _ -> r1128)
  | 1563 -> Select (function
    | -1 -> r1028
    | _ -> r1129)
  | 1562 -> Select (function
    | -1 -> r1029
    | _ -> r1130)
  | _ -> raise Not_found

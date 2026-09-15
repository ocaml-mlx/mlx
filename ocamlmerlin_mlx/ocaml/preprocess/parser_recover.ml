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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;1;2;1;2;3;1;1;2;3;4;5;6;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;2;1;1;2;1;2;3;4;5;6;7;8;1;2;3;4;1;1;1;2;1;1;2;3;4;5;6;7;8;1;2;1;2;3;1;2;3;1;1;1;2;3;4;1;1;1;2;1;2;1;1;1;1;1;2;3;1;1;1;2;3;4;1;1;2;1;2;2;1;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;1;2;1;2;2;3;4;5;4;1;2;1;1;2;1;1;1;1;1;1;2;3;2;1;2;1;2;3;2;3;2;3;2;3;4;5;3;1;1;2;3;4;3;3;3;2;3;4;5;6;7;8;2;2;3;2;3;4;3;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;5;6;7;8;9;10;11;12;13;9;1;2;2;1;2;2;1;1;2;3;4;1;5;6;6;1;2;1;2;3;1;2;1;4;2;1;2;1;1;2;3;3;1;1;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;2;3;2;3;1;1;4;5;2;3;4;2;3;4;1;3;2;3;5;3;4;5;7;8;1;1;1;2;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;4;5;6;7;8;9;5;4;5;1;1;1;1;2;3;1;1;2;3;4;1;1;1;2;2;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;1;2;3;1;2;4;5;6;2;3;2;3;2;3;4;5;6;7;8;4;3;4;3;3;3;4;5;2;3;2;3;2;4;4;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;8;9;8;8;2;3;4;5;6;7;8;9;5;4;5;4;4;2;3;3;4;5;4;1;1;2;1;3;4;5;1;1;1;2;3;1;4;1;1;1;1;1;2;3;1;1;1;1;2;1;1;2;3;4;5;6;7;8;9;10;11;12;13;9;8;9;8;1;8;2;3;2;1;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;4;5;6;7;8;9;5;4;5;4;4;1;2;3;4;5;6;7;8;9;5;4;5;4;4;1;1;2;1;2;1;2;3;3;1;3;4;2;1;2;3;1;1;2;3;1;1;2;3;4;1;2;3;1;1;1;1;1;1;1;2;3;1;3;2;3;1;1;1;2;3;1;2;3;1;1;2;1;1;2;3;4;1;1;4;5;6;1;2;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;7;8;9;10;1;1;1;1;2;3;4;1;2;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;1;1;2;1;2;3;3;4;5;1;2;1;2;3;4;2;3;4;5;6;7;1;2;3;4;5;6;7;8;2;1;1;1;2;4;1;2;5;6;1;2;3;4;5;6;7;8;1;2;3;4;9;10;7;6;7;2;3;2;3;1;2;3;4;5;1;2;3;4;1;2;3;1;2;3;4;1;1;1;1;1;2;3;3;4;1;2;3;3;1;2;5;6;2;1;2;3;4;5;1;1;1;2;3;4;5;2;1;2;1;2;1;2;2;3;1;2;3;4;5;6;1;2;3;4;5;6;7;4;3;4;3;4;5;6;2;3;1;2;1;2;3;1;1;2;3;4;5;6;3;2;3;4;5;6;3;2;1;2;1;2;3;4;5;2;2;3;4;5;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;7;4;3;4;3;4;5;6;3;2;3;4;5;6;3;1;2;1;1;2;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;1;2;3;1;4;1;1;2;3;4;5;1;2;3;2;3;4;5;6;7;8;4;3;4;3;3;2;3;2;3;1;3;1;2;4;2;3;1;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;3;4;5;6;7;8;5;1;2;2;1;2;6;1;1;7;8;9;10;11;4;5;3;4;5;3;4;5;3;4;5;6;7;5;6;7;5;6;7;3;6;7;8;9;6;7;3;4;5;2;3;3;2;4;4;5;6;7;8;9;10;11;12;13;14;11;6;7;8;9;10;11;8;4;5;3;4;5;3;4;5;3;4;5;6;7;8;5;4;5;4;5;6;7;4;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;3;4;4;5;3;4;5;3;1;2;3;1;2;3;4;5;1;4;5;1;2;3;4;4;4;5;2;3;2;3;4;5;2;2;3;4;2;2;3;2;3;1;2;3;4;5;6;5;6;7;8;1;2;3;2;3;4;5;4;5;5;6;2;3;4;5;1;2;3;4;5;1;2;6;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;1;1;2;3;1;4;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;1;1;2;3;1;2;1;1;2;3;4;1;1;2;6;7;8;9;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;1;2;3;1;1;2;1;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;1;1;2;1;2;1;1;1;1;2;3;1;1;1;3;4;3;4;4;5;6;2;4;5;2;2;3;4;5;6;3;2;2;3;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;2;3;2;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;5;6;2;3;4;2;3;4;2;3;5;6;1;2;3;4;5;6;1;7;1;2;3;2;2;3;2;4;5;6;7;8;9;10;11;8;7;8;7;8;9;10;7;2;3;4;5;6;7;8;5;4;5;4;5;6;7;4;4;5;6;3;4;9;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;6;7;8;5;6;7;8;9;3;4;5;9;10;11;12;4;5;6;7;8;9;3;4;5;3;4;5;6;7;2;3;4;5;6;7;2;3;4;2;2;2;2;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;3;4;7;8;9;10;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

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
  let r2 = [R 743] in
  let r3 = Sub (r1) :: r2 in
  let r4 = [R 155] in
  let r5 = S (T T_DONE) :: r4 in
  let r6 = Sub (r3) :: r5 in
  let r7 = S (T T_DO) :: r6 in
  let r8 = Sub (r3) :: r7 in
  let r9 = R 376 :: r8 in
  let r10 = [R 851] in
  let r11 = S (T T_AND) :: r10 in
  let r12 = [R 32] in
  let r13 = Sub (r11) :: r12 in
  let r14 = [R 133] in
  let r15 = [R 33] in
  let r16 = [R 613] in
  let r17 = S (N N_structure) :: r16 in
  let r18 = [R 34] in
  let r19 = Sub (r17) :: r18 in
  let r20 = [R 35] in
  let r21 = S (T T_RBRACKET) :: r20 in
  let r22 = Sub (r19) :: r21 in
  let r23 = [R 951] in
  let r24 = S (T T_LIDENT) :: r23 in
  let r25 = [R 31] in
  let r26 = S (T T_UNDERSCORE) :: r25 in
  let r27 = [R 924] in
  let r28 = Sub (r26) :: r27 in
  let r29 = [R 241] in
  let r30 = Sub (r28) :: r29 in
  let r31 = [R 17] in
  let r32 = Sub (r30) :: r31 in
  let r33 = [R 108] in
  let r34 = Sub (r32) :: r33 in
  let r35 = [R 618] in
  let r36 = Sub (r34) :: r35 in
  let r37 = [R 959] in
  let r38 = R 382 :: r37 in
  let r39 = Sub (r36) :: r38 in
  let r40 = S (T T_COLON) :: r39 in
  let r41 = Sub (r24) :: r40 in
  let r42 = R 376 :: r41 in
  let r43 = [R 551] in
  let r44 = S (T T_AMPERAMPER) :: r43 in
  let r45 = [R 950] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = Sub (r44) :: r46 in
  let r48 = [R 525] in
  let r49 = S (T T_RPAREN) :: r48 in
  let r50 = R 270 :: r49 in
  let r51 = [R 271] in
  let r52 = [R 527] in
  let r53 = S (T T_RBRACKET) :: r52 in
  let r54 = [R 529] in
  let r55 = S (T T_RBRACE) :: r54 in
  let r56 = [R 440] in
  let r57 = [R 135] in
  let r58 = [R 268] in
  let r59 = S (T T_LIDENT) :: r58 in
  let r60 = [R 703] in
  let r61 = [R 30] in
  let r62 = Sub (r59) :: r61 in
  let r63 = [R 577] in
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
  let r80 = [R 746] in
  let r81 = R 384 :: r80 in
  let r82 = [R 476] in
  let r83 = S (T T_END) :: r82 in
  let r84 = Sub (r81) :: r83 in
  let r85 = [R 265] in
  let r86 = R 382 :: r85 in
  let r87 = R 691 :: r86 in
  let r88 = R 929 :: r87 in
  let r89 = S (T T_LIDENT) :: r88 in
  let r90 = R 933 :: r89 in
  let r91 = R 376 :: r90 in
  let r92 = R 132 :: r91 in
  let r93 = [R 438] in
  let r94 = S (T T_LIDENT) :: r93 in
  let r95 = [R 931] in
  let r96 = Sub (r94) :: r95 in
  let r97 = [R 93] in
  let r98 = S (T T_FALSE) :: r97 in
  let r99 = [R 97] in
  let r100 = Sub (r98) :: r99 in
  let r101 = [R 262] in
  let r102 = R 376 :: r101 in
  let r103 = R 255 :: r102 in
  let r104 = Sub (r100) :: r103 in
  let r105 = [R 644] in
  let r106 = Sub (r104) :: r105 in
  let r107 = [R 753] in
  let r108 = R 382 :: r107 in
  let r109 = Sub (r106) :: r108 in
  let r110 = R 624 :: r109 in
  let r111 = S (T T_PLUSEQ) :: r110 in
  let r112 = Sub (r96) :: r111 in
  let r113 = R 933 :: r112 in
  let r114 = R 376 :: r113 in
  let r115 = [R 266] in
  let r116 = R 382 :: r115 in
  let r117 = R 691 :: r116 in
  let r118 = R 929 :: r117 in
  let r119 = S (T T_LIDENT) :: r118 in
  let r120 = R 933 :: r119 in
  let r121 = [R 754] in
  let r122 = R 382 :: r121 in
  let r123 = Sub (r106) :: r122 in
  let r124 = R 624 :: r123 in
  let r125 = S (T T_PLUSEQ) :: r124 in
  let r126 = Sub (r96) :: r125 in
  let r127 = [R 937] in
  let r128 = S (T T_UNDERSCORE) :: r127 in
  let r129 = [R 932] in
  let r130 = Sub (r128) :: r129 in
  let r131 = R 938 :: r130 in
  let r132 = [R 716] in
  let r133 = Sub (r131) :: r132 in
  let r134 = [R 935] in
  let r135 = S (T T_RPAREN) :: r134 in
  let r136 = [R 936] in
  let r137 = [R 717] in
  let r138 = [R 507] in
  let r139 = S (T T_DOTDOT) :: r138 in
  let r140 = [R 930] in
  let r141 = [R 508] in
  let r142 = [R 96] in
  let r143 = S (T T_RPAREN) :: r142 in
  let r144 = [R 92] in
  let r145 = [R 720] in
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
  let r156 = [R 479] in
  let r157 = S (N N_module_expr) :: r156 in
  let r158 = R 376 :: r157 in
  let r159 = S (T T_OF) :: r158 in
  let r160 = [R 452] in
  let r161 = [R 464] in
  let r162 = S (T T_END) :: r161 in
  let r163 = S (N N_structure) :: r162 in
  let r164 = [R 809] in
  let r165 = [R 638] in
  let r166 = Sub (r104) :: r165 in
  let r167 = [R 411] in
  let r168 = R 382 :: r167 in
  let r169 = Sub (r166) :: r168 in
  let r170 = R 624 :: r169 in
  let r171 = S (T T_PLUSEQ) :: r170 in
  let r172 = Sub (r96) :: r171 in
  let r173 = R 933 :: r172 in
  let r174 = R 376 :: r173 in
  let r175 = [R 412] in
  let r176 = R 382 :: r175 in
  let r177 = Sub (r166) :: r176 in
  let r178 = R 624 :: r177 in
  let r179 = S (T T_PLUSEQ) :: r178 in
  let r180 = Sub (r96) :: r179 in
  let r181 = [R 622] in
  let r182 = S (T T_RBRACKET) :: r181 in
  let r183 = Sub (r19) :: r182 in
  let r184 = [R 421] in
  let r185 = Sub (r3) :: r184 in
  let r186 = S (T T_MINUSGREATER) :: r185 in
  let r187 = S (N N_pattern) :: r186 in
  let r188 = [R 705] in
  let r189 = Sub (r187) :: r188 in
  let r190 = [R 148] in
  let r191 = Sub (r189) :: r190 in
  let r192 = S (T T_WITH) :: r191 in
  let r193 = Sub (r3) :: r192 in
  let r194 = R 376 :: r193 in
  let r195 = [R 667] in
  let r196 = S (N N_fun_expr) :: r195 in
  let r197 = S (T T_COMMA) :: r196 in
  let r198 = [R 926] in
  let r199 = Sub (r34) :: r198 in
  let r200 = S (T T_COLON) :: r199 in
  let r201 = [R 672] in
  let r202 = S (N N_fun_expr) :: r201 in
  let r203 = S (T T_COMMA) :: r202 in
  let r204 = S (T T_RPAREN) :: r203 in
  let r205 = Sub (r200) :: r204 in
  let r206 = [R 928] in
  let r207 = [R 517] in
  let r208 = [R 252] in
  let r209 = [R 480] in
  let r210 = S (T T_RPAREN) :: r209 in
  let r211 = [R 474] in
  let r212 = [R 134] in
  let r213 = S (T T_RBRACKET) :: r212 in
  let r214 = Sub (r17) :: r213 in
  let r215 = [R 388] in
  let r216 = [R 274] in
  let r217 = S (T T_UNDERSCORE) :: r164 in
  let r218 = [R 799] in
  let r219 = [R 793] in
  let r220 = S (T T_END) :: r219 in
  let r221 = R 393 :: r220 in
  let r222 = R 60 :: r221 in
  let r223 = R 376 :: r222 in
  let r224 = [R 58] in
  let r225 = S (T T_RPAREN) :: r224 in
  let r226 = [R 837] in
  let r227 = [R 681] in
  let r228 = S (T T_DOTDOT) :: r227 in
  let r229 = S (T T_COMMA) :: r228 in
  let r230 = [R 682] in
  let r231 = S (T T_DOTDOT) :: r230 in
  let r232 = S (T T_COMMA) :: r231 in
  let r233 = S (T T_RPAREN) :: r232 in
  let r234 = Sub (r34) :: r233 in
  let r235 = S (T T_COLON) :: r234 in
  let r236 = [R 727] in
  let r237 = Sub (r34) :: r236 in
  let r238 = [R 712] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 120] in
  let r241 = S (T T_RBRACKET) :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = [R 119] in
  let r244 = S (T T_RBRACKET) :: r243 in
  let r245 = [R 118] in
  let r246 = S (T T_RBRACKET) :: r245 in
  let r247 = [R 496] in
  let r248 = Sub (r59) :: r247 in
  let r249 = S (T T_BACKQUOTE) :: r248 in
  let r250 = [R 912] in
  let r251 = R 376 :: r250 in
  let r252 = Sub (r249) :: r251 in
  let r253 = [R 115] in
  let r254 = S (T T_RBRACKET) :: r253 in
  let r255 = [R 86] in
  let r256 = Sub (r94) :: r255 in
  let r257 = [R 26] in
  let r258 = [R 439] in
  let r259 = S (T T_LIDENT) :: r258 in
  let r260 = S (T T_DOT) :: r259 in
  let r261 = S (T T_UIDENT) :: r56 in
  let r262 = [R 456] in
  let r263 = Sub (r261) :: r262 in
  let r264 = [R 457] in
  let r265 = S (T T_RPAREN) :: r264 in
  let r266 = [R 441] in
  let r267 = S (T T_UIDENT) :: r266 in
  let r268 = [R 116] in
  let r269 = S (T T_RBRACKET) :: r268 in
  let r270 = [R 247] in
  let r271 = [R 243] in
  let r272 = Sub (r30) :: r271 in
  let r273 = S (T T_MINUSGREATER) :: r272 in
  let r274 = [R 25] in
  let r275 = Sub (r96) :: r274 in
  let r276 = [R 28] in
  let r277 = [R 724] in
  let r278 = S (T T_DOT) :: r267 in
  let r279 = S (T T_LBRACKETGREATER) :: r244 in
  let r280 = [R 29] in
  let r281 = Sub (r279) :: r280 in
  let r282 = [R 113] in
  let r283 = [R 925] in
  let r284 = [R 721] in
  let r285 = Sub (r26) :: r284 in
  let r286 = [R 27] in
  let r287 = [R 722] in
  let r288 = [R 723] in
  let r289 = [R 18] in
  let r290 = Sub (r59) :: r289 in
  let r291 = [R 242] in
  let r292 = Sub (r30) :: r291 in
  let r293 = S (T T_MINUSGREATER) :: r292 in
  let r294 = S (T T_RPAREN) :: r293 in
  let r295 = Sub (r34) :: r294 in
  let r296 = [R 704] in
  let r297 = [R 725] in
  let r298 = [R 713] in
  let r299 = [R 708] in
  let r300 = Sub (r32) :: r299 in
  let r301 = [R 911] in
  let r302 = R 376 :: r301 in
  let r303 = Sub (r300) :: r302 in
  let r304 = [R 709] in
  let r305 = [R 377] in
  let r306 = [R 117] in
  let r307 = S (T T_RBRACKET) :: r306 in
  let r308 = Sub (r239) :: r307 in
  let r309 = [R 701] in
  let r310 = Sub (r249) :: r309 in
  let r311 = [R 121] in
  let r312 = S (T T_RBRACKET) :: r311 in
  let r313 = [R 321] in
  let r314 = [R 322] in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = Sub (r34) :: r315 in
  let r317 = S (T T_COLON) :: r316 in
  let r318 = [R 769] in
  let r319 = [R 767] in
  let r320 = [R 833] in
  let r321 = S (T T_RPAREN) :: r320 in
  let r322 = S (N N_pattern) :: r321 in
  let r323 = S (T T_UNDERSCORE) :: r211 in
  let r324 = [R 835] in
  let r325 = S (T T_RPAREN) :: r324 in
  let r326 = Sub (r323) :: r325 in
  let r327 = R 376 :: r326 in
  let r328 = [R 836] in
  let r329 = S (T T_RPAREN) :: r328 in
  let r330 = [R 477] in
  let r331 = S (N N_module_type) :: r330 in
  let r332 = S (T T_MINUSGREATER) :: r331 in
  let r333 = S (N N_functor_args) :: r332 in
  let r334 = [R 253] in
  let r335 = S (T T_RPAREN) :: r334 in
  let r336 = S (N N_module_type) :: r335 in
  let r337 = [R 448] in
  let r338 = Sub (r59) :: r337 in
  let r339 = [R 488] in
  let r340 = Sub (r338) :: r339 in
  let r341 = [R 972] in
  let r342 = S (N N_module_type) :: r341 in
  let r343 = S (T T_EQUAL) :: r342 in
  let r344 = Sub (r340) :: r343 in
  let r345 = S (T T_TYPE) :: r344 in
  let r346 = S (T T_MODULE) :: r345 in
  let r347 = [R 710] in
  let r348 = Sub (r346) :: r347 in
  let r349 = [R 484] in
  let r350 = [R 450] in
  let r351 = S (T T_LIDENT) :: r350 in
  let r352 = [R 296] in
  let r353 = Sub (r351) :: r352 in
  let r354 = [R 969] in
  let r355 = Sub (r32) :: r354 in
  let r356 = S (T T_COLONEQUAL) :: r355 in
  let r357 = Sub (r353) :: r356 in
  let r358 = [R 451] in
  let r359 = S (T T_LIDENT) :: r358 in
  let r360 = [R 453] in
  let r361 = [R 458] in
  let r362 = [R 968] in
  let r363 = R 691 :: r362 in
  let r364 = [R 692] in
  let r365 = Sub (r34) :: r364 in
  let r366 = S (T T_EQUAL) :: r365 in
  let r367 = [R 449] in
  let r368 = Sub (r59) :: r367 in
  let r369 = [R 478] in
  let r370 = S (N N_module_type) :: r369 in
  let r371 = [R 483] in
  let r372 = [R 973] in
  let r373 = [R 970] in
  let r374 = Sub (r263) :: r373 in
  let r375 = S (T T_UIDENT) :: r360 in
  let r376 = [R 971] in
  let r377 = [R 711] in
  let r378 = [R 774] in
  let r379 = [R 91] in
  let r380 = [R 737] in
  let r381 = S (N N_pattern) :: r380 in
  let r382 = [R 772] in
  let r383 = S (T T_RBRACKET) :: r382 in
  let r384 = [R 402] in
  let r385 = R 570 :: r384 in
  let r386 = R 563 :: r385 in
  let r387 = Sub (r353) :: r386 in
  let r388 = [R 771] in
  let r389 = S (T T_RBRACE) :: r388 in
  let r390 = [R 564] in
  let r391 = [R 571] in
  let r392 = S (T T_UNDERSCORE) :: r226 in
  let r393 = [R 832] in
  let r394 = Sub (r392) :: r393 in
  let r395 = [R 604] in
  let r396 = Sub (r394) :: r395 in
  let r397 = R 376 :: r396 in
  let r398 = [R 87] in
  let r399 = [R 842] in
  let r400 = S (T T_INT) :: r398 in
  let r401 = [R 766] in
  let r402 = Sub (r400) :: r401 in
  let r403 = [R 839] in
  let r404 = [R 844] in
  let r405 = S (T T_RBRACKET) :: r404 in
  let r406 = S (T T_LBRACKET) :: r405 in
  let r407 = [R 845] in
  let r408 = [R 680] in
  let r409 = S (T T_DOTDOT) :: r408 in
  let r410 = S (T T_COMMA) :: r409 in
  let r411 = [R 313] in
  let r412 = [R 314] in
  let r413 = S (T T_RPAREN) :: r412 in
  let r414 = Sub (r34) :: r413 in
  let r415 = S (T T_COLON) :: r414 in
  let r416 = [R 312] in
  let r417 = [R 101] in
  let r418 = [R 598] in
  let r419 = S (N N_pattern) :: r418 in
  let r420 = R 376 :: r419 in
  let r421 = [R 600] in
  let r422 = Sub (r394) :: r421 in
  let r423 = [R 599] in
  let r424 = Sub (r394) :: r423 in
  let r425 = S (T T_COMMA) :: r424 in
  let r426 = [R 603] in
  let r427 = [R 678] in
  let r428 = [R 305] in
  let r429 = [R 306] in
  let r430 = S (T T_RPAREN) :: r429 in
  let r431 = Sub (r34) :: r430 in
  let r432 = S (T T_COLON) :: r431 in
  let r433 = [R 304] in
  let r434 = [R 592] in
  let r435 = [R 601] in
  let r436 = [R 497] in
  let r437 = S (T T_LIDENT) :: r436 in
  let r438 = [R 602] in
  let r439 = Sub (r394) :: r438 in
  let r440 = S (T T_RPAREN) :: r439 in
  let r441 = [R 100] in
  let r442 = S (T T_RPAREN) :: r441 in
  let r443 = [R 679] in
  let r444 = [R 309] in
  let r445 = [R 310] in
  let r446 = S (T T_RPAREN) :: r445 in
  let r447 = Sub (r34) :: r446 in
  let r448 = S (T T_COLON) :: r447 in
  let r449 = [R 308] in
  let r450 = [R 847] in
  let r451 = S (T T_RPAREN) :: r450 in
  let r452 = Sub (r34) :: r451 in
  let r453 = [R 597] in
  let r454 = [R 595] in
  let r455 = [R 99] in
  let r456 = S (T T_RPAREN) :: r455 in
  let r457 = [R 846] in
  let r458 = [R 404] in
  let r459 = [R 773] in
  let r460 = [R 320] in
  let r461 = [R 317] in
  let r462 = [R 318] in
  let r463 = S (T T_RPAREN) :: r462 in
  let r464 = Sub (r34) :: r463 in
  let r465 = S (T T_COLON) :: r464 in
  let r466 = [R 316] in
  let r467 = [R 59] in
  let r468 = S (T T_RPAREN) :: r467 in
  let r469 = [R 955] in
  let r470 = Sub (r3) :: r469 in
  let r471 = S (T T_EQUAL) :: r470 in
  let r472 = S (T T_LIDENT) :: r471 in
  let r473 = R 489 :: r472 in
  let r474 = R 376 :: r473 in
  let r475 = [R 46] in
  let r476 = R 382 :: r475 in
  let r477 = [R 956] in
  let r478 = Sub (r3) :: r477 in
  let r479 = S (T T_EQUAL) :: r478 in
  let r480 = S (T T_LIDENT) :: r479 in
  let r481 = R 489 :: r480 in
  let r482 = [R 57] in
  let r483 = Sub (r351) :: r482 in
  let r484 = [R 790] in
  let r485 = Sub (r483) :: r484 in
  let r486 = R 376 :: r485 in
  let r487 = [R 786] in
  let r488 = [R 787] in
  let r489 = S (T T_METAOCAML_BRACKET_CLOSE) :: r488 in
  let r490 = [R 147] in
  let r491 = Sub (r189) :: r490 in
  let r492 = S (T T_WITH) :: r491 in
  let r493 = Sub (r3) :: r492 in
  let r494 = R 376 :: r493 in
  let r495 = [R 775] in
  let r496 = S (T T_RPAREN) :: r495 in
  let r497 = [R 814] in
  let r498 = [R 211] in
  let r499 = [R 361] in
  let r500 = Sub (r24) :: r499 in
  let r501 = [R 364] in
  let r502 = Sub (r500) :: r501 in
  let r503 = [R 208] in
  let r504 = Sub (r3) :: r503 in
  let r505 = S (T T_IN) :: r504 in
  let r506 = [R 687] in
  let r507 = S (T T_DOTDOT) :: r506 in
  let r508 = S (T T_COMMA) :: r507 in
  let r509 = [R 688] in
  let r510 = S (T T_DOTDOT) :: r509 in
  let r511 = S (T T_COMMA) :: r510 in
  let r512 = S (T T_RPAREN) :: r511 in
  let r513 = Sub (r34) :: r512 in
  let r514 = S (T T_COLON) :: r513 in
  let r515 = [R 341] in
  let r516 = [R 342] in
  let r517 = S (T T_RPAREN) :: r516 in
  let r518 = Sub (r34) :: r517 in
  let r519 = S (T T_COLON) :: r518 in
  let r520 = [R 340] in
  let r521 = [R 605] in
  let r522 = [R 684] in
  let r523 = [R 325] in
  let r524 = [R 326] in
  let r525 = S (T T_RPAREN) :: r524 in
  let r526 = Sub (r34) :: r525 in
  let r527 = S (T T_COLON) :: r526 in
  let r528 = [R 324] in
  let r529 = [R 337] in
  let r530 = [R 338] in
  let r531 = S (T T_RPAREN) :: r530 in
  let r532 = Sub (r34) :: r531 in
  let r533 = S (T T_COLON) :: r532 in
  let r534 = [R 336] in
  let r535 = [R 686] in
  let r536 = S (T T_DOTDOT) :: r535 in
  let r537 = S (T T_COMMA) :: r536 in
  let r538 = [R 333] in
  let r539 = [R 334] in
  let r540 = S (T T_RPAREN) :: r539 in
  let r541 = Sub (r34) :: r540 in
  let r542 = S (T T_COLON) :: r541 in
  let r543 = [R 332] in
  let r544 = [R 826] in
  let r545 = [R 294] in
  let r546 = S (T T_LIDENT) :: r545 in
  let r547 = [R 825] in
  let r548 = S (T T_RPAREN) :: r547 in
  let r549 = [R 295] in
  let r550 = [R 619] in
  let r551 = Sub (r34) :: r550 in
  let r552 = [R 822] in
  let r553 = [R 821] in
  let r554 = S (T T_RPAREN) :: r553 in
  let r555 = R 572 :: r554 in
  let r556 = [R 573] in
  let r557 = [R 346] in
  let r558 = Sub (r24) :: r557 in
  let r559 = [R 353] in
  let r560 = R 382 :: r559 in
  let r561 = Sub (r558) :: r560 in
  let r562 = R 631 :: r561 in
  let r563 = R 376 :: r562 in
  let r564 = R 132 :: r563 in
  let r565 = S (T T_QUOTED_STRING_ITEM) :: r216 in
  let r566 = [R 406] in
  let r567 = R 382 :: r566 in
  let r568 = Sub (r565) :: r567 in
  let r569 = [R 145] in
  let r570 = Sub (r3) :: r569 in
  let r571 = S (T T_IN) :: r570 in
  let r572 = Sub (r568) :: r571 in
  let r573 = R 376 :: r572 in
  let r574 = [R 518] in
  let r575 = R 382 :: r574 in
  let r576 = S (N N_module_expr) :: r575 in
  let r577 = R 376 :: r576 in
  let r578 = [R 519] in
  let r579 = R 382 :: r578 in
  let r580 = S (N N_module_expr) :: r579 in
  let r581 = R 376 :: r580 in
  let r582 = [R 579] in
  let r583 = S (T T_RPAREN) :: r582 in
  let r584 = [R 124] in
  let r585 = S (N N_fun_expr) :: r584 in
  let r586 = [R 580] in
  let r587 = S (T T_RPAREN) :: r586 in
  let r588 = Sub (r585) :: r587 in
  let r589 = [R 728] in
  let r590 = S (N N_fun_expr) :: r589 in
  let r591 = [R 817] in
  let r592 = S (T T_RBRACKET) :: r591 in
  let r593 = [R 802] in
  let r594 = [R 734] in
  let r595 = R 565 :: r594 in
  let r596 = [R 566] in
  let r597 = [R 740] in
  let r598 = R 565 :: r597 in
  let r599 = R 574 :: r598 in
  let r600 = Sub (r353) :: r599 in
  let r601 = [R 633] in
  let r602 = Sub (r600) :: r601 in
  let r603 = [R 811] in
  let r604 = S (T T_RBRACE) :: r603 in
  let r605 = S (T T_UIDENT) :: r160 in
  let r606 = Sub (r605) :: r361 in
  let r607 = [R 279] in
  let r608 = [R 789] in
  let r609 = S (T T_END) :: r608 in
  let r610 = R 376 :: r609 in
  let r611 = [R 158] in
  let r612 = Sub (r217) :: r611 in
  let r613 = R 376 :: r612 in
  let r614 = [R 800] in
  let r615 = [R 810] in
  let r616 = S (T T_RPAREN) :: r615 in
  let r617 = S (T T_LPAREN) :: r616 in
  let r618 = S (T T_DOT) :: r617 in
  let r619 = [R 820] in
  let r620 = S (T T_RPAREN) :: r619 in
  let r621 = S (N N_module_type) :: r620 in
  let r622 = S (T T_COLON) :: r621 in
  let r623 = S (N N_module_expr) :: r622 in
  let r624 = R 376 :: r623 in
  let r625 = [R 465] in
  let r626 = S (N N_module_expr) :: r625 in
  let r627 = S (T T_MINUSGREATER) :: r626 in
  let r628 = S (N N_functor_args) :: r627 in
  let r629 = [R 470] in
  let r630 = [R 578] in
  let r631 = S (T T_RPAREN) :: r630 in
  let r632 = [R 362] in
  let r633 = Sub (r3) :: r632 in
  let r634 = S (T T_EQUAL) :: r633 in
  let r635 = [R 662] in
  let r636 = S (N N_fun_expr) :: r635 in
  let r637 = S (T T_COMMA) :: r636 in
  let r638 = [R 807] in
  let r639 = [R 780] in
  let r640 = S (T T_RPAREN) :: r639 in
  let r641 = Sub (r590) :: r640 in
  let r642 = S (T T_LPAREN) :: r641 in
  let r643 = [R 153] in
  let r644 = S (N N_fun_expr) :: r643 in
  let r645 = S (T T_THEN) :: r644 in
  let r646 = Sub (r3) :: r645 in
  let r647 = R 376 :: r646 in
  let r648 = [R 744] in
  let r649 = Sub (r189) :: r648 in
  let r650 = R 376 :: r649 in
  let r651 = [R 706] in
  let r652 = [R 422] in
  let r653 = Sub (r3) :: r652 in
  let r654 = S (T T_MINUSGREATER) :: r653 in
  let r655 = [R 828] in
  let r656 = Sub (r394) :: r655 in
  let r657 = [R 235] in
  let r658 = Sub (r656) :: r657 in
  let r659 = [R 695] in
  let r660 = Sub (r658) :: r659 in
  let r661 = [R 236] in
  let r662 = Sub (r660) :: r661 in
  let r663 = [R 143] in
  let r664 = Sub (r1) :: r663 in
  let r665 = [R 146] in
  let r666 = Sub (r664) :: r665 in
  let r667 = S (T T_MINUSGREATER) :: r666 in
  let r668 = R 561 :: r667 in
  let r669 = Sub (r662) :: r668 in
  let r670 = R 376 :: r669 in
  let r671 = [R 612] in
  let r672 = S (T T_UNDERSCORE) :: r671 in
  let r673 = [R 824] in
  let r674 = [R 823] in
  let r675 = S (T T_RPAREN) :: r674 in
  let r676 = R 572 :: r675 in
  let r677 = [R 359] in
  let r678 = [R 234] in
  let r679 = S (T T_RPAREN) :: r678 in
  let r680 = [R 830] in
  let r681 = S (T T_RPAREN) :: r680 in
  let r682 = Sub (r34) :: r681 in
  let r683 = [R 827] in
  let r684 = [R 829] in
  let r685 = S (T T_RPAREN) :: r684 in
  let r686 = Sub (r34) :: r685 in
  let r687 = [R 562] in
  let r688 = [R 142] in
  let r689 = Sub (r189) :: r688 in
  let r690 = R 376 :: r689 in
  let r691 = [R 657] in
  let r692 = [R 660] in
  let r693 = [R 661] in
  let r694 = S (T T_RPAREN) :: r693 in
  let r695 = Sub (r200) :: r694 in
  let r696 = [R 927] in
  let r697 = [R 659] in
  let r698 = [R 806] in
  let r699 = [R 777] in
  let r700 = S (T T_RPAREN) :: r699 in
  let r701 = Sub (r3) :: r700 in
  let r702 = S (T T_LPAREN) :: r701 in
  let r703 = [R 123] in
  let r704 = S (T T_DOWNTO) :: r703 in
  let r705 = [R 156] in
  let r706 = S (T T_DONE) :: r705 in
  let r707 = Sub (r3) :: r706 in
  let r708 = S (T T_DO) :: r707 in
  let r709 = Sub (r3) :: r708 in
  let r710 = Sub (r704) :: r709 in
  let r711 = Sub (r3) :: r710 in
  let r712 = S (T T_EQUAL) :: r711 in
  let r713 = S (N N_pattern) :: r712 in
  let r714 = R 376 :: r713 in
  let r715 = [R 157] in
  let r716 = Sub (r217) :: r715 in
  let r717 = R 376 :: r716 in
  let r718 = [R 275] in
  let r719 = S (T T_SLASHGREATER) :: r718 in
  let r720 = [R 286] in
  let r721 = [R 288] in
  let r722 = [R 287] in
  let r723 = [R 282] in
  let r724 = S (T T_JSX_LIDENT_E) :: r723 in
  let r725 = [R 276] in
  let r726 = S (T T_GREATER) :: r725 in
  let r727 = Sub (r724) :: r726 in
  let r728 = [R 283] in
  let r729 = [R 203] in
  let r730 = [R 204] in
  let r731 = Sub (r189) :: r730 in
  let r732 = R 376 :: r731 in
  let r733 = [R 299] in
  let r734 = [R 300] in
  let r735 = S (T T_RPAREN) :: r734 in
  let r736 = Sub (r200) :: r735 in
  let r737 = [R 301] in
  let r738 = [R 302] in
  let r739 = [R 298] in
  let r740 = [R 730] in
  let r741 = Sub (r189) :: r740 in
  let r742 = R 376 :: r741 in
  let r743 = [R 647] in
  let r744 = [R 650] in
  let r745 = [R 651] in
  let r746 = S (T T_RPAREN) :: r745 in
  let r747 = Sub (r200) :: r746 in
  let r748 = [R 649] in
  let r749 = [R 648] in
  let r750 = Sub (r189) :: r749 in
  let r751 = R 376 :: r750 in
  let r752 = [R 707] in
  let r753 = [R 207] in
  let r754 = Sub (r3) :: r753 in
  let r755 = [R 183] in
  let r756 = [R 184] in
  let r757 = Sub (r189) :: r756 in
  let r758 = R 376 :: r757 in
  let r759 = [R 171] in
  let r760 = [R 172] in
  let r761 = Sub (r189) :: r760 in
  let r762 = R 376 :: r761 in
  let r763 = [R 205] in
  let r764 = [R 206] in
  let r765 = Sub (r189) :: r764 in
  let r766 = R 376 :: r765 in
  let r767 = [R 240] in
  let r768 = Sub (r3) :: r767 in
  let r769 = [R 177] in
  let r770 = [R 178] in
  let r771 = Sub (r189) :: r770 in
  let r772 = R 376 :: r771 in
  let r773 = [R 185] in
  let r774 = [R 186] in
  let r775 = Sub (r189) :: r774 in
  let r776 = R 376 :: r775 in
  let r777 = [R 169] in
  let r778 = [R 170] in
  let r779 = Sub (r189) :: r778 in
  let r780 = R 376 :: r779 in
  let r781 = [R 175] in
  let r782 = [R 176] in
  let r783 = Sub (r189) :: r782 in
  let r784 = R 376 :: r783 in
  let r785 = [R 173] in
  let r786 = [R 174] in
  let r787 = Sub (r189) :: r786 in
  let r788 = R 376 :: r787 in
  let r789 = [R 193] in
  let r790 = [R 194] in
  let r791 = Sub (r189) :: r790 in
  let r792 = R 376 :: r791 in
  let r793 = [R 181] in
  let r794 = [R 182] in
  let r795 = Sub (r189) :: r794 in
  let r796 = R 376 :: r795 in
  let r797 = [R 179] in
  let r798 = [R 180] in
  let r799 = Sub (r189) :: r798 in
  let r800 = R 376 :: r799 in
  let r801 = [R 189] in
  let r802 = [R 190] in
  let r803 = Sub (r189) :: r802 in
  let r804 = R 376 :: r803 in
  let r805 = [R 167] in
  let r806 = [R 168] in
  let r807 = Sub (r189) :: r806 in
  let r808 = R 376 :: r807 in
  let r809 = [R 165] in
  let r810 = [R 166] in
  let r811 = Sub (r189) :: r810 in
  let r812 = R 376 :: r811 in
  let r813 = [R 209] in
  let r814 = [R 210] in
  let r815 = Sub (r189) :: r814 in
  let r816 = R 376 :: r815 in
  let r817 = [R 163] in
  let r818 = [R 164] in
  let r819 = Sub (r189) :: r818 in
  let r820 = R 376 :: r819 in
  let r821 = [R 191] in
  let r822 = [R 192] in
  let r823 = Sub (r189) :: r822 in
  let r824 = R 376 :: r823 in
  let r825 = [R 187] in
  let r826 = [R 188] in
  let r827 = Sub (r189) :: r826 in
  let r828 = R 376 :: r827 in
  let r829 = [R 195] in
  let r830 = [R 196] in
  let r831 = Sub (r189) :: r830 in
  let r832 = R 376 :: r831 in
  let r833 = [R 197] in
  let r834 = [R 198] in
  let r835 = Sub (r189) :: r834 in
  let r836 = R 376 :: r835 in
  let r837 = [R 199] in
  let r838 = [R 200] in
  let r839 = Sub (r189) :: r838 in
  let r840 = R 376 :: r839 in
  let r841 = [R 652] in
  let r842 = [R 655] in
  let r843 = [R 656] in
  let r844 = S (T T_RPAREN) :: r843 in
  let r845 = Sub (r200) :: r844 in
  let r846 = [R 654] in
  let r847 = [R 653] in
  let r848 = Sub (r189) :: r847 in
  let r849 = R 376 :: r848 in
  let r850 = [R 201] in
  let r851 = [R 202] in
  let r852 = Sub (r189) :: r851 in
  let r853 = R 376 :: r852 in
  let r854 = [R 19] in
  let r855 = R 382 :: r854 in
  let r856 = Sub (r558) :: r855 in
  let r857 = [R 902] in
  let r858 = Sub (r3) :: r857 in
  let r859 = [R 350] in
  let r860 = Sub (r3) :: r859 in
  let r861 = S (T T_EQUAL) :: r860 in
  let r862 = Sub (r34) :: r861 in
  let r863 = S (T T_DOT) :: r862 in
  let r864 = [R 349] in
  let r865 = Sub (r3) :: r864 in
  let r866 = S (T T_EQUAL) :: r865 in
  let r867 = Sub (r34) :: r866 in
  let r868 = [R 348] in
  let r869 = Sub (r3) :: r868 in
  let r870 = [R 903] in
  let r871 = Sub (r664) :: r870 in
  let r872 = S (T T_EQUAL) :: r871 in
  let r873 = [R 352] in
  let r874 = Sub (r3) :: r873 in
  let r875 = S (T T_EQUAL) :: r874 in
  let r876 = [R 351] in
  let r877 = Sub (r3) :: r876 in
  let r878 = [R 685] in
  let r879 = [R 329] in
  let r880 = [R 330] in
  let r881 = S (T T_RPAREN) :: r880 in
  let r882 = Sub (r34) :: r881 in
  let r883 = S (T T_COLON) :: r882 in
  let r884 = [R 328] in
  let r885 = [R 610] in
  let r886 = [R 608] in
  let r887 = [R 383] in
  let r888 = [R 221] in
  let r889 = [R 222] in
  let r890 = Sub (r189) :: r889 in
  let r891 = R 376 :: r890 in
  let r892 = [R 784] in
  let r893 = S (T T_RBRACKET) :: r892 in
  let r894 = Sub (r590) :: r893 in
  let r895 = [R 229] in
  let r896 = [R 230] in
  let r897 = Sub (r189) :: r896 in
  let r898 = R 376 :: r897 in
  let r899 = [R 782] in
  let r900 = S (T T_RBRACE) :: r899 in
  let r901 = Sub (r590) :: r900 in
  let r902 = [R 225] in
  let r903 = [R 226] in
  let r904 = Sub (r189) :: r903 in
  let r905 = R 376 :: r904 in
  let r906 = [R 215] in
  let r907 = [R 216] in
  let r908 = Sub (r189) :: r907 in
  let r909 = R 376 :: r908 in
  let r910 = [R 779] in
  let r911 = S (T T_RBRACKET) :: r910 in
  let r912 = Sub (r3) :: r911 in
  let r913 = [R 219] in
  let r914 = [R 220] in
  let r915 = Sub (r189) :: r914 in
  let r916 = R 376 :: r915 in
  let r917 = [R 778] in
  let r918 = S (T T_RBRACE) :: r917 in
  let r919 = Sub (r3) :: r918 in
  let r920 = [R 217] in
  let r921 = [R 218] in
  let r922 = Sub (r189) :: r921 in
  let r923 = R 376 :: r922 in
  let r924 = [R 781] in
  let r925 = S (T T_RPAREN) :: r924 in
  let r926 = Sub (r590) :: r925 in
  let r927 = S (T T_LPAREN) :: r926 in
  let r928 = [R 223] in
  let r929 = [R 224] in
  let r930 = Sub (r189) :: r929 in
  let r931 = R 376 :: r930 in
  let r932 = [R 785] in
  let r933 = S (T T_RBRACKET) :: r932 in
  let r934 = Sub (r590) :: r933 in
  let r935 = [R 231] in
  let r936 = [R 232] in
  let r937 = Sub (r189) :: r936 in
  let r938 = R 376 :: r937 in
  let r939 = [R 783] in
  let r940 = S (T T_RBRACE) :: r939 in
  let r941 = Sub (r590) :: r940 in
  let r942 = [R 227] in
  let r943 = [R 228] in
  let r944 = Sub (r189) :: r943 in
  let r945 = R 376 :: r944 in
  let r946 = [R 213] in
  let r947 = [R 214] in
  let r948 = Sub (r189) :: r947 in
  let r949 = R 376 :: r948 in
  let r950 = [R 658] in
  let r951 = Sub (r189) :: r950 in
  let r952 = R 376 :: r951 in
  let r953 = [R 154] in
  let r954 = Sub (r189) :: r953 in
  let r955 = R 376 :: r954 in
  let r956 = [R 151] in
  let r957 = [R 152] in
  let r958 = Sub (r189) :: r957 in
  let r959 = R 376 :: r958 in
  let r960 = [R 149] in
  let r961 = [R 150] in
  let r962 = Sub (r189) :: r961 in
  let r963 = R 376 :: r962 in
  let r964 = [R 665] in
  let r965 = [R 666] in
  let r966 = S (T T_RPAREN) :: r965 in
  let r967 = Sub (r200) :: r966 in
  let r968 = [R 664] in
  let r969 = [R 663] in
  let r970 = Sub (r189) :: r969 in
  let r971 = R 376 :: r970 in
  let r972 = [R 363] in
  let r973 = Sub (r3) :: r972 in
  let r974 = [R 365] in
  let r975 = [R 804] in
  let r976 = [R 816] in
  let r977 = [R 815] in
  let r978 = [R 819] in
  let r979 = [R 818] in
  let r980 = S (T T_LIDENT) :: r595 in
  let r981 = [R 805] in
  let r982 = S (T T_GREATERRBRACE) :: r981 in
  let r983 = [R 812] in
  let r984 = S (T T_RBRACE) :: r983 in
  let r985 = [R 634] in
  let r986 = Sub (r600) :: r985 in
  let r987 = [R 788] in
  let r988 = [R 567] in
  let r989 = Sub (r189) :: r988 in
  let r990 = R 376 :: r989 in
  let r991 = [R 801] in
  let r992 = [R 125] in
  let r993 = Sub (r189) :: r992 in
  let r994 = R 376 :: r993 in
  let r995 = [R 131] in
  let r996 = [R 127] in
  let r997 = [R 129] in
  let r998 = [R 130] in
  let r999 = [R 126] in
  let r1000 = [R 128] in
  let r1001 = [R 459] in
  let r1002 = S (N N_module_expr) :: r1001 in
  let r1003 = S (T T_EQUAL) :: r1002 in
  let r1004 = [R 419] in
  let r1005 = R 382 :: r1004 in
  let r1006 = Sub (r1003) :: r1005 in
  let r1007 = Sub (r323) :: r1006 in
  let r1008 = R 376 :: r1007 in
  let r1009 = [R 486] in
  let r1010 = R 382 :: r1009 in
  let r1011 = R 568 :: r1010 in
  let r1012 = Sub (r59) :: r1011 in
  let r1013 = R 376 :: r1012 in
  let r1014 = R 132 :: r1013 in
  let r1015 = [R 569] in
  let r1016 = [R 414] in
  let r1017 = R 372 :: r1016 in
  let r1018 = R 382 :: r1017 in
  let r1019 = Sub (r1003) :: r1018 in
  let r1020 = [R 460] in
  let r1021 = S (N N_module_expr) :: r1020 in
  let r1022 = S (T T_EQUAL) :: r1021 in
  let r1023 = [R 373] in
  let r1024 = R 372 :: r1023 in
  let r1025 = R 382 :: r1024 in
  let r1026 = Sub (r1003) :: r1025 in
  let r1027 = Sub (r323) :: r1026 in
  let r1028 = [R 461] in
  let r1029 = [R 273] in
  let r1030 = S (T T_RBRACKET) :: r1029 in
  let r1031 = Sub (r17) :: r1030 in
  let r1032 = [R 616] in
  let r1033 = [R 617] in
  let r1034 = [R 139] in
  let r1035 = S (T T_RBRACKET) :: r1034 in
  let r1036 = Sub (r19) :: r1035 in
  let r1037 = [R 907] in
  let r1038 = R 382 :: r1037 in
  let r1039 = S (N N_module_expr) :: r1038 in
  let r1040 = R 376 :: r1039 in
  let r1041 = [R 499] in
  let r1042 = S (T T_STRING) :: r1041 in
  let r1043 = [R 623] in
  let r1044 = R 382 :: r1043 in
  let r1045 = Sub (r1042) :: r1044 in
  let r1046 = S (T T_EQUAL) :: r1045 in
  let r1047 = Sub (r36) :: r1046 in
  let r1048 = S (T T_COLON) :: r1047 in
  let r1049 = Sub (r24) :: r1048 in
  let r1050 = R 376 :: r1049 in
  let r1051 = [R 745] in
  let r1052 = R 382 :: r1051 in
  let r1053 = R 376 :: r1052 in
  let r1054 = R 255 :: r1053 in
  let r1055 = Sub (r100) :: r1054 in
  let r1056 = R 376 :: r1055 in
  let r1057 = R 132 :: r1056 in
  let r1058 = [R 103] in
  let r1059 = Sub (r26) :: r1058 in
  let r1060 = [R 256] in
  let r1061 = [R 620] in
  let r1062 = Sub (r32) :: r1061 in
  let r1063 = [R 289] in
  let r1064 = R 376 :: r1063 in
  let r1065 = Sub (r1062) :: r1064 in
  let r1066 = S (T T_COLON) :: r1065 in
  let r1067 = S (T T_LIDENT) :: r1066 in
  let r1068 = R 489 :: r1067 in
  let r1069 = [R 291] in
  let r1070 = Sub (r1068) :: r1069 in
  let r1071 = [R 105] in
  let r1072 = S (T T_RBRACE) :: r1071 in
  let r1073 = [R 290] in
  let r1074 = R 376 :: r1073 in
  let r1075 = S (T T_SEMI) :: r1074 in
  let r1076 = R 376 :: r1075 in
  let r1077 = Sub (r1062) :: r1076 in
  let r1078 = S (T T_COLON) :: r1077 in
  let r1079 = [R 621] in
  let r1080 = Sub (r32) :: r1079 in
  let r1081 = [R 104] in
  let r1082 = Sub (r26) :: r1081 in
  let r1083 = Sub (r98) :: r417 in
  let r1084 = [R 901] in
  let r1085 = R 382 :: r1084 in
  let r1086 = R 376 :: r1085 in
  let r1087 = S (T T_COLONCOLON) :: r456 in
  let r1088 = [R 259] in
  let r1089 = [R 260] in
  let r1090 = Sub (r26) :: r1089 in
  let r1091 = [R 258] in
  let r1092 = Sub (r26) :: r1091 in
  let r1093 = [R 257] in
  let r1094 = Sub (r26) :: r1093 in
  let r1095 = [R 614] in
  let r1096 = [R 385] in
  let r1097 = [R 520] in
  let r1098 = R 382 :: r1097 in
  let r1099 = Sub (r263) :: r1098 in
  let r1100 = R 376 :: r1099 in
  let r1101 = [R 521] in
  let r1102 = R 382 :: r1101 in
  let r1103 = Sub (r263) :: r1102 in
  let r1104 = R 376 :: r1103 in
  let r1105 = [R 462] in
  let r1106 = S (N N_module_type) :: r1105 in
  let r1107 = S (T T_COLON) :: r1106 in
  let r1108 = [R 756] in
  let r1109 = R 382 :: r1108 in
  let r1110 = Sub (r1107) :: r1109 in
  let r1111 = Sub (r323) :: r1110 in
  let r1112 = R 376 :: r1111 in
  let r1113 = [R 487] in
  let r1114 = R 382 :: r1113 in
  let r1115 = S (N N_module_type) :: r1114 in
  let r1116 = S (T T_COLONEQUAL) :: r1115 in
  let r1117 = Sub (r59) :: r1116 in
  let r1118 = R 376 :: r1117 in
  let r1119 = [R 475] in
  let r1120 = R 382 :: r1119 in
  let r1121 = [R 759] in
  let r1122 = R 374 :: r1121 in
  let r1123 = R 382 :: r1122 in
  let r1124 = S (N N_module_type) :: r1123 in
  let r1125 = S (T T_COLON) :: r1124 in
  let r1126 = [R 375] in
  let r1127 = R 374 :: r1126 in
  let r1128 = R 382 :: r1127 in
  let r1129 = S (N N_module_type) :: r1128 in
  let r1130 = S (T T_COLON) :: r1129 in
  let r1131 = Sub (r323) :: r1130 in
  let r1132 = [R 757] in
  let r1133 = R 382 :: r1132 in
  let r1134 = [R 463] in
  let r1135 = [R 763] in
  let r1136 = R 382 :: r1135 in
  let r1137 = S (N N_module_type) :: r1136 in
  let r1138 = R 376 :: r1137 in
  let r1139 = S (T T_QUOTED_STRING_EXPR) :: r57 in
  let r1140 = [R 71] in
  let r1141 = Sub (r1139) :: r1140 in
  let r1142 = [R 81] in
  let r1143 = Sub (r1141) :: r1142 in
  let r1144 = [R 764] in
  let r1145 = R 368 :: r1144 in
  let r1146 = R 382 :: r1145 in
  let r1147 = Sub (r1143) :: r1146 in
  let r1148 = S (T T_COLON) :: r1147 in
  let r1149 = S (T T_LIDENT) :: r1148 in
  let r1150 = R 140 :: r1149 in
  let r1151 = R 960 :: r1150 in
  let r1152 = R 376 :: r1151 in
  let r1153 = [R 85] in
  let r1154 = R 370 :: r1153 in
  let r1155 = R 382 :: r1154 in
  let r1156 = Sub (r1141) :: r1155 in
  let r1157 = S (T T_EQUAL) :: r1156 in
  let r1158 = S (T T_LIDENT) :: r1157 in
  let r1159 = R 140 :: r1158 in
  let r1160 = R 960 :: r1159 in
  let r1161 = R 376 :: r1160 in
  let r1162 = [R 141] in
  let r1163 = S (T T_RBRACKET) :: r1162 in
  let r1164 = [R 72] in
  let r1165 = S (T T_END) :: r1164 in
  let r1166 = R 391 :: r1165 in
  let r1167 = R 62 :: r1166 in
  let r1168 = [R 61] in
  let r1169 = S (T T_RPAREN) :: r1168 in
  let r1170 = [R 64] in
  let r1171 = R 382 :: r1170 in
  let r1172 = Sub (r34) :: r1171 in
  let r1173 = S (T T_COLON) :: r1172 in
  let r1174 = S (T T_LIDENT) :: r1173 in
  let r1175 = R 491 :: r1174 in
  let r1176 = [R 65] in
  let r1177 = R 382 :: r1176 in
  let r1178 = Sub (r36) :: r1177 in
  let r1179 = S (T T_COLON) :: r1178 in
  let r1180 = S (T T_LIDENT) :: r1179 in
  let r1181 = R 626 :: r1180 in
  let r1182 = [R 63] in
  let r1183 = R 382 :: r1182 in
  let r1184 = Sub (r1141) :: r1183 in
  let r1185 = [R 74] in
  let r1186 = Sub (r1141) :: r1185 in
  let r1187 = S (T T_IN) :: r1186 in
  let r1188 = Sub (r606) :: r1187 in
  let r1189 = R 376 :: r1188 in
  let r1190 = [R 75] in
  let r1191 = Sub (r1141) :: r1190 in
  let r1192 = S (T T_IN) :: r1191 in
  let r1193 = Sub (r606) :: r1192 in
  let r1194 = [R 714] in
  let r1195 = Sub (r34) :: r1194 in
  let r1196 = [R 70] in
  let r1197 = Sub (r256) :: r1196 in
  let r1198 = S (T T_RBRACKET) :: r1197 in
  let r1199 = Sub (r1195) :: r1198 in
  let r1200 = [R 715] in
  let r1201 = [R 102] in
  let r1202 = Sub (r34) :: r1201 in
  let r1203 = S (T T_EQUAL) :: r1202 in
  let r1204 = Sub (r34) :: r1203 in
  let r1205 = [R 66] in
  let r1206 = R 382 :: r1205 in
  let r1207 = Sub (r1204) :: r1206 in
  let r1208 = [R 67] in
  let r1209 = [R 392] in
  let r1210 = [R 371] in
  let r1211 = R 370 :: r1210 in
  let r1212 = R 382 :: r1211 in
  let r1213 = Sub (r1141) :: r1212 in
  let r1214 = S (T T_EQUAL) :: r1213 in
  let r1215 = S (T T_LIDENT) :: r1214 in
  let r1216 = R 140 :: r1215 in
  let r1217 = R 960 :: r1216 in
  let r1218 = [R 83] in
  let r1219 = Sub (r1143) :: r1218 in
  let r1220 = S (T T_MINUSGREATER) :: r1219 in
  let r1221 = Sub (r28) :: r1220 in
  let r1222 = [R 84] in
  let r1223 = Sub (r1143) :: r1222 in
  let r1224 = [R 82] in
  let r1225 = Sub (r1143) :: r1224 in
  let r1226 = S (T T_MINUSGREATER) :: r1225 in
  let r1227 = [R 369] in
  let r1228 = R 368 :: r1227 in
  let r1229 = R 382 :: r1228 in
  let r1230 = Sub (r1143) :: r1229 in
  let r1231 = S (T T_COLON) :: r1230 in
  let r1232 = S (T T_LIDENT) :: r1231 in
  let r1233 = R 140 :: r1232 in
  let r1234 = R 960 :: r1233 in
  let r1235 = [R 386] in
  let r1236 = [R 747] in
  let r1237 = [R 751] in
  let r1238 = [R 379] in
  let r1239 = R 378 :: r1238 in
  let r1240 = R 382 :: r1239 in
  let r1241 = R 691 :: r1240 in
  let r1242 = R 929 :: r1241 in
  let r1243 = S (T T_LIDENT) :: r1242 in
  let r1244 = R 933 :: r1243 in
  let r1245 = [R 752] in
  let r1246 = [R 381] in
  let r1247 = R 380 :: r1246 in
  let r1248 = R 382 :: r1247 in
  let r1249 = R 691 :: r1248 in
  let r1250 = Sub (r139) :: r1249 in
  let r1251 = S (T T_COLONEQUAL) :: r1250 in
  let r1252 = S (T T_LIDENT) :: r1251 in
  let r1253 = R 933 :: r1252 in
  let r1254 = [R 511] in
  let r1255 = S (T T_RBRACE) :: r1254 in
  let r1256 = [R 515] in
  let r1257 = [R 261] in
  let r1258 = R 376 :: r1257 in
  let r1259 = R 255 :: r1258 in
  let r1260 = Sub (r100) :: r1259 in
  let r1261 = [R 509] in
  let r1262 = [R 510] in
  let r1263 = [R 514] in
  let r1264 = S (T T_RBRACE) :: r1263 in
  let r1265 = [R 513] in
  let r1266 = S (T T_RBRACE) :: r1265 in
  let r1267 = [R 43] in
  let r1268 = Sub (r1139) :: r1267 in
  let r1269 = [R 52] in
  let r1270 = Sub (r1268) :: r1269 in
  let r1271 = S (T T_EQUAL) :: r1270 in
  let r1272 = [R 416] in
  let r1273 = R 366 :: r1272 in
  let r1274 = R 382 :: r1273 in
  let r1275 = Sub (r1271) :: r1274 in
  let r1276 = S (T T_LIDENT) :: r1275 in
  let r1277 = R 140 :: r1276 in
  let r1278 = R 960 :: r1277 in
  let r1279 = R 376 :: r1278 in
  let r1280 = [R 80] in
  let r1281 = S (T T_END) :: r1280 in
  let r1282 = R 393 :: r1281 in
  let r1283 = R 60 :: r1282 in
  let r1284 = S (T T_EQUAL) :: r858 in
  let r1285 = [R 432] in
  let r1286 = Sub (r1284) :: r1285 in
  let r1287 = S (T T_LIDENT) :: r1286 in
  let r1288 = R 624 :: r1287 in
  let r1289 = R 376 :: r1288 in
  let r1290 = [R 47] in
  let r1291 = R 382 :: r1290 in
  let r1292 = [R 433] in
  let r1293 = Sub (r1284) :: r1292 in
  let r1294 = S (T T_LIDENT) :: r1293 in
  let r1295 = R 624 :: r1294 in
  let r1296 = [R 435] in
  let r1297 = Sub (r3) :: r1296 in
  let r1298 = S (T T_EQUAL) :: r1297 in
  let r1299 = [R 437] in
  let r1300 = Sub (r3) :: r1299 in
  let r1301 = S (T T_EQUAL) :: r1300 in
  let r1302 = Sub (r34) :: r1301 in
  let r1303 = S (T T_DOT) :: r1302 in
  let r1304 = [R 431] in
  let r1305 = Sub (r36) :: r1304 in
  let r1306 = S (T T_COLON) :: r1305 in
  let r1307 = [R 434] in
  let r1308 = Sub (r3) :: r1307 in
  let r1309 = S (T T_EQUAL) :: r1308 in
  let r1310 = [R 436] in
  let r1311 = Sub (r3) :: r1310 in
  let r1312 = S (T T_EQUAL) :: r1311 in
  let r1313 = Sub (r34) :: r1312 in
  let r1314 = S (T T_DOT) :: r1313 in
  let r1315 = [R 49] in
  let r1316 = R 382 :: r1315 in
  let r1317 = Sub (r3) :: r1316 in
  let r1318 = [R 44] in
  let r1319 = R 382 :: r1318 in
  let r1320 = R 559 :: r1319 in
  let r1321 = Sub (r1268) :: r1320 in
  let r1322 = [R 45] in
  let r1323 = R 382 :: r1322 in
  let r1324 = R 559 :: r1323 in
  let r1325 = Sub (r1268) :: r1324 in
  let r1326 = [R 76] in
  let r1327 = S (T T_RPAREN) :: r1326 in
  let r1328 = [R 39] in
  let r1329 = Sub (r1268) :: r1328 in
  let r1330 = S (T T_IN) :: r1329 in
  let r1331 = Sub (r606) :: r1330 in
  let r1332 = R 376 :: r1331 in
  let r1333 = [R 356] in
  let r1334 = R 382 :: r1333 in
  let r1335 = Sub (r558) :: r1334 in
  let r1336 = R 631 :: r1335 in
  let r1337 = R 376 :: r1336 in
  let r1338 = [R 40] in
  let r1339 = Sub (r1268) :: r1338 in
  let r1340 = S (T T_IN) :: r1339 in
  let r1341 = Sub (r606) :: r1340 in
  let r1342 = [R 78] in
  let r1343 = Sub (r483) :: r1342 in
  let r1344 = S (T T_RBRACKET) :: r1343 in
  let r1345 = [R 55] in
  let r1346 = Sub (r1268) :: r1345 in
  let r1347 = S (T T_MINUSGREATER) :: r1346 in
  let r1348 = Sub (r656) :: r1347 in
  let r1349 = [R 37] in
  let r1350 = Sub (r1348) :: r1349 in
  let r1351 = [R 38] in
  let r1352 = Sub (r1268) :: r1351 in
  let r1353 = [R 355] in
  let r1354 = R 382 :: r1353 in
  let r1355 = Sub (r558) :: r1354 in
  let r1356 = [R 79] in
  let r1357 = S (T T_RPAREN) :: r1356 in
  let r1358 = [R 560] in
  let r1359 = [R 48] in
  let r1360 = R 382 :: r1359 in
  let r1361 = Sub (r1204) :: r1360 in
  let r1362 = [R 50] in
  let r1363 = [R 394] in
  let r1364 = [R 53] in
  let r1365 = Sub (r1268) :: r1364 in
  let r1366 = S (T T_EQUAL) :: r1365 in
  let r1367 = [R 54] in
  let r1368 = [R 367] in
  let r1369 = R 366 :: r1368 in
  let r1370 = R 382 :: r1369 in
  let r1371 = Sub (r1271) :: r1370 in
  let r1372 = S (T T_LIDENT) :: r1371 in
  let r1373 = R 140 :: r1372 in
  let r1374 = R 960 :: r1373 in
  let r1375 = [R 390] in
  let r1376 = [R 410] in
  let r1377 = [R 905] in
  let r1378 = R 387 :: r1377 in
  let r1379 = [R 212] in
  let r1380 = Sub (r189) :: r1379 in
  let r1381 = R 376 :: r1380 in
  let r1382 = [R 813] in
  let r1383 = [R 791] in
  let r1384 = S (T T_RPAREN) :: r1383 in
  let r1385 = S (N N_module_expr) :: r1384 in
  let r1386 = R 376 :: r1385 in
  let r1387 = [R 792] in
  let r1388 = S (T T_RPAREN) :: r1387 in
  let r1389 = [R 776] in
  let r1390 = [R 958] in
  let r1391 = Sub (r3) :: r1390 in
  let r1392 = [R 954] in
  let r1393 = Sub (r34) :: r1392 in
  let r1394 = S (T T_COLON) :: r1393 in
  let r1395 = [R 957] in
  let r1396 = Sub (r3) :: r1395 in
  let r1397 = [R 389] in
  let r1398 = R 387 :: r1397 in
  let r1399 = [R 428] in
  let r1400 = R 376 :: r1399 in
  let r1401 = Sub (r1062) :: r1400 in
  let r1402 = [R 426] in
  let r1403 = [R 516] in
  let r1404 = [R 675] in
  let r1405 = [R 676] in
  let r1406 = S (T T_RPAREN) :: r1405 in
  let r1407 = Sub (r200) :: r1406 in
  let r1408 = [R 674] in
  let r1409 = [R 673] in
  let r1410 = Sub (r189) :: r1409 in
  let r1411 = R 376 :: r1410 in
  let r1412 = [R 670] in
  let r1413 = [R 671] in
  let r1414 = S (T T_RPAREN) :: r1413 in
  let r1415 = Sub (r200) :: r1414 in
  let r1416 = [R 669] in
  let r1417 = [R 668] in
  let r1418 = Sub (r189) :: r1417 in
  let r1419 = R 376 :: r1418 in
  let r1420 = [R 136] in
  let r1421 = R 376 :: r1420 in
  let r1422 = [R 137] in
  let r1423 = R 376 :: r1422 in
  let r1424 = [R 244] in
  let r1425 = Sub (r30) :: r1424 in
  let r1426 = S (T T_MINUSGREATER) :: r1425 in
  let r1427 = S (T T_RPAREN) :: r1426 in
  let r1428 = Sub (r34) :: r1427 in
  let r1429 = [R 245] in
  let r1430 = Sub (r30) :: r1429 in
  let r1431 = [R 248] in
  let r1432 = [R 246] in
  let r1433 = Sub (r30) :: r1432 in
  let r1434 = S (T T_MINUSGREATER) :: r1433 in
  let r1435 = S (T T_RPAREN) :: r1434 in
  let r1436 = Sub (r34) :: r1435 in
  let r1437 = [R 512] in
  let r1438 = S (T T_RBRACE) :: r1437 in
  let r1439 = [R 264] in
  let r1440 = R 382 :: r1439 in
  let r1441 = R 691 :: r1440 in
  let r1442 = [R 263] in
  let r1443 = R 382 :: r1442 in
  let r1444 = R 691 :: r1443 in
  let r1445 = [R 269] in
  let r1446 = [R 272] in
  let r1447 = [R 443] in
  let r1448 = [R 446] in
  let r1449 = S (T T_RPAREN) :: r1448 in
  let r1450 = S (T T_COLONCOLON) :: r1449 in
  let r1451 = S (T T_LPAREN) :: r1450 in
  let r1452 = [R 581] in
  let r1453 = [R 582] in
  let r1454 = [R 583] in
  let r1455 = [R 584] in
  let r1456 = [R 585] in
  let r1457 = [R 586] in
  let r1458 = [R 587] in
  let r1459 = [R 588] in
  let r1460 = [R 589] in
  let r1461 = [R 590] in
  let r1462 = [R 591] in
  let r1463 = [R 913] in
  let r1464 = [R 922] in
  let r1465 = [R 396] in
  let r1466 = [R 920] in
  let r1467 = S (T T_SEMISEMI) :: r1466 in
  let r1468 = [R 921] in
  let r1469 = [R 398] in
  let r1470 = [R 401] in
  let r1471 = [R 400] in
  let r1472 = [R 399] in
  let r1473 = R 397 :: r1472 in
  let r1474 = [R 949] in
  let r1475 = S (T T_EOF) :: r1474 in
  let r1476 = R 397 :: r1475 in
  let r1477 = [R 948] in
  function
  | 0 | 2183 | 2187 | 2205 | 2209 | 2213 | 2217 | 2221 | 2225 | 2229 | 2233 | 2237 | 2241 | 2246 | 2266 -> Nothing
  | 2182 -> One ([R 0])
  | 2186 -> One ([R 1])
  | 2192 -> One ([R 2])
  | 2206 -> One ([R 3])
  | 2210 -> One ([R 4])
  | 2216 -> One ([R 5])
  | 2218 -> One ([R 6])
  | 2222 -> One ([R 7])
  | 2226 -> One ([R 8])
  | 2230 -> One ([R 9])
  | 2234 -> One ([R 10])
  | 2240 -> One ([R 11])
  | 2244 -> One ([R 12])
  | 2256 -> One ([R 13])
  | 2276 -> One ([R 14])
  | 601 -> One ([R 15])
  | 600 -> One ([R 16])
  | 2200 -> One ([R 20])
  | 2202 -> One ([R 21])
  | 266 -> One ([R 22])
  | 246 -> One ([R 23])
  | 277 -> One ([R 24])
  | 1863 -> One ([R 36])
  | 1867 -> One ([R 41])
  | 1864 -> One ([R 42])
  | 1903 -> One ([R 51])
  | 1870 -> One ([R 56])
  | 1659 -> One ([R 68])
  | 1639 -> One ([R 69])
  | 1641 -> One ([R 73])
  | 1865 -> One ([R 77])
  | 454 -> One ([R 88])
  | 210 -> One ([R 89])
  | 452 -> One ([R 90])
  | 159 -> One ([R 94])
  | 158 | 1473 -> One ([R 95])
  | 1504 -> One ([R 98])
  | 1743 -> One ([R 106])
  | 1747 -> One ([R 107])
  | 269 -> One ([R 109])
  | 258 -> One ([R 110])
  | 263 -> One ([R 111])
  | 265 -> One ([R 112])
  | 1248 -> One ([R 122])
  | 1 -> One (R 132 :: r9)
  | 62 -> One (R 132 :: r42)
  | 192 -> One (R 132 :: r194)
  | 214 -> One (R 132 :: r223)
  | 358 -> One (R 132 :: r327)
  | 446 -> One (R 132 :: r397)
  | 484 -> One (R 132 :: r420)
  | 602 -> One (R 132 :: r486)
  | 611 -> One (R 132 :: r494)
  | 705 -> One (R 132 :: r577)
  | 706 -> One (R 132 :: r581)
  | 727 -> One (R 132 :: r610)
  | 730 -> One (R 132 :: r613)
  | 743 -> One (R 132 :: r624)
  | 780 -> One (R 132 :: r647)
  | 783 -> One (R 132 :: r650)
  | 789 -> One (R 132 :: r670)
  | 831 -> One (R 132 :: r690)
  | 852 -> One (R 132 :: r714)
  | 857 -> One (R 132 :: r717)
  | 887 -> One (R 132 :: r732)
  | 907 -> One (R 132 :: r742)
  | 923 -> One (R 132 :: r751)
  | 937 -> One (R 132 :: r758)
  | 943 -> One (R 132 :: r762)
  | 952 -> One (R 132 :: r766)
  | 963 -> One (R 132 :: r772)
  | 969 -> One (R 132 :: r776)
  | 975 -> One (R 132 :: r780)
  | 981 -> One (R 132 :: r784)
  | 987 -> One (R 132 :: r788)
  | 993 -> One (R 132 :: r792)
  | 999 -> One (R 132 :: r796)
  | 1005 -> One (R 132 :: r800)
  | 1011 -> One (R 132 :: r804)
  | 1017 -> One (R 132 :: r808)
  | 1023 -> One (R 132 :: r812)
  | 1029 -> One (R 132 :: r816)
  | 1035 -> One (R 132 :: r820)
  | 1041 -> One (R 132 :: r824)
  | 1047 -> One (R 132 :: r828)
  | 1053 -> One (R 132 :: r832)
  | 1059 -> One (R 132 :: r836)
  | 1065 -> One (R 132 :: r840)
  | 1079 -> One (R 132 :: r849)
  | 1085 -> One (R 132 :: r853)
  | 1155 -> One (R 132 :: r891)
  | 1164 -> One (R 132 :: r898)
  | 1173 -> One (R 132 :: r905)
  | 1183 -> One (R 132 :: r909)
  | 1192 -> One (R 132 :: r916)
  | 1201 -> One (R 132 :: r923)
  | 1212 -> One (R 132 :: r931)
  | 1221 -> One (R 132 :: r938)
  | 1230 -> One (R 132 :: r945)
  | 1237 -> One (R 132 :: r949)
  | 1275 -> One (R 132 :: r952)
  | 1291 -> One (R 132 :: r955)
  | 1296 -> One (R 132 :: r959)
  | 1303 -> One (R 132 :: r963)
  | 1325 -> One (R 132 :: r971)
  | 1375 -> One (R 132 :: r990)
  | 1388 -> One (R 132 :: r994)
  | 1413 -> One (R 132 :: r1008)
  | 1454 -> One (R 132 :: r1040)
  | 1459 -> One (R 132 :: r1050)
  | 1527 -> One (R 132 :: r1100)
  | 1528 -> One (R 132 :: r1104)
  | 1537 -> One (R 132 :: r1112)
  | 1574 -> One (R 132 :: r1138)
  | 1583 -> One (R 132 :: r1152)
  | 1584 -> One (R 132 :: r1161)
  | 1780 -> One (R 132 :: r1279)
  | 1965 -> One (R 132 :: r1381)
  | 1974 -> One (R 132 :: r1386)
  | 2052 -> One (R 132 :: r1411)
  | 2067 -> One (R 132 :: r1419)
  | 264 -> One ([R 138])
  | 892 -> One ([R 144])
  | 1243 -> One ([R 159])
  | 913 -> One ([R 160])
  | 950 -> One ([R 161])
  | 930 -> One ([R 162])
  | 948 -> One ([R 233])
  | 957 -> One ([R 238])
  | 961 -> One ([R 239])
  | 370 -> One ([R 254])
  | 115 -> One ([R 267])
  | 92 -> One (R 270 :: r53)
  | 96 -> One (R 270 :: r55)
  | 718 -> One ([R 277])
  | 726 -> One ([R 278])
  | 720 -> One ([R 280])
  | 877 -> One ([R 281])
  | 879 -> One ([R 284])
  | 872 -> One ([R 285])
  | 1494 -> One ([R 292])
  | 1495 -> One ([R 293])
  | 1242 -> One ([R 297])
  | 510 -> One ([R 303])
  | 536 -> One ([R 307])
  | 547 -> One ([R 311])
  | 586 -> One ([R 315])
  | 573 -> One ([R 319])
  | 656 -> One ([R 323])
  | 1137 -> One ([R 327])
  | 683 -> One ([R 331])
  | 669 -> One ([R 335])
  | 638 -> One ([R 339])
  | 493 -> One ([R 343])
  | 637 -> One ([R 344])
  | 1142 -> One ([R 345])
  | 1110 -> One ([R 347])
  | 1147 -> One ([R 354])
  | 1868 -> One ([R 357])
  | 795 -> One ([R 358])
  | 1964 -> One ([R 360])
  | 129 -> One (R 376 :: r84)
  | 179 -> One (R 376 :: r163)
  | 320 -> One (R 376 :: r305)
  | 365 -> One (R 376 :: r333)
  | 594 -> One (R 376 :: r481)
  | 710 -> One (R 376 :: r588)
  | 746 -> One (R 376 :: r628)
  | 1090 -> One (R 376 :: r856)
  | 1434 -> One (R 376 :: r1027)
  | 1556 -> One (R 376 :: r1131)
  | 1595 -> One (R 376 :: r1167)
  | 1601 -> One (R 376 :: r1175)
  | 1612 -> One (R 376 :: r1181)
  | 1623 -> One (R 376 :: r1184)
  | 1627 -> One (R 376 :: r1193)
  | 1648 -> One (R 376 :: r1207)
  | 1664 -> One (R 376 :: r1217)
  | 1699 -> One (R 376 :: r1234)
  | 1721 -> One (R 376 :: r1244)
  | 1731 -> One (R 376 :: r1253)
  | 1788 -> One (R 376 :: r1283)
  | 1792 -> One (R 376 :: r1295)
  | 1832 -> One (R 376 :: r1317)
  | 1836 -> One (R 376 :: r1321)
  | 1837 -> One (R 376 :: r1325)
  | 1848 -> One (R 376 :: r1341)
  | 1856 -> One (R 376 :: r1350)
  | 1895 -> One (R 376 :: r1361)
  | 1915 -> One (R 376 :: r1374)
  | 2030 -> One (R 376 :: r1402)
  | 1720 -> One (R 378 :: r1237)
  | 1942 -> One (R 378 :: r1376)
  | 1730 -> One (R 380 :: r1245)
  | 1144 -> One (R 382 :: r887)
  | 1657 -> One (R 382 :: r1208)
  | 1718 -> One (R 382 :: r1236)
  | 1901 -> One (R 382 :: r1362)
  | 1947 -> One (R 382 :: r1378)
  | 2016 -> One (R 382 :: r1398)
  | 2261 -> One (R 382 :: r1467)
  | 2272 -> One (R 382 :: r1473)
  | 2277 -> One (R 382 :: r1476)
  | 1526 -> One (R 384 :: r1096)
  | 1710 -> One (R 384 :: r1235)
  | 211 -> One (R 387 :: r215)
  | 1925 -> One (R 387 :: r1375)
  | 1660 -> One (R 391 :: r1209)
  | 1904 -> One (R 393 :: r1363)
  | 2259 -> One (R 395 :: r1465)
  | 2267 -> One (R 397 :: r1469)
  | 2268 -> One (R 397 :: r1470)
  | 2269 -> One (R 397 :: r1471)
  | 562 -> One ([R 403])
  | 566 -> One ([R 405])
  | 1944 -> One ([R 407])
  | 1934 -> One ([R 408])
  | 1924 -> One ([R 409])
  | 1932 -> One ([R 413])
  | 1936 -> One ([R 415])
  | 1945 -> One ([R 417])
  | 1933 -> One ([R 418])
  | 1935 -> One ([R 420])
  | 1285 -> One ([R 423])
  | 2033 -> One ([R 424])
  | 2036 -> One ([R 425])
  | 2035 -> One ([R 427])
  | 2034 -> One ([R 429])
  | 2032 -> One ([R 430])
  | 2201 -> One ([R 442])
  | 2191 -> One ([R 444])
  | 2199 -> One ([R 445])
  | 2198 -> One ([R 447])
  | 721 -> One ([R 454])
  | 724 -> One ([R 455])
  | 750 -> One ([R 466])
  | 760 -> One ([R 467])
  | 761 -> One ([R 468])
  | 759 -> One ([R 469])
  | 762 -> One ([R 471])
  | 177 -> One ([R 472])
  | 206 | 361 | 1547 -> One ([R 473])
  | 402 -> One ([R 481])
  | 372 -> One ([R 482])
  | 415 -> One ([R 485])
  | 596 | 2001 -> One ([R 490])
  | 1605 -> One ([R 492])
  | 1603 -> One ([R 493])
  | 1606 -> One ([R 494])
  | 1604 -> One ([R 495])
  | 517 -> One ([R 498])
  | 1467 -> One ([R 500])
  | 1756 -> One ([R 501])
  | 2139 -> One ([R 502])
  | 1772 -> One ([R 503])
  | 2140 -> One ([R 504])
  | 1771 -> One ([R 505])
  | 1763 -> One ([R 506])
  | 67 | 615 -> One ([R 522])
  | 75 | 769 -> One ([R 523])
  | 103 -> One ([R 524])
  | 91 -> One ([R 526])
  | 95 -> One ([R 528])
  | 99 -> One ([R 530])
  | 82 -> One ([R 531])
  | 102 | 1340 -> One ([R 532])
  | 81 -> One ([R 533])
  | 80 -> One ([R 534])
  | 79 -> One ([R 535])
  | 78 -> One ([R 536])
  | 77 -> One ([R 537])
  | 70 | 357 | 742 -> One ([R 538])
  | 69 | 741 -> One ([R 539])
  | 68 -> One ([R 540])
  | 74 | 434 | 768 -> One ([R 541])
  | 73 | 767 -> One ([R 542])
  | 66 -> One ([R 543])
  | 71 -> One ([R 544])
  | 84 -> One ([R 545])
  | 76 -> One ([R 546])
  | 83 -> One ([R 547])
  | 72 -> One ([R 548])
  | 101 -> One ([R 549])
  | 104 -> One ([R 550])
  | 100 -> One ([R 552])
  | 314 -> One ([R 553])
  | 313 -> One (R 554 :: r303)
  | 223 -> One (R 555 :: r242)
  | 224 -> One ([R 556])
  | 563 -> One (R 557 :: r458)
  | 564 -> One ([R 558])
  | 1111 -> One (R 574 :: r872)
  | 1112 -> One ([R 575])
  | 121 -> One ([R 576])
  | 496 -> One ([R 593])
  | 494 -> One ([R 594])
  | 497 -> One ([R 596])
  | 641 -> One ([R 606])
  | 642 -> One ([R 607])
  | 643 -> One ([R 609])
  | 801 -> One ([R 611])
  | 1779 -> One ([R 615])
  | 1794 | 1813 -> One ([R 625])
  | 1616 -> One ([R 627])
  | 1614 -> One ([R 628])
  | 1617 -> One ([R 629])
  | 1615 -> One ([R 630])
  | 1877 -> One (R 631 :: r1355)
  | 704 -> One ([R 632])
  | 1754 -> One ([R 635])
  | 1755 -> One ([R 636])
  | 1749 -> One ([R 637])
  | 2092 -> One ([R 639])
  | 2091 -> One ([R 640])
  | 2093 -> One ([R 641])
  | 2088 -> One ([R 642])
  | 2089 -> One ([R 643])
  | 2153 -> One ([R 645])
  | 2151 -> One ([R 646])
  | 498 -> One ([R 677])
  | 644 -> One ([R 683])
  | 861 -> One (R 689 :: r719)
  | 885 -> One ([R 690])
  | 875 -> One (R 693 :: r727)
  | 882 -> One ([R 694])
  | 825 -> One ([R 696])
  | 414 -> One ([R 697])
  | 371 -> One ([R 698])
  | 1245 -> One ([R 699])
  | 1244 -> One ([R 700])
  | 338 -> One ([R 702])
  | 306 -> One ([R 726])
  | 1150 -> One ([R 729])
  | 911 -> One ([R 731])
  | 1151 -> One ([R 732])
  | 912 -> One ([R 733])
  | 1381 -> One ([R 735])
  | 1382 -> One ([R 736])
  | 557 -> One ([R 738])
  | 558 -> One ([R 739])
  | 1361 -> One ([R 741])
  | 1362 -> One ([R 742])
  | 1774 -> One ([R 748])
  | 1709 -> One ([R 749])
  | 1712 -> One ([R 750])
  | 1711 -> One ([R 755])
  | 1716 -> One ([R 758])
  | 1715 -> One ([R 760])
  | 1714 -> One ([R 761])
  | 1713 -> One ([R 762])
  | 1775 -> One ([R 765])
  | 355 -> One ([R 768])
  | 352 -> One ([R 770])
  | 868 -> One ([R 794])
  | 734 -> One ([R 795])
  | 871 -> One ([R 796])
  | 870 | 949 -> One ([R 797])
  | 736 | 929 -> One ([R 798])
  | 1235 | 1274 -> One ([R 803])
  | 869 -> One ([R 808])
  | 455 -> One ([R 831])
  | 459 -> One ([R 834])
  | 460 -> One ([R 838])
  | 482 -> One ([R 840])
  | 464 -> One ([R 841])
  | 559 -> One ([R 843])
  | 481 -> One ([R 848])
  | 28 -> One ([R 849])
  | 8 -> One ([R 850])
  | 53 -> One ([R 852])
  | 52 -> One ([R 853])
  | 51 -> One ([R 854])
  | 50 -> One ([R 855])
  | 49 -> One ([R 856])
  | 48 -> One ([R 857])
  | 47 -> One ([R 858])
  | 46 -> One ([R 859])
  | 45 -> One ([R 860])
  | 44 -> One ([R 861])
  | 43 -> One ([R 862])
  | 42 -> One ([R 863])
  | 41 -> One ([R 864])
  | 40 -> One ([R 865])
  | 39 -> One ([R 866])
  | 38 -> One ([R 867])
  | 37 -> One ([R 868])
  | 36 -> One ([R 869])
  | 35 -> One ([R 870])
  | 34 -> One ([R 871])
  | 33 -> One ([R 872])
  | 32 -> One ([R 873])
  | 31 -> One ([R 874])
  | 30 -> One ([R 875])
  | 29 -> One ([R 876])
  | 27 -> One ([R 877])
  | 26 -> One ([R 878])
  | 25 -> One ([R 879])
  | 24 -> One ([R 880])
  | 23 -> One ([R 881])
  | 22 -> One ([R 882])
  | 21 -> One ([R 883])
  | 20 -> One ([R 884])
  | 19 -> One ([R 885])
  | 18 -> One ([R 886])
  | 17 -> One ([R 887])
  | 16 -> One ([R 888])
  | 15 -> One ([R 889])
  | 14 -> One ([R 890])
  | 13 -> One ([R 891])
  | 12 -> One ([R 892])
  | 11 -> One ([R 893])
  | 10 -> One ([R 894])
  | 9 -> One ([R 895])
  | 7 -> One ([R 896])
  | 6 -> One ([R 897])
  | 5 -> One ([R 898])
  | 4 -> One ([R 899])
  | 3 -> One ([R 900])
  | 1951 -> One ([R 904])
  | 1939 | 1952 -> One ([R 906])
  | 1937 -> One ([R 908])
  | 608 -> One ([R 909])
  | 607 -> One ([R 910])
  | 2250 -> One ([R 914])
  | 2251 -> One ([R 915])
  | 2253 -> One ([R 916])
  | 2254 -> One ([R 917])
  | 2252 -> One ([R 918])
  | 2249 -> One ([R 919])
  | 2255 -> One ([R 923])
  | 375 -> One (R 933 :: r357)
  | 396 -> One ([R 934])
  | 135 -> One ([R 939])
  | 138 -> One ([R 940])
  | 142 -> One ([R 941])
  | 136 -> One ([R 942])
  | 143 -> One ([R 943])
  | 139 -> One ([R 944])
  | 144 -> One ([R 945])
  | 141 -> One ([R 946])
  | 134 -> One ([R 947])
  | 456 -> One ([R 952])
  | 725 -> One ([R 953])
  | 1587 -> One ([R 961])
  | 1999 -> One ([R 962])
  | 2002 -> One ([R 963])
  | 2000 -> One ([R 964])
  | 1811 -> One ([R 965])
  | 1814 -> One ([R 966])
  | 1812 -> One ([R 967])
  | 385 -> One ([R 974])
  | 386 -> One ([R 975])
  | 1355 -> One (S (T T_WITH) :: r986)
  | 173 -> One (S (T T_TYPE) :: r159)
  | 1740 -> One (S (T T_STRING) :: r1256)
  | 1497 -> One (S (T T_STAR) :: r1082)
  | 2257 -> One (S (T T_SEMISEMI) :: r1464)
  | 2264 -> One (S (T T_SEMISEMI) :: r1468)
  | 2188 -> One (S (T T_RPAREN) :: r144)
  | 367 -> One (S (T T_RPAREN) :: r208)
  | 251 -> One (S (T T_RPAREN) :: r275)
  | 267 | 299 -> One (S (T T_RPAREN) :: r282)
  | 467 -> One (S (T T_RPAREN) :: r407)
  | 550 -> One (S (T T_RPAREN) :: r457)
  | 752 -> One (S (T T_RPAREN) :: r629)
  | 1341 -> One (S (T T_RPAREN) :: r975)
  | 1984 -> One (S (T T_RPAREN) :: r1389)
  | 2189 -> One (S (T T_RPAREN) :: r1447)
  | 1477 | 1736 -> One (S (T T_RBRACKET) :: r379)
  | 1347 -> One (S (T T_RBRACKET) :: r978)
  | 1349 -> One (S (T T_RBRACKET) :: r979)
  | 286 -> One (S (T T_QUOTE) :: r290)
  | 1625 -> One (S (T T_OPEN) :: r1189)
  | 1840 -> One (S (T T_OPEN) :: r1332)
  | 409 -> One (S (T T_MINUSGREATER) :: r370)
  | 1513 -> One (S (T T_MINUSGREATER) :: r1092)
  | 1517 -> One (S (T T_MINUSGREATER) :: r1094)
  | 1686 -> One (S (T T_MINUSGREATER) :: r1223)
  | 2121 -> One (S (T T_MINUSGREATER) :: r1430)
  | 85 -> One (S (T T_LPAREN) :: r50)
  | 118 -> One (S (T T_LIDENT) :: r64)
  | 195 -> One (S (T T_LIDENT) :: r197)
  | 196 -> One (S (T T_LIDENT) :: r205)
  | 219 -> One (S (T T_LIDENT) :: r229)
  | 220 -> One (S (T T_LIDENT) :: r235)
  | 343 -> One (S (T T_LIDENT) :: r313)
  | 344 -> One (S (T T_LIDENT) :: r317)
  | 472 -> One (S (T T_LIDENT) :: r411)
  | 473 -> One (S (T T_LIDENT) :: r415)
  | 500 -> One (S (T T_LIDENT) :: r428)
  | 501 -> One (S (T T_LIDENT) :: r432)
  | 526 -> One (S (T T_LIDENT) :: r444)
  | 527 -> One (S (T T_LIDENT) :: r448)
  | 576 -> One (S (T T_LIDENT) :: r461)
  | 577 -> One (S (T T_LIDENT) :: r465)
  | 620 -> One (S (T T_LIDENT) :: r508)
  | 621 -> One (S (T T_LIDENT) :: r514)
  | 627 -> One (S (T T_LIDENT) :: r515)
  | 628 -> One (S (T T_LIDENT) :: r519)
  | 646 -> One (S (T T_LIDENT) :: r523)
  | 647 -> One (S (T T_LIDENT) :: r527)
  | 659 -> One (S (T T_LIDENT) :: r529)
  | 660 -> One (S (T T_LIDENT) :: r533)
  | 673 -> One (S (T T_LIDENT) :: r538)
  | 674 -> One (S (T T_LIDENT) :: r542)
  | 685 -> One (S (T T_LIDENT) :: r544)
  | 697 -> One (S (T T_LIDENT) :: r552)
  | 836 -> One (S (T T_LIDENT) :: r692)
  | 837 -> One (S (T T_LIDENT) :: r695)
  | 848 -> One (S (T T_LIDENT) :: r698)
  | 864 -> One (S (T T_LIDENT) :: r720)
  | 893 -> One (S (T T_LIDENT) :: r733)
  | 894 -> One (S (T T_LIDENT) :: r736)
  | 899 -> One (S (T T_LIDENT) :: r737)
  | 915 -> One (S (T T_LIDENT) :: r744)
  | 916 -> One (S (T T_LIDENT) :: r747)
  | 1071 -> One (S (T T_LIDENT) :: r842)
  | 1072 -> One (S (T T_LIDENT) :: r845)
  | 1127 -> One (S (T T_LIDENT) :: r879)
  | 1128 -> One (S (T T_LIDENT) :: r883)
  | 1317 -> One (S (T T_LIDENT) :: r964)
  | 1318 -> One (S (T T_LIDENT) :: r967)
  | 1481 -> One (S (T T_LIDENT) :: r1078)
  | 1815 -> One (S (T T_LIDENT) :: r1306)
  | 1887 -> One (S (T T_LIDENT) :: r1358)
  | 2003 -> One (S (T T_LIDENT) :: r1394)
  | 2044 -> One (S (T T_LIDENT) :: r1404)
  | 2045 -> One (S (T T_LIDENT) :: r1407)
  | 2059 -> One (S (T T_LIDENT) :: r1412)
  | 2060 -> One (S (T T_LIDENT) :: r1415)
  | 350 -> One (S (T T_INT) :: r318)
  | 353 -> One (S (T T_INT) :: r319)
  | 931 -> One (S (T T_IN) :: r754)
  | 1860 -> One (S (T T_IN) :: r1352)
  | 713 -> One (S (T T_GREATERRBRACE) :: r593)
  | 1384 -> One (S (T T_GREATERRBRACE) :: r991)
  | 199 -> One (S (T T_GREATER) :: r207)
  | 2038 -> One (S (T T_GREATER) :: r1403)
  | 418 -> One (S (T T_EQUAL) :: r374)
  | 1107 -> One (S (T T_EQUAL) :: r869)
  | 1123 -> One (S (T T_EQUAL) :: r877)
  | 1331 -> One (S (T T_EQUAL) :: r973)
  | 1993 -> One (S (T T_EQUAL) :: r1391)
  | 2011 -> One (S (T T_EQUAL) :: r1396)
  | 2180 -> One (S (T T_EOF) :: r1445)
  | 2184 -> One (S (T T_EOF) :: r1446)
  | 2203 -> One (S (T T_EOF) :: r1452)
  | 2207 -> One (S (T T_EOF) :: r1453)
  | 2211 -> One (S (T T_EOF) :: r1454)
  | 2214 -> One (S (T T_EOF) :: r1455)
  | 2219 -> One (S (T T_EOF) :: r1456)
  | 2223 -> One (S (T T_EOF) :: r1457)
  | 2227 -> One (S (T T_EOF) :: r1458)
  | 2231 -> One (S (T T_EOF) :: r1459)
  | 2235 -> One (S (T T_EOF) :: r1460)
  | 2238 -> One (S (T T_EOF) :: r1461)
  | 2242 -> One (S (T T_EOF) :: r1462)
  | 2281 -> One (S (T T_EOF) :: r1477)
  | 1371 -> One (S (T T_END) :: r987)
  | 87 -> One (S (T T_DOTDOT) :: r51)
  | 162 -> One (S (T T_DOTDOT) :: r141)
  | 499 -> One (S (T T_DOTDOT) :: r427)
  | 525 -> One (S (T T_DOTDOT) :: r443)
  | 645 -> One (S (T T_DOTDOT) :: r522)
  | 1126 -> One (S (T T_DOTDOT) :: r878)
  | 1757 -> One (S (T T_DOTDOT) :: r1261)
  | 1758 -> One (S (T T_DOTDOT) :: r1262)
  | 291 -> One (S (T T_DOT) :: r295)
  | 378 | 1206 | 1263 -> One (S (T T_DOT) :: r359)
  | 2245 -> One (S (T T_DOT) :: r375)
  | 689 -> One (S (T T_DOT) :: r551)
  | 809 -> One (S (T T_DOT) :: r682)
  | 817 -> One (S (T T_DOT) :: r686)
  | 1102 -> One (S (T T_DOT) :: r867)
  | 1484 -> One (S (T T_DOT) :: r1080)
  | 1511 -> One (S (T T_DOT) :: r1090)
  | 2115 -> One (S (T T_DOT) :: r1428)
  | 2129 -> One (S (T T_DOT) :: r1436)
  | 2193 -> One (S (T T_DOT) :: r1451)
  | 163 | 1474 -> One (S (T T_COLONCOLON) :: r143)
  | 171 -> One (S (T T_COLON) :: r155)
  | 272 -> One (S (T T_COLON) :: r285)
  | 368 -> One (S (T T_COLON) :: r336)
  | 1680 -> One (S (T T_COLON) :: r1221)
  | 2026 -> One (S (T T_COLON) :: r1401)
  | 435 -> One (S (T T_BARRBRACKET) :: r378)
  | 568 -> One (S (T T_BARRBRACKET) :: r459)
  | 616 -> One (S (T T_BARRBRACKET) :: r497)
  | 1343 -> One (S (T T_BARRBRACKET) :: r976)
  | 1345 -> One (S (T T_BARRBRACKET) :: r977)
  | 1971 -> One (S (T T_BARRBRACKET) :: r1382)
  | 327 -> One (S (T T_BAR) :: r308)
  | 217 -> One (S (N N_pattern) :: r225)
  | 445 -> One (S (N N_pattern) :: r391)
  | 511 -> One (S (N N_pattern) :: r434)
  | 540 -> One (S (N N_pattern) :: r453)
  | 639 -> One (S (N N_pattern) :: r521)
  | 1138 -> One (S (N N_pattern) :: r885)
  | 1448 -> One (S (N N_pattern) :: r1032)
  | 364 -> One (S (N N_module_type) :: r329)
  | 412 -> One (S (N N_module_type) :: r371)
  | 416 -> One (S (N N_module_type) :: r372)
  | 756 -> One (S (N N_module_type) :: r631)
  | 1392 -> One (S (N N_module_type) :: r995)
  | 1394 -> One (S (N N_module_type) :: r996)
  | 1396 -> One (S (N N_module_type) :: r997)
  | 1399 -> One (S (N N_module_type) :: r998)
  | 1401 -> One (S (N N_module_type) :: r999)
  | 1403 -> One (S (N N_module_type) :: r1000)
  | 1418 -> One (S (N N_module_type) :: r1015)
  | 1428 -> One (S (N N_module_type) :: r1022)
  | 1979 -> One (S (N N_module_type) :: r1388)
  | 709 -> One (S (N N_module_expr) :: r583)
  | 794 -> One (S (N N_let_pattern) :: r676)
  | 618 -> One (S (N N_fun_expr) :: r498)
  | 715 -> One (S (N N_fun_expr) :: r596)
  | 835 -> One (S (N N_fun_expr) :: r691)
  | 886 -> One (S (N N_fun_expr) :: r729)
  | 914 -> One (S (N N_fun_expr) :: r743)
  | 936 -> One (S (N N_fun_expr) :: r755)
  | 942 -> One (S (N N_fun_expr) :: r759)
  | 951 -> One (S (N N_fun_expr) :: r763)
  | 962 -> One (S (N N_fun_expr) :: r769)
  | 968 -> One (S (N N_fun_expr) :: r773)
  | 974 -> One (S (N N_fun_expr) :: r777)
  | 980 -> One (S (N N_fun_expr) :: r781)
  | 986 -> One (S (N N_fun_expr) :: r785)
  | 992 -> One (S (N N_fun_expr) :: r789)
  | 998 -> One (S (N N_fun_expr) :: r793)
  | 1004 -> One (S (N N_fun_expr) :: r797)
  | 1010 -> One (S (N N_fun_expr) :: r801)
  | 1016 -> One (S (N N_fun_expr) :: r805)
  | 1022 -> One (S (N N_fun_expr) :: r809)
  | 1028 -> One (S (N N_fun_expr) :: r813)
  | 1034 -> One (S (N N_fun_expr) :: r817)
  | 1040 -> One (S (N N_fun_expr) :: r821)
  | 1046 -> One (S (N N_fun_expr) :: r825)
  | 1052 -> One (S (N N_fun_expr) :: r829)
  | 1058 -> One (S (N N_fun_expr) :: r833)
  | 1064 -> One (S (N N_fun_expr) :: r837)
  | 1070 -> One (S (N N_fun_expr) :: r841)
  | 1084 -> One (S (N N_fun_expr) :: r850)
  | 1154 -> One (S (N N_fun_expr) :: r888)
  | 1163 -> One (S (N N_fun_expr) :: r895)
  | 1172 -> One (S (N N_fun_expr) :: r902)
  | 1182 -> One (S (N N_fun_expr) :: r906)
  | 1191 -> One (S (N N_fun_expr) :: r913)
  | 1200 -> One (S (N N_fun_expr) :: r920)
  | 1211 -> One (S (N N_fun_expr) :: r928)
  | 1220 -> One (S (N N_fun_expr) :: r935)
  | 1229 -> One (S (N N_fun_expr) :: r942)
  | 1236 -> One (S (N N_fun_expr) :: r946)
  | 1295 -> One (S (N N_fun_expr) :: r956)
  | 1302 -> One (S (N N_fun_expr) :: r960)
  | 610 -> One (Sub (r3) :: r489)
  | 700 -> One (Sub (r3) :: r556)
  | 788 -> One (Sub (r3) :: r654)
  | 1450 -> One (Sub (r3) :: r1033)
  | 2 -> One (Sub (r13) :: r14)
  | 56 -> One (Sub (r13) :: r15)
  | 60 -> One (Sub (r13) :: r22)
  | 190 -> One (Sub (r13) :: r183)
  | 208 -> One (Sub (r13) :: r214)
  | 958 -> One (Sub (r13) :: r768)
  | 1446 -> One (Sub (r13) :: r1031)
  | 1452 -> One (Sub (r13) :: r1036)
  | 1841 -> One (Sub (r13) :: r1337)
  | 542 -> One (Sub (r24) :: r454)
  | 1140 -> One (Sub (r24) :: r886)
  | 279 -> One (Sub (r26) :: r287)
  | 281 -> One (Sub (r26) :: r288)
  | 827 -> One (Sub (r26) :: r687)
  | 1510 -> One (Sub (r26) :: r1088)
  | 249 -> One (Sub (r28) :: r273)
  | 1688 -> One (Sub (r28) :: r1226)
  | 248 -> One (Sub (r30) :: r270)
  | 2127 -> One (Sub (r30) :: r1431)
  | 317 -> One (Sub (r32) :: r304)
  | 389 -> One (Sub (r32) :: r363)
  | 198 -> One (Sub (r34) :: r206)
  | 257 -> One (Sub (r34) :: r277)
  | 300 -> One (Sub (r34) :: r297)
  | 392 -> One (Sub (r34) :: r366)
  | 442 -> One (Sub (r34) :: r390)
  | 589 -> One (Sub (r34) :: r468)
  | 771 -> One (Sub (r34) :: r634)
  | 841 -> One (Sub (r34) :: r696)
  | 1119 -> One (Sub (r34) :: r875)
  | 1597 -> One (Sub (r34) :: r1169)
  | 1635 -> One (Sub (r34) :: r1200)
  | 688 -> One (Sub (r36) :: r549)
  | 796 -> One (Sub (r36) :: r677)
  | 1797 -> One (Sub (r36) :: r1298)
  | 1821 -> One (Sub (r36) :: r1309)
  | 147 -> One (Sub (r59) :: r136)
  | 292 -> One (Sub (r59) :: r296)
  | 2247 -> One (Sub (r59) :: r1463)
  | 1525 -> One (Sub (r81) :: r1095)
  | 450 -> One (Sub (r96) :: r399)
  | 153 -> One (Sub (r131) :: r137)
  | 140 -> One (Sub (r133) :: r135)
  | 1589 -> One (Sub (r133) :: r1163)
  | 157 -> One (Sub (r139) :: r140)
  | 2142 -> One (Sub (r139) :: r1441)
  | 2156 -> One (Sub (r139) :: r1444)
  | 271 -> One (Sub (r146) :: r283)
  | 786 -> One (Sub (r187) :: r651)
  | 927 -> One (Sub (r187) :: r752)
  | 213 -> One (Sub (r217) :: r218)
  | 609 -> One (Sub (r217) :: r487)
  | 733 -> One (Sub (r217) :: r614)
  | 774 -> One (Sub (r217) :: r637)
  | 776 -> One (Sub (r217) :: r638)
  | 846 -> One (Sub (r217) :: r697)
  | 866 -> One (Sub (r217) :: r721)
  | 873 -> One (Sub (r217) :: r722)
  | 901 -> One (Sub (r217) :: r738)
  | 903 -> One (Sub (r217) :: r739)
  | 921 -> One (Sub (r217) :: r748)
  | 1077 -> One (Sub (r217) :: r846)
  | 1323 -> One (Sub (r217) :: r968)
  | 2050 -> One (Sub (r217) :: r1408)
  | 2065 -> One (Sub (r217) :: r1416)
  | 310 -> One (Sub (r237) :: r298)
  | 228 -> One (Sub (r239) :: r246)
  | 243 -> One (Sub (r239) :: r269)
  | 229 -> One (Sub (r252) :: r254)
  | 230 -> One (Sub (r256) :: r257)
  | 253 -> One (Sub (r256) :: r276)
  | 275 -> One (Sub (r256) :: r286)
  | 233 -> One (Sub (r263) :: r265)
  | 422 -> One (Sub (r263) :: r376)
  | 1548 -> One (Sub (r263) :: r1120)
  | 335 -> One (Sub (r310) :: r312)
  | 1424 -> One (Sub (r323) :: r1019)
  | 1551 -> One (Sub (r323) :: r1125)
  | 426 -> One (Sub (r346) :: r377)
  | 374 -> One (Sub (r348) :: r349)
  | 438 -> One (Sub (r387) :: r389)
  | 469 -> One (Sub (r394) :: r410)
  | 479 -> One (Sub (r394) :: r416)
  | 507 -> One (Sub (r394) :: r433)
  | 533 -> One (Sub (r394) :: r449)
  | 570 -> One (Sub (r394) :: r460)
  | 583 -> One (Sub (r394) :: r466)
  | 634 -> One (Sub (r394) :: r520)
  | 653 -> One (Sub (r394) :: r528)
  | 666 -> One (Sub (r394) :: r534)
  | 670 -> One (Sub (r394) :: r537)
  | 680 -> One (Sub (r394) :: r543)
  | 813 -> One (Sub (r394) :: r683)
  | 1134 -> One (Sub (r394) :: r884)
  | 461 -> One (Sub (r402) :: r403)
  | 487 -> One (Sub (r422) :: r425)
  | 515 -> One (Sub (r437) :: r440)
  | 804 -> One (Sub (r437) :: r679)
  | 1096 -> One (Sub (r437) :: r863)
  | 1798 -> One (Sub (r437) :: r1303)
  | 1822 -> One (Sub (r437) :: r1314)
  | 593 -> One (Sub (r474) :: r476)
  | 1337 -> One (Sub (r500) :: r974)
  | 619 -> One (Sub (r502) :: r505)
  | 686 -> One (Sub (r546) :: r548)
  | 698 -> One (Sub (r546) :: r555)
  | 716 -> One (Sub (r602) :: r604)
  | 1354 -> One (Sub (r602) :: r984)
  | 719 -> One (Sub (r606) :: r607)
  | 878 -> One (Sub (r606) :: r728)
  | 1565 -> One (Sub (r606) :: r1133)
  | 792 -> One (Sub (r672) :: r673)
  | 1351 -> One (Sub (r980) :: r982)
  | 1440 -> One (Sub (r1003) :: r1028)
  | 1479 -> One (Sub (r1059) :: r1060)
  | 1480 -> One (Sub (r1070) :: r1072)
  | 1737 -> One (Sub (r1070) :: r1255)
  | 1759 -> One (Sub (r1070) :: r1264)
  | 1767 -> One (Sub (r1070) :: r1266)
  | 2135 -> One (Sub (r1070) :: r1438)
  | 1502 -> One (Sub (r1083) :: r1086)
  | 2083 -> One (Sub (r1083) :: r1421)
  | 2095 -> One (Sub (r1083) :: r1423)
  | 1572 -> One (Sub (r1107) :: r1134)
  | 1883 -> One (Sub (r1143) :: r1357)
  | 1907 -> One (Sub (r1143) :: r1366)
  | 1852 -> One (Sub (r1195) :: r1344)
  | 1839 -> One (Sub (r1268) :: r1327)
  | 1911 -> One (Sub (r1271) :: r1367)
  | 1791 -> One (Sub (r1289) :: r1291)
  | 935 -> One (r0)
  | 934 -> One (r2)
  | 2179 -> One (r4)
  | 2178 -> One (r5)
  | 2177 -> One (r6)
  | 2176 -> One (r7)
  | 2175 -> One (r8)
  | 59 -> One (r9)
  | 54 -> One (r10)
  | 55 -> One (r12)
  | 58 -> One (r14)
  | 57 -> One (r15)
  | 1946 -> One (r16)
  | 1950 -> One (r18)
  | 2174 -> One (r20)
  | 2173 -> One (r21)
  | 61 -> One (r22)
  | 108 | 617 | 717 | 1368 -> One (r23)
  | 111 -> One (r25)
  | 270 -> One (r27)
  | 247 -> One (r29)
  | 262 -> One (r31)
  | 285 -> One (r33)
  | 693 -> One (r35)
  | 2172 -> One (r37)
  | 2171 -> One (r38)
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
  | 2025 -> One (r65)
  | 2024 -> One (r66)
  | 170 | 203 -> One (r67)
  | 169 | 202 -> One (r68)
  | 168 | 201 -> One (r69)
  | 167 | 200 | 250 | 261 -> One (r70)
  | 2170 -> One (r71)
  | 2169 -> One (r72)
  | 2168 -> One (r73)
  | 2167 -> One (r74)
  | 127 -> One (r75)
  | 126 -> One (r76)
  | 1778 -> One (r80)
  | 2166 -> One (r82)
  | 2165 -> One (r83)
  | 130 -> One (r84)
  | 2102 -> One (r85)
  | 2101 -> One (r86)
  | 2100 -> One (r87)
  | 231 | 280 -> One (r93)
  | 256 -> One (r95)
  | 453 -> One (r97)
  | 1524 -> One (r99)
  | 1766 -> One (r101)
  | 1765 -> One (r102)
  | 1764 | 2094 -> One (r103)
  | 2152 -> One (r105)
  | 2164 -> One (r107)
  | 2163 -> One (r108)
  | 2162 -> One (r109)
  | 2161 -> One (r110)
  | 2160 -> One (r111)
  | 2077 -> One (r115)
  | 189 -> One (r116)
  | 188 -> One (r117)
  | 2150 -> One (r121)
  | 2149 -> One (r122)
  | 2148 -> One (r123)
  | 2147 -> One (r124)
  | 2146 -> One (r125)
  | 146 -> One (r127)
  | 149 -> One (r129)
  | 145 -> One (r130)
  | 150 -> One (r132)
  | 152 -> One (r134)
  | 151 -> One (r135)
  | 148 -> One (r136)
  | 154 -> One (r137)
  | 1742 -> One (r138)
  | 2141 -> One (r140)
  | 2138 -> One (r141)
  | 1476 -> One (r142)
  | 1475 -> One (r143)
  | 164 -> One (r144)
  | 284 -> One (r145)
  | 2126 -> One (r147)
  | 2125 -> One (r148)
  | 2124 -> One (r149)
  | 166 -> One (r150)
  | 2114 -> One (r151)
  | 2113 -> One (r152)
  | 2112 -> One (r153)
  | 2111 -> One (r154)
  | 172 -> One (r155)
  | 2110 -> One (r156)
  | 176 -> One (r157)
  | 175 -> One (r158)
  | 174 -> One (r159)
  | 178 -> One (r160)
  | 2109 -> One (r161)
  | 2108 -> One (r162)
  | 180 -> One (r163)
  | 181 -> One (r164)
  | 2090 -> One (r165)
  | 2107 -> One (r167)
  | 2106 -> One (r168)
  | 2105 -> One (r169)
  | 2104 -> One (r170)
  | 2103 -> One (r171)
  | 2087 -> One (r175)
  | 2086 -> One (r176)
  | 2080 -> One (r177)
  | 2079 -> One (r178)
  | 2078 -> One (r179)
  | 2076 -> One (r181)
  | 2075 -> One (r182)
  | 191 -> One (r183)
  | 1286 -> One (r184)
  | 1284 -> One (r185)
  | 787 -> One (r186)
  | 891 -> One (r188)
  | 2074 -> One (r190)
  | 2073 -> One (r191)
  | 2072 -> One (r192)
  | 194 -> One (r193)
  | 193 -> One (r194)
  | 2071 -> One (r195)
  | 2058 -> One (r196)
  | 2057 -> One (r197)
  | 840 -> One (r198)
  | 839 | 1095 -> One (r199)
  | 2056 -> One (r201)
  | 2043 -> One (r202)
  | 2042 -> One (r203)
  | 2041 -> One (r204)
  | 197 -> One (r205)
  | 2040 -> One (r206)
  | 2037 -> One (r207)
  | 207 -> One (r208)
  | 2023 -> One (r209)
  | 2022 -> One (r210)
  | 205 -> One (r211)
  | 2021 -> One (r212)
  | 2020 -> One (r213)
  | 209 -> One (r214)
  | 2019 -> One (r215)
  | 212 -> One (r216)
  | 1973 -> One (r218)
  | 2015 -> One (r219)
  | 2014 -> One (r220)
  | 592 -> One (r221)
  | 216 -> One (r222)
  | 215 -> One (r223)
  | 588 -> One (r224)
  | 587 -> One (r225)
  | 218 -> One (r226)
  | 585 -> One (r227)
  | 575 -> One (r228)
  | 574 -> One (r229)
  | 572 -> One (r230)
  | 342 -> One (r231)
  | 341 -> One (r232)
  | 340 -> One (r233)
  | 222 -> One (r234)
  | 221 -> One (r235)
  | 324 -> One (r236)
  | 307 -> One (r238)
  | 334 -> One (r240)
  | 333 -> One (r241)
  | 225 -> One (r242)
  | 227 -> One (r243)
  | 226 -> One (r244)
  | 332 -> One (r245)
  | 331 -> One (r246)
  | 245 -> One (r247)
  | 244 -> One (r248)
  | 323 -> One (r250)
  | 312 -> One (r251)
  | 326 -> One (r253)
  | 325 -> One (r254)
  | 241 | 1691 -> One (r255)
  | 242 -> One (r257)
  | 240 -> One (r258)
  | 239 -> One (r259)
  | 232 -> One (r260)
  | 238 -> One (r262)
  | 235 -> One (r264)
  | 234 -> One (r265)
  | 237 -> One (r266)
  | 236 -> One (r267)
  | 309 -> One (r268)
  | 308 -> One (r269)
  | 305 -> One (r270)
  | 304 -> One (r271)
  | 303 -> One (r272)
  | 302 -> One (r273)
  | 255 -> One (r274)
  | 252 -> One (r275)
  | 254 -> One (r276)
  | 290 -> One (r277)
  | 289 -> One (r280)
  | 268 -> One (r282)
  | 278 -> One (r283)
  | 274 -> One (r284)
  | 273 -> One (r285)
  | 276 -> One (r286)
  | 283 -> One (r287)
  | 282 -> One (r288)
  | 288 -> One (r289)
  | 287 -> One (r290)
  | 298 -> One (r291)
  | 297 -> One (r292)
  | 296 -> One (r293)
  | 295 -> One (r294)
  | 294 -> One (r295)
  | 293 -> One (r296)
  | 301 -> One (r297)
  | 311 -> One (r298)
  | 322 -> One (r299)
  | 319 -> One (r301)
  | 316 -> One (r302)
  | 315 -> One (r303)
  | 318 -> One (r304)
  | 321 -> One (r305)
  | 330 -> One (r306)
  | 329 -> One (r307)
  | 328 -> One (r308)
  | 339 -> One (r309)
  | 337 -> One (r311)
  | 336 -> One (r312)
  | 349 -> One (r313)
  | 348 -> One (r314)
  | 347 -> One (r315)
  | 346 -> One (r316)
  | 345 -> One (r317)
  | 351 -> One (r318)
  | 354 -> One (r319)
  | 524 -> One (r320)
  | 523 | 807 | 815 -> One (r321)
  | 514 | 803 | 814 | 1786 -> One (r322)
  | 363 -> One (r324)
  | 362 -> One (r325)
  | 360 -> One (r326)
  | 359 -> One (r327)
  | 433 -> One (r328)
  | 432 -> One (r329)
  | 431 -> One (r330)
  | 430 -> One (r331)
  | 429 -> One (r332)
  | 366 -> One (r333)
  | 428 -> One (r334)
  | 373 -> One (r335)
  | 369 -> One (r336)
  | 408 -> One (r337)
  | 407 -> One (r339)
  | 401 -> One (r341)
  | 400 -> One (r342)
  | 399 -> One (r343)
  | 398 -> One (r344)
  | 397 -> One (r345)
  | 424 -> One (r347)
  | 425 -> One (r349)
  | 377 -> One (r350)
  | 383 -> One (r352)
  | 388 -> One (r354)
  | 387 -> One (r355)
  | 384 -> One (r356)
  | 376 -> One (r357)
  | 381 -> One (r358)
  | 379 -> One (r359)
  | 380 -> One (r360)
  | 382 -> One (r361)
  | 391 -> One (r362)
  | 390 -> One (r363)
  | 395 -> One (r364)
  | 394 -> One (r365)
  | 393 -> One (r366)
  | 406 -> One (r367)
  | 411 -> One (r369)
  | 410 -> One (r370)
  | 413 -> One (r371)
  | 417 -> One (r372)
  | 420 -> One (r373)
  | 419 -> One (r374)
  | 421 | 723 -> One (r375)
  | 423 -> One (r376)
  | 427 -> One (r377)
  | 567 -> One (r378)
  | 437 -> One (r379)
  | 556 -> One (r380)
  | 555 -> One (r382)
  | 554 -> One (r383)
  | 561 -> One (r384)
  | 444 -> One (r385)
  | 441 -> One (r386)
  | 440 -> One (r388)
  | 439 -> One (r389)
  | 443 -> One (r390)
  | 560 -> One (r391)
  | 457 | 1118 -> One (r393)
  | 458 -> One (r395)
  | 448 -> One (r396)
  | 447 -> One (r397)
  | 449 -> One (r398)
  | 451 -> One (r399)
  | 463 -> One (r401)
  | 462 -> One (r403)
  | 553 -> One (r404)
  | 552 -> One (r405)
  | 466 -> One (r406)
  | 468 -> One (r407)
  | 546 -> One (r408)
  | 471 -> One (r409)
  | 470 -> One (r410)
  | 478 -> One (r411)
  | 477 -> One (r412)
  | 476 -> One (r413)
  | 475 -> One (r414)
  | 474 -> One (r415)
  | 480 -> One (r416)
  | 483 -> One (r417)
  | 545 -> One (r418)
  | 486 -> One (r419)
  | 485 -> One (r420)
  | 488 | 770 -> One (r421)
  | 491 -> One (r423)
  | 490 -> One (r424)
  | 489 -> One (r425)
  | 495 -> One (r426)
  | 509 -> One (r427)
  | 506 -> One (r428)
  | 505 -> One (r429)
  | 504 -> One (r430)
  | 503 -> One (r431)
  | 502 -> One (r432)
  | 508 -> One (r433)
  | 512 -> One (r434)
  | 544 -> One (r435)
  | 516 -> One (r436)
  | 520 -> One (r438)
  | 519 -> One (r439)
  | 518 -> One (r440)
  | 522 -> One (r441)
  | 521 -> One (r442)
  | 535 -> One (r443)
  | 532 -> One (r444)
  | 531 -> One (r445)
  | 530 -> One (r446)
  | 529 -> One (r447)
  | 528 -> One (r448)
  | 534 -> One (r449)
  | 539 -> One (r450)
  | 538 -> One (r451)
  | 537 | 808 | 816 -> One (r452)
  | 541 -> One (r453)
  | 543 -> One (r454)
  | 549 -> One (r455)
  | 548 -> One (r456)
  | 551 -> One (r457)
  | 565 -> One (r458)
  | 569 -> One (r459)
  | 571 -> One (r460)
  | 582 -> One (r461)
  | 581 -> One (r462)
  | 580 -> One (r463)
  | 579 -> One (r464)
  | 578 -> One (r465)
  | 584 -> One (r466)
  | 591 -> One (r467)
  | 590 -> One (r468)
  | 2010 -> One (r469)
  | 2009 -> One (r470)
  | 2008 -> One (r471)
  | 2007 -> One (r472)
  | 1998 -> One (r473)
  | 1997 -> One (r475)
  | 1996 -> One (r476)
  | 1992 -> One (r477)
  | 599 -> One (r478)
  | 598 -> One (r479)
  | 597 -> One (r480)
  | 595 -> One (r481)
  | 605 -> One (r482)
  | 606 -> One (r484)
  | 604 -> One (r485)
  | 603 -> One (r486)
  | 1991 -> One (r487)
  | 1990 -> One (r488)
  | 1989 -> One (r489)
  | 1988 -> One (r490)
  | 1987 -> One (r491)
  | 1986 -> One (r492)
  | 613 -> One (r493)
  | 612 -> One (r494)
  | 1983 -> One (r495)
  | 1982 -> One (r496)
  | 1970 -> One (r497)
  | 1969 -> One (r498)
  | 684 -> One (r499)
  | 1339 -> One (r501)
  | 1336 -> One (r503)
  | 1335 -> One (r504)
  | 1334 -> One (r505)
  | 668 -> One (r506)
  | 658 -> One (r507)
  | 657 -> One (r508)
  | 636 -> One (r509)
  | 626 -> One (r510)
  | 625 -> One (r511)
  | 624 -> One (r512)
  | 623 -> One (r513)
  | 622 -> One (r514)
  | 633 -> One (r515)
  | 632 -> One (r516)
  | 631 -> One (r517)
  | 630 -> One (r518)
  | 629 -> One (r519)
  | 635 -> One (r520)
  | 640 -> One (r521)
  | 655 -> One (r522)
  | 652 -> One (r523)
  | 651 -> One (r524)
  | 650 -> One (r525)
  | 649 -> One (r526)
  | 648 -> One (r527)
  | 654 -> One (r528)
  | 665 -> One (r529)
  | 664 -> One (r530)
  | 663 -> One (r531)
  | 662 -> One (r532)
  | 661 -> One (r533)
  | 667 -> One (r534)
  | 682 -> One (r535)
  | 672 -> One (r536)
  | 671 -> One (r537)
  | 679 -> One (r538)
  | 678 -> One (r539)
  | 677 -> One (r540)
  | 676 -> One (r541)
  | 675 -> One (r542)
  | 681 -> One (r543)
  | 696 -> One (r544)
  | 687 -> One (r545)
  | 695 -> One (r547)
  | 694 -> One (r548)
  | 692 -> One (r549)
  | 691 -> One (r550)
  | 690 -> One (r551)
  | 1963 -> One (r552)
  | 1962 -> One (r553)
  | 1961 -> One (r554)
  | 699 -> One (r555)
  | 1960 -> One (r556)
  | 1092 -> One (r557)
  | 1931 -> One (r559)
  | 1930 -> One (r560)
  | 1929 -> One (r561)
  | 1928 -> One (r562)
  | 1927 -> One (r563)
  | 1926 -> One (r564)
  | 1941 -> One (r566)
  | 1940 -> One (r567)
  | 1959 -> One (r569)
  | 1958 -> One (r570)
  | 1957 -> One (r571)
  | 1412 -> One (r574)
  | 1411 -> One (r575)
  | 1410 -> One (r576)
  | 1409 -> One (r577)
  | 1408 -> One (r578)
  | 1407 -> One (r579)
  | 708 -> One (r580)
  | 707 -> One (r581)
  | 755 -> One (r582)
  | 754 -> One (r583)
  | 1398 -> One (r584)
  | 1406 -> One (r586)
  | 1405 -> One (r587)
  | 711 -> One (r588)
  | 1149 -> One (r589)
  | 1387 -> One (r591)
  | 1386 -> One (r592)
  | 1383 -> One (r593)
  | 1380 -> One (r594)
  | 714 -> One (r595)
  | 1379 -> One (r596)
  | 1360 -> One (r597)
  | 1359 -> One (r598)
  | 1358 -> One (r599)
  | 1363 -> One (r601)
  | 1374 -> One (r603)
  | 1373 -> One (r604)
  | 722 -> One (r607)
  | 1370 -> One (r608)
  | 729 -> One (r609)
  | 728 -> One (r610)
  | 1369 -> One (r611)
  | 732 -> One (r612)
  | 731 -> One (r613)
  | 735 -> One (r614)
  | 740 -> One (r615)
  | 739 -> One (r616)
  | 738 | 1367 -> One (r617)
  | 1366 -> One (r618)
  | 766 -> One (r619)
  | 765 -> One (r620)
  | 764 -> One (r621)
  | 763 -> One (r622)
  | 745 -> One (r623)
  | 744 -> One (r624)
  | 751 -> One (r625)
  | 749 -> One (r626)
  | 748 -> One (r627)
  | 747 -> One (r628)
  | 753 -> One (r629)
  | 758 -> One (r630)
  | 757 -> One (r631)
  | 1330 -> One (r632)
  | 773 -> One (r633)
  | 772 -> One (r634)
  | 1329 -> One (r635)
  | 1316 -> One (r636)
  | 775 -> One (r637)
  | 777 -> One (r638)
  | 1153 | 1309 -> One (r639)
  | 1152 | 1308 -> One (r640)
  | 779 | 906 -> One (r641)
  | 778 | 905 -> One (r642)
  | 1301 -> One (r643)
  | 1290 -> One (r644)
  | 1289 -> One (r645)
  | 782 -> One (r646)
  | 781 -> One (r647)
  | 1288 -> One (r648)
  | 785 -> One (r649)
  | 784 -> One (r650)
  | 1287 -> One (r651)
  | 1283 -> One (r652)
  | 1282 -> One (r653)
  | 1281 -> One (r654)
  | 822 -> One (r655)
  | 823 -> One (r657)
  | 1117 -> One (r659)
  | 824 -> One (r661)
  | 1115 -> One (r663)
  | 1280 -> One (r665)
  | 830 -> One (r666)
  | 829 -> One (r667)
  | 826 -> One (r668)
  | 791 -> One (r669)
  | 790 -> One (r670)
  | 793 -> One (r671)
  | 802 -> One (r673)
  | 800 -> One (r674)
  | 799 -> One (r675)
  | 798 -> One (r676)
  | 797 -> One (r677)
  | 806 -> One (r678)
  | 805 -> One (r679)
  | 812 -> One (r680)
  | 811 -> One (r681)
  | 810 -> One (r682)
  | 821 -> One (r683)
  | 820 -> One (r684)
  | 819 -> One (r685)
  | 818 -> One (r686)
  | 828 -> One (r687)
  | 834 -> One (r688)
  | 833 -> One (r689)
  | 832 -> One (r690)
  | 1279 -> One (r691)
  | 845 -> One (r692)
  | 844 -> One (r693)
  | 843 -> One (r694)
  | 838 -> One (r695)
  | 842 -> One (r696)
  | 847 -> One (r697)
  | 849 -> One (r698)
  | 1181 | 1256 -> One (r699)
  | 1180 | 1255 -> One (r700)
  | 851 | 1179 -> One (r701)
  | 850 | 1178 -> One (r702)
  | 1249 -> One (r703)
  | 1254 -> One (r705)
  | 1253 -> One (r706)
  | 1252 -> One (r707)
  | 1251 -> One (r708)
  | 1250 -> One (r709)
  | 1247 -> One (r710)
  | 856 -> One (r711)
  | 855 -> One (r712)
  | 854 -> One (r713)
  | 853 -> One (r714)
  | 860 -> One (r715)
  | 859 -> One (r716)
  | 858 -> One (r717)
  | 863 -> One (r718)
  | 862 -> One (r719)
  | 865 -> One (r720)
  | 867 -> One (r721)
  | 874 -> One (r722)
  | 881 -> One (r723)
  | 884 -> One (r725)
  | 883 -> One (r726)
  | 876 -> One (r727)
  | 880 -> One (r728)
  | 1246 -> One (r729)
  | 890 -> One (r730)
  | 889 -> One (r731)
  | 888 -> One (r732)
  | 898 -> One (r733)
  | 897 -> One (r734)
  | 896 -> One (r735)
  | 895 -> One (r736)
  | 900 -> One (r737)
  | 902 -> One (r738)
  | 904 -> One (r739)
  | 910 -> One (r740)
  | 909 -> One (r741)
  | 908 -> One (r742)
  | 1148 -> One (r743)
  | 920 -> One (r744)
  | 919 -> One (r745)
  | 918 -> One (r746)
  | 917 -> One (r747)
  | 922 -> One (r748)
  | 926 -> One (r749)
  | 925 -> One (r750)
  | 924 -> One (r751)
  | 928 -> One (r752)
  | 933 -> One (r753)
  | 932 -> One (r754)
  | 941 -> One (r755)
  | 940 -> One (r756)
  | 939 -> One (r757)
  | 938 -> One (r758)
  | 947 -> One (r759)
  | 946 -> One (r760)
  | 945 -> One (r761)
  | 944 -> One (r762)
  | 956 -> One (r763)
  | 955 -> One (r764)
  | 954 -> One (r765)
  | 953 -> One (r766)
  | 960 -> One (r767)
  | 959 -> One (r768)
  | 967 -> One (r769)
  | 966 -> One (r770)
  | 965 -> One (r771)
  | 964 -> One (r772)
  | 973 -> One (r773)
  | 972 -> One (r774)
  | 971 -> One (r775)
  | 970 -> One (r776)
  | 979 -> One (r777)
  | 978 -> One (r778)
  | 977 -> One (r779)
  | 976 -> One (r780)
  | 985 -> One (r781)
  | 984 -> One (r782)
  | 983 -> One (r783)
  | 982 -> One (r784)
  | 991 -> One (r785)
  | 990 -> One (r786)
  | 989 -> One (r787)
  | 988 -> One (r788)
  | 997 -> One (r789)
  | 996 -> One (r790)
  | 995 -> One (r791)
  | 994 -> One (r792)
  | 1003 -> One (r793)
  | 1002 -> One (r794)
  | 1001 -> One (r795)
  | 1000 -> One (r796)
  | 1009 -> One (r797)
  | 1008 -> One (r798)
  | 1007 -> One (r799)
  | 1006 -> One (r800)
  | 1015 -> One (r801)
  | 1014 -> One (r802)
  | 1013 -> One (r803)
  | 1012 -> One (r804)
  | 1021 -> One (r805)
  | 1020 -> One (r806)
  | 1019 -> One (r807)
  | 1018 -> One (r808)
  | 1027 -> One (r809)
  | 1026 -> One (r810)
  | 1025 -> One (r811)
  | 1024 -> One (r812)
  | 1033 -> One (r813)
  | 1032 -> One (r814)
  | 1031 -> One (r815)
  | 1030 -> One (r816)
  | 1039 -> One (r817)
  | 1038 -> One (r818)
  | 1037 -> One (r819)
  | 1036 -> One (r820)
  | 1045 -> One (r821)
  | 1044 -> One (r822)
  | 1043 -> One (r823)
  | 1042 -> One (r824)
  | 1051 -> One (r825)
  | 1050 -> One (r826)
  | 1049 -> One (r827)
  | 1048 -> One (r828)
  | 1057 -> One (r829)
  | 1056 -> One (r830)
  | 1055 -> One (r831)
  | 1054 -> One (r832)
  | 1063 -> One (r833)
  | 1062 -> One (r834)
  | 1061 -> One (r835)
  | 1060 -> One (r836)
  | 1069 -> One (r837)
  | 1068 -> One (r838)
  | 1067 -> One (r839)
  | 1066 -> One (r840)
  | 1083 -> One (r841)
  | 1076 -> One (r842)
  | 1075 -> One (r843)
  | 1074 -> One (r844)
  | 1073 -> One (r845)
  | 1078 -> One (r846)
  | 1082 -> One (r847)
  | 1081 -> One (r848)
  | 1080 -> One (r849)
  | 1089 -> One (r850)
  | 1088 -> One (r851)
  | 1087 -> One (r852)
  | 1086 -> One (r853)
  | 1146 -> One (r854)
  | 1143 -> One (r855)
  | 1091 -> One (r856)
  | 1094 -> One (r857)
  | 1093 -> One (r858)
  | 1101 -> One (r859)
  | 1100 -> One (r860)
  | 1099 -> One (r861)
  | 1098 -> One (r862)
  | 1097 -> One (r863)
  | 1106 -> One (r864)
  | 1105 -> One (r865)
  | 1104 -> One (r866)
  | 1103 -> One (r867)
  | 1109 -> One (r868)
  | 1108 -> One (r869)
  | 1116 -> One (r870)
  | 1114 -> One (r871)
  | 1113 -> One (r872)
  | 1122 -> One (r873)
  | 1121 -> One (r874)
  | 1120 -> One (r875)
  | 1125 -> One (r876)
  | 1124 -> One (r877)
  | 1136 -> One (r878)
  | 1133 -> One (r879)
  | 1132 -> One (r880)
  | 1131 -> One (r881)
  | 1130 -> One (r882)
  | 1129 -> One (r883)
  | 1135 -> One (r884)
  | 1139 -> One (r885)
  | 1141 -> One (r886)
  | 1145 -> One (r887)
  | 1159 -> One (r888)
  | 1158 -> One (r889)
  | 1157 -> One (r890)
  | 1156 -> One (r891)
  | 1162 | 1312 -> One (r892)
  | 1161 | 1311 -> One (r893)
  | 1160 | 1310 -> One (r894)
  | 1168 -> One (r895)
  | 1167 -> One (r896)
  | 1166 -> One (r897)
  | 1165 -> One (r898)
  | 1171 | 1315 -> One (r899)
  | 1170 | 1314 -> One (r900)
  | 1169 | 1313 -> One (r901)
  | 1177 -> One (r902)
  | 1176 -> One (r903)
  | 1175 -> One (r904)
  | 1174 -> One (r905)
  | 1187 -> One (r906)
  | 1186 -> One (r907)
  | 1185 -> One (r908)
  | 1184 -> One (r909)
  | 1190 | 1259 -> One (r910)
  | 1189 | 1258 -> One (r911)
  | 1188 | 1257 -> One (r912)
  | 1196 -> One (r913)
  | 1195 -> One (r914)
  | 1194 -> One (r915)
  | 1193 -> One (r916)
  | 1199 | 1262 -> One (r917)
  | 1198 | 1261 -> One (r918)
  | 1197 | 1260 -> One (r919)
  | 1205 -> One (r920)
  | 1204 -> One (r921)
  | 1203 -> One (r922)
  | 1202 -> One (r923)
  | 1210 | 1267 -> One (r924)
  | 1209 | 1266 -> One (r925)
  | 1208 | 1265 -> One (r926)
  | 1207 | 1264 -> One (r927)
  | 1216 -> One (r928)
  | 1215 -> One (r929)
  | 1214 -> One (r930)
  | 1213 -> One (r931)
  | 1219 | 1270 -> One (r932)
  | 1218 | 1269 -> One (r933)
  | 1217 | 1268 -> One (r934)
  | 1225 -> One (r935)
  | 1224 -> One (r936)
  | 1223 -> One (r937)
  | 1222 -> One (r938)
  | 1228 | 1273 -> One (r939)
  | 1227 | 1272 -> One (r940)
  | 1226 | 1271 -> One (r941)
  | 1234 -> One (r942)
  | 1233 -> One (r943)
  | 1232 -> One (r944)
  | 1231 -> One (r945)
  | 1241 -> One (r946)
  | 1240 -> One (r947)
  | 1239 -> One (r948)
  | 1238 -> One (r949)
  | 1278 -> One (r950)
  | 1277 -> One (r951)
  | 1276 -> One (r952)
  | 1294 -> One (r953)
  | 1293 -> One (r954)
  | 1292 -> One (r955)
  | 1300 -> One (r956)
  | 1299 -> One (r957)
  | 1298 -> One (r958)
  | 1297 -> One (r959)
  | 1307 -> One (r960)
  | 1306 -> One (r961)
  | 1305 -> One (r962)
  | 1304 -> One (r963)
  | 1322 -> One (r964)
  | 1321 -> One (r965)
  | 1320 -> One (r966)
  | 1319 -> One (r967)
  | 1324 -> One (r968)
  | 1328 -> One (r969)
  | 1327 -> One (r970)
  | 1326 -> One (r971)
  | 1333 -> One (r972)
  | 1332 -> One (r973)
  | 1338 -> One (r974)
  | 1342 -> One (r975)
  | 1344 -> One (r976)
  | 1346 -> One (r977)
  | 1348 -> One (r978)
  | 1350 -> One (r979)
  | 1353 -> One (r981)
  | 1352 -> One (r982)
  | 1365 -> One (r983)
  | 1364 -> One (r984)
  | 1357 -> One (r985)
  | 1356 -> One (r986)
  | 1372 -> One (r987)
  | 1378 -> One (r988)
  | 1377 -> One (r989)
  | 1376 -> One (r990)
  | 1385 -> One (r991)
  | 1391 -> One (r992)
  | 1390 -> One (r993)
  | 1389 -> One (r994)
  | 1393 -> One (r995)
  | 1395 -> One (r996)
  | 1397 -> One (r997)
  | 1400 -> One (r998)
  | 1402 -> One (r999)
  | 1404 -> One (r1000)
  | 1427 -> One (r1001)
  | 1426 -> One (r1002)
  | 1445 -> One (r1004)
  | 1444 -> One (r1005)
  | 1443 -> One (r1006)
  | 1423 -> One (r1007)
  | 1422 -> One (r1008)
  | 1421 -> One (r1009)
  | 1420 -> One (r1010)
  | 1417 -> One (r1011)
  | 1416 -> One (r1012)
  | 1415 -> One (r1013)
  | 1414 -> One (r1014)
  | 1419 -> One (r1015)
  | 1442 -> One (r1016)
  | 1433 -> One (r1017)
  | 1432 -> One (r1018)
  | 1425 -> One (r1019)
  | 1431 -> One (r1020)
  | 1430 -> One (r1021)
  | 1429 -> One (r1022)
  | 1439 -> One (r1023)
  | 1438 -> One (r1024)
  | 1437 -> One (r1025)
  | 1436 -> One (r1026)
  | 1435 -> One (r1027)
  | 1441 -> One (r1028)
  | 1956 -> One (r1029)
  | 1955 -> One (r1030)
  | 1447 -> One (r1031)
  | 1449 -> One (r1032)
  | 1451 -> One (r1033)
  | 1954 -> One (r1034)
  | 1953 -> One (r1035)
  | 1453 -> One (r1036)
  | 1458 -> One (r1037)
  | 1457 -> One (r1038)
  | 1456 -> One (r1039)
  | 1455 -> One (r1040)
  | 1466 -> One (r1041)
  | 1469 -> One (r1043)
  | 1468 -> One (r1044)
  | 1465 -> One (r1045)
  | 1464 -> One (r1046)
  | 1463 -> One (r1047)
  | 1462 -> One (r1048)
  | 1461 -> One (r1049)
  | 1460 -> One (r1050)
  | 1523 -> One (r1051)
  | 1522 -> One (r1052)
  | 1521 -> One (r1053)
  | 1478 | 1582 -> One (r1054)
  | 1472 | 1581 -> One (r1055)
  | 1471 | 1580 -> One (r1056)
  | 1470 | 1579 -> One (r1057)
  | 1501 -> One (r1058)
  | 1500 -> One (r1060)
  | 1491 -> One (r1061)
  | 1496 -> One (r1069)
  | 1493 -> One (r1071)
  | 1492 -> One (r1072)
  | 1490 -> One (r1073)
  | 1489 -> One (r1074)
  | 1488 -> One (r1075)
  | 1487 -> One (r1076)
  | 1483 -> One (r1077)
  | 1482 -> One (r1078)
  | 1486 -> One (r1079)
  | 1485 -> One (r1080)
  | 1499 -> One (r1081)
  | 1498 -> One (r1082)
  | 1509 -> One (r1084)
  | 1508 -> One (r1085)
  | 1507 -> One (r1086)
  | 1506 -> One (r1087)
  | 1520 -> One (r1088)
  | 1516 -> One (r1089)
  | 1512 -> One (r1090)
  | 1515 -> One (r1091)
  | 1514 -> One (r1092)
  | 1519 -> One (r1093)
  | 1518 -> One (r1094)
  | 1777 -> One (r1095)
  | 1776 -> One (r1096)
  | 1536 -> One (r1097)
  | 1535 -> One (r1098)
  | 1534 -> One (r1099)
  | 1533 -> One (r1100)
  | 1532 -> One (r1101)
  | 1531 -> One (r1102)
  | 1530 -> One (r1103)
  | 1529 -> One (r1104)
  | 1569 -> One (r1105)
  | 1568 -> One (r1106)
  | 1571 -> One (r1108)
  | 1570 -> One (r1109)
  | 1564 -> One (r1110)
  | 1546 -> One (r1111)
  | 1545 -> One (r1112)
  | 1544 -> One (r1113)
  | 1543 -> One (r1114)
  | 1542 -> One (r1115)
  | 1550 -> One (r1119)
  | 1549 -> One (r1120)
  | 1563 -> One (r1121)
  | 1555 -> One (r1122)
  | 1554 -> One (r1123)
  | 1553 -> One (r1124)
  | 1552 -> One (r1125)
  | 1562 -> One (r1126)
  | 1561 -> One (r1127)
  | 1560 -> One (r1128)
  | 1559 -> One (r1129)
  | 1558 -> One (r1130)
  | 1557 -> One (r1131)
  | 1567 -> One (r1132)
  | 1566 -> One (r1133)
  | 1573 -> One (r1134)
  | 1578 -> One (r1135)
  | 1577 -> One (r1136)
  | 1576 -> One (r1137)
  | 1575 -> One (r1138)
  | 1638 | 1692 -> One (r1140)
  | 1694 -> One (r1142)
  | 1708 -> One (r1144)
  | 1698 -> One (r1145)
  | 1697 -> One (r1146)
  | 1679 -> One (r1147)
  | 1678 -> One (r1148)
  | 1677 -> One (r1149)
  | 1676 -> One (r1150)
  | 1675 -> One (r1151)
  | 1674 -> One (r1152)
  | 1673 -> One (r1153)
  | 1663 -> One (r1154)
  | 1662 -> One (r1155)
  | 1594 -> One (r1156)
  | 1593 -> One (r1157)
  | 1592 -> One (r1158)
  | 1588 -> One (r1159)
  | 1586 -> One (r1160)
  | 1585 -> One (r1161)
  | 1591 -> One (r1162)
  | 1590 -> One (r1163)
  | 1656 -> One (r1164)
  | 1655 -> One (r1165)
  | 1600 -> One (r1166)
  | 1596 -> One (r1167)
  | 1599 -> One (r1168)
  | 1598 -> One (r1169)
  | 1611 -> One (r1170)
  | 1610 -> One (r1171)
  | 1609 -> One (r1172)
  | 1608 -> One (r1173)
  | 1607 -> One (r1174)
  | 1602 -> One (r1175)
  | 1622 -> One (r1176)
  | 1621 -> One (r1177)
  | 1620 -> One (r1178)
  | 1619 -> One (r1179)
  | 1618 -> One (r1180)
  | 1613 -> One (r1181)
  | 1647 -> One (r1182)
  | 1646 -> One (r1183)
  | 1624 -> One (r1184)
  | 1645 -> One (r1185)
  | 1644 -> One (r1186)
  | 1643 -> One (r1187)
  | 1642 -> One (r1188)
  | 1626 -> One (r1189)
  | 1640 -> One (r1190)
  | 1630 -> One (r1191)
  | 1629 -> One (r1192)
  | 1628 -> One (r1193)
  | 1637 | 1685 -> One (r1194)
  | 1634 -> One (r1196)
  | 1633 -> One (r1197)
  | 1632 -> One (r1198)
  | 1631 | 1684 -> One (r1199)
  | 1636 -> One (r1200)
  | 1652 -> One (r1201)
  | 1651 -> One (r1202)
  | 1650 -> One (r1203)
  | 1654 -> One (r1205)
  | 1653 -> One (r1206)
  | 1649 -> One (r1207)
  | 1658 -> One (r1208)
  | 1661 -> One (r1209)
  | 1672 -> One (r1210)
  | 1671 -> One (r1211)
  | 1670 -> One (r1212)
  | 1669 -> One (r1213)
  | 1668 -> One (r1214)
  | 1667 -> One (r1215)
  | 1666 -> One (r1216)
  | 1665 -> One (r1217)
  | 1696 -> One (r1218)
  | 1683 -> One (r1219)
  | 1682 -> One (r1220)
  | 1681 -> One (r1221)
  | 1695 -> One (r1222)
  | 1687 -> One (r1223)
  | 1693 -> One (r1224)
  | 1690 -> One (r1225)
  | 1689 -> One (r1226)
  | 1707 -> One (r1227)
  | 1706 -> One (r1228)
  | 1705 -> One (r1229)
  | 1704 -> One (r1230)
  | 1703 -> One (r1231)
  | 1702 -> One (r1232)
  | 1701 -> One (r1233)
  | 1700 -> One (r1234)
  | 1717 -> One (r1235)
  | 1719 -> One (r1236)
  | 1729 -> One (r1237)
  | 1728 -> One (r1238)
  | 1727 -> One (r1239)
  | 1726 -> One (r1240)
  | 1725 -> One (r1241)
  | 1724 -> One (r1242)
  | 1723 -> One (r1243)
  | 1722 -> One (r1244)
  | 1773 -> One (r1245)
  | 1753 -> One (r1246)
  | 1752 -> One (r1247)
  | 1751 -> One (r1248)
  | 1750 -> One (r1249)
  | 1735 -> One (r1250)
  | 1734 -> One (r1251)
  | 1733 -> One (r1252)
  | 1732 -> One (r1253)
  | 1739 -> One (r1254)
  | 1738 -> One (r1255)
  | 1741 -> One (r1256)
  | 1746 -> One (r1257)
  | 1745 -> One (r1258)
  | 1744 | 2082 -> One (r1259)
  | 1748 | 2081 -> One (r1260)
  | 1770 -> One (r1261)
  | 1762 -> One (r1262)
  | 1761 -> One (r1263)
  | 1760 -> One (r1264)
  | 1769 -> One (r1265)
  | 1768 -> One (r1266)
  | 1862 -> One (r1267)
  | 1906 -> One (r1269)
  | 1787 -> One (r1270)
  | 1923 -> One (r1272)
  | 1914 -> One (r1273)
  | 1913 -> One (r1274)
  | 1785 -> One (r1275)
  | 1784 -> One (r1276)
  | 1783 -> One (r1277)
  | 1782 -> One (r1278)
  | 1781 -> One (r1279)
  | 1900 -> One (r1280)
  | 1899 -> One (r1281)
  | 1790 -> One (r1282)
  | 1789 -> One (r1283)
  | 1831 -> One (r1285)
  | 1820 -> One (r1286)
  | 1819 -> One (r1287)
  | 1810 -> One (r1288)
  | 1809 -> One (r1290)
  | 1808 -> One (r1291)
  | 1807 -> One (r1292)
  | 1796 -> One (r1293)
  | 1795 -> One (r1294)
  | 1793 -> One (r1295)
  | 1806 -> One (r1296)
  | 1805 -> One (r1297)
  | 1804 -> One (r1298)
  | 1803 -> One (r1299)
  | 1802 -> One (r1300)
  | 1801 -> One (r1301)
  | 1800 -> One (r1302)
  | 1799 -> One (r1303)
  | 1818 -> One (r1304)
  | 1817 -> One (r1305)
  | 1816 -> One (r1306)
  | 1830 -> One (r1307)
  | 1829 -> One (r1308)
  | 1828 -> One (r1309)
  | 1827 -> One (r1310)
  | 1826 -> One (r1311)
  | 1825 -> One (r1312)
  | 1824 -> One (r1313)
  | 1823 -> One (r1314)
  | 1835 -> One (r1315)
  | 1834 -> One (r1316)
  | 1833 -> One (r1317)
  | 1894 -> One (r1318)
  | 1893 -> One (r1319)
  | 1892 -> One (r1320)
  | 1891 -> One (r1321)
  | 1890 -> One (r1322)
  | 1889 -> One (r1323)
  | 1886 -> One (r1324)
  | 1838 -> One (r1325)
  | 1882 -> One (r1326)
  | 1881 -> One (r1327)
  | 1876 -> One (r1328)
  | 1875 -> One (r1329)
  | 1874 -> One (r1330)
  | 1873 -> One (r1331)
  | 1847 -> One (r1332)
  | 1846 -> One (r1333)
  | 1845 -> One (r1334)
  | 1844 -> One (r1335)
  | 1843 -> One (r1336)
  | 1842 -> One (r1337)
  | 1872 -> One (r1338)
  | 1851 -> One (r1339)
  | 1850 -> One (r1340)
  | 1849 -> One (r1341)
  | 1855 -> One (r1342)
  | 1854 -> One (r1343)
  | 1853 -> One (r1344)
  | 1869 -> One (r1345)
  | 1859 -> One (r1346)
  | 1858 -> One (r1347)
  | 1871 -> One (r1349)
  | 1857 -> One (r1350)
  | 1866 -> One (r1351)
  | 1861 -> One (r1352)
  | 1880 -> One (r1353)
  | 1879 -> One (r1354)
  | 1878 -> One (r1355)
  | 1885 -> One (r1356)
  | 1884 -> One (r1357)
  | 1888 -> One (r1358)
  | 1898 -> One (r1359)
  | 1897 -> One (r1360)
  | 1896 -> One (r1361)
  | 1902 -> One (r1362)
  | 1905 -> One (r1363)
  | 1910 -> One (r1364)
  | 1909 -> One (r1365)
  | 1908 -> One (r1366)
  | 1912 -> One (r1367)
  | 1922 -> One (r1368)
  | 1921 -> One (r1369)
  | 1920 -> One (r1370)
  | 1919 -> One (r1371)
  | 1918 -> One (r1372)
  | 1917 -> One (r1373)
  | 1916 -> One (r1374)
  | 1938 -> One (r1375)
  | 1943 -> One (r1376)
  | 1949 -> One (r1377)
  | 1948 -> One (r1378)
  | 1968 -> One (r1379)
  | 1967 -> One (r1380)
  | 1966 -> One (r1381)
  | 1972 -> One (r1382)
  | 1978 -> One (r1383)
  | 1977 -> One (r1384)
  | 1976 -> One (r1385)
  | 1975 -> One (r1386)
  | 1981 -> One (r1387)
  | 1980 -> One (r1388)
  | 1985 -> One (r1389)
  | 1995 -> One (r1390)
  | 1994 -> One (r1391)
  | 2006 -> One (r1392)
  | 2005 -> One (r1393)
  | 2004 -> One (r1394)
  | 2013 -> One (r1395)
  | 2012 -> One (r1396)
  | 2018 -> One (r1397)
  | 2017 -> One (r1398)
  | 2029 -> One (r1399)
  | 2028 -> One (r1400)
  | 2027 -> One (r1401)
  | 2031 -> One (r1402)
  | 2039 -> One (r1403)
  | 2049 -> One (r1404)
  | 2048 -> One (r1405)
  | 2047 -> One (r1406)
  | 2046 -> One (r1407)
  | 2051 -> One (r1408)
  | 2055 -> One (r1409)
  | 2054 -> One (r1410)
  | 2053 -> One (r1411)
  | 2064 -> One (r1412)
  | 2063 -> One (r1413)
  | 2062 -> One (r1414)
  | 2061 -> One (r1415)
  | 2066 -> One (r1416)
  | 2070 -> One (r1417)
  | 2069 -> One (r1418)
  | 2068 -> One (r1419)
  | 2085 -> One (r1420)
  | 2084 -> One (r1421)
  | 2097 -> One (r1422)
  | 2096 -> One (r1423)
  | 2120 -> One (r1424)
  | 2119 -> One (r1425)
  | 2118 -> One (r1426)
  | 2117 -> One (r1427)
  | 2116 -> One (r1428)
  | 2123 -> One (r1429)
  | 2122 -> One (r1430)
  | 2128 -> One (r1431)
  | 2134 -> One (r1432)
  | 2133 -> One (r1433)
  | 2132 -> One (r1434)
  | 2131 -> One (r1435)
  | 2130 -> One (r1436)
  | 2137 -> One (r1437)
  | 2136 -> One (r1438)
  | 2145 -> One (r1439)
  | 2144 -> One (r1440)
  | 2143 -> One (r1441)
  | 2159 -> One (r1442)
  | 2158 -> One (r1443)
  | 2157 -> One (r1444)
  | 2181 -> One (r1445)
  | 2185 -> One (r1446)
  | 2190 -> One (r1447)
  | 2197 -> One (r1448)
  | 2196 -> One (r1449)
  | 2195 -> One (r1450)
  | 2194 -> One (r1451)
  | 2204 -> One (r1452)
  | 2208 -> One (r1453)
  | 2212 -> One (r1454)
  | 2215 -> One (r1455)
  | 2220 -> One (r1456)
  | 2224 -> One (r1457)
  | 2228 -> One (r1458)
  | 2232 -> One (r1459)
  | 2236 -> One (r1460)
  | 2239 -> One (r1461)
  | 2243 -> One (r1462)
  | 2248 -> One (r1463)
  | 2258 -> One (r1464)
  | 2260 -> One (r1465)
  | 2263 -> One (r1466)
  | 2262 -> One (r1467)
  | 2265 -> One (r1468)
  | 2275 -> One (r1469)
  | 2271 -> One (r1470)
  | 2270 -> One (r1471)
  | 2274 -> One (r1472)
  | 2273 -> One (r1473)
  | 2280 -> One (r1474)
  | 2279 -> One (r1475)
  | 2278 -> One (r1476)
  | 2282 -> One (r1477)
  | 465 -> Select (function
    | -1 -> [R 98]
    | _ -> S (T T_DOT) :: r406)
  | 737 -> Select (function
    | -1 -> [R 98]
    | _ -> r618)
  | 131 -> Select (function
    | -1 -> r92
    | _ -> R 132 :: r114)
  | 182 -> Select (function
    | -1 -> r92
    | _ -> R 132 :: r174)
  | 701 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1447 | 1453 | 2267 -> r564
    | _ -> R 132 :: r573)
  | 1538 -> Select (function
    | -1 -> r1014
    | _ -> R 132 :: r1118)
  | 405 -> Select (function
    | -1 -> r266
    | _ -> [R 267])
  | 513 -> Select (function
    | -1 -> [R 840]
    | _ -> S (N N_pattern) :: r435)
  | 492 -> Select (function
    | -1 -> [R 841]
    | _ -> S (N N_pattern) :: r426)
  | 137 -> Select (function
    | -1 -> r120
    | _ -> R 933 :: r126)
  | 185 -> Select (function
    | -1 -> r120
    | _ -> R 933 :: r180)
  | 1503 -> Select (function
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> S (T T_COLONCOLON) :: r442)
  | 614 -> Select (function
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> Sub (r3) :: r496)
  | 356 -> Select (function
    | 619 | 769 | 1091 | 1337 | 1844 | 1878 | 1929 -> r47
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> r322)
  | 204 -> Select (function
    | -1 -> S (T T_RPAREN) :: r208
    | _ -> S (N N_module_type) :: r210)
  | 436 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r379
    | _ -> Sub (r381) :: r383)
  | 712 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r379
    | _ -> Sub (r590) :: r592)
  | 122 -> Select (function
    | -1 -> r70
    | _ -> S (T T_MODULE) :: r79)
  | 1505 -> Select (function
    | -1 -> r375
    | _ -> S (T T_LPAREN) :: r1087)
  | 259 -> Select (function
    | 1679 | 1683 | 1687 | 1690 | 1704 | 1883 | 1907 -> r260
    | -1 -> r278
    | _ -> S (T T_DOT) :: r281)
  | 403 -> Select (function
    | -1 -> r278
    | _ -> S (T T_DOT) :: r368)
  | 165 -> Select (function
    | -1 -> r93
    | _ -> S (T T_COLON) :: r150)
  | 114 -> Select (function
    | 122 | 163 | 167 | 250 | 808 | 816 | 1095 | 1510 -> r62
    | _ -> Sub (r59) :: r60)
  | 117 -> Select (function
    | 122 | 163 | 167 | 250 | 808 | 816 | 1095 | 1510 -> r61
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
  | 2099 -> Select (function
    | -1 -> r88
    | _ -> r93)
  | 2155 -> Select (function
    | -1 -> r88
    | _ -> r93)
  | 2154 -> Select (function
    | -1 -> r89
    | _ -> r112)
  | 2098 -> Select (function
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
  | 260 -> Select (function
    | 1679 | 1683 | 1687 | 1690 | 1704 | 1883 | 1907 -> r259
    | -1 -> r267
    | _ -> r281)
  | 404 -> Select (function
    | -1 -> r267
    | _ -> r368)
  | 703 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1447 | 1453 | 2267 -> r562
    | _ -> r572)
  | 702 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1447 | 1453 | 2267 -> r563
    | _ -> r573)
  | 1541 -> Select (function
    | -1 -> r1011
    | _ -> r1116)
  | 1540 -> Select (function
    | -1 -> r1012
    | _ -> r1117)
  | 1539 -> Select (function
    | -1 -> r1013
    | _ -> r1118)
  | _ -> raise Not_found

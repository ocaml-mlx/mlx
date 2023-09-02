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
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> ()
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
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> ()
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
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_jsx_prop_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_41_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;1;2;2;1;2;2;1;1;2;1;1;1;1;1;1;2;3;4;1;1;5;6;6;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;2;1;2;3;1;1;1;2;2;3;4;1;2;3;4;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;1;2;1;1;1;2;1;2;1;1;1;1;1;2;3;4;1;2;3;1;2;3;1;1;2;3;3;1;1;4;1;2;1;1;1;2;3;1;2;3;1;1;1;1;2;1;2;3;1;4;1;1;2;1;1;2;3;1;1;1;1;2;1;2;2;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;1;2;1;2;1;1;3;2;3;2;1;2;3;4;1;2;3;3;1;1;3;4;2;3;1;2;1;3;4;2;1;3;2;3;4;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;1;1;1;2;3;2;3;2;1;2;1;1;2;3;1;2;4;5;6;1;1;1;2;3;2;3;2;3;3;4;5;2;3;2;3;2;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;1;1;2;1;2;3;3;4;2;1;2;3;1;1;1;1;1;2;1;2;3;3;4;5;1;2;1;2;3;4;1;2;1;1;2;3;4;5;1;2;1;2;2;3;1;2;3;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;4;5;2;3;2;3;4;2;3;4;1;3;2;3;1;4;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;2;3;1;1;2;3;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;5;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;1;1;2;3;1;2;1;2;3;4;4;5;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;2;3;1;2;3;1;1;1;1;3;4;1;2;3;1;2;5;6;2;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;1;3;1;2;1;2;3;4;1;2;3;4;5;1;2;6;1;2;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;1;2;3;4;5;1;2;3;4;5;1;2;3;2;3;6;7;1;2;8;9;1;1;2;3;1;1;2;3;1;4;1;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;2;3;4;5;6;7;1;1;2;3;1;1;2;3;4;1;1;2;8;9;10;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;7;8;9;6;7;1;8;9;10;2;1;1;4;5;6;7;8;9;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;2;3;1;1;2;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;6;7;8;5;1;1;1;2;3;1;2;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;1;2;1;2;2;1;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;2;1;2;1;2;1;2;1;1;1;1;2;3;3;4;1;1;1;3;4;3;4;4;3;3;4;5;3;4;5;3;4;5;6;7;1;2;3;5;6;7;5;6;7;3;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;3;4;4;5;3;4;5;3;1;2;3;1;1;2;3;4;5;1;4;5;1;2;3;6;1;1;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;3;4;5;2;3;3;2;4;2;3;1;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;2;5;2;2;4;5;2;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;5;6;3;2;2;3;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;4;5;5;6;7;5;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;3;4;5;6;1;7;1;2;3;2;2;3;3;4;5;2;3;4;5;4;2;3;2;3;2;3;2;3;4;2;2;2;2;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;5;6;7;8;9;3;4;5;5;6;6;7;3;4;7;8;2;3;3;4;5;4;5;6;4;5;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY_LWT -> true
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
  | T_MATCH_LWT -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LET_LWT -> true
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
  | T_GREATERDOT -> true
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR_LWT -> true
  | T_FOR -> true
  | T_FINALLY_LWT -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_DOWNTO -> true
  | T_DOTTILDE -> true
  | T_DOTLESS -> true
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
  let r0 = [R 600] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 127] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 292 :: r6 in
  let r8 = [R 700] in
  let r9 = S (T T_AND) :: r8 in
  let r10 = [R 42] in
  let r11 = Sub (r9) :: r10 in
  let r12 = [R 188] in
  let r13 = [R 43] in
  let r14 = [R 517] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 44] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 142] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = R 292 :: r23 in
  let r25 = [R 668] in
  let r26 = [R 356] in
  let r27 = [R 123] in
  let r28 = Sub (r1) :: r27 in
  let r29 = R 292 :: r28 in
  let r30 = [R 325] in
  let r31 = Sub (r1) :: r30 in
  let r32 = S (T T_MINUSGREATER) :: r31 in
  let r33 = S (N N_pattern) :: r32 in
  let r34 = [R 565] in
  let r35 = Sub (r33) :: r34 in
  let r36 = [R 139] in
  let r37 = Sub (r35) :: r36 in
  let r38 = S (T T_WITH) :: r37 in
  let r39 = Sub (r1) :: r38 in
  let r40 = R 292 :: r39 in
  let r41 = [R 190] in
  let r42 = S (T T_UNDERSCORE) :: r25 in
  let r43 = [R 658] in
  let r44 = [R 652] in
  let r45 = S (T T_END) :: r44 in
  let r46 = R 309 :: r45 in
  let r47 = R 69 :: r46 in
  let r48 = R 292 :: r47 in
  let r49 = [R 67] in
  let r50 = S (T T_RPAREN) :: r49 in
  let r51 = [R 686] in
  let r52 = [R 628] in
  let r53 = [R 626] in
  let r54 = [R 101] in
  let r55 = [R 682] in
  let r56 = S (T T_RPAREN) :: r55 in
  let r57 = [R 452] in
  let r58 = S (T T_AMPERAMPER) :: r57 in
  let r59 = [R 814] in
  let r60 = S (T T_RPAREN) :: r59 in
  let r61 = Sub (r58) :: r60 in
  let r62 = [R 378] in
  let r63 = S (T T_UNDERSCORE) :: r62 in
  let r64 = [R 684] in
  let r65 = S (T T_RPAREN) :: r64 in
  let r66 = Sub (r63) :: r65 in
  let r67 = R 292 :: r66 in
  let r68 = [R 685] in
  let r69 = S (T T_RPAREN) :: r68 in
  let r70 = [R 344] in
  let r71 = [R 605] in
  let r72 = R 300 :: r71 in
  let r73 = [R 380] in
  let r74 = S (T T_END) :: r73 in
  let r75 = Sub (r72) :: r74 in
  let r76 = [R 815] in
  let r77 = S (T T_LIDENT) :: r76 in
  let r78 = [R 25] in
  let r79 = S (T T_UNDERSCORE) :: r78 in
  let r80 = [R 788] in
  let r81 = Sub (r79) :: r80 in
  let r82 = [R 202] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 17] in
  let r85 = Sub (r83) :: r84 in
  let r86 = [R 117] in
  let r87 = Sub (r85) :: r86 in
  let r88 = [R 522] in
  let r89 = Sub (r87) :: r88 in
  let r90 = [R 823] in
  let r91 = R 298 :: r90 in
  let r92 = Sub (r89) :: r91 in
  let r93 = S (T T_COLON) :: r92 in
  let r94 = Sub (r77) :: r93 in
  let r95 = R 292 :: r94 in
  let r96 = [R 426] in
  let r97 = S (T T_RPAREN) :: r96 in
  let r98 = R 224 :: r97 in
  let r99 = [R 225] in
  let r100 = [R 428] in
  let r101 = S (T T_RBRACKET) :: r100 in
  let r102 = [R 430] in
  let r103 = S (T T_RBRACE) :: r102 in
  let r104 = [R 222] in
  let r105 = S (T T_LIDENT) :: r104 in
  let r106 = [R 24] in
  let r107 = Sub (r105) :: r106 in
  let r108 = [R 563] in
  let r109 = [R 475] in
  let r110 = S (T T_COLON) :: r109 in
  let r111 = [R 23] in
  let r112 = S (T T_RPAREN) :: r111 in
  let r113 = S (N N_module_type) :: r112 in
  let r114 = R 292 :: r113 in
  let r115 = R 187 :: r114 in
  let r116 = [R 382] in
  let r117 = S (N N_module_expr) :: r116 in
  let r118 = R 292 :: r117 in
  let r119 = S (T T_OF) :: r118 in
  let r120 = [R 368] in
  let r121 = S (T T_END) :: r120 in
  let r122 = S (N N_structure) :: r121 in
  let r123 = [R 342] in
  let r124 = S (T T_LIDENT) :: r123 in
  let r125 = [R 795] in
  let r126 = Sub (r124) :: r125 in
  let r127 = [R 102] in
  let r128 = S (T T_FALSE) :: r127 in
  let r129 = [R 106] in
  let r130 = Sub (r128) :: r129 in
  let r131 = [R 216] in
  let r132 = R 292 :: r131 in
  let r133 = R 209 :: r132 in
  let r134 = Sub (r130) :: r133 in
  let r135 = [R 542] in
  let r136 = Sub (r134) :: r135 in
  let r137 = [R 763] in
  let r138 = R 298 :: r137 in
  let r139 = Sub (r136) :: r138 in
  let r140 = R 528 :: r139 in
  let r141 = S (T T_PLUSEQ) :: r140 in
  let r142 = Sub (r126) :: r141 in
  let r143 = R 797 :: r142 in
  let r144 = R 292 :: r143 in
  let r145 = [R 219] in
  let r146 = R 298 :: r145 in
  let r147 = R 553 :: r146 in
  let r148 = R 793 :: r147 in
  let r149 = S (T T_LIDENT) :: r148 in
  let r150 = R 797 :: r149 in
  let r151 = R 292 :: r150 in
  let r152 = R 187 :: r151 in
  let r153 = [R 764] in
  let r154 = R 298 :: r153 in
  let r155 = Sub (r136) :: r154 in
  let r156 = R 528 :: r155 in
  let r157 = S (T T_PLUSEQ) :: r156 in
  let r158 = Sub (r126) :: r157 in
  let r159 = [R 220] in
  let r160 = R 298 :: r159 in
  let r161 = R 553 :: r160 in
  let r162 = R 793 :: r161 in
  let r163 = S (T T_LIDENT) :: r162 in
  let r164 = R 797 :: r163 in
  let r165 = [R 801] in
  let r166 = S (T T_UNDERSCORE) :: r165 in
  let r167 = [R 796] in
  let r168 = Sub (r166) :: r167 in
  let r169 = R 802 :: r168 in
  let r170 = [R 576] in
  let r171 = Sub (r169) :: r170 in
  let r172 = [R 799] in
  let r173 = S (T T_RPAREN) :: r172 in
  let r174 = [R 800] in
  let r175 = [R 577] in
  let r176 = [R 411] in
  let r177 = S (T T_DOTDOT) :: r176 in
  let r178 = [R 794] in
  let r179 = [R 412] in
  let r180 = [R 105] in
  let r181 = S (T T_RPAREN) :: r180 in
  let r182 = [R 204] in
  let r183 = Sub (r83) :: r182 in
  let r184 = S (T T_MINUSGREATER) :: r183 in
  let r185 = Sub (r81) :: r184 in
  let r186 = [R 30] in
  let r187 = [R 524] in
  let r188 = Sub (r85) :: r187 in
  let r189 = [R 332] in
  let r190 = R 292 :: r189 in
  let r191 = Sub (r188) :: r190 in
  let r192 = [R 189] in
  let r193 = S (T T_RBRACKET) :: r192 in
  let r194 = Sub (r15) :: r193 in
  let r195 = [R 304] in
  let r196 = [R 419] in
  let r197 = R 298 :: r196 in
  let r198 = S (N N_module_expr) :: r197 in
  let r199 = R 292 :: r198 in
  let r200 = [R 420] in
  let r201 = R 298 :: r200 in
  let r202 = S (N N_module_expr) :: r201 in
  let r203 = R 292 :: r202 in
  let r204 = [R 477] in
  let r205 = S (T T_RPAREN) :: r204 in
  let r206 = [R 478] in
  let r207 = S (T T_RPAREN) :: r206 in
  let r208 = S (N N_expr) :: r207 in
  let r209 = [R 354] in
  let r210 = S (T T_LIDENT) :: r209 in
  let r211 = [R 66] in
  let r212 = Sub (r210) :: r211 in
  let r213 = [R 649] in
  let r214 = Sub (r212) :: r213 in
  let r215 = R 292 :: r214 in
  let r216 = [R 355] in
  let r217 = S (T T_LIDENT) :: r216 in
  let r218 = [R 357] in
  let r219 = [R 362] in
  let r220 = [R 293] in
  let r221 = [R 122] in
  let r222 = Sub (r35) :: r221 in
  let r223 = S (T T_WITH) :: r222 in
  let r224 = Sub (r1) :: r223 in
  let r225 = R 292 :: r224 in
  let r226 = [R 138] in
  let r227 = Sub (r35) :: r226 in
  let r228 = S (T T_WITH) :: r227 in
  let r229 = Sub (r1) :: r228 in
  let r230 = R 292 :: r229 in
  let r231 = [R 636] in
  let r232 = S (T T_RPAREN) :: r231 in
  let r233 = [R 673] in
  let r234 = [R 175] in
  let r235 = [R 262] in
  let r236 = Sub (r77) :: r235 in
  let r237 = [R 322] in
  let r238 = R 298 :: r237 in
  let r239 = Sub (r236) :: r238 in
  let r240 = R 535 :: r239 in
  let r241 = R 292 :: r240 in
  let r242 = [R 633] in
  let r243 = [R 100] in
  let r244 = [R 594] in
  let r245 = S (N N_pattern) :: r244 in
  let r246 = [R 631] in
  let r247 = S (T T_RBRACKET) :: r246 in
  let r248 = [R 246] in
  let r249 = Sub (r210) :: r248 in
  let r250 = [R 318] in
  let r251 = R 468 :: r250 in
  let r252 = R 462 :: r251 in
  let r253 = Sub (r249) :: r252 in
  let r254 = [R 630] in
  let r255 = S (T T_RBRACE) :: r254 in
  let r256 = [R 463] in
  let r257 = [R 587] in
  let r258 = Sub (r87) :: r257 in
  let r259 = [R 572] in
  let r260 = Sub (r258) :: r259 in
  let r261 = [R 39] in
  let r262 = S (T T_RBRACKET) :: r261 in
  let r263 = Sub (r260) :: r262 in
  let r264 = [R 38] in
  let r265 = [R 37] in
  let r266 = S (T T_RBRACKET) :: r265 in
  let r267 = [R 400] in
  let r268 = Sub (r105) :: r267 in
  let r269 = S (T T_BACKQUOTE) :: r268 in
  let r270 = [R 776] in
  let r271 = R 292 :: r270 in
  let r272 = Sub (r269) :: r271 in
  let r273 = [R 34] in
  let r274 = S (T T_RBRACKET) :: r273 in
  let r275 = [R 95] in
  let r276 = Sub (r124) :: r275 in
  let r277 = [R 31] in
  let r278 = [R 345] in
  let r279 = S (T T_UIDENT) :: r278 in
  let r280 = S (T T_DOT) :: r279 in
  let r281 = [R 343] in
  let r282 = S (T T_LIDENT) :: r281 in
  let r283 = S (T T_UIDENT) :: r70 in
  let r284 = [R 360] in
  let r285 = Sub (r283) :: r284 in
  let r286 = [R 361] in
  let r287 = S (T T_RPAREN) :: r286 in
  let r288 = [R 35] in
  let r289 = S (T T_RBRACKET) :: r288 in
  let r290 = [R 205] in
  let r291 = [R 584] in
  let r292 = [R 32] in
  let r293 = [R 203] in
  let r294 = Sub (r83) :: r293 in
  let r295 = S (T T_MINUSGREATER) :: r294 in
  let r296 = [R 585] in
  let r297 = [R 573] in
  let r298 = [R 568] in
  let r299 = Sub (r85) :: r298 in
  let r300 = [R 775] in
  let r301 = R 292 :: r300 in
  let r302 = Sub (r299) :: r301 in
  let r303 = [R 569] in
  let r304 = [R 18] in
  let r305 = Sub (r105) :: r304 in
  let r306 = [R 36] in
  let r307 = S (T T_RBRACKET) :: r306 in
  let r308 = Sub (r260) :: r307 in
  let r309 = [R 561] in
  let r310 = Sub (r269) :: r309 in
  let r311 = [R 40] in
  let r312 = S (T T_RBRACKET) :: r311 in
  let r313 = [R 469] in
  let r314 = S (T T_UNDERSCORE) :: r51 in
  let r315 = [R 681] in
  let r316 = Sub (r314) :: r315 in
  let r317 = [R 508] in
  let r318 = Sub (r316) :: r317 in
  let r319 = R 292 :: r318 in
  let r320 = [R 96] in
  let r321 = [R 691] in
  let r322 = S (T T_INT) :: r320 in
  let r323 = [R 625] in
  let r324 = Sub (r322) :: r323 in
  let r325 = [R 688] in
  let r326 = [R 693] in
  let r327 = S (T T_RBRACKET) :: r326 in
  let r328 = S (T T_LBRACKET) :: r327 in
  let r329 = [R 694] in
  let r330 = [R 499] in
  let r331 = S (N N_pattern) :: r330 in
  let r332 = R 292 :: r331 in
  let r333 = [R 500] in
  let r334 = [R 493] in
  let r335 = [R 507] in
  let r336 = [R 505] in
  let r337 = [R 401] in
  let r338 = S (T T_LIDENT) :: r337 in
  let r339 = [R 506] in
  let r340 = Sub (r316) :: r339 in
  let r341 = S (T T_RPAREN) :: r340 in
  let r342 = [R 110] in
  let r343 = [R 109] in
  let r344 = S (T T_RPAREN) :: r343 in
  let r345 = [R 501] in
  let r346 = [R 696] in
  let r347 = S (T T_RPAREN) :: r346 in
  let r348 = [R 498] in
  let r349 = [R 496] in
  let r350 = [R 108] in
  let r351 = S (T T_RPAREN) :: r350 in
  let r352 = [R 695] in
  let r353 = [R 320] in
  let r354 = [R 632] in
  let r355 = [R 258] in
  let r356 = [R 244] in
  let r357 = S (T T_LIDENT) :: r356 in
  let r358 = [R 257] in
  let r359 = S (T T_RPAREN) :: r358 in
  let r360 = [R 245] in
  let r361 = [R 254] in
  let r362 = [R 253] in
  let r363 = S (T T_RPAREN) :: r362 in
  let r364 = R 470 :: r363 in
  let r365 = [R 471] in
  let r366 = [R 277] in
  let r367 = Sub (r77) :: r366 in
  let r368 = [R 280] in
  let r369 = Sub (r367) :: r368 in
  let r370 = [R 173] in
  let r371 = Sub (r1) :: r370 in
  let r372 = S (T T_IN) :: r371 in
  let r373 = [R 516] in
  let r374 = S (T T_UNDERSCORE) :: r373 in
  let r375 = [R 256] in
  let r376 = [R 255] in
  let r377 = S (T T_RPAREN) :: r376 in
  let r378 = R 470 :: r377 in
  let r379 = [R 275] in
  let r380 = [R 751] in
  let r381 = Sub (r1) :: r380 in
  let r382 = S (T T_EQUAL) :: r381 in
  let r383 = [R 196] in
  let r384 = Sub (r382) :: r383 in
  let r385 = [R 753] in
  let r386 = Sub (r384) :: r385 in
  let r387 = S (T T_RPAREN) :: r386 in
  let r388 = Sub (r338) :: r387 in
  let r389 = [R 259] in
  let r390 = [R 133] in
  let r391 = Sub (r1) :: r390 in
  let r392 = S (T T_IN) :: r391 in
  let r393 = S (N N_module_expr) :: r392 in
  let r394 = R 292 :: r393 in
  let r395 = R 187 :: r394 in
  let r396 = [R 269] in
  let r397 = R 298 :: r396 in
  let r398 = Sub (r236) :: r397 in
  let r399 = R 535 :: r398 in
  let r400 = R 292 :: r399 in
  let r401 = R 187 :: r400 in
  let r402 = [R 134] in
  let r403 = Sub (r1) :: r402 in
  let r404 = S (T T_IN) :: r403 in
  let r405 = S (N N_module_expr) :: r404 in
  let r406 = R 292 :: r405 in
  let r407 = [R 369] in
  let r408 = S (N N_module_expr) :: r407 in
  let r409 = S (T T_MINUSGREATER) :: r408 in
  let r410 = S (N N_functor_args) :: r409 in
  let r411 = [R 206] in
  let r412 = [R 207] in
  let r413 = S (T T_RPAREN) :: r412 in
  let r414 = S (N N_module_type) :: r413 in
  let r415 = [R 383] in
  let r416 = S (T T_RPAREN) :: r415 in
  let r417 = [R 386] in
  let r418 = S (N N_module_type) :: r417 in
  let r419 = [R 381] in
  let r420 = S (N N_module_type) :: r419 in
  let r421 = S (T T_MINUSGREATER) :: r420 in
  let r422 = S (N N_functor_args) :: r421 in
  let r423 = [R 352] in
  let r424 = Sub (r105) :: r423 in
  let r425 = [R 392] in
  let r426 = Sub (r424) :: r425 in
  let r427 = [R 836] in
  let r428 = S (N N_module_type) :: r427 in
  let r429 = S (T T_EQUAL) :: r428 in
  let r430 = Sub (r426) :: r429 in
  let r431 = S (T T_TYPE) :: r430 in
  let r432 = S (T T_MODULE) :: r431 in
  let r433 = [R 570] in
  let r434 = Sub (r432) :: r433 in
  let r435 = [R 388] in
  let r436 = [R 833] in
  let r437 = Sub (r85) :: r436 in
  let r438 = S (T T_COLONEQUAL) :: r437 in
  let r439 = Sub (r249) :: r438 in
  let r440 = [R 832] in
  let r441 = R 553 :: r440 in
  let r442 = [R 554] in
  let r443 = Sub (r87) :: r442 in
  let r444 = S (T T_EQUAL) :: r443 in
  let r445 = [R 353] in
  let r446 = Sub (r105) :: r445 in
  let r447 = [R 837] in
  let r448 = [R 387] in
  let r449 = [R 834] in
  let r450 = Sub (r285) :: r449 in
  let r451 = S (T T_UIDENT) :: r218 in
  let r452 = [R 835] in
  let r453 = [R 571] in
  let r454 = [R 374] in
  let r455 = [R 476] in
  let r456 = S (T T_RPAREN) :: r455 in
  let r457 = [R 588] in
  let r458 = S (N N_expr) :: r457 in
  let r459 = [R 676] in
  let r460 = S (T T_RBRACKET) :: r459 in
  let r461 = [R 661] in
  let r462 = [R 591] in
  let r463 = R 464 :: r462 in
  let r464 = [R 465] in
  let r465 = [R 597] in
  let r466 = R 464 :: r465 in
  let r467 = R 472 :: r466 in
  let r468 = Sub (r249) :: r467 in
  let r469 = [R 537] in
  let r470 = Sub (r468) :: r469 in
  let r471 = [R 670] in
  let r472 = S (T T_RBRACE) :: r471 in
  let r473 = [R 358] in
  let r474 = Sub (r77) :: r473 in
  let r475 = [R 817] in
  let r476 = Sub (r474) :: r475 in
  let r477 = [R 233] in
  let r478 = [R 359] in
  let r479 = Sub (r77) :: r478 in
  let r480 = [R 635] in
  let r481 = [R 634] in
  let r482 = S (T T_GREATERDOT) :: r481 in
  let r483 = [R 145] in
  let r484 = Sub (r42) :: r483 in
  let r485 = R 292 :: r484 in
  let r486 = [R 648] in
  let r487 = S (T T_END) :: r486 in
  let r488 = R 292 :: r487 in
  let r489 = [R 141] in
  let r490 = S (N N_expr) :: r489 in
  let r491 = S (T T_THEN) :: r490 in
  let r492 = Sub (r1) :: r491 in
  let r493 = R 292 :: r492 in
  let r494 = [R 135] in
  let r495 = Sub (r35) :: r494 in
  let r496 = R 292 :: r495 in
  let r497 = [R 566] in
  let r498 = [R 326] in
  let r499 = Sub (r1) :: r498 in
  let r500 = S (T T_MINUSGREATER) :: r499 in
  let r501 = [R 260] in
  let r502 = Sub (r316) :: r501 in
  let r503 = [R 198] in
  let r504 = Sub (r1) :: r503 in
  let r505 = S (T T_MINUSGREATER) :: r504 in
  let r506 = [R 136] in
  let r507 = Sub (r505) :: r506 in
  let r508 = Sub (r502) :: r507 in
  let r509 = R 292 :: r508 in
  let r510 = [R 137] in
  let r511 = Sub (r505) :: r510 in
  let r512 = S (T T_RPAREN) :: r511 in
  let r513 = [R 129] in
  let r514 = S (T T_DONE) :: r513 in
  let r515 = Sub (r1) :: r514 in
  let r516 = S (T T_DO) :: r515 in
  let r517 = Sub (r1) :: r516 in
  let r518 = S (T T_IN) :: r517 in
  let r519 = S (N N_pattern) :: r518 in
  let r520 = R 292 :: r519 in
  let r521 = [R 120] in
  let r522 = S (T T_DOWNTO) :: r521 in
  let r523 = [R 143] in
  let r524 = S (T T_DONE) :: r523 in
  let r525 = Sub (r1) :: r524 in
  let r526 = S (T T_DO) :: r525 in
  let r527 = Sub (r1) :: r526 in
  let r528 = Sub (r522) :: r527 in
  let r529 = Sub (r1) :: r528 in
  let r530 = S (T T_EQUAL) :: r529 in
  let r531 = S (N N_pattern) :: r530 in
  let r532 = R 292 :: r531 in
  let r533 = [R 659] in
  let r534 = [R 669] in
  let r535 = S (T T_RPAREN) :: r534 in
  let r536 = S (T T_LPAREN) :: r535 in
  let r537 = S (T T_DOT) :: r536 in
  let r538 = [R 679] in
  let r539 = S (T T_RPAREN) :: r538 in
  let r540 = S (N N_module_type) :: r539 in
  let r541 = S (T T_COLON) :: r540 in
  let r542 = S (N N_module_expr) :: r541 in
  let r543 = R 292 :: r542 in
  let r544 = [R 278] in
  let r545 = Sub (r1) :: r544 in
  let r546 = S (T T_EQUAL) :: r545 in
  let r547 = [R 144] in
  let r548 = Sub (r42) :: r547 in
  let r549 = R 292 :: r548 in
  let r550 = [R 666] in
  let r551 = [R 641] in
  let r552 = S (T T_RPAREN) :: r551 in
  let r553 = Sub (r458) :: r552 in
  let r554 = S (T T_LPAREN) :: r553 in
  let r555 = [R 170] in
  let r556 = [R 249] in
  let r557 = [R 790] in
  let r558 = Sub (r87) :: r557 in
  let r559 = S (T T_COLON) :: r558 in
  let r560 = [R 250] in
  let r561 = S (T T_RPAREN) :: r560 in
  let r562 = Sub (r559) :: r561 in
  let r563 = [R 792] in
  let r564 = [R 791] in
  let r565 = [R 251] in
  let r566 = [R 252] in
  let r567 = [R 665] in
  let r568 = [R 638] in
  let r569 = S (T T_RPAREN) :: r568 in
  let r570 = Sub (r1) :: r569 in
  let r571 = S (T T_LPAREN) :: r570 in
  let r572 = [R 582] in
  let r573 = [R 229] in
  let r574 = S (T T_SLASHGREATER) :: r573 in
  let r575 = [R 238] in
  let r576 = [R 235] in
  let r577 = S (T T_JSX_LIDENT_E) :: r576 in
  let r578 = [R 230] in
  let r579 = S (T T_GREATER) :: r578 in
  let r580 = Sub (r577) :: r579 in
  let r581 = [R 236] in
  let r582 = [R 121] in
  let r583 = Sub (r1) :: r582 in
  let r584 = [R 172] in
  let r585 = Sub (r1) :: r584 in
  let r586 = [R 160] in
  let r587 = [R 154] in
  let r588 = [R 171] in
  let r589 = [R 603] in
  let r590 = Sub (r1) :: r589 in
  let r591 = [R 157] in
  let r592 = [R 161] in
  let r593 = [R 153] in
  let r594 = [R 156] in
  let r595 = [R 155] in
  let r596 = [R 165] in
  let r597 = [R 159] in
  let r598 = [R 158] in
  let r599 = [R 163] in
  let r600 = [R 152] in
  let r601 = [R 151] in
  let r602 = [R 174] in
  let r603 = [R 150] in
  let r604 = [R 164] in
  let r605 = [R 162] in
  let r606 = [R 166] in
  let r607 = [R 167] in
  let r608 = [R 168] in
  let r609 = [R 583] in
  let r610 = [R 169] in
  let r611 = [R 19] in
  let r612 = R 298 :: r611 in
  let r613 = Sub (r236) :: r612 in
  let r614 = [R 268] in
  let r615 = Sub (r1) :: r614 in
  let r616 = S (T T_EQUAL) :: r615 in
  let r617 = [R 267] in
  let r618 = Sub (r1) :: r617 in
  let r619 = [R 503] in
  let r620 = [R 509] in
  let r621 = [R 514] in
  let r622 = [R 512] in
  let r623 = [R 502] in
  let r624 = [R 526] in
  let r625 = S (T T_RBRACKET) :: r624 in
  let r626 = Sub (r15) :: r625 in
  let r627 = [R 520] in
  let r628 = [R 521] in
  let r629 = [R 363] in
  let r630 = S (N N_module_expr) :: r629 in
  let r631 = S (T T_EQUAL) :: r630 in
  let r632 = [R 766] in
  let r633 = R 298 :: r632 in
  let r634 = Sub (r631) :: r633 in
  let r635 = Sub (r63) :: r634 in
  let r636 = R 292 :: r635 in
  let r637 = [R 390] in
  let r638 = R 298 :: r637 in
  let r639 = R 466 :: r638 in
  let r640 = Sub (r105) :: r639 in
  let r641 = R 292 :: r640 in
  let r642 = R 187 :: r641 in
  let r643 = [R 467] in
  let r644 = [R 299] in
  let r645 = [R 767] in
  let r646 = R 288 :: r645 in
  let r647 = R 298 :: r646 in
  let r648 = Sub (r631) :: r647 in
  let r649 = [R 364] in
  let r650 = S (N N_module_expr) :: r649 in
  let r651 = S (T T_EQUAL) :: r650 in
  let r652 = [R 289] in
  let r653 = R 288 :: r652 in
  let r654 = R 298 :: r653 in
  let r655 = Sub (r631) :: r654 in
  let r656 = Sub (r63) :: r655 in
  let r657 = [R 365] in
  let r658 = [R 227] in
  let r659 = S (T T_RBRACKET) :: r658 in
  let r660 = Sub (r15) :: r659 in
  let r661 = [R 193] in
  let r662 = S (T T_RBRACKET) :: r661 in
  let r663 = Sub (r15) :: r662 in
  let r664 = [R 772] in
  let r665 = R 298 :: r664 in
  let r666 = S (N N_module_expr) :: r665 in
  let r667 = R 292 :: r666 in
  let r668 = [R 403] in
  let r669 = S (T T_STRING) :: r668 in
  let r670 = [R 527] in
  let r671 = R 298 :: r670 in
  let r672 = Sub (r669) :: r671 in
  let r673 = S (T T_EQUAL) :: r672 in
  let r674 = Sub (r89) :: r673 in
  let r675 = S (T T_COLON) :: r674 in
  let r676 = Sub (r77) :: r675 in
  let r677 = R 292 :: r676 in
  let r678 = [R 523] in
  let r679 = Sub (r87) :: r678 in
  let r680 = [R 564] in
  let r681 = Sub (r128) :: r342 in
  let r682 = [R 750] in
  let r683 = R 298 :: r682 in
  let r684 = R 292 :: r683 in
  let r685 = Sub (r681) :: r684 in
  let r686 = S (T T_EQUAL) :: r685 in
  let r687 = Sub (r130) :: r686 in
  let r688 = R 292 :: r687 in
  let r689 = [R 604] in
  let r690 = R 298 :: r689 in
  let r691 = R 292 :: r690 in
  let r692 = R 209 :: r691 in
  let r693 = Sub (r130) :: r692 in
  let r694 = R 292 :: r693 in
  let r695 = R 187 :: r694 in
  let r696 = [R 112] in
  let r697 = Sub (r79) :: r696 in
  let r698 = [R 210] in
  let r699 = [R 239] in
  let r700 = R 292 :: r699 in
  let r701 = Sub (r188) :: r700 in
  let r702 = S (T T_COLON) :: r701 in
  let r703 = S (T T_LIDENT) :: r702 in
  let r704 = R 393 :: r703 in
  let r705 = [R 241] in
  let r706 = Sub (r704) :: r705 in
  let r707 = [R 114] in
  let r708 = S (T T_RBRACE) :: r707 in
  let r709 = [R 240] in
  let r710 = R 292 :: r709 in
  let r711 = S (T T_SEMI) :: r710 in
  let r712 = R 292 :: r711 in
  let r713 = Sub (r188) :: r712 in
  let r714 = S (T T_COLON) :: r713 in
  let r715 = [R 525] in
  let r716 = Sub (r85) :: r715 in
  let r717 = [R 113] in
  let r718 = Sub (r79) :: r717 in
  let r719 = S (T T_COLONCOLON) :: r351 in
  let r720 = [R 213] in
  let r721 = [R 214] in
  let r722 = Sub (r79) :: r721 in
  let r723 = [R 212] in
  let r724 = Sub (r79) :: r723 in
  let r725 = [R 211] in
  let r726 = Sub (r79) :: r725 in
  let r727 = [R 518] in
  let r728 = [R 548] in
  let r729 = Sub (r134) :: r728 in
  let r730 = [R 612] in
  let r731 = R 298 :: r730 in
  let r732 = Sub (r729) :: r731 in
  let r733 = R 528 :: r732 in
  let r734 = S (T T_PLUSEQ) :: r733 in
  let r735 = Sub (r126) :: r734 in
  let r736 = R 797 :: r735 in
  let r737 = R 292 :: r736 in
  let r738 = [R 613] in
  let r739 = R 298 :: r738 in
  let r740 = Sub (r729) :: r739 in
  let r741 = R 528 :: r740 in
  let r742 = S (T T_PLUSEQ) :: r741 in
  let r743 = Sub (r126) :: r742 in
  let r744 = [R 218] in
  let r745 = R 298 :: r744 in
  let r746 = R 553 :: r745 in
  let r747 = [R 415] in
  let r748 = S (T T_RBRACE) :: r747 in
  let r749 = [R 215] in
  let r750 = R 292 :: r749 in
  let r751 = R 209 :: r750 in
  let r752 = Sub (r130) :: r751 in
  let r753 = [R 413] in
  let r754 = [R 414] in
  let r755 = [R 418] in
  let r756 = S (T T_RBRACE) :: r755 in
  let r757 = [R 417] in
  let r758 = S (T T_RBRACE) :: r757 in
  let r759 = [R 217] in
  let r760 = R 298 :: r759 in
  let r761 = R 553 :: r760 in
  let r762 = [R 301] in
  let r763 = [R 421] in
  let r764 = R 298 :: r763 in
  let r765 = Sub (r285) :: r764 in
  let r766 = R 292 :: r765 in
  let r767 = [R 422] in
  let r768 = R 298 :: r767 in
  let r769 = Sub (r285) :: r768 in
  let r770 = R 292 :: r769 in
  let r771 = [R 366] in
  let r772 = S (N N_module_type) :: r771 in
  let r773 = S (T T_COLON) :: r772 in
  let r774 = [R 615] in
  let r775 = R 298 :: r774 in
  let r776 = Sub (r773) :: r775 in
  let r777 = Sub (r63) :: r776 in
  let r778 = R 292 :: r777 in
  let r779 = [R 391] in
  let r780 = R 298 :: r779 in
  let r781 = S (N N_module_type) :: r780 in
  let r782 = S (T T_COLONEQUAL) :: r781 in
  let r783 = Sub (r105) :: r782 in
  let r784 = R 292 :: r783 in
  let r785 = [R 379] in
  let r786 = R 298 :: r785 in
  let r787 = [R 618] in
  let r788 = R 290 :: r787 in
  let r789 = R 298 :: r788 in
  let r790 = S (N N_module_type) :: r789 in
  let r791 = S (T T_COLON) :: r790 in
  let r792 = [R 291] in
  let r793 = R 290 :: r792 in
  let r794 = R 298 :: r793 in
  let r795 = S (N N_module_type) :: r794 in
  let r796 = S (T T_COLON) :: r795 in
  let r797 = Sub (r63) :: r796 in
  let r798 = S (T T_UIDENT) :: r26 in
  let r799 = Sub (r798) :: r219 in
  let r800 = [R 616] in
  let r801 = R 298 :: r800 in
  let r802 = [R 367] in
  let r803 = [R 622] in
  let r804 = R 298 :: r803 in
  let r805 = S (N N_module_type) :: r804 in
  let r806 = R 292 :: r805 in
  let r807 = S (T T_QUOTED_STRING_EXPR) :: r41 in
  let r808 = [R 80] in
  let r809 = Sub (r807) :: r808 in
  let r810 = [R 90] in
  let r811 = Sub (r809) :: r810 in
  let r812 = [R 623] in
  let r813 = R 284 :: r812 in
  let r814 = R 298 :: r813 in
  let r815 = Sub (r811) :: r814 in
  let r816 = S (T T_COLON) :: r815 in
  let r817 = S (T T_LIDENT) :: r816 in
  let r818 = R 194 :: r817 in
  let r819 = R 824 :: r818 in
  let r820 = R 292 :: r819 in
  let r821 = [R 94] in
  let r822 = R 286 :: r821 in
  let r823 = R 298 :: r822 in
  let r824 = Sub (r809) :: r823 in
  let r825 = S (T T_EQUAL) :: r824 in
  let r826 = S (T T_LIDENT) :: r825 in
  let r827 = R 194 :: r826 in
  let r828 = R 824 :: r827 in
  let r829 = R 292 :: r828 in
  let r830 = [R 195] in
  let r831 = S (T T_RBRACKET) :: r830 in
  let r832 = [R 81] in
  let r833 = S (T T_END) :: r832 in
  let r834 = R 307 :: r833 in
  let r835 = R 71 :: r834 in
  let r836 = [R 70] in
  let r837 = S (T T_RPAREN) :: r836 in
  let r838 = [R 73] in
  let r839 = R 298 :: r838 in
  let r840 = Sub (r87) :: r839 in
  let r841 = S (T T_COLON) :: r840 in
  let r842 = S (T T_LIDENT) :: r841 in
  let r843 = R 395 :: r842 in
  let r844 = [R 74] in
  let r845 = R 298 :: r844 in
  let r846 = Sub (r89) :: r845 in
  let r847 = S (T T_COLON) :: r846 in
  let r848 = S (T T_LIDENT) :: r847 in
  let r849 = R 530 :: r848 in
  let r850 = [R 72] in
  let r851 = R 298 :: r850 in
  let r852 = Sub (r809) :: r851 in
  let r853 = [R 83] in
  let r854 = Sub (r809) :: r853 in
  let r855 = S (T T_IN) :: r854 in
  let r856 = Sub (r799) :: r855 in
  let r857 = R 292 :: r856 in
  let r858 = [R 84] in
  let r859 = Sub (r809) :: r858 in
  let r860 = S (T T_IN) :: r859 in
  let r861 = Sub (r799) :: r860 in
  let r862 = [R 574] in
  let r863 = Sub (r87) :: r862 in
  let r864 = [R 79] in
  let r865 = Sub (r276) :: r864 in
  let r866 = S (T T_RBRACKET) :: r865 in
  let r867 = Sub (r863) :: r866 in
  let r868 = [R 575] in
  let r869 = [R 111] in
  let r870 = Sub (r87) :: r869 in
  let r871 = S (T T_EQUAL) :: r870 in
  let r872 = Sub (r87) :: r871 in
  let r873 = [R 75] in
  let r874 = R 298 :: r873 in
  let r875 = Sub (r872) :: r874 in
  let r876 = [R 76] in
  let r877 = [R 308] in
  let r878 = [R 287] in
  let r879 = R 286 :: r878 in
  let r880 = R 298 :: r879 in
  let r881 = Sub (r809) :: r880 in
  let r882 = S (T T_EQUAL) :: r881 in
  let r883 = S (T T_LIDENT) :: r882 in
  let r884 = R 194 :: r883 in
  let r885 = R 824 :: r884 in
  let r886 = [R 92] in
  let r887 = Sub (r811) :: r886 in
  let r888 = S (T T_MINUSGREATER) :: r887 in
  let r889 = Sub (r81) :: r888 in
  let r890 = [R 93] in
  let r891 = Sub (r811) :: r890 in
  let r892 = [R 91] in
  let r893 = Sub (r811) :: r892 in
  let r894 = S (T T_MINUSGREATER) :: r893 in
  let r895 = [R 285] in
  let r896 = R 284 :: r895 in
  let r897 = R 298 :: r896 in
  let r898 = Sub (r811) :: r897 in
  let r899 = S (T T_COLON) :: r898 in
  let r900 = S (T T_LIDENT) :: r899 in
  let r901 = R 194 :: r900 in
  let r902 = R 824 :: r901 in
  let r903 = [R 302] in
  let r904 = [R 606] in
  let r905 = [R 610] in
  let r906 = [R 295] in
  let r907 = R 294 :: r906 in
  let r908 = R 298 :: r907 in
  let r909 = R 553 :: r908 in
  let r910 = R 793 :: r909 in
  let r911 = S (T T_LIDENT) :: r910 in
  let r912 = R 797 :: r911 in
  let r913 = [R 611] in
  let r914 = [R 297] in
  let r915 = R 296 :: r914 in
  let r916 = R 298 :: r915 in
  let r917 = R 553 :: r916 in
  let r918 = Sub (r177) :: r917 in
  let r919 = S (T T_COLONEQUAL) :: r918 in
  let r920 = S (T T_LIDENT) :: r919 in
  let r921 = R 797 :: r920 in
  let r922 = [R 52] in
  let r923 = Sub (r807) :: r922 in
  let r924 = [R 61] in
  let r925 = Sub (r923) :: r924 in
  let r926 = S (T T_EQUAL) :: r925 in
  let r927 = [R 770] in
  let r928 = R 282 :: r927 in
  let r929 = R 298 :: r928 in
  let r930 = Sub (r926) :: r929 in
  let r931 = S (T T_LIDENT) :: r930 in
  let r932 = R 194 :: r931 in
  let r933 = R 824 :: r932 in
  let r934 = R 292 :: r933 in
  let r935 = [R 89] in
  let r936 = S (T T_END) :: r935 in
  let r937 = R 309 :: r936 in
  let r938 = R 69 :: r937 in
  let r939 = [R 819] in
  let r940 = Sub (r1) :: r939 in
  let r941 = S (T T_EQUAL) :: r940 in
  let r942 = S (T T_LIDENT) :: r941 in
  let r943 = R 393 :: r942 in
  let r944 = R 292 :: r943 in
  let r945 = [R 55] in
  let r946 = R 298 :: r945 in
  let r947 = [R 820] in
  let r948 = Sub (r1) :: r947 in
  let r949 = S (T T_EQUAL) :: r948 in
  let r950 = S (T T_LIDENT) :: r949 in
  let r951 = R 393 :: r950 in
  let r952 = [R 822] in
  let r953 = Sub (r1) :: r952 in
  let r954 = [R 818] in
  let r955 = Sub (r87) :: r954 in
  let r956 = S (T T_COLON) :: r955 in
  let r957 = [R 821] in
  let r958 = Sub (r1) :: r957 in
  let r959 = [R 336] in
  let r960 = Sub (r382) :: r959 in
  let r961 = S (T T_LIDENT) :: r960 in
  let r962 = R 528 :: r961 in
  let r963 = R 292 :: r962 in
  let r964 = [R 56] in
  let r965 = R 298 :: r964 in
  let r966 = [R 337] in
  let r967 = Sub (r382) :: r966 in
  let r968 = S (T T_LIDENT) :: r967 in
  let r969 = R 528 :: r968 in
  let r970 = [R 339] in
  let r971 = Sub (r1) :: r970 in
  let r972 = S (T T_EQUAL) :: r971 in
  let r973 = [R 341] in
  let r974 = Sub (r1) :: r973 in
  let r975 = S (T T_EQUAL) :: r974 in
  let r976 = Sub (r87) :: r975 in
  let r977 = S (T T_DOT) :: r976 in
  let r978 = [R 752] in
  let r979 = [R 197] in
  let r980 = Sub (r1) :: r979 in
  let r981 = [R 335] in
  let r982 = Sub (r89) :: r981 in
  let r983 = S (T T_COLON) :: r982 in
  let r984 = [R 338] in
  let r985 = Sub (r1) :: r984 in
  let r986 = S (T T_EQUAL) :: r985 in
  let r987 = [R 340] in
  let r988 = Sub (r1) :: r987 in
  let r989 = S (T T_EQUAL) :: r988 in
  let r990 = Sub (r87) :: r989 in
  let r991 = S (T T_DOT) :: r990 in
  let r992 = [R 58] in
  let r993 = R 298 :: r992 in
  let r994 = Sub (r1) :: r993 in
  let r995 = [R 53] in
  let r996 = R 298 :: r995 in
  let r997 = R 460 :: r996 in
  let r998 = Sub (r923) :: r997 in
  let r999 = [R 54] in
  let r1000 = R 298 :: r999 in
  let r1001 = R 460 :: r1000 in
  let r1002 = Sub (r923) :: r1001 in
  let r1003 = [R 85] in
  let r1004 = S (T T_RPAREN) :: r1003 in
  let r1005 = [R 48] in
  let r1006 = Sub (r923) :: r1005 in
  let r1007 = S (T T_IN) :: r1006 in
  let r1008 = Sub (r799) :: r1007 in
  let r1009 = R 292 :: r1008 in
  let r1010 = [R 272] in
  let r1011 = R 298 :: r1010 in
  let r1012 = Sub (r236) :: r1011 in
  let r1013 = R 535 :: r1012 in
  let r1014 = R 292 :: r1013 in
  let r1015 = [R 49] in
  let r1016 = Sub (r923) :: r1015 in
  let r1017 = S (T T_IN) :: r1016 in
  let r1018 = Sub (r799) :: r1017 in
  let r1019 = [R 87] in
  let r1020 = Sub (r212) :: r1019 in
  let r1021 = S (T T_RBRACKET) :: r1020 in
  let r1022 = [R 64] in
  let r1023 = Sub (r923) :: r1022 in
  let r1024 = S (T T_MINUSGREATER) :: r1023 in
  let r1025 = Sub (r502) :: r1024 in
  let r1026 = [R 46] in
  let r1027 = Sub (r1025) :: r1026 in
  let r1028 = [R 47] in
  let r1029 = Sub (r923) :: r1028 in
  let r1030 = [R 248] in
  let r1031 = [R 271] in
  let r1032 = R 298 :: r1031 in
  let r1033 = Sub (r236) :: r1032 in
  let r1034 = [R 88] in
  let r1035 = S (T T_RPAREN) :: r1034 in
  let r1036 = [R 461] in
  let r1037 = [R 57] in
  let r1038 = R 298 :: r1037 in
  let r1039 = Sub (r872) :: r1038 in
  let r1040 = [R 59] in
  let r1041 = [R 310] in
  let r1042 = [R 62] in
  let r1043 = Sub (r923) :: r1042 in
  let r1044 = S (T T_EQUAL) :: r1043 in
  let r1045 = [R 63] in
  let r1046 = [R 283] in
  let r1047 = R 282 :: r1046 in
  let r1048 = R 298 :: r1047 in
  let r1049 = Sub (r926) :: r1048 in
  let r1050 = S (T T_LIDENT) :: r1049 in
  let r1051 = R 194 :: r1050 in
  let r1052 = R 824 :: r1051 in
  let r1053 = [R 306] in
  let r1054 = [R 758] in
  let r1055 = [R 762] in
  let r1056 = [R 755] in
  let r1057 = R 303 :: r1056 in
  let r1058 = [R 640] in
  let r1059 = S (T T_RBRACKET) :: r1058 in
  let r1060 = Sub (r1) :: r1059 in
  let r1061 = [R 639] in
  let r1062 = S (T T_RBRACE) :: r1061 in
  let r1063 = Sub (r1) :: r1062 in
  let r1064 = [R 642] in
  let r1065 = S (T T_RPAREN) :: r1064 in
  let r1066 = Sub (r458) :: r1065 in
  let r1067 = S (T T_LPAREN) :: r1066 in
  let r1068 = [R 646] in
  let r1069 = S (T T_RBRACKET) :: r1068 in
  let r1070 = Sub (r458) :: r1069 in
  let r1071 = [R 644] in
  let r1072 = S (T T_RBRACE) :: r1071 in
  let r1073 = Sub (r458) :: r1072 in
  let r1074 = [R 180] in
  let r1075 = [R 645] in
  let r1076 = S (T T_RBRACKET) :: r1075 in
  let r1077 = Sub (r458) :: r1076 in
  let r1078 = [R 184] in
  let r1079 = [R 643] in
  let r1080 = S (T T_RBRACE) :: r1079 in
  let r1081 = Sub (r458) :: r1080 in
  let r1082 = [R 182] in
  let r1083 = [R 177] in
  let r1084 = [R 179] in
  let r1085 = [R 178] in
  let r1086 = [R 181] in
  let r1087 = [R 185] in
  let r1088 = [R 183] in
  let r1089 = [R 176] in
  let r1090 = [R 279] in
  let r1091 = Sub (r1) :: r1090 in
  let r1092 = [R 281] in
  let r1093 = [R 663] in
  let r1094 = [R 675] in
  let r1095 = [R 674] in
  let r1096 = [R 678] in
  let r1097 = [R 677] in
  let r1098 = S (T T_LIDENT) :: r463 in
  let r1099 = [R 664] in
  let r1100 = S (T T_GREATERRBRACE) :: r1099 in
  let r1101 = [R 671] in
  let r1102 = S (T T_RBRACE) :: r1101 in
  let r1103 = [R 538] in
  let r1104 = Sub (r468) :: r1103 in
  let r1105 = [R 128] in
  let r1106 = S (T T_DONE) :: r1105 in
  let r1107 = Sub (r1) :: r1106 in
  let r1108 = S (T T_DO) :: r1107 in
  let r1109 = Sub (r1) :: r1108 in
  let r1110 = Sub (r522) :: r1109 in
  let r1111 = [R 201] in
  let r1112 = Sub (r505) :: r1111 in
  let r1113 = S (T T_RPAREN) :: r1112 in
  let r1114 = [R 199] in
  let r1115 = Sub (r1) :: r1114 in
  let r1116 = S (T T_MINUSGREATER) :: r1115 in
  let r1117 = [R 200] in
  let r1118 = [R 567] in
  let r1119 = [R 140] in
  let r1120 = [R 647] in
  let r1121 = [R 660] in
  let r1122 = [R 131] in
  let r1123 = Sub (r1) :: r1122 in
  let r1124 = S (T T_IN) :: r1123 in
  let r1125 = Sub (r631) :: r1124 in
  let r1126 = Sub (r63) :: r1125 in
  let r1127 = R 292 :: r1126 in
  let r1128 = [R 132] in
  let r1129 = Sub (r1) :: r1128 in
  let r1130 = S (T T_IN) :: r1129 in
  let r1131 = R 292 :: r1130 in
  let r1132 = R 209 :: r1131 in
  let r1133 = Sub (r130) :: r1132 in
  let r1134 = R 292 :: r1133 in
  let r1135 = [R 266] in
  let r1136 = Sub (r1) :: r1135 in
  let r1137 = S (T T_EQUAL) :: r1136 in
  let r1138 = Sub (r87) :: r1137 in
  let r1139 = S (T T_DOT) :: r1138 in
  let r1140 = [R 265] in
  let r1141 = Sub (r1) :: r1140 in
  let r1142 = S (T T_EQUAL) :: r1141 in
  let r1143 = Sub (r87) :: r1142 in
  let r1144 = [R 264] in
  let r1145 = Sub (r1) :: r1144 in
  let r1146 = [R 672] in
  let r1147 = [R 650] in
  let r1148 = S (T T_RPAREN) :: r1147 in
  let r1149 = S (N N_module_expr) :: r1148 in
  let r1150 = R 292 :: r1149 in
  let r1151 = [R 651] in
  let r1152 = S (T T_RPAREN) :: r1151 in
  let r1153 = [R 637] in
  let r1154 = [R 481] in
  let r1155 = S (T T_RPAREN) :: r1154 in
  let r1156 = [R 479] in
  let r1157 = S (T T_RPAREN) :: r1156 in
  let r1158 = [R 480] in
  let r1159 = S (T T_RPAREN) :: r1158 in
  let r1160 = [R 305] in
  let r1161 = R 303 :: r1160 in
  let r1162 = [R 330] in
  let r1163 = [R 29] in
  let r1164 = [R 28] in
  let r1165 = Sub (r126) :: r1164 in
  let r1166 = [R 33] in
  let r1167 = [R 580] in
  let r1168 = [R 22] in
  let r1169 = [R 581] in
  let r1170 = [R 416] in
  let r1171 = S (T T_RBRACE) :: r1170 in
  let r1172 = [R 191] in
  let r1173 = R 292 :: r1172 in
  let r1174 = [R 192] in
  let r1175 = R 292 :: r1174 in
  let r1176 = [R 68] in
  let r1177 = S (T T_RPAREN) :: r1176 in
  let r1178 = [R 124] in
  let r1179 = [R 126] in
  let r1180 = [R 125] in
  let r1181 = [R 223] in
  let r1182 = [R 226] in
  let r1183 = [R 347] in
  let r1184 = [R 350] in
  let r1185 = S (T T_RPAREN) :: r1184 in
  let r1186 = S (T T_COLONCOLON) :: r1185 in
  let r1187 = S (T T_LPAREN) :: r1186 in
  let r1188 = [R 482] in
  let r1189 = [R 483] in
  let r1190 = [R 484] in
  let r1191 = [R 485] in
  let r1192 = [R 486] in
  let r1193 = [R 487] in
  let r1194 = [R 488] in
  let r1195 = [R 489] in
  let r1196 = [R 490] in
  let r1197 = [R 491] in
  let r1198 = [R 492] in
  let r1199 = [R 777] in
  let r1200 = [R 786] in
  let r1201 = [R 312] in
  let r1202 = [R 784] in
  let r1203 = S (T T_SEMISEMI) :: r1202 in
  let r1204 = [R 785] in
  let r1205 = [R 314] in
  let r1206 = [R 317] in
  let r1207 = [R 316] in
  let r1208 = [R 315] in
  let r1209 = R 313 :: r1208 in
  let r1210 = [R 813] in
  let r1211 = S (T T_EOF) :: r1210 in
  let r1212 = R 313 :: r1211 in
  let r1213 = [R 812] in
  function
  | 0 | 1786 | 1790 | 1808 | 1812 | 1816 | 1820 | 1824 | 1828 | 1832 | 1836 | 1840 | 1844 | 1848 | 1868 -> Nothing
  | 1785 -> One ([R 0])
  | 1789 -> One ([R 1])
  | 1795 -> One ([R 2])
  | 1809 -> One ([R 3])
  | 1813 -> One ([R 4])
  | 1819 -> One ([R 5])
  | 1821 -> One ([R 6])
  | 1825 -> One ([R 7])
  | 1829 -> One ([R 8])
  | 1833 -> One ([R 9])
  | 1837 -> One ([R 10])
  | 1843 -> One ([R 11])
  | 1847 -> One ([R 12])
  | 1858 -> One ([R 13])
  | 1878 -> One ([R 14])
  | 214 -> One ([R 15])
  | 213 -> One ([R 16])
  | 1803 -> One ([R 20])
  | 1805 -> One ([R 21])
  | 284 -> One ([R 26])
  | 294 -> One ([R 27])
  | 290 -> One ([R 41])
  | 1295 -> One ([R 45])
  | 1304 -> One ([R 50])
  | 1299 -> One ([R 51])
  | 1340 -> One ([R 60])
  | 1307 -> One ([R 65])
  | 1091 -> One ([R 77])
  | 1071 -> One ([R 78])
  | 1073 -> One ([R 82])
  | 1302 -> One ([R 86])
  | 352 -> One ([R 97])
  | 73 -> One ([R 98])
  | 350 -> One ([R 99])
  | 72 -> One ([R 103])
  | 200 | 837 -> One ([R 104])
  | 869 -> One ([R 107])
  | 903 -> One ([R 115])
  | 907 -> One ([R 116])
  | 324 -> One ([R 118])
  | 1524 -> One ([R 119])
  | 633 -> One ([R 130])
  | 1473 -> One ([R 146])
  | 656 -> One ([R 147])
  | 694 -> One ([R 148])
  | 659 -> One ([R 149])
  | 692 -> One ([R 186])
  | 1 -> One (R 187 :: r7)
  | 61 -> One (R 187 :: r24)
  | 66 -> One (R 187 :: r29)
  | 69 -> One (R 187 :: r40)
  | 76 -> One (R 187 :: r48)
  | 96 -> One (R 187 :: r67)
  | 107 -> One (R 187 :: r95)
  | 215 -> One (R 187 :: r199)
  | 216 -> One (R 187 :: r203)
  | 222 -> One (R 187 :: r215)
  | 237 -> One (R 187 :: r225)
  | 240 -> One (R 187 :: r230)
  | 248 -> One (R 187 :: r241)
  | 344 -> One (R 187 :: r319)
  | 367 -> One (R 187 :: r332)
  | 464 -> One (R 187 :: r406)
  | 567 -> One (R 187 :: r485)
  | 570 -> One (R 187 :: r488)
  | 573 -> One (R 187 :: r493)
  | 576 -> One (R 187 :: r496)
  | 582 -> One (R 187 :: r509)
  | 590 -> One (R 187 :: r520)
  | 595 -> One (R 187 :: r532)
  | 610 -> One (R 187 :: r543)
  | 624 -> One (R 187 :: r549)
  | 773 -> One (R 187 :: r636)
  | 812 -> One (R 187 :: r667)
  | 817 -> One (R 187 :: r677)
  | 959 -> One (R 187 :: r766)
  | 960 -> One (R 187 :: r770)
  | 969 -> One (R 187 :: r778)
  | 1006 -> One (R 187 :: r806)
  | 1015 -> One (R 187 :: r820)
  | 1016 -> One (R 187 :: r829)
  | 1179 -> One (R 187 :: r934)
  | 1597 -> One (R 187 :: r1127)
  | 1604 -> One (R 187 :: r1134)
  | 1642 -> One (R 187 :: r1150)
  | 478 -> One ([R 208])
  | 153 -> One ([R 221])
  | 131 -> One (R 224 :: r101)
  | 135 -> One (R 224 :: r103)
  | 212 -> One ([R 228])
  | 556 -> One ([R 231])
  | 564 -> One ([R 232])
  | 672 -> One ([R 234])
  | 663 -> One ([R 237])
  | 859 -> One ([R 242])
  | 860 -> One ([R 243])
  | 1298 -> One ([R 247])
  | 765 -> One ([R 261])
  | 1634 -> One ([R 263])
  | 1378 -> One ([R 270])
  | 1305 -> One ([R 273])
  | 447 -> One ([R 274])
  | 1614 -> One ([R 276])
  | 105 -> One (R 292 :: r75)
  | 171 -> One (R 292 :: r122)
  | 220 -> One (R 292 :: r208)
  | 233 -> One (R 292 :: r220)
  | 467 -> One (R 292 :: r410)
  | 476 -> One (R 292 :: r422)
  | 742 -> One (R 292 :: r613)
  | 796 -> One (R 292 :: r656)
  | 988 -> One (R 292 :: r797)
  | 1027 -> One (R 292 :: r835)
  | 1033 -> One (R 292 :: r843)
  | 1044 -> One (R 292 :: r849)
  | 1055 -> One (R 292 :: r852)
  | 1059 -> One (R 292 :: r861)
  | 1080 -> One (R 292 :: r875)
  | 1096 -> One (R 292 :: r885)
  | 1131 -> One (R 292 :: r902)
  | 1153 -> One (R 292 :: r912)
  | 1163 -> One (R 292 :: r921)
  | 1186 -> One (R 292 :: r938)
  | 1190 -> One (R 292 :: r951)
  | 1218 -> One (R 292 :: r969)
  | 1264 -> One (R 292 :: r994)
  | 1268 -> One (R 292 :: r998)
  | 1269 -> One (R 292 :: r1002)
  | 1280 -> One (R 292 :: r1018)
  | 1288 -> One (R 292 :: r1027)
  | 1332 -> One (R 292 :: r1039)
  | 1352 -> One (R 292 :: r1052)
  | 1685 -> One (R 292 :: r1162)
  | 1152 -> One (R 294 :: r905)
  | 1381 -> One (R 294 :: r1055)
  | 1162 -> One (R 296 :: r913)
  | 781 -> One (R 298 :: r644)
  | 1089 -> One (R 298 :: r876)
  | 1150 -> One (R 298 :: r904)
  | 1338 -> One (R 298 :: r1040)
  | 1379 -> One (R 298 :: r1054)
  | 1386 -> One (R 298 :: r1057)
  | 1677 -> One (R 298 :: r1161)
  | 1863 -> One (R 298 :: r1203)
  | 1874 -> One (R 298 :: r1209)
  | 1879 -> One (R 298 :: r1212)
  | 958 -> One (R 300 :: r762)
  | 1142 -> One (R 300 :: r903)
  | 211 -> One (R 303 :: r195)
  | 1362 -> One (R 303 :: r1053)
  | 1092 -> One (R 307 :: r877)
  | 1341 -> One (R 309 :: r1041)
  | 1861 -> One (R 311 :: r1201)
  | 1869 -> One (R 313 :: r1205)
  | 1870 -> One (R 313 :: r1206)
  | 1871 -> One (R 313 :: r1207)
  | 421 -> One ([R 319])
  | 425 -> One ([R 321])
  | 683 -> One ([R 323])
  | 1375 -> One ([R 324])
  | 1561 -> One ([R 327])
  | 1688 -> One ([R 328])
  | 1691 -> One ([R 329])
  | 1690 -> One ([R 331])
  | 1689 -> One ([R 333])
  | 1687 -> One ([R 334])
  | 1804 -> One ([R 346])
  | 1794 -> One ([R 348])
  | 1802 -> One ([R 349])
  | 1801 -> One ([R 351])
  | 535 -> One ([R 370])
  | 545 -> One ([R 371])
  | 546 -> One ([R 372])
  | 544 -> One ([R 373])
  | 547 -> One ([R 375])
  | 170 -> One ([R 376])
  | 100 | 979 -> One ([R 377])
  | 505 -> One ([R 384])
  | 482 -> One ([R 385])
  | 512 -> One ([R 389])
  | 845 | 1204 -> One ([R 394])
  | 1037 -> One ([R 396])
  | 1035 -> One ([R 397])
  | 1038 -> One ([R 398])
  | 1036 -> One ([R 399])
  | 385 -> One ([R 402])
  | 830 -> One ([R 404])
  | 915 -> One ([R 405])
  | 1713 -> One ([R 406])
  | 931 -> One ([R 407])
  | 1714 -> One ([R 408])
  | 930 -> One ([R 409])
  | 922 -> One ([R 410])
  | 90 | 244 -> One ([R 423])
  | 114 | 619 -> One ([R 424])
  | 142 -> One ([R 425])
  | 130 -> One ([R 427])
  | 134 -> One ([R 429])
  | 138 -> One ([R 431])
  | 121 -> One ([R 432])
  | 141 | 1493 -> One ([R 433])
  | 120 -> One ([R 434])
  | 119 -> One ([R 435])
  | 118 -> One ([R 436])
  | 117 -> One ([R 437])
  | 116 -> One ([R 438])
  | 93 | 111 | 609 -> One ([R 439])
  | 92 | 608 -> One ([R 440])
  | 91 -> One ([R 441])
  | 113 | 391 | 618 -> One ([R 442])
  | 112 | 617 -> One ([R 443])
  | 88 -> One ([R 444])
  | 94 -> One ([R 445])
  | 123 -> One ([R 446])
  | 115 -> One ([R 447])
  | 122 -> One ([R 448])
  | 95 -> One ([R 449])
  | 140 -> One ([R 450])
  | 143 -> One ([R 451])
  | 139 -> One ([R 453])
  | 311 -> One ([R 454])
  | 310 -> One (R 455 :: r302)
  | 262 -> One (R 456 :: r263)
  | 263 -> One ([R 457])
  | 422 -> One (R 458 :: r353)
  | 423 -> One ([R 459])
  | 1512 -> One ([R 473])
  | 159 -> One ([R 474])
  | 377 -> One ([R 494])
  | 371 -> One ([R 495])
  | 372 -> One ([R 497])
  | 370 | 620 -> One ([R 504])
  | 760 -> One ([R 510])
  | 761 -> One ([R 511])
  | 762 -> One ([R 513])
  | 453 -> One ([R 515])
  | 1178 -> One ([R 519])
  | 937 | 1245 -> One ([R 529])
  | 1048 -> One ([R 531])
  | 1046 -> One ([R 532])
  | 1049 -> One ([R 533])
  | 1047 -> One ([R 534])
  | 1314 -> One (R 535 :: r1033)
  | 251 -> One ([R 536])
  | 913 -> One ([R 539])
  | 914 -> One ([R 540])
  | 909 -> One ([R 541])
  | 1730 -> One ([R 543])
  | 1729 -> One ([R 544])
  | 1731 -> One ([R 545])
  | 1726 -> One ([R 546])
  | 1727 -> One ([R 547])
  | 943 -> One ([R 549])
  | 941 -> One ([R 550])
  | 660 -> One (R 551 :: r574)
  | 679 -> One ([R 552])
  | 670 -> One (R 555 :: r580)
  | 676 -> One ([R 556])
  | 527 -> One ([R 557])
  | 479 -> One ([R 558])
  | 1301 -> One ([R 559])
  | 1300 -> One ([R 560])
  | 339 -> One ([R 562])
  | 303 -> One ([R 586])
  | 1412 -> One ([R 589])
  | 1413 -> One ([R 590])
  | 1584 -> One ([R 592])
  | 1585 -> One ([R 593])
  | 416 -> One ([R 595])
  | 417 -> One ([R 596])
  | 1515 -> One ([R 598])
  | 1516 -> One ([R 599])
  | 697 -> One ([R 601])
  | 701 -> One ([R 602])
  | 1173 -> One ([R 607])
  | 1141 -> One ([R 608])
  | 1144 -> One ([R 609])
  | 1143 -> One ([R 614])
  | 1148 -> One ([R 617])
  | 1147 -> One ([R 619])
  | 1146 -> One ([R 620])
  | 1145 -> One ([R 621])
  | 1174 -> One ([R 624])
  | 86 -> One ([R 627])
  | 83 -> One ([R 629])
  | 666 -> One ([R 653])
  | 601 -> One ([R 654])
  | 669 -> One ([R 655])
  | 668 | 693 -> One ([R 656])
  | 603 | 658 -> One ([R 657])
  | 1420 | 1470 -> One ([R 662])
  | 667 -> One ([R 667])
  | 353 -> One ([R 680])
  | 357 -> One ([R 683])
  | 358 -> One ([R 687])
  | 389 -> One ([R 689])
  | 362 -> One ([R 690])
  | 418 -> One ([R 692])
  | 380 -> One ([R 697])
  | 28 -> One ([R 698])
  | 8 -> One ([R 699])
  | 52 -> One ([R 701])
  | 51 -> One ([R 702])
  | 50 -> One ([R 703])
  | 49 -> One ([R 704])
  | 48 -> One ([R 705])
  | 47 -> One ([R 706])
  | 46 -> One ([R 707])
  | 45 -> One ([R 708])
  | 44 -> One ([R 709])
  | 43 -> One ([R 710])
  | 42 -> One ([R 711])
  | 41 -> One ([R 712])
  | 40 -> One ([R 713])
  | 39 -> One ([R 714])
  | 38 -> One ([R 715])
  | 37 -> One ([R 716])
  | 36 -> One ([R 717])
  | 35 -> One ([R 718])
  | 34 -> One ([R 719])
  | 33 -> One ([R 720])
  | 32 -> One ([R 721])
  | 31 -> One ([R 722])
  | 30 -> One ([R 723])
  | 29 -> One ([R 724])
  | 27 -> One ([R 725])
  | 26 -> One ([R 726])
  | 25 -> One ([R 727])
  | 24 -> One ([R 728])
  | 23 -> One ([R 729])
  | 22 -> One ([R 730])
  | 21 -> One ([R 731])
  | 20 -> One ([R 732])
  | 19 -> One ([R 733])
  | 18 -> One ([R 734])
  | 17 -> One ([R 735])
  | 16 -> One ([R 736])
  | 15 -> One ([R 737])
  | 14 -> One ([R 738])
  | 13 -> One ([R 739])
  | 12 -> One ([R 740])
  | 11 -> One ([R 741])
  | 10 -> One ([R 742])
  | 9 -> One ([R 743])
  | 7 -> One ([R 744])
  | 6 -> One ([R 745])
  | 5 -> One ([R 746])
  | 4 -> One ([R 747])
  | 3 -> One ([R 748])
  | 1370 -> One ([R 749])
  | 1392 -> One ([R 754])
  | 1374 | 1391 -> One ([R 756])
  | 1377 | 1393 -> One ([R 757])
  | 1383 -> One ([R 759])
  | 1371 -> One ([R 760])
  | 1361 -> One ([R 761])
  | 1369 -> One ([R 765])
  | 1373 -> One ([R 768])
  | 1372 -> One ([R 769])
  | 1384 -> One ([R 771])
  | 236 -> One ([R 773])
  | 235 -> One ([R 774])
  | 1852 -> One ([R 778])
  | 1853 -> One ([R 779])
  | 1855 -> One ([R 780])
  | 1856 -> One ([R 781])
  | 1854 -> One ([R 782])
  | 1851 -> One ([R 783])
  | 1857 -> One ([R 787])
  | 287 -> One ([R 789])
  | 485 -> One (R 797 :: r439)
  | 499 -> One ([R 798])
  | 177 -> One ([R 803])
  | 180 -> One ([R 804])
  | 184 -> One ([R 805])
  | 178 -> One ([R 806])
  | 185 -> One ([R 807])
  | 181 -> One ([R 808])
  | 186 -> One ([R 809])
  | 183 -> One ([R 810])
  | 176 -> One ([R 811])
  | 354 -> One ([R 816])
  | 1019 -> One ([R 825])
  | 1202 -> One ([R 826])
  | 1205 -> One ([R 827])
  | 1203 -> One ([R 828])
  | 1243 -> One ([R 829])
  | 1246 -> One ([R 830])
  | 1244 -> One ([R 831])
  | 488 -> One ([R 838])
  | 489 -> One ([R 839])
  | 1508 -> One (S (T T_WITH) :: r1104)
  | 166 -> One (S (T T_TYPE) :: r119)
  | 455 -> One (S (T T_TYPE) :: r388)
  | 862 -> One (S (T T_STAR) :: r718)
  | 1859 -> One (S (T T_SEMISEMI) :: r1200)
  | 1866 -> One (S (T T_SEMISEMI) :: r1204)
  | 1791 -> One (S (T T_RPAREN) :: r54)
  | 365 -> One (S (T T_RPAREN) :: r329)
  | 409 -> One (S (T T_RPAREN) :: r352)
  | 469 -> One (S (T T_RPAREN) :: r411)
  | 537 -> One (S (T T_RPAREN) :: r454)
  | 1494 -> One (S (T T_RPAREN) :: r1093)
  | 1652 -> One (S (T T_RPAREN) :: r1153)
  | 1698 -> One (S (T T_RPAREN) :: r1165)
  | 1705 -> One (S (T T_RPAREN) :: r1168)
  | 1792 -> One (S (T T_RPAREN) :: r1183)
  | 841 | 898 -> One (S (T T_RBRACKET) :: r243)
  | 265 -> One (S (T T_RBRACKET) :: r264)
  | 1500 -> One (S (T T_RBRACKET) :: r1096)
  | 1502 -> One (S (T T_RBRACKET) :: r1097)
  | 317 -> One (S (T T_QUOTE) :: r305)
  | 1057 -> One (S (T T_OPEN) :: r857)
  | 1272 -> One (S (T T_OPEN) :: r1009)
  | 160 -> One (S (T T_MODULE) :: r115)
  | 474 -> One (S (T T_MINUSGREATER) :: r418)
  | 878 -> One (S (T T_MINUSGREATER) :: r724)
  | 882 -> One (S (T T_MINUSGREATER) :: r726)
  | 1118 -> One (S (T T_MINUSGREATER) :: r891)
  | 124 -> One (S (T T_LPAREN) :: r98)
  | 156 -> One (S (T T_LIDENT) :: r110)
  | 430 -> One (S (T T_LIDENT) :: r355)
  | 438 -> One (S (T T_LIDENT) :: r361)
  | 634 -> One (S (T T_LIDENT) :: r556)
  | 635 -> One (S (T T_LIDENT) :: r562)
  | 646 -> One (S (T T_LIDENT) :: r565)
  | 650 -> One (S (T T_LIDENT) :: r567)
  | 846 -> One (S (T T_LIDENT) :: r714)
  | 1206 -> One (S (T T_LIDENT) :: r956)
  | 1247 -> One (S (T T_LIDENT) :: r983)
  | 1324 -> One (S (T T_LIDENT) :: r1036)
  | 81 -> One (S (T T_INT) :: r52)
  | 84 -> One (S (T T_INT) :: r53)
  | 680 -> One (S (T T_IN) :: r583)
  | 684 -> One (S (T T_IN) :: r585)
  | 1292 -> One (S (T T_IN) :: r1029)
  | 551 -> One (S (T T_GREATERRBRACE) :: r461)
  | 1587 -> One (S (T T_GREATERRBRACE) :: r1121)
  | 206 -> One (S (T T_GREATER) :: r186)
  | 1693 -> One (S (T T_GREATER) :: r1163)
  | 517 -> One (S (T T_EQUAL) :: r450)
  | 749 -> One (S (T T_EQUAL) :: r618)
  | 1196 -> One (S (T T_EQUAL) :: r953)
  | 1214 -> One (S (T T_EQUAL) :: r958)
  | 1235 -> One (S (T T_EQUAL) :: r980)
  | 1484 -> One (S (T T_EQUAL) :: r1091)
  | 1631 -> One (S (T T_EQUAL) :: r1145)
  | 1783 -> One (S (T T_EOF) :: r1181)
  | 1787 -> One (S (T T_EOF) :: r1182)
  | 1806 -> One (S (T T_EOF) :: r1188)
  | 1810 -> One (S (T T_EOF) :: r1189)
  | 1814 -> One (S (T T_EOF) :: r1190)
  | 1817 -> One (S (T T_EOF) :: r1191)
  | 1822 -> One (S (T T_EOF) :: r1192)
  | 1826 -> One (S (T T_EOF) :: r1193)
  | 1830 -> One (S (T T_EOF) :: r1194)
  | 1834 -> One (S (T T_EOF) :: r1195)
  | 1838 -> One (S (T T_EOF) :: r1196)
  | 1841 -> One (S (T T_EOF) :: r1197)
  | 1845 -> One (S (T T_EOF) :: r1198)
  | 1883 -> One (S (T T_EOF) :: r1213)
  | 1574 -> One (S (T T_END) :: r1120)
  | 126 -> One (S (T T_DOTDOT) :: r99)
  | 201 -> One (S (T T_DOTDOT) :: r179)
  | 916 -> One (S (T T_DOTDOT) :: r753)
  | 917 -> One (S (T T_DOTDOT) :: r754)
  | 226 | 1406 | 1453 -> One (S (T T_DOT) :: r217)
  | 822 -> One (S (T T_DOT) :: r679)
  | 849 -> One (S (T T_DOT) :: r716)
  | 876 -> One (S (T T_DOT) :: r722)
  | 1626 -> One (S (T T_DOT) :: r1143)
  | 1796 -> One (S (T T_DOT) :: r1187)
  | 202 | 838 -> One (S (T T_COLONCOLON) :: r181)
  | 207 -> One (S (T T_COLON) :: r191)
  | 471 -> One (S (T T_COLON) :: r414)
  | 1112 -> One (S (T T_COLON) :: r889)
  | 245 -> One (S (T T_BARRBRACKET) :: r233)
  | 253 -> One (S (T T_BARRBRACKET) :: r242)
  | 427 -> One (S (T T_BARRBRACKET) :: r354)
  | 1496 -> One (S (T T_BARRBRACKET) :: r1094)
  | 1498 -> One (S (T T_BARRBRACKET) :: r1095)
  | 1639 -> One (S (T T_BARRBRACKET) :: r1146)
  | 328 -> One (S (T T_BAR) :: r308)
  | 79 -> One (S (N N_pattern) :: r50)
  | 382 | 585 | 1543 -> One (S (N N_pattern) :: r56)
  | 343 -> One (S (N N_pattern) :: r313)
  | 373 -> One (S (N N_pattern) :: r333)
  | 375 -> One (S (N N_pattern) :: r334)
  | 396 -> One (S (N N_pattern) :: r345)
  | 401 -> One (S (N N_pattern) :: r348)
  | 752 -> One (S (N N_pattern) :: r619)
  | 754 -> One (S (N N_pattern) :: r620)
  | 756 -> One (S (N N_pattern) :: r621)
  | 763 -> One (S (N N_pattern) :: r623)
  | 769 -> One (S (N N_pattern) :: r627)
  | 103 -> One (S (N N_module_type) :: r69)
  | 473 -> One (S (N N_module_type) :: r416)
  | 513 -> One (S (N N_module_type) :: r447)
  | 515 -> One (S (N N_module_type) :: r448)
  | 541 -> One (S (N N_module_type) :: r456)
  | 778 -> One (S (N N_module_type) :: r643)
  | 790 -> One (S (N N_module_type) :: r651)
  | 1647 -> One (S (N N_module_type) :: r1152)
  | 1662 -> One (S (N N_module_type) :: r1155)
  | 1665 -> One (S (N N_module_type) :: r1157)
  | 1668 -> One (S (N N_module_type) :: r1159)
  | 219 -> One (S (N N_module_expr) :: r205)
  | 446 -> One (S (N N_let_pattern) :: r378)
  | 247 -> One (S (N N_expr) :: r234)
  | 553 -> One (S (N N_expr) :: r464)
  | 566 -> One (S (N N_expr) :: r482)
  | 632 -> One (S (N N_expr) :: r555)
  | 657 -> One (S (N N_expr) :: r572)
  | 688 -> One (S (N N_expr) :: r586)
  | 690 -> One (S (N N_expr) :: r587)
  | 695 -> One (S (N N_expr) :: r588)
  | 702 -> One (S (N N_expr) :: r591)
  | 704 -> One (S (N N_expr) :: r592)
  | 706 -> One (S (N N_expr) :: r593)
  | 708 -> One (S (N N_expr) :: r594)
  | 710 -> One (S (N N_expr) :: r595)
  | 712 -> One (S (N N_expr) :: r596)
  | 714 -> One (S (N N_expr) :: r597)
  | 716 -> One (S (N N_expr) :: r598)
  | 718 -> One (S (N N_expr) :: r599)
  | 720 -> One (S (N N_expr) :: r600)
  | 722 -> One (S (N N_expr) :: r601)
  | 724 -> One (S (N N_expr) :: r602)
  | 726 -> One (S (N N_expr) :: r603)
  | 728 -> One (S (N N_expr) :: r604)
  | 730 -> One (S (N N_expr) :: r605)
  | 732 -> One (S (N N_expr) :: r606)
  | 734 -> One (S (N N_expr) :: r607)
  | 736 -> One (S (N N_expr) :: r608)
  | 738 -> One (S (N N_expr) :: r609)
  | 740 -> One (S (N N_expr) :: r610)
  | 1425 -> One (S (N N_expr) :: r1074)
  | 1430 -> One (S (N N_expr) :: r1078)
  | 1435 -> One (S (N N_expr) :: r1082)
  | 1441 -> One (S (N N_expr) :: r1083)
  | 1446 -> One (S (N N_expr) :: r1084)
  | 1451 -> One (S (N N_expr) :: r1085)
  | 1458 -> One (S (N N_expr) :: r1086)
  | 1463 -> One (S (N N_expr) :: r1087)
  | 1468 -> One (S (N N_expr) :: r1088)
  | 1471 -> One (S (N N_expr) :: r1089)
  | 1571 -> One (S (N N_expr) :: r1119)
  | 441 -> One (Sub (r1) :: r365)
  | 581 -> One (Sub (r1) :: r500)
  | 771 -> One (Sub (r1) :: r628)
  | 1535 -> One (Sub (r1) :: r1110)
  | 1768 -> One (Sub (r1) :: r1179)
  | 1770 -> One (Sub (r1) :: r1180)
  | 2 -> One (Sub (r11) :: r12)
  | 55 -> One (Sub (r11) :: r13)
  | 59 -> One (Sub (r11) :: r18)
  | 209 -> One (Sub (r11) :: r194)
  | 698 -> One (Sub (r11) :: r590)
  | 767 -> One (Sub (r11) :: r626)
  | 808 -> One (Sub (r11) :: r660)
  | 810 -> One (Sub (r11) :: r663)
  | 1273 -> One (Sub (r11) :: r1014)
  | 579 -> One (Sub (r33) :: r497)
  | 1565 -> One (Sub (r33) :: r1118)
  | 1766 -> One (Sub (r35) :: r1178)
  | 75 -> One (Sub (r42) :: r43)
  | 565 -> One (Sub (r42) :: r480)
  | 600 -> One (Sub (r42) :: r533)
  | 628 -> One (Sub (r42) :: r550)
  | 648 -> One (Sub (r42) :: r566)
  | 664 -> One (Sub (r42) :: r575)
  | 1296 -> One (Sub (r42) :: r1030)
  | 786 -> One (Sub (r63) :: r648)
  | 983 -> One (Sub (r63) :: r791)
  | 890 -> One (Sub (r72) :: r727)
  | 403 -> One (Sub (r77) :: r349)
  | 758 -> One (Sub (r77) :: r622)
  | 288 -> One (Sub (r79) :: r291)
  | 300 -> One (Sub (r79) :: r296)
  | 875 -> One (Sub (r79) :: r720)
  | 1547 -> One (Sub (r79) :: r1116)
  | 295 -> One (Sub (r81) :: r295)
  | 1120 -> One (Sub (r81) :: r894)
  | 286 -> One (Sub (r83) :: r290)
  | 314 -> One (Sub (r85) :: r303)
  | 492 -> One (Sub (r85) :: r441)
  | 261 -> One (Sub (r87) :: r256)
  | 398 -> One (Sub (r87) :: r347)
  | 433 -> One (Sub (r87) :: r360)
  | 448 -> One (Sub (r87) :: r379)
  | 495 -> One (Sub (r87) :: r444)
  | 621 -> One (Sub (r87) :: r546)
  | 637 -> One (Sub (r87) :: r563)
  | 641 -> One (Sub (r87) :: r564)
  | 745 -> One (Sub (r87) :: r616)
  | 1029 -> One (Sub (r87) :: r837)
  | 1067 -> One (Sub (r87) :: r868)
  | 1703 -> One (Sub (r87) :: r1167)
  | 1707 -> One (Sub (r87) :: r1169)
  | 1756 -> One (Sub (r87) :: r1177)
  | 1222 -> One (Sub (r89) :: r972)
  | 1253 -> One (Sub (r89) :: r986)
  | 189 -> One (Sub (r105) :: r174)
  | 823 -> One (Sub (r105) :: r680)
  | 1849 -> One (Sub (r105) :: r1199)
  | 348 -> One (Sub (r126) :: r321)
  | 195 -> One (Sub (r169) :: r175)
  | 182 -> One (Sub (r171) :: r173)
  | 1021 -> One (Sub (r171) :: r831)
  | 199 -> One (Sub (r177) :: r178)
  | 897 -> One (Sub (r177) :: r746)
  | 946 -> One (Sub (r177) :: r761)
  | 256 -> One (Sub (r253) :: r255)
  | 307 -> One (Sub (r258) :: r297)
  | 267 -> One (Sub (r260) :: r266)
  | 281 -> One (Sub (r260) :: r289)
  | 268 -> One (Sub (r272) :: r274)
  | 269 -> One (Sub (r276) :: r277)
  | 292 -> One (Sub (r276) :: r292)
  | 1700 -> One (Sub (r276) :: r1166)
  | 271 -> One (Sub (r285) :: r287)
  | 521 -> One (Sub (r285) :: r452)
  | 980 -> One (Sub (r285) :: r786)
  | 336 -> One (Sub (r310) :: r312)
  | 459 -> One (Sub (r316) :: r389)
  | 359 -> One (Sub (r324) :: r325)
  | 383 -> One (Sub (r338) :: r341)
  | 586 -> One (Sub (r338) :: r512)
  | 1223 -> One (Sub (r338) :: r977)
  | 1254 -> One (Sub (r338) :: r991)
  | 1544 -> One (Sub (r338) :: r1113)
  | 1620 -> One (Sub (r338) :: r1139)
  | 431 -> One (Sub (r357) :: r359)
  | 439 -> One (Sub (r357) :: r364)
  | 1490 -> One (Sub (r367) :: r1092)
  | 442 -> One (Sub (r369) :: r372)
  | 444 -> One (Sub (r374) :: r375)
  | 1234 -> One (Sub (r384) :: r978)
  | 525 -> One (Sub (r432) :: r453)
  | 484 -> One (Sub (r434) :: r435)
  | 554 -> One (Sub (r470) :: r472)
  | 1507 -> One (Sub (r470) :: r1102)
  | 557 -> One (Sub (r476) :: r477)
  | 673 -> One (Sub (r476) :: r581)
  | 1551 -> One (Sub (r505) :: r1117)
  | 802 -> One (Sub (r631) :: r657)
  | 1721 -> One (Sub (r681) :: r1173)
  | 1733 -> One (Sub (r681) :: r1175)
  | 843 -> One (Sub (r697) :: r698)
  | 844 -> One (Sub (r706) :: r708)
  | 899 -> One (Sub (r706) :: r748)
  | 918 -> One (Sub (r706) :: r756)
  | 926 -> One (Sub (r706) :: r758)
  | 1709 -> One (Sub (r706) :: r1171)
  | 1004 -> One (Sub (r773) :: r802)
  | 997 -> One (Sub (r799) :: r801)
  | 1320 -> One (Sub (r811) :: r1035)
  | 1344 -> One (Sub (r811) :: r1044)
  | 1284 -> One (Sub (r863) :: r1021)
  | 1271 -> One (Sub (r923) :: r1004)
  | 1348 -> One (Sub (r926) :: r1045)
  | 1189 -> One (Sub (r944) :: r946)
  | 1217 -> One (Sub (r963) :: r965)
  | 1504 -> One (Sub (r1098) :: r1100)
  | 687 -> One (r0)
  | 1782 -> One (r2)
  | 1781 -> One (r3)
  | 1780 -> One (r4)
  | 1779 -> One (r5)
  | 1778 -> One (r6)
  | 58 -> One (r7)
  | 53 -> One (r8)
  | 54 -> One (r10)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1385 -> One (r14)
  | 1777 -> One (r16)
  | 1776 -> One (r17)
  | 60 -> One (r18)
  | 1775 -> One (r19)
  | 1774 -> One (r20)
  | 1773 -> One (r21)
  | 1772 -> One (r22)
  | 63 -> One (r23)
  | 62 -> One (r24)
  | 64 -> One (r25)
  | 65 -> One (r26)
  | 1765 -> One (r27)
  | 68 -> One (r28)
  | 67 -> One (r29)
  | 1562 -> One (r30)
  | 1560 -> One (r31)
  | 580 -> One (r32)
  | 1567 -> One (r34)
  | 1764 -> One (r36)
  | 1763 -> One (r37)
  | 1762 -> One (r38)
  | 71 -> One (r39)
  | 70 -> One (r40)
  | 74 -> One (r41)
  | 1641 -> One (r43)
  | 1761 -> One (r44)
  | 1760 -> One (r45)
  | 1759 -> One (r46)
  | 78 -> One (r47)
  | 77 -> One (r48)
  | 1755 -> One (r49)
  | 1754 -> One (r50)
  | 80 -> One (r51)
  | 82 -> One (r52)
  | 85 -> One (r53)
  | 89 -> One (r54)
  | 395 -> One (r55)
  | 394 -> One (r56)
  | 144 -> One (r57)
  | 146 -> One (r59)
  | 145 -> One (r60)
  | 110 -> One (r61)
  | 99 -> One (r62)
  | 102 -> One (r64)
  | 101 -> One (r65)
  | 98 -> One (r66)
  | 97 -> One (r67)
  | 1753 -> One (r68)
  | 1752 -> One (r69)
  | 104 | 151 -> One (r70)
  | 1177 -> One (r71)
  | 1751 -> One (r73)
  | 1750 -> One (r74)
  | 106 -> One (r75)
  | 147 | 246 | 555 | 1522 -> One (r76)
  | 150 -> One (r78)
  | 299 -> One (r80)
  | 285 -> One (r82)
  | 315 -> One (r84)
  | 325 -> One (r86)
  | 833 -> One (r88)
  | 1749 -> One (r90)
  | 1748 -> One (r91)
  | 149 -> One (r92)
  | 148 -> One (r93)
  | 109 -> One (r94)
  | 108 -> One (r95)
  | 129 -> One (r96)
  | 128 -> One (r97)
  | 125 -> One (r98)
  | 127 -> One (r99)
  | 133 -> One (r100)
  | 132 -> One (r101)
  | 137 -> One (r102)
  | 136 -> One (r103)
  | 154 -> One (r104)
  | 162 -> One (r106)
  | 161 -> One (r107)
  | 158 -> One (r109)
  | 157 -> One (r110)
  | 1747 -> One (r111)
  | 1746 -> One (r112)
  | 165 -> One (r113)
  | 164 -> One (r114)
  | 163 -> One (r115)
  | 1745 -> One (r116)
  | 169 -> One (r117)
  | 168 -> One (r118)
  | 167 -> One (r119)
  | 1744 -> One (r120)
  | 1743 -> One (r121)
  | 172 -> One (r122)
  | 205 -> One (r123)
  | 289 -> One (r125)
  | 351 -> One (r127)
  | 889 -> One (r129)
  | 925 -> One (r131)
  | 924 -> One (r132)
  | 923 | 1732 -> One (r133)
  | 1728 -> One (r135)
  | 1742 -> One (r137)
  | 1741 -> One (r138)
  | 1740 -> One (r139)
  | 1739 -> One (r140)
  | 1738 -> One (r141)
  | 952 -> One (r145)
  | 951 -> One (r146)
  | 950 -> One (r147)
  | 1725 -> One (r153)
  | 1724 -> One (r154)
  | 1718 -> One (r155)
  | 1717 -> One (r156)
  | 1716 -> One (r157)
  | 934 -> One (r159)
  | 933 -> One (r160)
  | 932 -> One (r161)
  | 188 -> One (r165)
  | 191 -> One (r167)
  | 187 -> One (r168)
  | 192 -> One (r170)
  | 194 -> One (r172)
  | 193 -> One (r173)
  | 190 -> One (r174)
  | 196 -> One (r175)
  | 902 -> One (r176)
  | 1715 -> One (r178)
  | 1712 -> One (r179)
  | 840 -> One (r180)
  | 839 -> One (r181)
  | 1697 -> One (r182)
  | 1696 -> One (r183)
  | 1695 -> One (r184)
  | 204 -> One (r185)
  | 1692 -> One (r186)
  | 856 -> One (r187)
  | 1684 -> One (r189)
  | 1683 -> One (r190)
  | 208 -> One (r191)
  | 1682 -> One (r192)
  | 1681 -> One (r193)
  | 210 -> One (r194)
  | 1680 -> One (r195)
  | 1676 -> One (r196)
  | 1675 -> One (r197)
  | 1674 -> One (r198)
  | 1673 -> One (r199)
  | 1672 -> One (r200)
  | 1671 -> One (r201)
  | 218 -> One (r202)
  | 217 -> One (r203)
  | 540 -> One (r204)
  | 539 -> One (r205)
  | 1661 -> One (r206)
  | 1660 -> One (r207)
  | 221 -> One (r208)
  | 225 -> One (r209)
  | 231 -> One (r211)
  | 232 -> One (r213)
  | 224 -> One (r214)
  | 223 -> One (r215)
  | 229 -> One (r216)
  | 227 -> One (r217)
  | 228 -> One (r218)
  | 230 -> One (r219)
  | 234 -> One (r220)
  | 1659 -> One (r221)
  | 1658 -> One (r222)
  | 1657 -> One (r223)
  | 239 -> One (r224)
  | 238 -> One (r225)
  | 1656 -> One (r226)
  | 1655 -> One (r227)
  | 1654 -> One (r228)
  | 242 -> One (r229)
  | 241 -> One (r230)
  | 1651 -> One (r231)
  | 1650 -> One (r232)
  | 1638 -> One (r233)
  | 1637 -> One (r234)
  | 429 -> One (r235)
  | 1636 -> One (r237)
  | 1635 -> One (r238)
  | 252 -> One (r239)
  | 250 -> One (r240)
  | 249 -> One (r241)
  | 426 -> One (r242)
  | 255 -> One (r243)
  | 415 -> One (r244)
  | 414 -> One (r246)
  | 413 -> One (r247)
  | 257 -> One (r248)
  | 420 -> One (r250)
  | 342 -> One (r251)
  | 260 -> One (r252)
  | 259 -> One (r254)
  | 258 -> One (r255)
  | 341 -> One (r256)
  | 323 -> One (r257)
  | 304 -> One (r259)
  | 335 -> One (r261)
  | 334 -> One (r262)
  | 264 -> One (r263)
  | 266 -> One (r264)
  | 333 -> One (r265)
  | 332 -> One (r266)
  | 283 -> One (r267)
  | 282 -> One (r268)
  | 322 -> One (r270)
  | 309 -> One (r271)
  | 327 -> One (r273)
  | 326 -> One (r274)
  | 279 | 1123 -> One (r275)
  | 280 -> One (r277)
  | 275 -> One (r278)
  | 274 -> One (r279)
  | 278 -> One (r281)
  | 276 -> One (r284)
  | 273 -> One (r286)
  | 272 -> One (r287)
  | 306 -> One (r288)
  | 305 -> One (r289)
  | 302 -> One (r290)
  | 291 -> One (r291)
  | 293 -> One (r292)
  | 298 -> One (r293)
  | 297 -> One (r294)
  | 296 -> One (r295)
  | 301 -> One (r296)
  | 308 -> One (r297)
  | 321 -> One (r298)
  | 320 -> One (r300)
  | 313 -> One (r301)
  | 312 -> One (r302)
  | 316 -> One (r303)
  | 319 -> One (r304)
  | 318 -> One (r305)
  | 331 -> One (r306)
  | 330 -> One (r307)
  | 329 -> One (r308)
  | 340 -> One (r309)
  | 338 -> One (r311)
  | 337 -> One (r312)
  | 419 -> One (r313)
  | 355 | 744 -> One (r315)
  | 356 -> One (r317)
  | 346 -> One (r318)
  | 345 -> One (r319)
  | 347 -> One (r320)
  | 349 -> One (r321)
  | 361 -> One (r323)
  | 360 -> One (r325)
  | 412 -> One (r326)
  | 411 -> One (r327)
  | 364 -> One (r328)
  | 366 -> One (r329)
  | 406 -> One (r330)
  | 369 -> One (r331)
  | 368 -> One (r332)
  | 374 -> One (r333)
  | 376 -> One (r334)
  | 379 -> One (r335)
  | 405 -> One (r336)
  | 384 -> One (r337)
  | 388 -> One (r339)
  | 387 -> One (r340)
  | 386 -> One (r341)
  | 390 -> One (r342)
  | 393 -> One (r343)
  | 392 -> One (r344)
  | 397 -> One (r345)
  | 400 -> One (r346)
  | 399 -> One (r347)
  | 402 -> One (r348)
  | 404 -> One (r349)
  | 408 -> One (r350)
  | 407 -> One (r351)
  | 410 -> One (r352)
  | 424 -> One (r353)
  | 428 -> One (r354)
  | 437 -> One (r355)
  | 432 -> One (r356)
  | 436 -> One (r358)
  | 435 -> One (r359)
  | 434 -> One (r360)
  | 1618 -> One (r361)
  | 1617 -> One (r362)
  | 1616 -> One (r363)
  | 440 -> One (r364)
  | 1615 -> One (r365)
  | 443 -> One (r366)
  | 1492 -> One (r368)
  | 1489 -> One (r370)
  | 1488 -> One (r371)
  | 1487 -> One (r372)
  | 445 -> One (r373)
  | 454 -> One (r375)
  | 452 -> One (r376)
  | 451 -> One (r377)
  | 450 -> One (r378)
  | 449 -> One (r379)
  | 1612 -> One (r380)
  | 461 -> One (r381)
  | 1238 -> One (r383)
  | 1613 -> One (r385)
  | 458 -> One (r386)
  | 457 -> One (r387)
  | 456 -> One (r388)
  | 460 -> One (r389)
  | 1596 -> One (r390)
  | 1595 -> One (r391)
  | 1594 -> One (r392)
  | 1593 -> One (r393)
  | 1592 -> One (r394)
  | 463 -> One (r395)
  | 1368 -> One (r396)
  | 1367 -> One (r397)
  | 1366 -> One (r398)
  | 1365 -> One (r399)
  | 1364 -> One (r400)
  | 1363 -> One (r401)
  | 1591 -> One (r402)
  | 549 -> One (r403)
  | 548 -> One (r404)
  | 466 -> One (r405)
  | 465 -> One (r406)
  | 536 -> One (r407)
  | 534 -> One (r408)
  | 533 -> One (r409)
  | 468 -> One (r410)
  | 470 -> One (r411)
  | 532 -> One (r412)
  | 531 -> One (r413)
  | 472 -> One (r414)
  | 530 -> One (r415)
  | 529 -> One (r416)
  | 528 -> One (r417)
  | 475 -> One (r418)
  | 483 -> One (r419)
  | 481 -> One (r420)
  | 480 -> One (r421)
  | 477 -> One (r422)
  | 511 -> One (r423)
  | 510 -> One (r425)
  | 504 -> One (r427)
  | 503 -> One (r428)
  | 502 -> One (r429)
  | 501 -> One (r430)
  | 500 -> One (r431)
  | 523 -> One (r433)
  | 524 -> One (r435)
  | 491 -> One (r436)
  | 490 -> One (r437)
  | 487 -> One (r438)
  | 486 -> One (r439)
  | 494 -> One (r440)
  | 493 -> One (r441)
  | 498 -> One (r442)
  | 497 -> One (r443)
  | 496 -> One (r444)
  | 509 -> One (r445)
  | 514 -> One (r447)
  | 516 -> One (r448)
  | 519 -> One (r449)
  | 518 -> One (r450)
  | 520 -> One (r451)
  | 522 -> One (r452)
  | 526 -> One (r453)
  | 538 -> One (r454)
  | 543 -> One (r455)
  | 542 -> One (r456)
  | 1411 -> One (r457)
  | 1590 -> One (r459)
  | 1589 -> One (r460)
  | 1586 -> One (r461)
  | 1583 -> One (r462)
  | 552 -> One (r463)
  | 1582 -> One (r464)
  | 1514 -> One (r465)
  | 1513 -> One (r466)
  | 1511 -> One (r467)
  | 1517 -> One (r469)
  | 1581 -> One (r471)
  | 1580 -> One (r472)
  | 559 -> One (r473)
  | 563 -> One (r475)
  | 558 -> One (r477)
  | 562 -> One (r478)
  | 1579 -> One (r480)
  | 1578 -> One (r481)
  | 1577 -> One (r482)
  | 1576 -> One (r483)
  | 569 -> One (r484)
  | 568 -> One (r485)
  | 1573 -> One (r486)
  | 572 -> One (r487)
  | 571 -> One (r488)
  | 1570 -> One (r489)
  | 1569 -> One (r490)
  | 1568 -> One (r491)
  | 575 -> One (r492)
  | 574 -> One (r493)
  | 1564 -> One (r494)
  | 578 -> One (r495)
  | 577 -> One (r496)
  | 1563 -> One (r497)
  | 1559 -> One (r498)
  | 1558 -> One (r499)
  | 1557 -> One (r500)
  | 1233 -> One (r501)
  | 1542 -> One (r503)
  | 589 -> One (r504)
  | 1556 -> One (r506)
  | 1555 -> One (r507)
  | 584 -> One (r508)
  | 583 -> One (r509)
  | 1554 -> One (r510)
  | 588 -> One (r511)
  | 587 -> One (r512)
  | 1534 -> One (r513)
  | 1533 -> One (r514)
  | 1532 -> One (r515)
  | 1531 -> One (r516)
  | 594 -> One (r517)
  | 593 -> One (r518)
  | 592 -> One (r519)
  | 591 -> One (r520)
  | 1525 -> One (r521)
  | 1530 -> One (r523)
  | 1529 -> One (r524)
  | 1528 -> One (r525)
  | 1527 -> One (r526)
  | 1526 -> One (r527)
  | 1523 -> One (r528)
  | 599 -> One (r529)
  | 598 -> One (r530)
  | 597 -> One (r531)
  | 596 -> One (r532)
  | 602 -> One (r533)
  | 607 -> One (r534)
  | 606 -> One (r535)
  | 605 | 1521 -> One (r536)
  | 1520 -> One (r537)
  | 616 -> One (r538)
  | 615 -> One (r539)
  | 614 -> One (r540)
  | 613 -> One (r541)
  | 612 -> One (r542)
  | 611 -> One (r543)
  | 1483 -> One (r544)
  | 623 -> One (r545)
  | 622 -> One (r546)
  | 627 -> One (r547)
  | 626 -> One (r548)
  | 625 -> One (r549)
  | 629 -> One (r550)
  | 1424 | 1476 -> One (r551)
  | 1423 | 1475 -> One (r552)
  | 631 | 1422 -> One (r553)
  | 630 | 1421 -> One (r554)
  | 1474 -> One (r555)
  | 645 -> One (r556)
  | 640 -> One (r557)
  | 639 | 1619 -> One (r558)
  | 644 -> One (r560)
  | 643 -> One (r561)
  | 636 -> One (r562)
  | 638 -> One (r563)
  | 642 -> One (r564)
  | 647 -> One (r565)
  | 649 -> One (r566)
  | 651 -> One (r567)
  | 655 | 1440 -> One (r568)
  | 654 | 1439 -> One (r569)
  | 653 | 1438 -> One (r570)
  | 652 | 1437 -> One (r571)
  | 1399 -> One (r572)
  | 662 -> One (r573)
  | 661 -> One (r574)
  | 665 -> One (r575)
  | 675 -> One (r576)
  | 678 -> One (r578)
  | 677 -> One (r579)
  | 671 -> One (r580)
  | 674 -> One (r581)
  | 682 -> One (r582)
  | 681 -> One (r583)
  | 686 -> One (r584)
  | 685 -> One (r585)
  | 689 -> One (r586)
  | 691 -> One (r587)
  | 696 -> One (r588)
  | 700 -> One (r589)
  | 699 -> One (r590)
  | 703 -> One (r591)
  | 705 -> One (r592)
  | 707 -> One (r593)
  | 709 -> One (r594)
  | 711 -> One (r595)
  | 713 -> One (r596)
  | 715 -> One (r597)
  | 717 -> One (r598)
  | 719 -> One (r599)
  | 721 -> One (r600)
  | 723 -> One (r601)
  | 725 -> One (r602)
  | 727 -> One (r603)
  | 729 -> One (r604)
  | 731 -> One (r605)
  | 733 -> One (r606)
  | 735 -> One (r607)
  | 737 -> One (r608)
  | 739 -> One (r609)
  | 741 -> One (r610)
  | 1398 -> One (r611)
  | 766 -> One (r612)
  | 743 -> One (r613)
  | 748 -> One (r614)
  | 747 -> One (r615)
  | 746 -> One (r616)
  | 751 -> One (r617)
  | 750 -> One (r618)
  | 753 -> One (r619)
  | 755 -> One (r620)
  | 757 -> One (r621)
  | 759 -> One (r622)
  | 764 -> One (r623)
  | 1397 -> One (r624)
  | 1396 -> One (r625)
  | 768 -> One (r626)
  | 770 -> One (r627)
  | 772 -> One (r628)
  | 789 -> One (r629)
  | 788 -> One (r630)
  | 807 -> One (r632)
  | 806 -> One (r633)
  | 805 -> One (r634)
  | 785 -> One (r635)
  | 784 -> One (r636)
  | 783 -> One (r637)
  | 780 -> One (r638)
  | 777 -> One (r639)
  | 776 -> One (r640)
  | 775 -> One (r641)
  | 774 -> One (r642)
  | 779 -> One (r643)
  | 782 -> One (r644)
  | 804 -> One (r645)
  | 795 -> One (r646)
  | 794 -> One (r647)
  | 787 -> One (r648)
  | 793 -> One (r649)
  | 792 -> One (r650)
  | 791 -> One (r651)
  | 801 -> One (r652)
  | 800 -> One (r653)
  | 799 -> One (r654)
  | 798 -> One (r655)
  | 797 -> One (r656)
  | 803 -> One (r657)
  | 1395 -> One (r658)
  | 1394 -> One (r659)
  | 809 -> One (r660)
  | 1390 -> One (r661)
  | 1389 -> One (r662)
  | 811 -> One (r663)
  | 816 -> One (r664)
  | 815 -> One (r665)
  | 814 -> One (r666)
  | 813 -> One (r667)
  | 829 -> One (r668)
  | 832 -> One (r670)
  | 831 -> One (r671)
  | 828 -> One (r672)
  | 827 -> One (r673)
  | 821 -> One (r674)
  | 820 -> One (r675)
  | 819 -> One (r676)
  | 818 -> One (r677)
  | 826 -> One (r678)
  | 825 -> One (r679)
  | 824 -> One (r680)
  | 874 -> One (r682)
  | 873 -> One (r683)
  | 872 -> One (r684)
  | 867 -> One (r685)
  | 888 -> One (r689)
  | 887 -> One (r690)
  | 886 -> One (r691)
  | 1014 -> One (r692)
  | 1013 -> One (r693)
  | 1012 -> One (r694)
  | 1011 -> One (r695)
  | 866 -> One (r696)
  | 865 -> One (r698)
  | 861 -> One (r705)
  | 858 -> One (r707)
  | 857 -> One (r708)
  | 855 -> One (r709)
  | 854 -> One (r710)
  | 853 -> One (r711)
  | 852 -> One (r712)
  | 848 -> One (r713)
  | 847 -> One (r714)
  | 851 -> One (r715)
  | 850 -> One (r716)
  | 864 -> One (r717)
  | 863 -> One (r718)
  | 871 -> One (r719)
  | 885 -> One (r720)
  | 881 -> One (r721)
  | 877 -> One (r722)
  | 880 -> One (r723)
  | 879 -> One (r724)
  | 884 -> One (r725)
  | 883 -> One (r726)
  | 1176 -> One (r727)
  | 942 -> One (r728)
  | 957 -> One (r730)
  | 956 -> One (r731)
  | 955 -> One (r732)
  | 954 -> One (r733)
  | 953 -> One (r734)
  | 940 -> One (r738)
  | 939 -> One (r739)
  | 938 -> One (r740)
  | 936 -> One (r741)
  | 935 -> One (r742)
  | 912 -> One (r744)
  | 911 -> One (r745)
  | 910 -> One (r746)
  | 901 -> One (r747)
  | 900 -> One (r748)
  | 906 -> One (r749)
  | 905 -> One (r750)
  | 904 | 1720 -> One (r751)
  | 908 | 1719 -> One (r752)
  | 929 -> One (r753)
  | 921 -> One (r754)
  | 920 -> One (r755)
  | 919 -> One (r756)
  | 928 -> One (r757)
  | 927 -> One (r758)
  | 949 -> One (r759)
  | 948 -> One (r760)
  | 947 -> One (r761)
  | 1175 -> One (r762)
  | 968 -> One (r763)
  | 967 -> One (r764)
  | 966 -> One (r765)
  | 965 -> One (r766)
  | 964 -> One (r767)
  | 963 -> One (r768)
  | 962 -> One (r769)
  | 961 -> One (r770)
  | 1001 -> One (r771)
  | 1000 -> One (r772)
  | 1003 -> One (r774)
  | 1002 -> One (r775)
  | 996 -> One (r776)
  | 978 -> One (r777)
  | 977 -> One (r778)
  | 976 -> One (r779)
  | 975 -> One (r780)
  | 974 -> One (r781)
  | 982 -> One (r785)
  | 981 -> One (r786)
  | 995 -> One (r787)
  | 987 -> One (r788)
  | 986 -> One (r789)
  | 985 -> One (r790)
  | 984 -> One (r791)
  | 994 -> One (r792)
  | 993 -> One (r793)
  | 992 -> One (r794)
  | 991 -> One (r795)
  | 990 -> One (r796)
  | 989 -> One (r797)
  | 999 -> One (r800)
  | 998 -> One (r801)
  | 1005 -> One (r802)
  | 1010 -> One (r803)
  | 1009 -> One (r804)
  | 1008 -> One (r805)
  | 1007 -> One (r806)
  | 1070 | 1124 -> One (r808)
  | 1126 -> One (r810)
  | 1140 -> One (r812)
  | 1130 -> One (r813)
  | 1129 -> One (r814)
  | 1111 -> One (r815)
  | 1110 -> One (r816)
  | 1109 -> One (r817)
  | 1108 -> One (r818)
  | 1107 -> One (r819)
  | 1106 -> One (r820)
  | 1105 -> One (r821)
  | 1095 -> One (r822)
  | 1094 -> One (r823)
  | 1026 -> One (r824)
  | 1025 -> One (r825)
  | 1024 -> One (r826)
  | 1020 -> One (r827)
  | 1018 -> One (r828)
  | 1017 -> One (r829)
  | 1023 -> One (r830)
  | 1022 -> One (r831)
  | 1088 -> One (r832)
  | 1087 -> One (r833)
  | 1032 -> One (r834)
  | 1028 -> One (r835)
  | 1031 -> One (r836)
  | 1030 -> One (r837)
  | 1043 -> One (r838)
  | 1042 -> One (r839)
  | 1041 -> One (r840)
  | 1040 -> One (r841)
  | 1039 -> One (r842)
  | 1034 -> One (r843)
  | 1054 -> One (r844)
  | 1053 -> One (r845)
  | 1052 -> One (r846)
  | 1051 -> One (r847)
  | 1050 -> One (r848)
  | 1045 -> One (r849)
  | 1079 -> One (r850)
  | 1078 -> One (r851)
  | 1056 -> One (r852)
  | 1077 -> One (r853)
  | 1076 -> One (r854)
  | 1075 -> One (r855)
  | 1074 -> One (r856)
  | 1058 -> One (r857)
  | 1072 -> One (r858)
  | 1062 -> One (r859)
  | 1061 -> One (r860)
  | 1060 -> One (r861)
  | 1069 | 1117 -> One (r862)
  | 1066 -> One (r864)
  | 1065 -> One (r865)
  | 1064 -> One (r866)
  | 1063 | 1116 -> One (r867)
  | 1068 -> One (r868)
  | 1084 -> One (r869)
  | 1083 -> One (r870)
  | 1082 -> One (r871)
  | 1086 -> One (r873)
  | 1085 -> One (r874)
  | 1081 -> One (r875)
  | 1090 -> One (r876)
  | 1093 -> One (r877)
  | 1104 -> One (r878)
  | 1103 -> One (r879)
  | 1102 -> One (r880)
  | 1101 -> One (r881)
  | 1100 -> One (r882)
  | 1099 -> One (r883)
  | 1098 -> One (r884)
  | 1097 -> One (r885)
  | 1128 -> One (r886)
  | 1115 -> One (r887)
  | 1114 -> One (r888)
  | 1113 -> One (r889)
  | 1127 -> One (r890)
  | 1119 -> One (r891)
  | 1125 -> One (r892)
  | 1122 -> One (r893)
  | 1121 -> One (r894)
  | 1139 -> One (r895)
  | 1138 -> One (r896)
  | 1137 -> One (r897)
  | 1136 -> One (r898)
  | 1135 -> One (r899)
  | 1134 -> One (r900)
  | 1133 -> One (r901)
  | 1132 -> One (r902)
  | 1149 -> One (r903)
  | 1151 -> One (r904)
  | 1161 -> One (r905)
  | 1160 -> One (r906)
  | 1159 -> One (r907)
  | 1158 -> One (r908)
  | 1157 -> One (r909)
  | 1156 -> One (r910)
  | 1155 -> One (r911)
  | 1154 -> One (r912)
  | 1172 -> One (r913)
  | 1171 -> One (r914)
  | 1170 -> One (r915)
  | 1169 -> One (r916)
  | 1168 -> One (r917)
  | 1167 -> One (r918)
  | 1166 -> One (r919)
  | 1165 -> One (r920)
  | 1164 -> One (r921)
  | 1294 -> One (r922)
  | 1343 -> One (r924)
  | 1185 -> One (r925)
  | 1360 -> One (r927)
  | 1351 -> One (r928)
  | 1350 -> One (r929)
  | 1184 -> One (r930)
  | 1183 -> One (r931)
  | 1182 -> One (r932)
  | 1181 -> One (r933)
  | 1180 -> One (r934)
  | 1337 -> One (r935)
  | 1336 -> One (r936)
  | 1188 -> One (r937)
  | 1187 -> One (r938)
  | 1213 -> One (r939)
  | 1212 -> One (r940)
  | 1211 -> One (r941)
  | 1210 -> One (r942)
  | 1201 -> One (r943)
  | 1200 -> One (r945)
  | 1199 -> One (r946)
  | 1195 -> One (r947)
  | 1194 -> One (r948)
  | 1193 -> One (r949)
  | 1192 -> One (r950)
  | 1191 -> One (r951)
  | 1198 -> One (r952)
  | 1197 -> One (r953)
  | 1209 -> One (r954)
  | 1208 -> One (r955)
  | 1207 -> One (r956)
  | 1216 -> One (r957)
  | 1215 -> One (r958)
  | 1263 -> One (r959)
  | 1252 -> One (r960)
  | 1251 -> One (r961)
  | 1242 -> One (r962)
  | 1241 -> One (r964)
  | 1240 -> One (r965)
  | 1232 -> One (r966)
  | 1221 -> One (r967)
  | 1220 -> One (r968)
  | 1219 -> One (r969)
  | 1231 -> One (r970)
  | 1230 -> One (r971)
  | 1229 -> One (r972)
  | 1228 -> One (r973)
  | 1227 -> One (r974)
  | 1226 -> One (r975)
  | 1225 -> One (r976)
  | 1224 -> One (r977)
  | 1239 -> One (r978)
  | 1237 -> One (r979)
  | 1236 -> One (r980)
  | 1250 -> One (r981)
  | 1249 -> One (r982)
  | 1248 -> One (r983)
  | 1262 -> One (r984)
  | 1261 -> One (r985)
  | 1260 -> One (r986)
  | 1259 -> One (r987)
  | 1258 -> One (r988)
  | 1257 -> One (r989)
  | 1256 -> One (r990)
  | 1255 -> One (r991)
  | 1267 -> One (r992)
  | 1266 -> One (r993)
  | 1265 -> One (r994)
  | 1331 -> One (r995)
  | 1330 -> One (r996)
  | 1329 -> One (r997)
  | 1328 -> One (r998)
  | 1327 -> One (r999)
  | 1326 -> One (r1000)
  | 1323 -> One (r1001)
  | 1270 -> One (r1002)
  | 1319 -> One (r1003)
  | 1318 -> One (r1004)
  | 1313 -> One (r1005)
  | 1312 -> One (r1006)
  | 1311 -> One (r1007)
  | 1310 -> One (r1008)
  | 1279 -> One (r1009)
  | 1278 -> One (r1010)
  | 1277 -> One (r1011)
  | 1276 -> One (r1012)
  | 1275 -> One (r1013)
  | 1274 -> One (r1014)
  | 1309 -> One (r1015)
  | 1283 -> One (r1016)
  | 1282 -> One (r1017)
  | 1281 -> One (r1018)
  | 1287 -> One (r1019)
  | 1286 -> One (r1020)
  | 1285 -> One (r1021)
  | 1306 -> One (r1022)
  | 1291 -> One (r1023)
  | 1290 -> One (r1024)
  | 1308 -> One (r1026)
  | 1289 -> One (r1027)
  | 1303 -> One (r1028)
  | 1293 -> One (r1029)
  | 1297 -> One (r1030)
  | 1317 -> One (r1031)
  | 1316 -> One (r1032)
  | 1315 -> One (r1033)
  | 1322 -> One (r1034)
  | 1321 -> One (r1035)
  | 1325 -> One (r1036)
  | 1335 -> One (r1037)
  | 1334 -> One (r1038)
  | 1333 -> One (r1039)
  | 1339 -> One (r1040)
  | 1342 -> One (r1041)
  | 1347 -> One (r1042)
  | 1346 -> One (r1043)
  | 1345 -> One (r1044)
  | 1349 -> One (r1045)
  | 1359 -> One (r1046)
  | 1358 -> One (r1047)
  | 1357 -> One (r1048)
  | 1356 -> One (r1049)
  | 1355 -> One (r1050)
  | 1354 -> One (r1051)
  | 1353 -> One (r1052)
  | 1376 -> One (r1053)
  | 1380 -> One (r1054)
  | 1382 -> One (r1055)
  | 1388 -> One (r1056)
  | 1387 -> One (r1057)
  | 1402 | 1445 -> One (r1058)
  | 1401 | 1444 -> One (r1059)
  | 1400 | 1443 -> One (r1060)
  | 1405 | 1450 -> One (r1061)
  | 1404 | 1449 -> One (r1062)
  | 1403 | 1448 -> One (r1063)
  | 1410 | 1457 -> One (r1064)
  | 1409 | 1456 -> One (r1065)
  | 1408 | 1455 -> One (r1066)
  | 1407 | 1454 -> One (r1067)
  | 1416 | 1462 -> One (r1068)
  | 1415 | 1461 -> One (r1069)
  | 1414 | 1460 -> One (r1070)
  | 1419 | 1467 -> One (r1071)
  | 1418 | 1466 -> One (r1072)
  | 1417 | 1465 -> One (r1073)
  | 1426 -> One (r1074)
  | 1429 | 1479 -> One (r1075)
  | 1428 | 1478 -> One (r1076)
  | 1427 | 1477 -> One (r1077)
  | 1431 -> One (r1078)
  | 1434 | 1482 -> One (r1079)
  | 1433 | 1481 -> One (r1080)
  | 1432 | 1480 -> One (r1081)
  | 1436 -> One (r1082)
  | 1442 -> One (r1083)
  | 1447 -> One (r1084)
  | 1452 -> One (r1085)
  | 1459 -> One (r1086)
  | 1464 -> One (r1087)
  | 1469 -> One (r1088)
  | 1472 -> One (r1089)
  | 1486 -> One (r1090)
  | 1485 -> One (r1091)
  | 1491 -> One (r1092)
  | 1495 -> One (r1093)
  | 1497 -> One (r1094)
  | 1499 -> One (r1095)
  | 1501 -> One (r1096)
  | 1503 -> One (r1097)
  | 1506 -> One (r1099)
  | 1505 -> One (r1100)
  | 1519 -> One (r1101)
  | 1518 -> One (r1102)
  | 1510 -> One (r1103)
  | 1509 -> One (r1104)
  | 1541 -> One (r1105)
  | 1540 -> One (r1106)
  | 1539 -> One (r1107)
  | 1538 -> One (r1108)
  | 1537 -> One (r1109)
  | 1536 -> One (r1110)
  | 1553 -> One (r1111)
  | 1546 -> One (r1112)
  | 1545 -> One (r1113)
  | 1550 -> One (r1114)
  | 1549 -> One (r1115)
  | 1548 -> One (r1116)
  | 1552 -> One (r1117)
  | 1566 -> One (r1118)
  | 1572 -> One (r1119)
  | 1575 -> One (r1120)
  | 1588 -> One (r1121)
  | 1603 -> One (r1122)
  | 1602 -> One (r1123)
  | 1601 -> One (r1124)
  | 1600 -> One (r1125)
  | 1599 -> One (r1126)
  | 1598 -> One (r1127)
  | 1611 -> One (r1128)
  | 1610 -> One (r1129)
  | 1609 -> One (r1130)
  | 1608 -> One (r1131)
  | 1607 -> One (r1132)
  | 1606 -> One (r1133)
  | 1605 -> One (r1134)
  | 1625 -> One (r1135)
  | 1624 -> One (r1136)
  | 1623 -> One (r1137)
  | 1622 -> One (r1138)
  | 1621 -> One (r1139)
  | 1630 -> One (r1140)
  | 1629 -> One (r1141)
  | 1628 -> One (r1142)
  | 1627 -> One (r1143)
  | 1633 -> One (r1144)
  | 1632 -> One (r1145)
  | 1640 -> One (r1146)
  | 1646 -> One (r1147)
  | 1645 -> One (r1148)
  | 1644 -> One (r1149)
  | 1643 -> One (r1150)
  | 1649 -> One (r1151)
  | 1648 -> One (r1152)
  | 1653 -> One (r1153)
  | 1664 -> One (r1154)
  | 1663 -> One (r1155)
  | 1667 -> One (r1156)
  | 1666 -> One (r1157)
  | 1670 -> One (r1158)
  | 1669 -> One (r1159)
  | 1679 -> One (r1160)
  | 1678 -> One (r1161)
  | 1686 -> One (r1162)
  | 1694 -> One (r1163)
  | 1702 -> One (r1164)
  | 1699 -> One (r1165)
  | 1701 -> One (r1166)
  | 1704 -> One (r1167)
  | 1706 -> One (r1168)
  | 1708 -> One (r1169)
  | 1711 -> One (r1170)
  | 1710 -> One (r1171)
  | 1723 -> One (r1172)
  | 1722 -> One (r1173)
  | 1735 -> One (r1174)
  | 1734 -> One (r1175)
  | 1758 -> One (r1176)
  | 1757 -> One (r1177)
  | 1767 -> One (r1178)
  | 1769 -> One (r1179)
  | 1771 -> One (r1180)
  | 1784 -> One (r1181)
  | 1788 -> One (r1182)
  | 1793 -> One (r1183)
  | 1800 -> One (r1184)
  | 1799 -> One (r1185)
  | 1798 -> One (r1186)
  | 1797 -> One (r1187)
  | 1807 -> One (r1188)
  | 1811 -> One (r1189)
  | 1815 -> One (r1190)
  | 1818 -> One (r1191)
  | 1823 -> One (r1192)
  | 1827 -> One (r1193)
  | 1831 -> One (r1194)
  | 1835 -> One (r1195)
  | 1839 -> One (r1196)
  | 1842 -> One (r1197)
  | 1846 -> One (r1198)
  | 1850 -> One (r1199)
  | 1860 -> One (r1200)
  | 1862 -> One (r1201)
  | 1865 -> One (r1202)
  | 1864 -> One (r1203)
  | 1867 -> One (r1204)
  | 1877 -> One (r1205)
  | 1873 -> One (r1206)
  | 1872 -> One (r1207)
  | 1876 -> One (r1208)
  | 1875 -> One (r1209)
  | 1882 -> One (r1210)
  | 1881 -> One (r1211)
  | 1880 -> One (r1212)
  | 1884 -> One (r1213)
  | 363 -> Select (function
    | -1 -> [R 107]
    | _ -> S (T T_DOT) :: r328)
  | 604 -> Select (function
    | -1 -> [R 107]
    | _ -> r537)
  | 173 -> Select (function
    | -1 -> r152
    | _ -> R 187 :: r144)
  | 834 -> Select (function
    | -1 -> r695
    | _ -> R 187 :: r688)
  | 891 -> Select (function
    | -1 -> r152
    | _ -> R 187 :: r737)
  | 970 -> Select (function
    | -1 -> r642
    | _ -> R 187 :: r784)
  | 508 -> Select (function
    | -1 -> r278
    | _ -> [R 221])
  | 381 -> Select (function
    | -1 -> [R 689]
    | _ -> S (N N_pattern) :: r336)
  | 378 -> Select (function
    | -1 -> [R 690]
    | _ -> S (N N_pattern) :: r335)
  | 179 -> Select (function
    | -1 -> r164
    | _ -> R 797 :: r158)
  | 894 -> Select (function
    | -1 -> r164
    | _ -> R 797 :: r743)
  | 868 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (T T_COLONCOLON) :: r344)
  | 87 -> Select (function
    | 252 | 442 | 619 | 743 | 1276 | 1315 | 1366 | 1490 -> r61
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (N N_pattern) :: r56)
  | 243 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> Sub (r1) :: r232)
  | 254 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r243
    | _ -> Sub (r245) :: r247)
  | 550 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r243
    | _ -> Sub (r458) :: r460)
  | 462 -> Select (function
    | 60 | 172 | 210 | 768 | 809 | 811 -> r401
    | _ -> S (T T_OPEN) :: r395)
  | 870 -> Select (function
    | -1 -> r451
    | _ -> S (T T_LPAREN) :: r719)
  | 270 -> Select (function
    | -1 -> r280
    | _ -> S (T T_DOT) :: r282)
  | 506 -> Select (function
    | -1 -> r280
    | _ -> S (T T_DOT) :: r446)
  | 560 -> Select (function
    | -1 -> S (T T_DOT) :: r451
    | _ -> S (T T_DOT) :: r479)
  | 203 -> Select (function
    | -1 -> r123
    | _ -> S (T T_COLON) :: r185)
  | 152 -> Select (function
    | 875 | 1619 -> r107
    | _ -> Sub (r105) :: r108)
  | 155 -> Select (function
    | 875 | 1619 -> r106
    | _ -> r108)
  | 1737 -> Select (function
    | -1 -> r148
    | _ -> r123)
  | 198 -> Select (function
    | -1 -> r162
    | _ -> r123)
  | 945 -> Select (function
    | -1 -> r148
    | _ -> r123)
  | 896 -> Select (function
    | -1 -> r162
    | _ -> r123)
  | 1736 -> Select (function
    | -1 -> r149
    | _ -> r142)
  | 175 -> Select (function
    | -1 -> r150
    | _ -> r143)
  | 174 -> Select (function
    | -1 -> r151
    | _ -> r144)
  | 944 -> Select (function
    | -1 -> r149
    | _ -> r735)
  | 893 -> Select (function
    | -1 -> r150
    | _ -> r736)
  | 892 -> Select (function
    | -1 -> r151
    | _ -> r737)
  | 197 -> Select (function
    | -1 -> r163
    | _ -> r158)
  | 895 -> Select (function
    | -1 -> r163
    | _ -> r743)
  | 277 -> Select (function
    | -1 -> r279
    | _ -> r282)
  | 507 -> Select (function
    | -1 -> r279
    | _ -> r446)
  | 561 -> Select (function
    | 557 | 673 -> r479
    | _ -> r451)
  | 973 -> Select (function
    | -1 -> r639
    | _ -> r782)
  | 972 -> Select (function
    | -1 -> r640
    | _ -> r783)
  | 971 -> Select (function
    | -1 -> r641
    | _ -> r784)
  | 842 -> Select (function
    | -1 -> r692
    | _ -> r686)
  | 836 -> Select (function
    | -1 -> r693
    | _ -> r687)
  | 835 -> Select (function
    | -1 -> r694
    | _ -> r688)
  | _ -> raise Not_found

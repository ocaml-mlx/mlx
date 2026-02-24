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
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_42_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;1;2;1;2;3;1;1;1;2;2;3;4;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;1;2;1;2;1;1;1;2;1;2;1;1;1;2;3;4;5;6;7;8;1;2;1;2;3;1;2;3;1;1;1;2;1;2;2;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;1;2;1;2;3;1;1;2;1;1;1;1;1;1;1;2;3;2;1;2;1;2;3;2;3;2;3;2;3;4;5;3;1;1;2;3;4;3;4;3;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;1;2;3;4;4;5;6;7;8;9;10;11;8;1;1;1;2;3;1;1;1;2;3;4;5;6;7;8;9;10;11;12;13;9;1;2;2;1;2;2;1;1;2;3;4;1;1;5;6;6;1;2;3;4;1;1;2;1;1;1;1;1;2;3;4;1;2;3;1;2;3;1;1;2;3;3;1;1;4;1;1;1;1;1;2;3;1;1;1;1;2;1;1;2;3;4;5;6;7;8;9;10;11;12;13;9;1;1;2;1;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;4;5;6;7;8;9;5;4;5;1;1;1;1;2;3;1;1;2;3;4;1;1;1;2;2;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;1;2;3;1;2;4;5;6;1;2;3;2;3;2;3;4;5;6;7;8;4;3;4;3;3;3;4;5;2;3;2;3;2;4;4;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;8;9;8;1;8;2;3;2;1;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;4;5;6;7;8;9;5;4;5;4;4;1;2;3;4;5;6;7;8;9;5;4;5;4;4;1;1;2;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;1;2;1;2;2;3;1;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;2;3;2;3;1;1;4;5;2;3;4;2;3;4;1;3;2;3;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;2;3;1;3;2;3;1;1;1;2;3;1;2;3;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;1;1;2;1;2;3;3;4;5;1;2;1;2;3;4;1;2;1;1;1;2;4;1;2;5;6;1;2;3;4;5;6;7;8;9;10;7;6;7;2;3;2;3;1;2;3;4;5;1;2;3;4;1;2;3;1;2;3;4;1;1;1;1;1;2;3;3;4;1;2;3;3;1;2;5;6;2;1;2;3;4;5;1;1;1;2;3;4;5;2;1;2;1;2;1;2;2;3;1;2;3;4;5;6;1;2;3;4;5;6;7;4;3;4;3;4;5;6;2;3;1;2;1;2;3;1;1;2;3;4;5;6;3;2;3;4;5;6;3;2;1;2;1;2;3;4;5;2;2;3;4;5;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;7;4;3;4;3;4;5;6;3;2;3;4;5;6;3;1;2;1;1;2;2;3;4;5;6;7;8;3;2;3;4;5;6;7;2;3;4;2;1;1;2;3;1;4;1;1;2;3;4;5;1;2;3;2;3;4;5;6;7;8;4;3;4;3;3;2;3;2;3;1;3;1;2;4;2;3;1;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;3;4;5;6;7;8;5;1;2;2;1;2;6;1;1;7;8;9;10;11;4;5;3;4;5;3;4;5;3;4;5;6;7;5;6;7;5;6;7;3;6;7;8;9;6;7;3;4;5;2;3;3;2;4;4;5;6;7;8;9;10;11;12;13;14;11;6;7;8;9;10;11;8;4;5;3;4;5;3;4;5;3;4;5;6;7;8;5;4;5;4;5;6;7;4;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;3;4;4;5;3;4;5;3;1;2;3;1;2;3;4;5;1;4;5;1;2;3;4;4;4;5;2;3;2;3;4;5;2;2;3;4;2;2;3;2;3;8;3;4;5;6;7;2;3;4;5;1;2;1;2;3;4;6;7;8;1;2;2;3;4;1;1;2;3;1;5;1;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;3;2;1;1;2;3;4;5;6;4;2;3;4;2;6;7;8;9;1;2;3;1;4;5;6;2;4;5;2;2;3;4;5;6;3;2;2;3;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;2;3;2;4;5;6;7;8;8;9;10;8;9;10;10;11;12;4;5;5;6;7;5;6;7;7;8;9;5;6;2;3;4;5;1;2;3;4;5;1;2;6;7;2;3;4;5;6;7;1;2;3;4;5;6;8;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;5;1;2;3;4;5;1;2;3;6;7;1;2;8;9;1;1;2;3;4;5;1;1;2;3;6;7;8;5;6;7;1;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;1;1;2;3;1;1;2;3;4;1;1;2;6;7;8;9;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;2;3;1;1;2;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;1;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;1;1;1;1;1;2;1;1;2;1;2;1;1;1;1;2;3;1;1;1;3;4;3;4;2;3;4;2;3;4;5;7;8;8;9;8;8;2;3;4;5;6;7;8;9;5;4;5;4;4;2;3;3;4;5;4;5;6;7;8;7;8;9;10;7;2;3;4;5;6;7;8;5;4;5;4;5;6;7;4;4;5;6;3;4;9;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;6;7;8;5;6;7;8;9;3;4;3;4;5;6;1;7;1;2;3;2;2;3;3;4;5;3;4;5;6;7;2;3;4;5;4;2;3;2;2;3;2;3;4;2;2;2;2;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;3;4;5;6;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

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
  let r0 = [R 232] in
  let r1 = S (N N_fun_expr) :: r0 in
  let r2 = [R 732] in
  let r3 = Sub (r1) :: r2 in
  let r4 = [R 150] in
  let r5 = S (T T_DONE) :: r4 in
  let r6 = Sub (r3) :: r5 in
  let r7 = S (T T_DO) :: r6 in
  let r8 = Sub (r3) :: r7 in
  let r9 = R 374 :: r8 in
  let r10 = [R 830] in
  let r11 = S (T T_AND) :: r10 in
  let r12 = [R 32] in
  let r13 = Sub (r11) :: r12 in
  let r14 = [R 125] in
  let r15 = [R 33] in
  let r16 = [R 602] in
  let r17 = S (N N_structure) :: r16 in
  let r18 = [R 34] in
  let r19 = Sub (r17) :: r18 in
  let r20 = [R 35] in
  let r21 = S (T T_RBRACKET) :: r20 in
  let r22 = Sub (r19) :: r21 in
  let r23 = [R 944] in
  let r24 = S (T T_LIDENT) :: r23 in
  let r25 = [R 31] in
  let r26 = S (T T_UNDERSCORE) :: r25 in
  let r27 = [R 917] in
  let r28 = Sub (r26) :: r27 in
  let r29 = [R 236] in
  let r30 = Sub (r28) :: r29 in
  let r31 = [R 17] in
  let r32 = Sub (r30) :: r31 in
  let r33 = [R 108] in
  let r34 = Sub (r32) :: r33 in
  let r35 = [R 607] in
  let r36 = Sub (r34) :: r35 in
  let r37 = [R 952] in
  let r38 = R 380 :: r37 in
  let r39 = Sub (r36) :: r38 in
  let r40 = S (T T_COLON) :: r39 in
  let r41 = Sub (r24) :: r40 in
  let r42 = R 374 :: r41 in
  let r43 = [R 533] in
  let r44 = S (T T_AMPERAMPER) :: r43 in
  let r45 = [R 943] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = Sub (r44) :: r46 in
  let r48 = [R 507] in
  let r49 = S (T T_RPAREN) :: r48 in
  let r50 = R 260 :: r49 in
  let r51 = [R 261] in
  let r52 = [R 509] in
  let r53 = S (T T_RBRACKET) :: r52 in
  let r54 = [R 511] in
  let r55 = S (T T_RBRACE) :: r54 in
  let r56 = [R 423] in
  let r57 = [R 127] in
  let r58 = [R 258] in
  let r59 = S (T T_LIDENT) :: r58 in
  let r60 = [R 692] in
  let r61 = [R 30] in
  let r62 = Sub (r59) :: r61 in
  let r63 = [R 559] in
  let r64 = S (T T_COLON) :: r63 in
  let r65 = [R 114] in
  let r66 = S (T T_RPAREN) :: r65 in
  let r67 = S (N N_module_type) :: r66 in
  let r68 = R 374 :: r67 in
  let r69 = R 124 :: r68 in
  let r70 = [R 735] in
  let r71 = R 382 :: r70 in
  let r72 = [R 459] in
  let r73 = S (T T_END) :: r72 in
  let r74 = Sub (r71) :: r73 in
  let r75 = [R 255] in
  let r76 = R 380 :: r75 in
  let r77 = R 680 :: r76 in
  let r78 = R 922 :: r77 in
  let r79 = S (T T_LIDENT) :: r78 in
  let r80 = R 926 :: r79 in
  let r81 = R 374 :: r80 in
  let r82 = R 124 :: r81 in
  let r83 = [R 421] in
  let r84 = S (T T_LIDENT) :: r83 in
  let r85 = [R 924] in
  let r86 = Sub (r84) :: r85 in
  let r87 = [R 93] in
  let r88 = S (T T_FALSE) :: r87 in
  let r89 = [R 97] in
  let r90 = Sub (r88) :: r89 in
  let r91 = [R 252] in
  let r92 = R 374 :: r91 in
  let r93 = R 245 :: r92 in
  let r94 = Sub (r90) :: r93 in
  let r95 = [R 633] in
  let r96 = Sub (r94) :: r95 in
  let r97 = [R 742] in
  let r98 = R 380 :: r97 in
  let r99 = Sub (r96) :: r98 in
  let r100 = R 613 :: r99 in
  let r101 = S (T T_PLUSEQ) :: r100 in
  let r102 = Sub (r86) :: r101 in
  let r103 = R 926 :: r102 in
  let r104 = R 374 :: r103 in
  let r105 = [R 256] in
  let r106 = R 380 :: r105 in
  let r107 = R 680 :: r106 in
  let r108 = R 922 :: r107 in
  let r109 = S (T T_LIDENT) :: r108 in
  let r110 = R 926 :: r109 in
  let r111 = [R 743] in
  let r112 = R 380 :: r111 in
  let r113 = Sub (r96) :: r112 in
  let r114 = R 613 :: r113 in
  let r115 = S (T T_PLUSEQ) :: r114 in
  let r116 = Sub (r86) :: r115 in
  let r117 = [R 930] in
  let r118 = S (T T_UNDERSCORE) :: r117 in
  let r119 = [R 925] in
  let r120 = Sub (r118) :: r119 in
  let r121 = R 931 :: r120 in
  let r122 = [R 705] in
  let r123 = Sub (r121) :: r122 in
  let r124 = [R 928] in
  let r125 = S (T T_RPAREN) :: r124 in
  let r126 = [R 929] in
  let r127 = [R 706] in
  let r128 = [R 490] in
  let r129 = S (T T_DOTDOT) :: r128 in
  let r130 = [R 923] in
  let r131 = [R 491] in
  let r132 = [R 96] in
  let r133 = S (T T_RPAREN) :: r132 in
  let r134 = [R 92] in
  let r135 = [R 709] in
  let r136 = Sub (r26) :: r135 in
  let r137 = [R 241] in
  let r138 = Sub (r136) :: r137 in
  let r139 = S (T T_STAR) :: r138 in
  let r140 = Sub (r26) :: r139 in
  let r141 = [R 499] in
  let r142 = [R 609] in
  let r143 = Sub (r32) :: r142 in
  let r144 = [R 411] in
  let r145 = R 374 :: r144 in
  let r146 = Sub (r143) :: r145 in
  let r147 = [R 126] in
  let r148 = S (T T_RBRACKET) :: r147 in
  let r149 = Sub (r17) :: r148 in
  let r150 = [R 798] in
  let r151 = [R 435] in
  let r152 = [R 627] in
  let r153 = Sub (r94) :: r152 in
  let r154 = [R 892] in
  let r155 = R 380 :: r154 in
  let r156 = Sub (r153) :: r155 in
  let r157 = R 613 :: r156 in
  let r158 = S (T T_PLUSEQ) :: r157 in
  let r159 = Sub (r86) :: r158 in
  let r160 = R 926 :: r159 in
  let r161 = R 374 :: r160 in
  let r162 = [R 893] in
  let r163 = R 380 :: r162 in
  let r164 = Sub (r153) :: r163 in
  let r165 = R 613 :: r164 in
  let r166 = S (T T_PLUSEQ) :: r165 in
  let r167 = Sub (r86) :: r166 in
  let r168 = [R 611] in
  let r169 = S (T T_RBRACKET) :: r168 in
  let r170 = Sub (r19) :: r169 in
  let r171 = [R 404] in
  let r172 = Sub (r3) :: r171 in
  let r173 = S (T T_MINUSGREATER) :: r172 in
  let r174 = S (N N_pattern) :: r173 in
  let r175 = [R 694] in
  let r176 = Sub (r174) :: r175 in
  let r177 = [R 143] in
  let r178 = Sub (r176) :: r177 in
  let r179 = S (T T_WITH) :: r178 in
  let r180 = Sub (r3) :: r179 in
  let r181 = R 374 :: r180 in
  let r182 = [R 656] in
  let r183 = S (N N_fun_expr) :: r182 in
  let r184 = S (T T_COMMA) :: r183 in
  let r185 = [R 919] in
  let r186 = Sub (r34) :: r185 in
  let r187 = S (T T_COLON) :: r186 in
  let r188 = [R 661] in
  let r189 = S (N N_fun_expr) :: r188 in
  let r190 = S (T T_COMMA) :: r189 in
  let r191 = S (T T_RPAREN) :: r190 in
  let r192 = Sub (r187) :: r191 in
  let r193 = [R 921] in
  let r194 = [R 716] in
  let r195 = Sub (r34) :: r194 in
  let r196 = [R 701] in
  let r197 = Sub (r195) :: r196 in
  let r198 = [R 120] in
  let r199 = S (T T_RBRACKET) :: r198 in
  let r200 = Sub (r197) :: r199 in
  let r201 = [R 119] in
  let r202 = S (T T_RBRACKET) :: r201 in
  let r203 = [R 118] in
  let r204 = S (T T_RBRACKET) :: r203 in
  let r205 = [R 479] in
  let r206 = Sub (r59) :: r205 in
  let r207 = S (T T_BACKQUOTE) :: r206 in
  let r208 = [R 905] in
  let r209 = R 374 :: r208 in
  let r210 = Sub (r207) :: r209 in
  let r211 = [R 115] in
  let r212 = S (T T_RBRACKET) :: r211 in
  let r213 = [R 86] in
  let r214 = Sub (r84) :: r213 in
  let r215 = [R 26] in
  let r216 = [R 422] in
  let r217 = S (T T_LIDENT) :: r216 in
  let r218 = S (T T_DOT) :: r217 in
  let r219 = S (T T_UIDENT) :: r56 in
  let r220 = [R 439] in
  let r221 = Sub (r219) :: r220 in
  let r222 = [R 440] in
  let r223 = S (T T_RPAREN) :: r222 in
  let r224 = [R 424] in
  let r225 = S (T T_UIDENT) :: r224 in
  let r226 = [R 116] in
  let r227 = S (T T_RBRACKET) :: r226 in
  let r228 = [R 239] in
  let r229 = [R 237] in
  let r230 = Sub (r30) :: r229 in
  let r231 = S (T T_MINUSGREATER) :: r230 in
  let r232 = S (T T_DOT) :: r225 in
  let r233 = S (T T_LBRACKETGREATER) :: r202 in
  let r234 = [R 29] in
  let r235 = Sub (r233) :: r234 in
  let r236 = [R 113] in
  let r237 = [R 918] in
  let r238 = [R 710] in
  let r239 = Sub (r26) :: r238 in
  let r240 = [R 27] in
  let r241 = [R 711] in
  let r242 = [R 712] in
  let r243 = [R 18] in
  let r244 = Sub (r59) :: r243 in
  let r245 = [R 702] in
  let r246 = [R 697] in
  let r247 = Sub (r32) :: r246 in
  let r248 = [R 904] in
  let r249 = R 374 :: r248 in
  let r250 = Sub (r247) :: r249 in
  let r251 = [R 698] in
  let r252 = [R 375] in
  let r253 = [R 117] in
  let r254 = S (T T_RBRACKET) :: r253 in
  let r255 = Sub (r197) :: r254 in
  let r256 = [R 690] in
  let r257 = Sub (r207) :: r256 in
  let r258 = [R 121] in
  let r259 = S (T T_RBRACKET) :: r258 in
  let r260 = [R 920] in
  let r261 = [R 664] in
  let r262 = [R 665] in
  let r263 = S (T T_RPAREN) :: r262 in
  let r264 = Sub (r187) :: r263 in
  let r265 = S (T T_UNDERSCORE) :: r150 in
  let r266 = [R 788] in
  let r267 = [R 782] in
  let r268 = S (T T_END) :: r267 in
  let r269 = R 391 :: r268 in
  let r270 = R 60 :: r269 in
  let r271 = R 374 :: r270 in
  let r272 = [R 58] in
  let r273 = S (T T_RPAREN) :: r272 in
  let r274 = [R 816] in
  let r275 = [R 670] in
  let r276 = S (T T_DOTDOT) :: r275 in
  let r277 = S (T T_COMMA) :: r276 in
  let r278 = [R 671] in
  let r279 = S (T T_DOTDOT) :: r278 in
  let r280 = S (T T_COMMA) :: r279 in
  let r281 = S (T T_RPAREN) :: r280 in
  let r282 = Sub (r34) :: r281 in
  let r283 = S (T T_COLON) :: r282 in
  let r284 = [R 319] in
  let r285 = [R 320] in
  let r286 = S (T T_RPAREN) :: r285 in
  let r287 = Sub (r34) :: r286 in
  let r288 = S (T T_COLON) :: r287 in
  let r289 = [R 758] in
  let r290 = [R 756] in
  let r291 = [R 812] in
  let r292 = S (T T_RPAREN) :: r291 in
  let r293 = [R 457] in
  let r294 = S (T T_UNDERSCORE) :: r293 in
  let r295 = [R 814] in
  let r296 = S (T T_RPAREN) :: r295 in
  let r297 = Sub (r294) :: r296 in
  let r298 = R 374 :: r297 in
  let r299 = [R 815] in
  let r300 = S (T T_RPAREN) :: r299 in
  let r301 = [R 462] in
  let r302 = S (N N_module_expr) :: r301 in
  let r303 = R 374 :: r302 in
  let r304 = S (T T_OF) :: r303 in
  let r305 = [R 447] in
  let r306 = S (T T_END) :: r305 in
  let r307 = S (N N_structure) :: r306 in
  let r308 = [R 386] in
  let r309 = [R 500] in
  let r310 = R 380 :: r309 in
  let r311 = S (N N_module_expr) :: r310 in
  let r312 = R 374 :: r311 in
  let r313 = [R 501] in
  let r314 = R 380 :: r313 in
  let r315 = S (N N_module_expr) :: r314 in
  let r316 = R 374 :: r315 in
  let r317 = [R 561] in
  let r318 = S (T T_RPAREN) :: r317 in
  let r319 = [R 562] in
  let r320 = S (T T_RPAREN) :: r319 in
  let r321 = S (N N_fun_expr) :: r320 in
  let r322 = [R 433] in
  let r323 = S (T T_LIDENT) :: r322 in
  let r324 = [R 57] in
  let r325 = Sub (r323) :: r324 in
  let r326 = [R 779] in
  let r327 = Sub (r325) :: r326 in
  let r328 = R 374 :: r327 in
  let r329 = [R 434] in
  let r330 = S (T T_LIDENT) :: r329 in
  let r331 = [R 436] in
  let r332 = [R 441] in
  let r333 = [R 775] in
  let r334 = [R 776] in
  let r335 = S (T T_METAOCAML_BRACKET_CLOSE) :: r334 in
  let r336 = [R 142] in
  let r337 = Sub (r176) :: r336 in
  let r338 = S (T T_WITH) :: r337 in
  let r339 = Sub (r3) :: r338 in
  let r340 = R 374 :: r339 in
  let r341 = [R 764] in
  let r342 = S (T T_RPAREN) :: r341 in
  let r343 = [R 803] in
  let r344 = [R 206] in
  let r345 = [R 359] in
  let r346 = Sub (r24) :: r345 in
  let r347 = [R 362] in
  let r348 = Sub (r346) :: r347 in
  let r349 = [R 203] in
  let r350 = Sub (r3) :: r349 in
  let r351 = S (T T_IN) :: r350 in
  let r352 = [R 676] in
  let r353 = S (T T_DOTDOT) :: r352 in
  let r354 = S (T T_COMMA) :: r353 in
  let r355 = [R 677] in
  let r356 = S (T T_DOTDOT) :: r355 in
  let r357 = S (T T_COMMA) :: r356 in
  let r358 = S (T T_RPAREN) :: r357 in
  let r359 = Sub (r34) :: r358 in
  let r360 = S (T T_COLON) :: r359 in
  let r361 = [R 339] in
  let r362 = [R 340] in
  let r363 = S (T T_RPAREN) :: r362 in
  let r364 = Sub (r34) :: r363 in
  let r365 = S (T T_COLON) :: r364 in
  let r366 = [R 763] in
  let r367 = [R 91] in
  let r368 = [R 726] in
  let r369 = S (N N_pattern) :: r368 in
  let r370 = [R 761] in
  let r371 = S (T T_RBRACKET) :: r370 in
  let r372 = [R 286] in
  let r373 = Sub (r323) :: r372 in
  let r374 = [R 400] in
  let r375 = R 552 :: r374 in
  let r376 = R 545 :: r375 in
  let r377 = Sub (r373) :: r376 in
  let r378 = [R 760] in
  let r379 = S (T T_RBRACE) :: r378 in
  let r380 = [R 546] in
  let r381 = [R 553] in
  let r382 = S (T T_UNDERSCORE) :: r274 in
  let r383 = [R 811] in
  let r384 = Sub (r382) :: r383 in
  let r385 = [R 593] in
  let r386 = Sub (r384) :: r385 in
  let r387 = R 374 :: r386 in
  let r388 = [R 87] in
  let r389 = [R 821] in
  let r390 = S (T T_INT) :: r388 in
  let r391 = [R 755] in
  let r392 = Sub (r390) :: r391 in
  let r393 = [R 818] in
  let r394 = [R 823] in
  let r395 = S (T T_RBRACKET) :: r394 in
  let r396 = S (T T_LBRACKET) :: r395 in
  let r397 = [R 824] in
  let r398 = [R 669] in
  let r399 = S (T T_DOTDOT) :: r398 in
  let r400 = S (T T_COMMA) :: r399 in
  let r401 = [R 311] in
  let r402 = [R 312] in
  let r403 = S (T T_RPAREN) :: r402 in
  let r404 = Sub (r34) :: r403 in
  let r405 = S (T T_COLON) :: r404 in
  let r406 = [R 310] in
  let r407 = [R 101] in
  let r408 = [R 587] in
  let r409 = S (N N_pattern) :: r408 in
  let r410 = R 374 :: r409 in
  let r411 = [R 589] in
  let r412 = Sub (r384) :: r411 in
  let r413 = [R 588] in
  let r414 = Sub (r384) :: r413 in
  let r415 = S (T T_COMMA) :: r414 in
  let r416 = [R 592] in
  let r417 = [R 667] in
  let r418 = [R 303] in
  let r419 = [R 304] in
  let r420 = S (T T_RPAREN) :: r419 in
  let r421 = Sub (r34) :: r420 in
  let r422 = S (T T_COLON) :: r421 in
  let r423 = [R 302] in
  let r424 = [R 581] in
  let r425 = [R 590] in
  let r426 = [R 480] in
  let r427 = S (T T_LIDENT) :: r426 in
  let r428 = [R 591] in
  let r429 = Sub (r384) :: r428 in
  let r430 = S (T T_RPAREN) :: r429 in
  let r431 = [R 100] in
  let r432 = S (T T_RPAREN) :: r431 in
  let r433 = [R 668] in
  let r434 = [R 307] in
  let r435 = [R 308] in
  let r436 = S (T T_RPAREN) :: r435 in
  let r437 = Sub (r34) :: r436 in
  let r438 = S (T T_COLON) :: r437 in
  let r439 = [R 306] in
  let r440 = [R 826] in
  let r441 = S (T T_RPAREN) :: r440 in
  let r442 = [R 586] in
  let r443 = [R 584] in
  let r444 = [R 99] in
  let r445 = S (T T_RPAREN) :: r444 in
  let r446 = [R 825] in
  let r447 = [R 402] in
  let r448 = [R 762] in
  let r449 = [R 338] in
  let r450 = [R 594] in
  let r451 = [R 673] in
  let r452 = [R 323] in
  let r453 = [R 324] in
  let r454 = S (T T_RPAREN) :: r453 in
  let r455 = Sub (r34) :: r454 in
  let r456 = S (T T_COLON) :: r455 in
  let r457 = [R 322] in
  let r458 = [R 335] in
  let r459 = [R 336] in
  let r460 = S (T T_RPAREN) :: r459 in
  let r461 = Sub (r34) :: r460 in
  let r462 = S (T T_COLON) :: r461 in
  let r463 = [R 334] in
  let r464 = [R 675] in
  let r465 = S (T T_DOTDOT) :: r464 in
  let r466 = S (T T_COMMA) :: r465 in
  let r467 = [R 331] in
  let r468 = [R 332] in
  let r469 = S (T T_RPAREN) :: r468 in
  let r470 = Sub (r34) :: r469 in
  let r471 = S (T T_COLON) :: r470 in
  let r472 = [R 330] in
  let r473 = [R 298] in
  let r474 = [R 284] in
  let r475 = S (T T_LIDENT) :: r474 in
  let r476 = [R 297] in
  let r477 = S (T T_RPAREN) :: r476 in
  let r478 = [R 285] in
  let r479 = [R 294] in
  let r480 = [R 293] in
  let r481 = S (T T_RPAREN) :: r480 in
  let r482 = R 554 :: r481 in
  let r483 = [R 555] in
  let r484 = [R 139] in
  let r485 = Sub (r3) :: r484 in
  let r486 = S (T T_IN) :: r485 in
  let r487 = S (N N_module_expr) :: r486 in
  let r488 = R 374 :: r487 in
  let r489 = R 124 :: r488 in
  let r490 = [R 344] in
  let r491 = Sub (r24) :: r490 in
  let r492 = [R 351] in
  let r493 = R 380 :: r492 in
  let r494 = Sub (r491) :: r493 in
  let r495 = R 620 :: r494 in
  let r496 = R 374 :: r495 in
  let r497 = R 124 :: r496 in
  let r498 = [R 140] in
  let r499 = Sub (r3) :: r498 in
  let r500 = S (T T_IN) :: r499 in
  let r501 = S (N N_module_expr) :: r500 in
  let r502 = R 374 :: r501 in
  let r503 = [R 448] in
  let r504 = S (N N_module_expr) :: r503 in
  let r505 = S (T T_MINUSGREATER) :: r504 in
  let r506 = S (N N_functor_args) :: r505 in
  let r507 = [R 242] in
  let r508 = [R 243] in
  let r509 = S (T T_RPAREN) :: r508 in
  let r510 = S (N N_module_type) :: r509 in
  let r511 = [R 463] in
  let r512 = S (T T_RPAREN) :: r511 in
  let r513 = [R 460] in
  let r514 = S (N N_module_type) :: r513 in
  let r515 = S (T T_MINUSGREATER) :: r514 in
  let r516 = S (N N_functor_args) :: r515 in
  let r517 = [R 431] in
  let r518 = Sub (r59) :: r517 in
  let r519 = [R 471] in
  let r520 = Sub (r518) :: r519 in
  let r521 = [R 965] in
  let r522 = S (N N_module_type) :: r521 in
  let r523 = S (T T_EQUAL) :: r522 in
  let r524 = Sub (r520) :: r523 in
  let r525 = S (T T_TYPE) :: r524 in
  let r526 = S (T T_MODULE) :: r525 in
  let r527 = [R 699] in
  let r528 = Sub (r526) :: r527 in
  let r529 = [R 467] in
  let r530 = [R 962] in
  let r531 = Sub (r32) :: r530 in
  let r532 = S (T T_COLONEQUAL) :: r531 in
  let r533 = Sub (r373) :: r532 in
  let r534 = [R 961] in
  let r535 = R 680 :: r534 in
  let r536 = [R 681] in
  let r537 = Sub (r34) :: r536 in
  let r538 = S (T T_EQUAL) :: r537 in
  let r539 = [R 432] in
  let r540 = Sub (r59) :: r539 in
  let r541 = [R 461] in
  let r542 = S (N N_module_type) :: r541 in
  let r543 = [R 466] in
  let r544 = [R 966] in
  let r545 = [R 963] in
  let r546 = Sub (r221) :: r545 in
  let r547 = S (T T_UIDENT) :: r331 in
  let r548 = [R 964] in
  let r549 = [R 700] in
  let r550 = [R 453] in
  let r551 = [R 560] in
  let r552 = S (T T_RPAREN) :: r551 in
  let r553 = [R 717] in
  let r554 = S (N N_fun_expr) :: r553 in
  let r555 = [R 806] in
  let r556 = S (T T_RBRACKET) :: r555 in
  let r557 = [R 791] in
  let r558 = [R 723] in
  let r559 = R 547 :: r558 in
  let r560 = [R 548] in
  let r561 = [R 729] in
  let r562 = R 547 :: r561 in
  let r563 = R 556 :: r562 in
  let r564 = Sub (r373) :: r563 in
  let r565 = [R 622] in
  let r566 = Sub (r564) :: r565 in
  let r567 = [R 800] in
  let r568 = S (T T_RBRACE) :: r567 in
  let r569 = S (T T_UIDENT) :: r151 in
  let r570 = Sub (r569) :: r332 in
  let r571 = [R 269] in
  let r572 = [R 778] in
  let r573 = S (T T_END) :: r572 in
  let r574 = R 374 :: r573 in
  let r575 = [R 153] in
  let r576 = Sub (r265) :: r575 in
  let r577 = R 374 :: r576 in
  let r578 = [R 789] in
  let r579 = [R 799] in
  let r580 = S (T T_RPAREN) :: r579 in
  let r581 = S (T T_LPAREN) :: r580 in
  let r582 = S (T T_DOT) :: r581 in
  let r583 = [R 809] in
  let r584 = S (T T_RPAREN) :: r583 in
  let r585 = S (N N_module_type) :: r584 in
  let r586 = S (T T_COLON) :: r585 in
  let r587 = S (N N_module_expr) :: r586 in
  let r588 = R 374 :: r587 in
  let r589 = [R 360] in
  let r590 = Sub (r3) :: r589 in
  let r591 = S (T T_EQUAL) :: r590 in
  let r592 = [R 651] in
  let r593 = S (N N_fun_expr) :: r592 in
  let r594 = S (T T_COMMA) :: r593 in
  let r595 = [R 796] in
  let r596 = [R 769] in
  let r597 = S (T T_RPAREN) :: r596 in
  let r598 = Sub (r554) :: r597 in
  let r599 = S (T T_LPAREN) :: r598 in
  let r600 = [R 148] in
  let r601 = S (N N_fun_expr) :: r600 in
  let r602 = S (T T_THEN) :: r601 in
  let r603 = Sub (r3) :: r602 in
  let r604 = R 374 :: r603 in
  let r605 = [R 733] in
  let r606 = Sub (r176) :: r605 in
  let r607 = R 374 :: r606 in
  let r608 = [R 695] in
  let r609 = [R 405] in
  let r610 = Sub (r3) :: r609 in
  let r611 = S (T T_MINUSGREATER) :: r610 in
  let r612 = [R 300] in
  let r613 = Sub (r384) :: r612 in
  let r614 = [R 230] in
  let r615 = Sub (r613) :: r614 in
  let r616 = [R 684] in
  let r617 = Sub (r615) :: r616 in
  let r618 = [R 231] in
  let r619 = Sub (r617) :: r618 in
  let r620 = [R 135] in
  let r621 = Sub (r1) :: r620 in
  let r622 = [R 141] in
  let r623 = Sub (r621) :: r622 in
  let r624 = S (T T_MINUSGREATER) :: r623 in
  let r625 = R 543 :: r624 in
  let r626 = Sub (r619) :: r625 in
  let r627 = R 374 :: r626 in
  let r628 = [R 601] in
  let r629 = S (T T_UNDERSCORE) :: r628 in
  let r630 = [R 296] in
  let r631 = [R 295] in
  let r632 = S (T T_RPAREN) :: r631 in
  let r633 = R 554 :: r632 in
  let r634 = [R 357] in
  let r635 = [R 229] in
  let r636 = S (T T_RPAREN) :: r635 in
  let r637 = [R 299] in
  let r638 = [R 544] in
  let r639 = [R 134] in
  let r640 = Sub (r176) :: r639 in
  let r641 = R 374 :: r640 in
  let r642 = [R 646] in
  let r643 = [R 649] in
  let r644 = [R 650] in
  let r645 = S (T T_RPAREN) :: r644 in
  let r646 = Sub (r187) :: r645 in
  let r647 = [R 648] in
  let r648 = [R 795] in
  let r649 = [R 766] in
  let r650 = S (T T_RPAREN) :: r649 in
  let r651 = Sub (r3) :: r650 in
  let r652 = S (T T_LPAREN) :: r651 in
  let r653 = [R 123] in
  let r654 = S (T T_DOWNTO) :: r653 in
  let r655 = [R 151] in
  let r656 = S (T T_DONE) :: r655 in
  let r657 = Sub (r3) :: r656 in
  let r658 = S (T T_DO) :: r657 in
  let r659 = Sub (r3) :: r658 in
  let r660 = Sub (r654) :: r659 in
  let r661 = Sub (r3) :: r660 in
  let r662 = S (T T_EQUAL) :: r661 in
  let r663 = S (N N_pattern) :: r662 in
  let r664 = R 374 :: r663 in
  let r665 = [R 152] in
  let r666 = Sub (r265) :: r665 in
  let r667 = R 374 :: r666 in
  let r668 = [R 265] in
  let r669 = S (T T_SLASHGREATER) :: r668 in
  let r670 = [R 276] in
  let r671 = [R 278] in
  let r672 = [R 277] in
  let r673 = [R 272] in
  let r674 = S (T T_JSX_LIDENT_E) :: r673 in
  let r675 = [R 266] in
  let r676 = S (T T_GREATER) :: r675 in
  let r677 = Sub (r674) :: r676 in
  let r678 = [R 273] in
  let r679 = [R 198] in
  let r680 = [R 199] in
  let r681 = Sub (r176) :: r680 in
  let r682 = R 374 :: r681 in
  let r683 = [R 289] in
  let r684 = [R 290] in
  let r685 = S (T T_RPAREN) :: r684 in
  let r686 = Sub (r187) :: r685 in
  let r687 = [R 291] in
  let r688 = [R 292] in
  let r689 = [R 288] in
  let r690 = [R 719] in
  let r691 = Sub (r176) :: r690 in
  let r692 = R 374 :: r691 in
  let r693 = [R 636] in
  let r694 = [R 639] in
  let r695 = [R 640] in
  let r696 = S (T T_RPAREN) :: r695 in
  let r697 = Sub (r187) :: r696 in
  let r698 = [R 638] in
  let r699 = [R 637] in
  let r700 = Sub (r176) :: r699 in
  let r701 = R 374 :: r700 in
  let r702 = [R 696] in
  let r703 = [R 202] in
  let r704 = Sub (r3) :: r703 in
  let r705 = [R 178] in
  let r706 = [R 179] in
  let r707 = Sub (r176) :: r706 in
  let r708 = R 374 :: r707 in
  let r709 = [R 166] in
  let r710 = [R 167] in
  let r711 = Sub (r176) :: r710 in
  let r712 = R 374 :: r711 in
  let r713 = [R 200] in
  let r714 = [R 201] in
  let r715 = Sub (r176) :: r714 in
  let r716 = R 374 :: r715 in
  let r717 = [R 235] in
  let r718 = Sub (r3) :: r717 in
  let r719 = [R 172] in
  let r720 = [R 173] in
  let r721 = Sub (r176) :: r720 in
  let r722 = R 374 :: r721 in
  let r723 = [R 180] in
  let r724 = [R 181] in
  let r725 = Sub (r176) :: r724 in
  let r726 = R 374 :: r725 in
  let r727 = [R 164] in
  let r728 = [R 165] in
  let r729 = Sub (r176) :: r728 in
  let r730 = R 374 :: r729 in
  let r731 = [R 170] in
  let r732 = [R 171] in
  let r733 = Sub (r176) :: r732 in
  let r734 = R 374 :: r733 in
  let r735 = [R 168] in
  let r736 = [R 169] in
  let r737 = Sub (r176) :: r736 in
  let r738 = R 374 :: r737 in
  let r739 = [R 188] in
  let r740 = [R 189] in
  let r741 = Sub (r176) :: r740 in
  let r742 = R 374 :: r741 in
  let r743 = [R 176] in
  let r744 = [R 177] in
  let r745 = Sub (r176) :: r744 in
  let r746 = R 374 :: r745 in
  let r747 = [R 174] in
  let r748 = [R 175] in
  let r749 = Sub (r176) :: r748 in
  let r750 = R 374 :: r749 in
  let r751 = [R 184] in
  let r752 = [R 185] in
  let r753 = Sub (r176) :: r752 in
  let r754 = R 374 :: r753 in
  let r755 = [R 162] in
  let r756 = [R 163] in
  let r757 = Sub (r176) :: r756 in
  let r758 = R 374 :: r757 in
  let r759 = [R 160] in
  let r760 = [R 161] in
  let r761 = Sub (r176) :: r760 in
  let r762 = R 374 :: r761 in
  let r763 = [R 204] in
  let r764 = [R 205] in
  let r765 = Sub (r176) :: r764 in
  let r766 = R 374 :: r765 in
  let r767 = [R 158] in
  let r768 = [R 159] in
  let r769 = Sub (r176) :: r768 in
  let r770 = R 374 :: r769 in
  let r771 = [R 186] in
  let r772 = [R 187] in
  let r773 = Sub (r176) :: r772 in
  let r774 = R 374 :: r773 in
  let r775 = [R 182] in
  let r776 = [R 183] in
  let r777 = Sub (r176) :: r776 in
  let r778 = R 374 :: r777 in
  let r779 = [R 190] in
  let r780 = [R 191] in
  let r781 = Sub (r176) :: r780 in
  let r782 = R 374 :: r781 in
  let r783 = [R 192] in
  let r784 = [R 193] in
  let r785 = Sub (r176) :: r784 in
  let r786 = R 374 :: r785 in
  let r787 = [R 194] in
  let r788 = [R 195] in
  let r789 = Sub (r176) :: r788 in
  let r790 = R 374 :: r789 in
  let r791 = [R 641] in
  let r792 = [R 644] in
  let r793 = [R 645] in
  let r794 = S (T T_RPAREN) :: r793 in
  let r795 = Sub (r187) :: r794 in
  let r796 = [R 643] in
  let r797 = [R 642] in
  let r798 = Sub (r176) :: r797 in
  let r799 = R 374 :: r798 in
  let r800 = [R 196] in
  let r801 = [R 197] in
  let r802 = Sub (r176) :: r801 in
  let r803 = R 374 :: r802 in
  let r804 = [R 19] in
  let r805 = R 380 :: r804 in
  let r806 = Sub (r491) :: r805 in
  let r807 = [R 882] in
  let r808 = Sub (r3) :: r807 in
  let r809 = [R 348] in
  let r810 = Sub (r3) :: r809 in
  let r811 = S (T T_EQUAL) :: r810 in
  let r812 = Sub (r34) :: r811 in
  let r813 = S (T T_DOT) :: r812 in
  let r814 = [R 347] in
  let r815 = Sub (r3) :: r814 in
  let r816 = S (T T_EQUAL) :: r815 in
  let r817 = Sub (r34) :: r816 in
  let r818 = [R 693] in
  let r819 = [R 346] in
  let r820 = Sub (r3) :: r819 in
  let r821 = [R 883] in
  let r822 = Sub (r621) :: r821 in
  let r823 = S (T T_EQUAL) :: r822 in
  let r824 = [R 350] in
  let r825 = Sub (r3) :: r824 in
  let r826 = S (T T_EQUAL) :: r825 in
  let r827 = [R 349] in
  let r828 = Sub (r3) :: r827 in
  let r829 = [R 674] in
  let r830 = [R 327] in
  let r831 = [R 328] in
  let r832 = S (T T_RPAREN) :: r831 in
  let r833 = Sub (r34) :: r832 in
  let r834 = S (T T_COLON) :: r833 in
  let r835 = [R 326] in
  let r836 = [R 599] in
  let r837 = [R 597] in
  let r838 = [R 381] in
  let r839 = [R 216] in
  let r840 = [R 217] in
  let r841 = Sub (r176) :: r840 in
  let r842 = R 374 :: r841 in
  let r843 = [R 773] in
  let r844 = S (T T_RBRACKET) :: r843 in
  let r845 = Sub (r554) :: r844 in
  let r846 = [R 224] in
  let r847 = [R 225] in
  let r848 = Sub (r176) :: r847 in
  let r849 = R 374 :: r848 in
  let r850 = [R 771] in
  let r851 = S (T T_RBRACE) :: r850 in
  let r852 = Sub (r554) :: r851 in
  let r853 = [R 220] in
  let r854 = [R 221] in
  let r855 = Sub (r176) :: r854 in
  let r856 = R 374 :: r855 in
  let r857 = [R 210] in
  let r858 = [R 211] in
  let r859 = Sub (r176) :: r858 in
  let r860 = R 374 :: r859 in
  let r861 = [R 768] in
  let r862 = S (T T_RBRACKET) :: r861 in
  let r863 = Sub (r3) :: r862 in
  let r864 = [R 214] in
  let r865 = [R 215] in
  let r866 = Sub (r176) :: r865 in
  let r867 = R 374 :: r866 in
  let r868 = [R 767] in
  let r869 = S (T T_RBRACE) :: r868 in
  let r870 = Sub (r3) :: r869 in
  let r871 = [R 212] in
  let r872 = [R 213] in
  let r873 = Sub (r176) :: r872 in
  let r874 = R 374 :: r873 in
  let r875 = [R 770] in
  let r876 = S (T T_RPAREN) :: r875 in
  let r877 = Sub (r554) :: r876 in
  let r878 = S (T T_LPAREN) :: r877 in
  let r879 = [R 218] in
  let r880 = [R 219] in
  let r881 = Sub (r176) :: r880 in
  let r882 = R 374 :: r881 in
  let r883 = [R 774] in
  let r884 = S (T T_RBRACKET) :: r883 in
  let r885 = Sub (r554) :: r884 in
  let r886 = [R 226] in
  let r887 = [R 227] in
  let r888 = Sub (r176) :: r887 in
  let r889 = R 374 :: r888 in
  let r890 = [R 772] in
  let r891 = S (T T_RBRACE) :: r890 in
  let r892 = Sub (r554) :: r891 in
  let r893 = [R 222] in
  let r894 = [R 223] in
  let r895 = Sub (r176) :: r894 in
  let r896 = R 374 :: r895 in
  let r897 = [R 208] in
  let r898 = [R 209] in
  let r899 = Sub (r176) :: r898 in
  let r900 = R 374 :: r899 in
  let r901 = [R 647] in
  let r902 = Sub (r176) :: r901 in
  let r903 = R 374 :: r902 in
  let r904 = [R 149] in
  let r905 = Sub (r176) :: r904 in
  let r906 = R 374 :: r905 in
  let r907 = [R 146] in
  let r908 = [R 147] in
  let r909 = Sub (r176) :: r908 in
  let r910 = R 374 :: r909 in
  let r911 = [R 144] in
  let r912 = [R 145] in
  let r913 = Sub (r176) :: r912 in
  let r914 = R 374 :: r913 in
  let r915 = [R 654] in
  let r916 = [R 655] in
  let r917 = S (T T_RPAREN) :: r916 in
  let r918 = Sub (r187) :: r917 in
  let r919 = [R 653] in
  let r920 = [R 652] in
  let r921 = Sub (r176) :: r920 in
  let r922 = R 374 :: r921 in
  let r923 = [R 361] in
  let r924 = Sub (r3) :: r923 in
  let r925 = [R 363] in
  let r926 = [R 793] in
  let r927 = [R 805] in
  let r928 = [R 804] in
  let r929 = [R 808] in
  let r930 = [R 807] in
  let r931 = S (T T_LIDENT) :: r559 in
  let r932 = [R 794] in
  let r933 = S (T T_GREATERRBRACE) :: r932 in
  let r934 = [R 801] in
  let r935 = S (T T_RBRACE) :: r934 in
  let r936 = [R 623] in
  let r937 = Sub (r564) :: r936 in
  let r938 = [R 777] in
  let r939 = [R 549] in
  let r940 = Sub (r176) :: r939 in
  let r941 = R 374 :: r940 in
  let r942 = [R 790] in
  let r943 = [R 442] in
  let r944 = S (N N_module_expr) :: r943 in
  let r945 = S (T T_EQUAL) :: r944 in
  let r946 = [R 137] in
  let r947 = Sub (r3) :: r946 in
  let r948 = S (T T_IN) :: r947 in
  let r949 = Sub (r945) :: r948 in
  let r950 = Sub (r294) :: r949 in
  let r951 = R 374 :: r950 in
  let r952 = [R 443] in
  let r953 = S (N N_module_expr) :: r952 in
  let r954 = S (T T_EQUAL) :: r953 in
  let r955 = [R 444] in
  let r956 = [R 138] in
  let r957 = Sub (r3) :: r956 in
  let r958 = S (T T_IN) :: r957 in
  let r959 = R 374 :: r958 in
  let r960 = R 245 :: r959 in
  let r961 = Sub (r90) :: r960 in
  let r962 = R 374 :: r961 in
  let r963 = [R 103] in
  let r964 = Sub (r26) :: r963 in
  let r965 = [R 246] in
  let r966 = [R 279] in
  let r967 = R 374 :: r966 in
  let r968 = Sub (r143) :: r967 in
  let r969 = S (T T_COLON) :: r968 in
  let r970 = S (T T_LIDENT) :: r969 in
  let r971 = R 472 :: r970 in
  let r972 = [R 281] in
  let r973 = Sub (r971) :: r972 in
  let r974 = [R 105] in
  let r975 = S (T T_RBRACE) :: r974 in
  let r976 = [R 280] in
  let r977 = R 374 :: r976 in
  let r978 = S (T T_SEMI) :: r977 in
  let r979 = R 374 :: r978 in
  let r980 = Sub (r143) :: r979 in
  let r981 = S (T T_COLON) :: r980 in
  let r982 = [R 610] in
  let r983 = Sub (r32) :: r982 in
  let r984 = [R 104] in
  let r985 = Sub (r26) :: r984 in
  let r986 = [R 249] in
  let r987 = [R 250] in
  let r988 = Sub (r26) :: r987 in
  let r989 = [R 248] in
  let r990 = Sub (r26) :: r989 in
  let r991 = [R 247] in
  let r992 = Sub (r26) :: r991 in
  let r993 = [R 207] in
  let r994 = Sub (r176) :: r993 in
  let r995 = R 374 :: r994 in
  let r996 = [R 802] in
  let r997 = [R 780] in
  let r998 = S (T T_RPAREN) :: r997 in
  let r999 = S (N N_module_expr) :: r998 in
  let r1000 = R 374 :: r999 in
  let r1001 = [R 781] in
  let r1002 = S (T T_RPAREN) :: r1001 in
  let r1003 = [R 765] in
  let r1004 = [R 563] in
  let r1005 = S (T T_RPAREN) :: r1004 in
  let r1006 = Sub (r176) :: r1005 in
  let r1007 = R 374 :: r1006 in
  let r1008 = [R 569] in
  let r1009 = S (T T_RPAREN) :: r1008 in
  let r1010 = [R 565] in
  let r1011 = S (T T_RPAREN) :: r1010 in
  let r1012 = [R 567] in
  let r1013 = S (T T_RPAREN) :: r1012 in
  let r1014 = [R 568] in
  let r1015 = S (T T_RPAREN) :: r1014 in
  let r1016 = [R 564] in
  let r1017 = S (T T_RPAREN) :: r1016 in
  let r1018 = [R 566] in
  let r1019 = S (T T_RPAREN) :: r1018 in
  let r1020 = [R 895] in
  let r1021 = R 380 :: r1020 in
  let r1022 = Sub (r945) :: r1021 in
  let r1023 = Sub (r294) :: r1022 in
  let r1024 = R 374 :: r1023 in
  let r1025 = [R 469] in
  let r1026 = R 380 :: r1025 in
  let r1027 = R 550 :: r1026 in
  let r1028 = Sub (r59) :: r1027 in
  let r1029 = R 374 :: r1028 in
  let r1030 = R 124 :: r1029 in
  let r1031 = [R 551] in
  let r1032 = [R 896] in
  let r1033 = R 370 :: r1032 in
  let r1034 = R 380 :: r1033 in
  let r1035 = Sub (r945) :: r1034 in
  let r1036 = [R 371] in
  let r1037 = R 370 :: r1036 in
  let r1038 = R 380 :: r1037 in
  let r1039 = Sub (r945) :: r1038 in
  let r1040 = Sub (r294) :: r1039 in
  let r1041 = [R 263] in
  let r1042 = S (T T_RBRACKET) :: r1041 in
  let r1043 = Sub (r17) :: r1042 in
  let r1044 = [R 605] in
  let r1045 = [R 606] in
  let r1046 = [R 131] in
  let r1047 = S (T T_RBRACKET) :: r1046 in
  let r1048 = Sub (r19) :: r1047 in
  let r1049 = [R 901] in
  let r1050 = R 380 :: r1049 in
  let r1051 = S (N N_module_expr) :: r1050 in
  let r1052 = R 374 :: r1051 in
  let r1053 = [R 482] in
  let r1054 = S (T T_STRING) :: r1053 in
  let r1055 = [R 612] in
  let r1056 = R 380 :: r1055 in
  let r1057 = Sub (r1054) :: r1056 in
  let r1058 = S (T T_EQUAL) :: r1057 in
  let r1059 = Sub (r36) :: r1058 in
  let r1060 = S (T T_COLON) :: r1059 in
  let r1061 = Sub (r24) :: r1060 in
  let r1062 = R 374 :: r1061 in
  let r1063 = [R 608] in
  let r1064 = Sub (r34) :: r1063 in
  let r1065 = Sub (r88) :: r407 in
  let r1066 = [R 881] in
  let r1067 = R 380 :: r1066 in
  let r1068 = R 374 :: r1067 in
  let r1069 = Sub (r1065) :: r1068 in
  let r1070 = S (T T_EQUAL) :: r1069 in
  let r1071 = Sub (r90) :: r1070 in
  let r1072 = R 374 :: r1071 in
  let r1073 = [R 734] in
  let r1074 = R 380 :: r1073 in
  let r1075 = R 374 :: r1074 in
  let r1076 = R 245 :: r1075 in
  let r1077 = Sub (r90) :: r1076 in
  let r1078 = R 374 :: r1077 in
  let r1079 = R 124 :: r1078 in
  let r1080 = S (T T_COLONCOLON) :: r445 in
  let r1081 = [R 603] in
  let r1082 = [R 383] in
  let r1083 = [R 502] in
  let r1084 = R 380 :: r1083 in
  let r1085 = Sub (r221) :: r1084 in
  let r1086 = R 374 :: r1085 in
  let r1087 = [R 503] in
  let r1088 = R 380 :: r1087 in
  let r1089 = Sub (r221) :: r1088 in
  let r1090 = R 374 :: r1089 in
  let r1091 = [R 445] in
  let r1092 = S (N N_module_type) :: r1091 in
  let r1093 = S (T T_COLON) :: r1092 in
  let r1094 = [R 745] in
  let r1095 = R 380 :: r1094 in
  let r1096 = Sub (r1093) :: r1095 in
  let r1097 = Sub (r294) :: r1096 in
  let r1098 = R 374 :: r1097 in
  let r1099 = [R 470] in
  let r1100 = R 380 :: r1099 in
  let r1101 = S (N N_module_type) :: r1100 in
  let r1102 = S (T T_COLONEQUAL) :: r1101 in
  let r1103 = Sub (r59) :: r1102 in
  let r1104 = R 374 :: r1103 in
  let r1105 = [R 458] in
  let r1106 = R 380 :: r1105 in
  let r1107 = [R 748] in
  let r1108 = R 372 :: r1107 in
  let r1109 = R 380 :: r1108 in
  let r1110 = S (N N_module_type) :: r1109 in
  let r1111 = S (T T_COLON) :: r1110 in
  let r1112 = [R 373] in
  let r1113 = R 372 :: r1112 in
  let r1114 = R 380 :: r1113 in
  let r1115 = S (N N_module_type) :: r1114 in
  let r1116 = S (T T_COLON) :: r1115 in
  let r1117 = Sub (r294) :: r1116 in
  let r1118 = [R 746] in
  let r1119 = R 380 :: r1118 in
  let r1120 = [R 446] in
  let r1121 = [R 752] in
  let r1122 = R 380 :: r1121 in
  let r1123 = S (N N_module_type) :: r1122 in
  let r1124 = R 374 :: r1123 in
  let r1125 = S (T T_QUOTED_STRING_EXPR) :: r57 in
  let r1126 = [R 71] in
  let r1127 = Sub (r1125) :: r1126 in
  let r1128 = [R 81] in
  let r1129 = Sub (r1127) :: r1128 in
  let r1130 = [R 753] in
  let r1131 = R 366 :: r1130 in
  let r1132 = R 380 :: r1131 in
  let r1133 = Sub (r1129) :: r1132 in
  let r1134 = S (T T_COLON) :: r1133 in
  let r1135 = S (T T_LIDENT) :: r1134 in
  let r1136 = R 132 :: r1135 in
  let r1137 = R 953 :: r1136 in
  let r1138 = R 374 :: r1137 in
  let r1139 = [R 85] in
  let r1140 = R 368 :: r1139 in
  let r1141 = R 380 :: r1140 in
  let r1142 = Sub (r1127) :: r1141 in
  let r1143 = S (T T_EQUAL) :: r1142 in
  let r1144 = S (T T_LIDENT) :: r1143 in
  let r1145 = R 132 :: r1144 in
  let r1146 = R 953 :: r1145 in
  let r1147 = R 374 :: r1146 in
  let r1148 = [R 133] in
  let r1149 = S (T T_RBRACKET) :: r1148 in
  let r1150 = [R 72] in
  let r1151 = S (T T_END) :: r1150 in
  let r1152 = R 389 :: r1151 in
  let r1153 = R 62 :: r1152 in
  let r1154 = [R 61] in
  let r1155 = S (T T_RPAREN) :: r1154 in
  let r1156 = [R 64] in
  let r1157 = R 380 :: r1156 in
  let r1158 = Sub (r34) :: r1157 in
  let r1159 = S (T T_COLON) :: r1158 in
  let r1160 = S (T T_LIDENT) :: r1159 in
  let r1161 = R 474 :: r1160 in
  let r1162 = [R 65] in
  let r1163 = R 380 :: r1162 in
  let r1164 = Sub (r36) :: r1163 in
  let r1165 = S (T T_COLON) :: r1164 in
  let r1166 = S (T T_LIDENT) :: r1165 in
  let r1167 = R 615 :: r1166 in
  let r1168 = [R 63] in
  let r1169 = R 380 :: r1168 in
  let r1170 = Sub (r1127) :: r1169 in
  let r1171 = [R 74] in
  let r1172 = Sub (r1127) :: r1171 in
  let r1173 = S (T T_IN) :: r1172 in
  let r1174 = Sub (r570) :: r1173 in
  let r1175 = R 374 :: r1174 in
  let r1176 = [R 75] in
  let r1177 = Sub (r1127) :: r1176 in
  let r1178 = S (T T_IN) :: r1177 in
  let r1179 = Sub (r570) :: r1178 in
  let r1180 = [R 703] in
  let r1181 = Sub (r34) :: r1180 in
  let r1182 = [R 70] in
  let r1183 = Sub (r214) :: r1182 in
  let r1184 = S (T T_RBRACKET) :: r1183 in
  let r1185 = Sub (r1181) :: r1184 in
  let r1186 = [R 704] in
  let r1187 = [R 102] in
  let r1188 = Sub (r34) :: r1187 in
  let r1189 = S (T T_EQUAL) :: r1188 in
  let r1190 = Sub (r34) :: r1189 in
  let r1191 = [R 66] in
  let r1192 = R 380 :: r1191 in
  let r1193 = Sub (r1190) :: r1192 in
  let r1194 = [R 67] in
  let r1195 = [R 390] in
  let r1196 = [R 369] in
  let r1197 = R 368 :: r1196 in
  let r1198 = R 380 :: r1197 in
  let r1199 = Sub (r1127) :: r1198 in
  let r1200 = S (T T_EQUAL) :: r1199 in
  let r1201 = S (T T_LIDENT) :: r1200 in
  let r1202 = R 132 :: r1201 in
  let r1203 = R 953 :: r1202 in
  let r1204 = [R 83] in
  let r1205 = Sub (r1129) :: r1204 in
  let r1206 = S (T T_MINUSGREATER) :: r1205 in
  let r1207 = Sub (r28) :: r1206 in
  let r1208 = [R 84] in
  let r1209 = Sub (r1129) :: r1208 in
  let r1210 = [R 82] in
  let r1211 = Sub (r1129) :: r1210 in
  let r1212 = S (T T_MINUSGREATER) :: r1211 in
  let r1213 = [R 367] in
  let r1214 = R 366 :: r1213 in
  let r1215 = R 380 :: r1214 in
  let r1216 = Sub (r1129) :: r1215 in
  let r1217 = S (T T_COLON) :: r1216 in
  let r1218 = S (T T_LIDENT) :: r1217 in
  let r1219 = R 132 :: r1218 in
  let r1220 = R 953 :: r1219 in
  let r1221 = [R 384] in
  let r1222 = [R 736] in
  let r1223 = [R 740] in
  let r1224 = [R 377] in
  let r1225 = R 376 :: r1224 in
  let r1226 = R 380 :: r1225 in
  let r1227 = R 680 :: r1226 in
  let r1228 = R 922 :: r1227 in
  let r1229 = S (T T_LIDENT) :: r1228 in
  let r1230 = R 926 :: r1229 in
  let r1231 = [R 741] in
  let r1232 = [R 379] in
  let r1233 = R 378 :: r1232 in
  let r1234 = R 380 :: r1233 in
  let r1235 = R 680 :: r1234 in
  let r1236 = Sub (r129) :: r1235 in
  let r1237 = S (T T_COLONEQUAL) :: r1236 in
  let r1238 = S (T T_LIDENT) :: r1237 in
  let r1239 = R 926 :: r1238 in
  let r1240 = [R 494] in
  let r1241 = S (T T_RBRACE) :: r1240 in
  let r1242 = [R 251] in
  let r1243 = R 374 :: r1242 in
  let r1244 = R 245 :: r1243 in
  let r1245 = Sub (r90) :: r1244 in
  let r1246 = [R 492] in
  let r1247 = [R 493] in
  let r1248 = [R 497] in
  let r1249 = S (T T_RBRACE) :: r1248 in
  let r1250 = [R 496] in
  let r1251 = S (T T_RBRACE) :: r1250 in
  let r1252 = [R 43] in
  let r1253 = Sub (r1125) :: r1252 in
  let r1254 = [R 52] in
  let r1255 = Sub (r1253) :: r1254 in
  let r1256 = S (T T_EQUAL) :: r1255 in
  let r1257 = [R 899] in
  let r1258 = R 364 :: r1257 in
  let r1259 = R 380 :: r1258 in
  let r1260 = Sub (r1256) :: r1259 in
  let r1261 = S (T T_LIDENT) :: r1260 in
  let r1262 = R 132 :: r1261 in
  let r1263 = R 953 :: r1262 in
  let r1264 = R 374 :: r1263 in
  let r1265 = [R 80] in
  let r1266 = S (T T_END) :: r1265 in
  let r1267 = R 391 :: r1266 in
  let r1268 = R 60 :: r1267 in
  let r1269 = [R 948] in
  let r1270 = Sub (r3) :: r1269 in
  let r1271 = S (T T_EQUAL) :: r1270 in
  let r1272 = S (T T_LIDENT) :: r1271 in
  let r1273 = R 472 :: r1272 in
  let r1274 = R 374 :: r1273 in
  let r1275 = [R 46] in
  let r1276 = R 380 :: r1275 in
  let r1277 = [R 949] in
  let r1278 = Sub (r3) :: r1277 in
  let r1279 = S (T T_EQUAL) :: r1278 in
  let r1280 = S (T T_LIDENT) :: r1279 in
  let r1281 = R 472 :: r1280 in
  let r1282 = [R 951] in
  let r1283 = Sub (r3) :: r1282 in
  let r1284 = [R 947] in
  let r1285 = Sub (r34) :: r1284 in
  let r1286 = S (T T_COLON) :: r1285 in
  let r1287 = [R 950] in
  let r1288 = Sub (r3) :: r1287 in
  let r1289 = S (T T_EQUAL) :: r808 in
  let r1290 = [R 415] in
  let r1291 = Sub (r1289) :: r1290 in
  let r1292 = S (T T_LIDENT) :: r1291 in
  let r1293 = R 613 :: r1292 in
  let r1294 = R 374 :: r1293 in
  let r1295 = [R 47] in
  let r1296 = R 380 :: r1295 in
  let r1297 = [R 416] in
  let r1298 = Sub (r1289) :: r1297 in
  let r1299 = S (T T_LIDENT) :: r1298 in
  let r1300 = R 613 :: r1299 in
  let r1301 = [R 418] in
  let r1302 = Sub (r3) :: r1301 in
  let r1303 = S (T T_EQUAL) :: r1302 in
  let r1304 = [R 420] in
  let r1305 = Sub (r3) :: r1304 in
  let r1306 = S (T T_EQUAL) :: r1305 in
  let r1307 = Sub (r34) :: r1306 in
  let r1308 = S (T T_DOT) :: r1307 in
  let r1309 = [R 414] in
  let r1310 = Sub (r36) :: r1309 in
  let r1311 = S (T T_COLON) :: r1310 in
  let r1312 = [R 417] in
  let r1313 = Sub (r3) :: r1312 in
  let r1314 = S (T T_EQUAL) :: r1313 in
  let r1315 = [R 419] in
  let r1316 = Sub (r3) :: r1315 in
  let r1317 = S (T T_EQUAL) :: r1316 in
  let r1318 = Sub (r34) :: r1317 in
  let r1319 = S (T T_DOT) :: r1318 in
  let r1320 = [R 49] in
  let r1321 = R 380 :: r1320 in
  let r1322 = Sub (r3) :: r1321 in
  let r1323 = [R 44] in
  let r1324 = R 380 :: r1323 in
  let r1325 = R 541 :: r1324 in
  let r1326 = Sub (r1253) :: r1325 in
  let r1327 = [R 45] in
  let r1328 = R 380 :: r1327 in
  let r1329 = R 541 :: r1328 in
  let r1330 = Sub (r1253) :: r1329 in
  let r1331 = [R 76] in
  let r1332 = S (T T_RPAREN) :: r1331 in
  let r1333 = [R 39] in
  let r1334 = Sub (r1253) :: r1333 in
  let r1335 = S (T T_IN) :: r1334 in
  let r1336 = Sub (r570) :: r1335 in
  let r1337 = R 374 :: r1336 in
  let r1338 = [R 354] in
  let r1339 = R 380 :: r1338 in
  let r1340 = Sub (r491) :: r1339 in
  let r1341 = R 620 :: r1340 in
  let r1342 = R 374 :: r1341 in
  let r1343 = [R 40] in
  let r1344 = Sub (r1253) :: r1343 in
  let r1345 = S (T T_IN) :: r1344 in
  let r1346 = Sub (r570) :: r1345 in
  let r1347 = [R 78] in
  let r1348 = Sub (r325) :: r1347 in
  let r1349 = S (T T_RBRACKET) :: r1348 in
  let r1350 = [R 55] in
  let r1351 = Sub (r1253) :: r1350 in
  let r1352 = S (T T_MINUSGREATER) :: r1351 in
  let r1353 = Sub (r613) :: r1352 in
  let r1354 = [R 37] in
  let r1355 = Sub (r1353) :: r1354 in
  let r1356 = [R 38] in
  let r1357 = Sub (r1253) :: r1356 in
  let r1358 = [R 353] in
  let r1359 = R 380 :: r1358 in
  let r1360 = Sub (r491) :: r1359 in
  let r1361 = [R 79] in
  let r1362 = S (T T_RPAREN) :: r1361 in
  let r1363 = [R 542] in
  let r1364 = [R 48] in
  let r1365 = R 380 :: r1364 in
  let r1366 = Sub (r1190) :: r1365 in
  let r1367 = [R 50] in
  let r1368 = [R 392] in
  let r1369 = [R 53] in
  let r1370 = Sub (r1253) :: r1369 in
  let r1371 = S (T T_EQUAL) :: r1370 in
  let r1372 = [R 54] in
  let r1373 = [R 365] in
  let r1374 = R 364 :: r1373 in
  let r1375 = R 380 :: r1374 in
  let r1376 = Sub (r1256) :: r1375 in
  let r1377 = S (T T_LIDENT) :: r1376 in
  let r1378 = R 132 :: r1377 in
  let r1379 = R 953 :: r1378 in
  let r1380 = [R 388] in
  let r1381 = [R 887] in
  let r1382 = [R 891] in
  let r1383 = [R 885] in
  let r1384 = R 385 :: r1383 in
  let r1385 = [R 387] in
  let r1386 = R 385 :: r1385 in
  let r1387 = [R 318] in
  let r1388 = [R 315] in
  let r1389 = [R 316] in
  let r1390 = S (T T_RPAREN) :: r1389 in
  let r1391 = Sub (r34) :: r1390 in
  let r1392 = S (T T_COLON) :: r1391 in
  let r1393 = [R 314] in
  let r1394 = [R 59] in
  let r1395 = S (T T_RPAREN) :: r1394 in
  let r1396 = [R 663] in
  let r1397 = [R 662] in
  let r1398 = Sub (r176) :: r1397 in
  let r1399 = R 374 :: r1398 in
  let r1400 = [R 659] in
  let r1401 = [R 660] in
  let r1402 = S (T T_RPAREN) :: r1401 in
  let r1403 = Sub (r187) :: r1402 in
  let r1404 = [R 658] in
  let r1405 = [R 657] in
  let r1406 = Sub (r176) :: r1405 in
  let r1407 = R 374 :: r1406 in
  let r1408 = [R 128] in
  let r1409 = R 374 :: r1408 in
  let r1410 = [R 129] in
  let r1411 = R 374 :: r1410 in
  let r1412 = [R 409] in
  let r1413 = [R 498] in
  let r1414 = [R 238] in
  let r1415 = Sub (r30) :: r1414 in
  let r1416 = [R 240] in
  let r1417 = [R 25] in
  let r1418 = Sub (r86) :: r1417 in
  let r1419 = [R 28] in
  let r1420 = [R 713] in
  let r1421 = [R 714] in
  let r1422 = [R 495] in
  let r1423 = S (T T_RBRACE) :: r1422 in
  let r1424 = [R 254] in
  let r1425 = R 380 :: r1424 in
  let r1426 = R 680 :: r1425 in
  let r1427 = [R 253] in
  let r1428 = R 380 :: r1427 in
  let r1429 = R 680 :: r1428 in
  let r1430 = [R 259] in
  let r1431 = [R 262] in
  let r1432 = [R 426] in
  let r1433 = [R 429] in
  let r1434 = S (T T_RPAREN) :: r1433 in
  let r1435 = S (T T_COLONCOLON) :: r1434 in
  let r1436 = S (T T_LPAREN) :: r1435 in
  let r1437 = [R 570] in
  let r1438 = [R 571] in
  let r1439 = [R 572] in
  let r1440 = [R 573] in
  let r1441 = [R 574] in
  let r1442 = [R 575] in
  let r1443 = [R 576] in
  let r1444 = [R 577] in
  let r1445 = [R 578] in
  let r1446 = [R 579] in
  let r1447 = [R 580] in
  let r1448 = [R 906] in
  let r1449 = [R 915] in
  let r1450 = [R 394] in
  let r1451 = [R 913] in
  let r1452 = S (T T_SEMISEMI) :: r1451 in
  let r1453 = [R 914] in
  let r1454 = [R 396] in
  let r1455 = [R 399] in
  let r1456 = [R 398] in
  let r1457 = [R 397] in
  let r1458 = R 395 :: r1457 in
  let r1459 = [R 942] in
  let r1460 = S (T T_EOF) :: r1459 in
  let r1461 = R 395 :: r1460 in
  let r1462 = [R 941] in
  function
  | 0 | 2155 | 2159 | 2177 | 2181 | 2185 | 2189 | 2193 | 2197 | 2201 | 2205 | 2209 | 2213 | 2218 | 2238 -> Nothing
  | 2154 -> One ([R 0])
  | 2158 -> One ([R 1])
  | 2164 -> One ([R 2])
  | 2178 -> One ([R 3])
  | 2182 -> One ([R 4])
  | 2188 -> One ([R 5])
  | 2190 -> One ([R 6])
  | 2194 -> One ([R 7])
  | 2198 -> One ([R 8])
  | 2202 -> One ([R 9])
  | 2206 -> One ([R 10])
  | 2212 -> One ([R 11])
  | 2216 -> One ([R 12])
  | 2228 -> One ([R 13])
  | 2248 -> One ([R 14])
  | 348 -> One ([R 15])
  | 347 -> One ([R 16])
  | 2172 -> One ([R 20])
  | 2174 -> One ([R 21])
  | 227 -> One ([R 22])
  | 212 -> One ([R 23])
  | 238 -> One ([R 24])
  | 1897 -> One ([R 36])
  | 1901 -> One ([R 41])
  | 1898 -> One ([R 42])
  | 1937 -> One ([R 51])
  | 1904 -> One ([R 56])
  | 1668 -> One ([R 68])
  | 1648 -> One ([R 69])
  | 1650 -> One ([R 73])
  | 1899 -> One ([R 77])
  | 414 -> One ([R 88])
  | 301 -> One ([R 89])
  | 412 -> One ([R 90])
  | 159 -> One ([R 94])
  | 158 | 1351 -> One ([R 95])
  | 1525 -> One ([R 98])
  | 1750 -> One ([R 106])
  | 1754 -> One ([R 107])
  | 230 -> One ([R 109])
  | 218 -> One ([R 110])
  | 224 -> One ([R 111])
  | 226 -> One ([R 112])
  | 1187 -> One ([R 122])
  | 1 -> One (R 124 :: r9)
  | 62 -> One (R 124 :: r42)
  | 183 -> One (R 124 :: r181)
  | 303 -> One (R 124 :: r271)
  | 330 -> One (R 124 :: r298)
  | 349 -> One (R 124 :: r312)
  | 350 -> One (R 124 :: r316)
  | 356 -> One (R 124 :: r328)
  | 371 -> One (R 124 :: r340)
  | 406 -> One (R 124 :: r387)
  | 444 -> One (R 124 :: r410)
  | 596 -> One (R 124 :: r502)
  | 698 -> One (R 124 :: r574)
  | 701 -> One (R 124 :: r577)
  | 714 -> One (R 124 :: r588)
  | 734 -> One (R 124 :: r604)
  | 737 -> One (R 124 :: r607)
  | 743 -> One (R 124 :: r627)
  | 772 -> One (R 124 :: r641)
  | 789 -> One (R 124 :: r664)
  | 794 -> One (R 124 :: r667)
  | 824 -> One (R 124 :: r682)
  | 844 -> One (R 124 :: r692)
  | 860 -> One (R 124 :: r701)
  | 874 -> One (R 124 :: r708)
  | 880 -> One (R 124 :: r712)
  | 889 -> One (R 124 :: r716)
  | 900 -> One (R 124 :: r722)
  | 906 -> One (R 124 :: r726)
  | 912 -> One (R 124 :: r730)
  | 918 -> One (R 124 :: r734)
  | 924 -> One (R 124 :: r738)
  | 930 -> One (R 124 :: r742)
  | 936 -> One (R 124 :: r746)
  | 942 -> One (R 124 :: r750)
  | 948 -> One (R 124 :: r754)
  | 954 -> One (R 124 :: r758)
  | 960 -> One (R 124 :: r762)
  | 966 -> One (R 124 :: r766)
  | 972 -> One (R 124 :: r770)
  | 978 -> One (R 124 :: r774)
  | 984 -> One (R 124 :: r778)
  | 990 -> One (R 124 :: r782)
  | 996 -> One (R 124 :: r786)
  | 1002 -> One (R 124 :: r790)
  | 1016 -> One (R 124 :: r799)
  | 1022 -> One (R 124 :: r803)
  | 1094 -> One (R 124 :: r842)
  | 1103 -> One (R 124 :: r849)
  | 1112 -> One (R 124 :: r856)
  | 1122 -> One (R 124 :: r860)
  | 1131 -> One (R 124 :: r867)
  | 1140 -> One (R 124 :: r874)
  | 1151 -> One (R 124 :: r882)
  | 1160 -> One (R 124 :: r889)
  | 1169 -> One (R 124 :: r896)
  | 1176 -> One (R 124 :: r900)
  | 1214 -> One (R 124 :: r903)
  | 1230 -> One (R 124 :: r906)
  | 1235 -> One (R 124 :: r910)
  | 1242 -> One (R 124 :: r914)
  | 1264 -> One (R 124 :: r922)
  | 1314 -> One (R 124 :: r941)
  | 1333 -> One (R 124 :: r951)
  | 1348 -> One (R 124 :: r962)
  | 1408 -> One (R 124 :: r995)
  | 1417 -> One (R 124 :: r1000)
  | 1435 -> One (R 124 :: r1007)
  | 1466 -> One (R 124 :: r1024)
  | 1499 -> One (R 124 :: r1052)
  | 1504 -> One (R 124 :: r1062)
  | 1536 -> One (R 124 :: r1086)
  | 1537 -> One (R 124 :: r1090)
  | 1546 -> One (R 124 :: r1098)
  | 1583 -> One (R 124 :: r1124)
  | 1592 -> One (R 124 :: r1138)
  | 1593 -> One (R 124 :: r1147)
  | 1787 -> One (R 124 :: r1264)
  | 2021 -> One (R 124 :: r1399)
  | 2036 -> One (R 124 :: r1407)
  | 225 -> One ([R 130])
  | 829 -> One ([R 136])
  | 1182 -> One ([R 154])
  | 850 -> One ([R 155])
  | 887 -> One ([R 156])
  | 867 -> One ([R 157])
  | 885 -> One ([R 228])
  | 894 -> One ([R 233])
  | 898 -> One ([R 234])
  | 609 -> One ([R 244])
  | 115 -> One ([R 257])
  | 92 -> One (R 260 :: r53)
  | 96 -> One (R 260 :: r55)
  | 346 -> One ([R 264])
  | 689 -> One ([R 267])
  | 697 -> One ([R 268])
  | 691 -> One ([R 270])
  | 814 -> One ([R 271])
  | 816 -> One ([R 274])
  | 809 -> One ([R 275])
  | 1373 -> One ([R 282])
  | 1374 -> One ([R 283])
  | 1181 -> One ([R 287])
  | 470 -> One ([R 301])
  | 497 -> One ([R 305])
  | 508 -> One ([R 309])
  | 2010 -> One ([R 313])
  | 1997 -> One ([R 317])
  | 553 -> One ([R 321])
  | 1076 -> One ([R 325])
  | 580 -> One ([R 329])
  | 566 -> One ([R 333])
  | 535 -> One ([R 337])
  | 453 -> One ([R 341])
  | 534 -> One ([R 342])
  | 1081 -> One ([R 343])
  | 1049 -> One ([R 345])
  | 1086 -> One ([R 352])
  | 1902 -> One ([R 355])
  | 749 -> One ([R 356])
  | 1407 -> One ([R 358])
  | 129 -> One (R 374 :: r74)
  | 267 -> One (R 374 :: r252)
  | 343 -> One (R 374 :: r307)
  | 354 -> One (R 374 :: r321)
  | 599 -> One (R 374 :: r506)
  | 607 -> One (R 374 :: r516)
  | 1027 -> One (R 374 :: r806)
  | 1481 -> One (R 374 :: r1040)
  | 1565 -> One (R 374 :: r1117)
  | 1604 -> One (R 374 :: r1153)
  | 1610 -> One (R 374 :: r1161)
  | 1621 -> One (R 374 :: r1167)
  | 1632 -> One (R 374 :: r1170)
  | 1636 -> One (R 374 :: r1179)
  | 1657 -> One (R 374 :: r1193)
  | 1673 -> One (R 374 :: r1203)
  | 1708 -> One (R 374 :: r1220)
  | 1730 -> One (R 374 :: r1230)
  | 1740 -> One (R 374 :: r1239)
  | 1794 -> One (R 374 :: r1268)
  | 1798 -> One (R 374 :: r1281)
  | 1826 -> One (R 374 :: r1300)
  | 1866 -> One (R 374 :: r1322)
  | 1870 -> One (R 374 :: r1326)
  | 1871 -> One (R 374 :: r1330)
  | 1882 -> One (R 374 :: r1346)
  | 1890 -> One (R 374 :: r1355)
  | 1929 -> One (R 374 :: r1366)
  | 1949 -> One (R 374 :: r1379)
  | 2081 -> One (R 374 :: r1412)
  | 1729 -> One (R 376 :: r1223)
  | 1970 -> One (R 376 :: r1382)
  | 1739 -> One (R 378 :: r1231)
  | 1083 -> One (R 380 :: r838)
  | 1666 -> One (R 380 :: r1194)
  | 1727 -> One (R 380 :: r1222)
  | 1935 -> One (R 380 :: r1367)
  | 1968 -> One (R 380 :: r1381)
  | 1975 -> One (R 380 :: r1384)
  | 1985 -> One (R 380 :: r1386)
  | 2233 -> One (R 380 :: r1452)
  | 2244 -> One (R 380 :: r1458)
  | 2249 -> One (R 380 :: r1461)
  | 1535 -> One (R 382 :: r1082)
  | 1719 -> One (R 382 :: r1221)
  | 345 -> One (R 385 :: r308)
  | 1959 -> One (R 385 :: r1380)
  | 1669 -> One (R 389 :: r1195)
  | 1938 -> One (R 391 :: r1368)
  | 2231 -> One (R 393 :: r1450)
  | 2239 -> One (R 395 :: r1454)
  | 2240 -> One (R 395 :: r1455)
  | 2241 -> One (R 395 :: r1456)
  | 523 -> One ([R 401])
  | 527 -> One ([R 403])
  | 1224 -> One ([R 406])
  | 2084 -> One ([R 407])
  | 2087 -> One ([R 408])
  | 2086 -> One ([R 410])
  | 2085 -> One ([R 412])
  | 2083 -> One ([R 413])
  | 2173 -> One ([R 425])
  | 2163 -> One ([R 427])
  | 2171 -> One ([R 428])
  | 2170 -> One ([R 430])
  | 692 -> One ([R 437])
  | 695 -> One ([R 438])
  | 668 -> One ([R 449])
  | 678 -> One ([R 450])
  | 679 -> One ([R 451])
  | 677 -> One ([R 452])
  | 680 -> One ([R 454])
  | 342 -> One ([R 455])
  | 334 | 606 | 1556 -> One ([R 456])
  | 636 -> One ([R 464])
  | 613 -> One ([R 465])
  | 649 -> One ([R 468])
  | 1359 | 1812 -> One ([R 473])
  | 1614 -> One ([R 475])
  | 1612 -> One ([R 476])
  | 1615 -> One ([R 477])
  | 1613 -> One ([R 478])
  | 477 -> One ([R 481])
  | 1515 -> One ([R 483])
  | 1763 -> One ([R 484])
  | 2113 -> One ([R 485])
  | 1779 -> One ([R 486])
  | 2114 -> One ([R 487])
  | 1778 -> One ([R 488])
  | 1770 -> One ([R 489])
  | 67 | 375 -> One ([R 504])
  | 75 | 723 -> One ([R 505])
  | 103 -> One ([R 506])
  | 91 -> One ([R 508])
  | 95 -> One ([R 510])
  | 99 -> One ([R 512])
  | 82 -> One ([R 513])
  | 102 | 1279 -> One ([R 514])
  | 81 -> One ([R 515])
  | 80 -> One ([R 516])
  | 79 -> One ([R 517])
  | 78 -> One ([R 518])
  | 77 -> One ([R 519])
  | 70 | 329 | 713 -> One ([R 520])
  | 69 | 712 -> One ([R 521])
  | 68 -> One ([R 522])
  | 74 | 481 | 722 -> One ([R 523])
  | 73 | 721 -> One ([R 524])
  | 66 -> One ([R 525])
  | 71 -> One ([R 526])
  | 84 -> One ([R 527])
  | 76 -> One ([R 528])
  | 83 -> One ([R 529])
  | 72 -> One ([R 530])
  | 101 -> One ([R 531])
  | 104 -> One ([R 532])
  | 100 -> One ([R 534])
  | 261 -> One ([R 535])
  | 260 -> One (R 536 :: r250)
  | 190 -> One (R 537 :: r200)
  | 191 -> One ([R 538])
  | 524 -> One (R 539 :: r447)
  | 525 -> One ([R 540])
  | 1050 -> One (R 556 :: r823)
  | 1051 -> One ([R 557])
  | 121 -> One ([R 558])
  | 456 -> One ([R 582])
  | 454 -> One ([R 583])
  | 457 -> One ([R 585])
  | 538 -> One ([R 595])
  | 539 -> One ([R 596])
  | 540 -> One ([R 598])
  | 755 -> One ([R 600])
  | 1786 -> One ([R 604])
  | 1828 | 1847 -> One ([R 614])
  | 1625 -> One ([R 616])
  | 1623 -> One ([R 617])
  | 1626 -> One ([R 618])
  | 1624 -> One ([R 619])
  | 1911 -> One (R 620 :: r1360)
  | 1399 -> One ([R 621])
  | 1761 -> One ([R 624])
  | 1762 -> One ([R 625])
  | 1756 -> One ([R 626])
  | 2061 -> One ([R 628])
  | 2060 -> One ([R 629])
  | 2062 -> One ([R 630])
  | 2057 -> One ([R 631])
  | 2058 -> One ([R 632])
  | 2127 -> One ([R 634])
  | 2125 -> One ([R 635])
  | 458 -> One ([R 666])
  | 541 -> One ([R 672])
  | 798 -> One (R 678 :: r669)
  | 822 -> One ([R 679])
  | 812 -> One (R 682 :: r677)
  | 819 -> One ([R 683])
  | 766 -> One ([R 685])
  | 648 -> One ([R 686])
  | 610 -> One ([R 687])
  | 1184 -> One ([R 688])
  | 1183 -> One ([R 689])
  | 285 -> One ([R 691])
  | 253 -> One ([R 715])
  | 1089 -> One ([R 718])
  | 848 -> One ([R 720])
  | 1090 -> One ([R 721])
  | 849 -> One ([R 722])
  | 1320 -> One ([R 724])
  | 1321 -> One ([R 725])
  | 518 -> One ([R 727])
  | 519 -> One ([R 728])
  | 1300 -> One ([R 730])
  | 1301 -> One ([R 731])
  | 1781 -> One ([R 737])
  | 1718 -> One ([R 738])
  | 1721 -> One ([R 739])
  | 1720 -> One ([R 744])
  | 1725 -> One ([R 747])
  | 1724 -> One ([R 749])
  | 1723 -> One ([R 750])
  | 1722 -> One ([R 751])
  | 1782 -> One ([R 754])
  | 327 -> One ([R 757])
  | 324 -> One ([R 759])
  | 805 -> One ([R 783])
  | 705 -> One ([R 784])
  | 808 -> One ([R 785])
  | 807 | 886 -> One ([R 786])
  | 707 | 866 -> One ([R 787])
  | 1174 | 1213 -> One ([R 792])
  | 806 -> One ([R 797])
  | 415 -> One ([R 810])
  | 419 -> One ([R 813])
  | 420 -> One ([R 817])
  | 442 -> One ([R 819])
  | 424 -> One ([R 820])
  | 520 -> One ([R 822])
  | 441 -> One ([R 827])
  | 28 -> One ([R 828])
  | 8 -> One ([R 829])
  | 53 -> One ([R 831])
  | 52 -> One ([R 832])
  | 51 -> One ([R 833])
  | 50 -> One ([R 834])
  | 49 -> One ([R 835])
  | 48 -> One ([R 836])
  | 47 -> One ([R 837])
  | 46 -> One ([R 838])
  | 45 -> One ([R 839])
  | 44 -> One ([R 840])
  | 43 -> One ([R 841])
  | 42 -> One ([R 842])
  | 41 -> One ([R 843])
  | 40 -> One ([R 844])
  | 39 -> One ([R 845])
  | 38 -> One ([R 846])
  | 37 -> One ([R 847])
  | 36 -> One ([R 848])
  | 35 -> One ([R 849])
  | 34 -> One ([R 850])
  | 33 -> One ([R 851])
  | 32 -> One ([R 852])
  | 31 -> One ([R 853])
  | 30 -> One ([R 854])
  | 29 -> One ([R 855])
  | 27 -> One ([R 856])
  | 26 -> One ([R 857])
  | 25 -> One ([R 858])
  | 24 -> One ([R 859])
  | 23 -> One ([R 860])
  | 22 -> One ([R 861])
  | 21 -> One ([R 862])
  | 20 -> One ([R 863])
  | 19 -> One ([R 864])
  | 18 -> One ([R 865])
  | 17 -> One ([R 866])
  | 16 -> One ([R 867])
  | 15 -> One ([R 868])
  | 14 -> One ([R 869])
  | 13 -> One ([R 870])
  | 12 -> One ([R 871])
  | 11 -> One ([R 872])
  | 10 -> One ([R 873])
  | 9 -> One ([R 874])
  | 7 -> One ([R 875])
  | 6 -> One ([R 876])
  | 5 -> One ([R 877])
  | 4 -> One ([R 878])
  | 3 -> One ([R 879])
  | 1962 -> One ([R 880])
  | 1979 -> One ([R 884])
  | 1967 | 1980 -> One ([R 886])
  | 1972 -> One ([R 888])
  | 1963 -> One ([R 889])
  | 1958 -> One ([R 890])
  | 1961 -> One ([R 894])
  | 1965 -> One ([R 897])
  | 1964 -> One ([R 898])
  | 1973 -> One ([R 900])
  | 368 -> One ([R 902])
  | 367 -> One ([R 903])
  | 2222 -> One ([R 907])
  | 2223 -> One ([R 908])
  | 2225 -> One ([R 909])
  | 2226 -> One ([R 910])
  | 2224 -> One ([R 911])
  | 2221 -> One ([R 912])
  | 2227 -> One ([R 916])
  | 616 -> One (R 926 :: r533)
  | 630 -> One ([R 927])
  | 135 -> One ([R 932])
  | 138 -> One ([R 933])
  | 142 -> One ([R 934])
  | 136 -> One ([R 935])
  | 143 -> One ([R 936])
  | 139 -> One ([R 937])
  | 144 -> One ([R 938])
  | 141 -> One ([R 939])
  | 134 -> One ([R 940])
  | 416 -> One ([R 945])
  | 696 -> One ([R 946])
  | 1596 -> One ([R 954])
  | 1810 -> One ([R 955])
  | 1813 -> One ([R 956])
  | 1811 -> One ([R 957])
  | 1845 -> One ([R 958])
  | 1848 -> One ([R 959])
  | 1846 -> One ([R 960])
  | 619 -> One ([R 967])
  | 620 -> One ([R 968])
  | 1294 -> One (S (T T_WITH) :: r937)
  | 338 -> One (S (T T_TYPE) :: r304)
  | 1376 -> One (S (T T_STAR) :: r985)
  | 2229 -> One (S (T T_SEMISEMI) :: r1449)
  | 2236 -> One (S (T T_SEMISEMI) :: r1453)
  | 2160 -> One (S (T T_RPAREN) :: r134)
  | 228 | 2106 -> One (S (T T_RPAREN) :: r236)
  | 427 -> One (S (T T_RPAREN) :: r397)
  | 511 -> One (S (T T_RPAREN) :: r446)
  | 601 -> One (S (T T_RPAREN) :: r507)
  | 670 -> One (S (T T_RPAREN) :: r550)
  | 1280 -> One (S (T T_RPAREN) :: r926)
  | 1427 -> One (S (T T_RPAREN) :: r1003)
  | 2099 -> One (S (T T_RPAREN) :: r1418)
  | 2161 -> One (S (T T_RPAREN) :: r1432)
  | 1355 | 1745 -> One (S (T T_RBRACKET) :: r367)
  | 1286 -> One (S (T T_RBRACKET) :: r929)
  | 1288 -> One (S (T T_RBRACKET) :: r930)
  | 247 -> One (S (T T_QUOTE) :: r244)
  | 1634 -> One (S (T T_OPEN) :: r1175)
  | 1874 -> One (S (T T_OPEN) :: r1337)
  | 122 | 221 -> One (S (T T_MODULE) :: r69)
  | 643 -> One (S (T T_MINUSGREATER) :: r542)
  | 1384 -> One (S (T T_MINUSGREATER) :: r990)
  | 1388 -> One (S (T T_MINUSGREATER) :: r992)
  | 1695 -> One (S (T T_MINUSGREATER) :: r1209)
  | 2091 -> One (S (T T_MINUSGREATER) :: r1415)
  | 85 -> One (S (T T_LPAREN) :: r50)
  | 118 -> One (S (T T_LIDENT) :: r64)
  | 186 -> One (S (T T_LIDENT) :: r184)
  | 187 -> One (S (T T_LIDENT) :: r192)
  | 295 -> One (S (T T_LIDENT) :: r261)
  | 296 -> One (S (T T_LIDENT) :: r264)
  | 308 -> One (S (T T_LIDENT) :: r277)
  | 309 -> One (S (T T_LIDENT) :: r283)
  | 315 -> One (S (T T_LIDENT) :: r284)
  | 316 -> One (S (T T_LIDENT) :: r288)
  | 380 -> One (S (T T_LIDENT) :: r354)
  | 381 -> One (S (T T_LIDENT) :: r360)
  | 387 -> One (S (T T_LIDENT) :: r361)
  | 388 -> One (S (T T_LIDENT) :: r365)
  | 432 -> One (S (T T_LIDENT) :: r401)
  | 433 -> One (S (T T_LIDENT) :: r405)
  | 460 -> One (S (T T_LIDENT) :: r418)
  | 461 -> One (S (T T_LIDENT) :: r422)
  | 487 -> One (S (T T_LIDENT) :: r434)
  | 488 -> One (S (T T_LIDENT) :: r438)
  | 543 -> One (S (T T_LIDENT) :: r452)
  | 544 -> One (S (T T_LIDENT) :: r456)
  | 556 -> One (S (T T_LIDENT) :: r458)
  | 557 -> One (S (T T_LIDENT) :: r462)
  | 570 -> One (S (T T_LIDENT) :: r467)
  | 571 -> One (S (T T_LIDENT) :: r471)
  | 582 -> One (S (T T_LIDENT) :: r473)
  | 590 -> One (S (T T_LIDENT) :: r479)
  | 777 -> One (S (T T_LIDENT) :: r643)
  | 778 -> One (S (T T_LIDENT) :: r646)
  | 785 -> One (S (T T_LIDENT) :: r648)
  | 801 -> One (S (T T_LIDENT) :: r670)
  | 830 -> One (S (T T_LIDENT) :: r683)
  | 831 -> One (S (T T_LIDENT) :: r686)
  | 836 -> One (S (T T_LIDENT) :: r687)
  | 852 -> One (S (T T_LIDENT) :: r694)
  | 853 -> One (S (T T_LIDENT) :: r697)
  | 1008 -> One (S (T T_LIDENT) :: r792)
  | 1009 -> One (S (T T_LIDENT) :: r795)
  | 1066 -> One (S (T T_LIDENT) :: r830)
  | 1067 -> One (S (T T_LIDENT) :: r834)
  | 1256 -> One (S (T T_LIDENT) :: r915)
  | 1257 -> One (S (T T_LIDENT) :: r918)
  | 1360 -> One (S (T T_LIDENT) :: r981)
  | 1814 -> One (S (T T_LIDENT) :: r1286)
  | 1849 -> One (S (T T_LIDENT) :: r1311)
  | 1921 -> One (S (T T_LIDENT) :: r1363)
  | 2000 -> One (S (T T_LIDENT) :: r1388)
  | 2001 -> One (S (T T_LIDENT) :: r1392)
  | 2028 -> One (S (T T_LIDENT) :: r1400)
  | 2029 -> One (S (T T_LIDENT) :: r1403)
  | 322 -> One (S (T T_INT) :: r289)
  | 325 -> One (S (T T_INT) :: r290)
  | 868 -> One (S (T T_IN) :: r704)
  | 1894 -> One (S (T T_IN) :: r1357)
  | 684 -> One (S (T T_GREATERRBRACE) :: r557)
  | 1323 -> One (S (T T_GREATERRBRACE) :: r942)
  | 166 -> One (S (T T_GREATER) :: r141)
  | 2089 -> One (S (T T_GREATER) :: r1413)
  | 652 -> One (S (T T_EQUAL) :: r546)
  | 1046 -> One (S (T T_EQUAL) :: r820)
  | 1062 -> One (S (T T_EQUAL) :: r828)
  | 1270 -> One (S (T T_EQUAL) :: r924)
  | 1804 -> One (S (T T_EQUAL) :: r1283)
  | 1822 -> One (S (T T_EQUAL) :: r1288)
  | 2152 -> One (S (T T_EOF) :: r1430)
  | 2156 -> One (S (T T_EOF) :: r1431)
  | 2175 -> One (S (T T_EOF) :: r1437)
  | 2179 -> One (S (T T_EOF) :: r1438)
  | 2183 -> One (S (T T_EOF) :: r1439)
  | 2186 -> One (S (T T_EOF) :: r1440)
  | 2191 -> One (S (T T_EOF) :: r1441)
  | 2195 -> One (S (T T_EOF) :: r1442)
  | 2199 -> One (S (T T_EOF) :: r1443)
  | 2203 -> One (S (T T_EOF) :: r1444)
  | 2207 -> One (S (T T_EOF) :: r1445)
  | 2210 -> One (S (T T_EOF) :: r1446)
  | 2214 -> One (S (T T_EOF) :: r1447)
  | 2253 -> One (S (T T_EOF) :: r1462)
  | 1310 -> One (S (T T_END) :: r938)
  | 87 -> One (S (T T_DOTDOT) :: r51)
  | 160 -> One (S (T T_DOTDOT) :: r131)
  | 459 -> One (S (T T_DOTDOT) :: r417)
  | 486 -> One (S (T T_DOTDOT) :: r433)
  | 542 -> One (S (T T_DOTDOT) :: r451)
  | 1065 -> One (S (T T_DOTDOT) :: r829)
  | 1764 -> One (S (T T_DOTDOT) :: r1246)
  | 1765 -> One (S (T T_DOTDOT) :: r1247)
  | 360 | 1145 | 1202 -> One (S (T T_DOT) :: r330)
  | 2217 -> One (S (T T_DOT) :: r547)
  | 1039 -> One (S (T T_DOT) :: r817)
  | 1363 -> One (S (T T_DOT) :: r983)
  | 1382 -> One (S (T T_DOT) :: r988)
  | 1509 -> One (S (T T_DOT) :: r1064)
  | 2165 -> One (S (T T_DOT) :: r1436)
  | 161 | 1352 -> One (S (T T_COLONCOLON) :: r133)
  | 167 -> One (S (T T_COLON) :: r146)
  | 233 -> One (S (T T_COLON) :: r239)
  | 603 -> One (S (T T_COLON) :: r510)
  | 1689 -> One (S (T T_COLON) :: r1207)
  | 376 -> One (S (T T_BARRBRACKET) :: r343)
  | 394 -> One (S (T T_BARRBRACKET) :: r366)
  | 529 -> One (S (T T_BARRBRACKET) :: r448)
  | 1282 -> One (S (T T_BARRBRACKET) :: r927)
  | 1284 -> One (S (T T_BARRBRACKET) :: r928)
  | 1414 -> One (S (T T_BARRBRACKET) :: r996)
  | 274 -> One (S (T T_BAR) :: r255)
  | 306 -> One (S (N N_pattern) :: r273)
  | 474 | 757 -> One (S (N N_pattern) :: r292)
  | 405 -> One (S (N N_pattern) :: r381)
  | 471 -> One (S (N N_pattern) :: r424)
  | 501 -> One (S (N N_pattern) :: r442)
  | 536 -> One (S (N N_pattern) :: r450)
  | 1077 -> One (S (N N_pattern) :: r836)
  | 1493 -> One (S (N N_pattern) :: r1044)
  | 337 -> One (S (N N_module_type) :: r300)
  | 646 -> One (S (N N_module_type) :: r543)
  | 650 -> One (S (N N_module_type) :: r544)
  | 674 -> One (S (N N_module_type) :: r552)
  | 1339 -> One (S (N N_module_type) :: r954)
  | 1422 -> One (S (N N_module_type) :: r1002)
  | 1440 -> One (S (N N_module_type) :: r1009)
  | 1443 -> One (S (N N_module_type) :: r1011)
  | 1446 -> One (S (N N_module_type) :: r1013)
  | 1451 -> One (S (N N_module_type) :: r1015)
  | 1454 -> One (S (N N_module_type) :: r1017)
  | 1457 -> One (S (N N_module_type) :: r1019)
  | 1471 -> One (S (N N_module_type) :: r1031)
  | 353 -> One (S (N N_module_expr) :: r318)
  | 748 -> One (S (N N_let_pattern) :: r633)
  | 378 -> One (S (N N_fun_expr) :: r344)
  | 686 -> One (S (N N_fun_expr) :: r560)
  | 776 -> One (S (N N_fun_expr) :: r642)
  | 823 -> One (S (N N_fun_expr) :: r679)
  | 851 -> One (S (N N_fun_expr) :: r693)
  | 873 -> One (S (N N_fun_expr) :: r705)
  | 879 -> One (S (N N_fun_expr) :: r709)
  | 888 -> One (S (N N_fun_expr) :: r713)
  | 899 -> One (S (N N_fun_expr) :: r719)
  | 905 -> One (S (N N_fun_expr) :: r723)
  | 911 -> One (S (N N_fun_expr) :: r727)
  | 917 -> One (S (N N_fun_expr) :: r731)
  | 923 -> One (S (N N_fun_expr) :: r735)
  | 929 -> One (S (N N_fun_expr) :: r739)
  | 935 -> One (S (N N_fun_expr) :: r743)
  | 941 -> One (S (N N_fun_expr) :: r747)
  | 947 -> One (S (N N_fun_expr) :: r751)
  | 953 -> One (S (N N_fun_expr) :: r755)
  | 959 -> One (S (N N_fun_expr) :: r759)
  | 965 -> One (S (N N_fun_expr) :: r763)
  | 971 -> One (S (N N_fun_expr) :: r767)
  | 977 -> One (S (N N_fun_expr) :: r771)
  | 983 -> One (S (N N_fun_expr) :: r775)
  | 989 -> One (S (N N_fun_expr) :: r779)
  | 995 -> One (S (N N_fun_expr) :: r783)
  | 1001 -> One (S (N N_fun_expr) :: r787)
  | 1007 -> One (S (N N_fun_expr) :: r791)
  | 1021 -> One (S (N N_fun_expr) :: r800)
  | 1093 -> One (S (N N_fun_expr) :: r839)
  | 1102 -> One (S (N N_fun_expr) :: r846)
  | 1111 -> One (S (N N_fun_expr) :: r853)
  | 1121 -> One (S (N N_fun_expr) :: r857)
  | 1130 -> One (S (N N_fun_expr) :: r864)
  | 1139 -> One (S (N N_fun_expr) :: r871)
  | 1150 -> One (S (N N_fun_expr) :: r879)
  | 1159 -> One (S (N N_fun_expr) :: r886)
  | 1168 -> One (S (N N_fun_expr) :: r893)
  | 1175 -> One (S (N N_fun_expr) :: r897)
  | 1234 -> One (S (N N_fun_expr) :: r907)
  | 1241 -> One (S (N N_fun_expr) :: r911)
  | 370 -> One (Sub (r3) :: r335)
  | 593 -> One (Sub (r3) :: r483)
  | 742 -> One (Sub (r3) :: r611)
  | 1495 -> One (Sub (r3) :: r1045)
  | 2 -> One (Sub (r13) :: r14)
  | 56 -> One (Sub (r13) :: r15)
  | 60 -> One (Sub (r13) :: r22)
  | 169 -> One (Sub (r13) :: r149)
  | 181 -> One (Sub (r13) :: r170)
  | 895 -> One (Sub (r13) :: r718)
  | 1491 -> One (Sub (r13) :: r1043)
  | 1497 -> One (Sub (r13) :: r1048)
  | 1875 -> One (Sub (r13) :: r1342)
  | 503 -> One (Sub (r24) :: r443)
  | 1079 -> One (Sub (r24) :: r837)
  | 240 -> One (Sub (r26) :: r241)
  | 242 -> One (Sub (r26) :: r242)
  | 768 -> One (Sub (r26) :: r638)
  | 1381 -> One (Sub (r26) :: r986)
  | 215 -> One (Sub (r28) :: r231)
  | 1697 -> One (Sub (r28) :: r1212)
  | 214 -> One (Sub (r30) :: r228)
  | 2097 -> One (Sub (r30) :: r1416)
  | 264 -> One (Sub (r32) :: r251)
  | 623 -> One (Sub (r32) :: r535)
  | 189 -> One (Sub (r34) :: r193)
  | 290 -> One (Sub (r34) :: r260)
  | 402 -> One (Sub (r34) :: r380)
  | 498 -> One (Sub (r34) :: r441)
  | 585 -> One (Sub (r34) :: r478)
  | 626 -> One (Sub (r34) :: r538)
  | 725 -> One (Sub (r34) :: r591)
  | 750 -> One (Sub (r34) :: r634)
  | 1058 -> One (Sub (r34) :: r826)
  | 1606 -> One (Sub (r34) :: r1155)
  | 1644 -> One (Sub (r34) :: r1186)
  | 2013 -> One (Sub (r34) :: r1395)
  | 2104 -> One (Sub (r34) :: r1420)
  | 2107 -> One (Sub (r34) :: r1421)
  | 1831 -> One (Sub (r36) :: r1303)
  | 1855 -> One (Sub (r36) :: r1314)
  | 147 -> One (Sub (r59) :: r126)
  | 1040 -> One (Sub (r59) :: r818)
  | 2219 -> One (Sub (r59) :: r1448)
  | 1534 -> One (Sub (r71) :: r1081)
  | 410 -> One (Sub (r86) :: r389)
  | 153 -> One (Sub (r121) :: r127)
  | 140 -> One (Sub (r123) :: r125)
  | 1598 -> One (Sub (r123) :: r1149)
  | 157 -> One (Sub (r129) :: r130)
  | 2116 -> One (Sub (r129) :: r1426)
  | 2130 -> One (Sub (r129) :: r1429)
  | 232 -> One (Sub (r136) :: r237)
  | 740 -> One (Sub (r174) :: r608)
  | 864 -> One (Sub (r174) :: r702)
  | 257 -> One (Sub (r195) :: r245)
  | 195 -> One (Sub (r197) :: r204)
  | 209 -> One (Sub (r197) :: r227)
  | 196 -> One (Sub (r210) :: r212)
  | 197 -> One (Sub (r214) :: r215)
  | 236 -> One (Sub (r214) :: r240)
  | 2101 -> One (Sub (r214) :: r1419)
  | 199 -> One (Sub (r221) :: r223)
  | 656 -> One (Sub (r221) :: r548)
  | 1557 -> One (Sub (r221) :: r1106)
  | 282 -> One (Sub (r257) :: r259)
  | 302 -> One (Sub (r265) :: r266)
  | 369 -> One (Sub (r265) :: r333)
  | 704 -> One (Sub (r265) :: r578)
  | 728 -> One (Sub (r265) :: r594)
  | 730 -> One (Sub (r265) :: r595)
  | 783 -> One (Sub (r265) :: r647)
  | 803 -> One (Sub (r265) :: r671)
  | 810 -> One (Sub (r265) :: r672)
  | 838 -> One (Sub (r265) :: r688)
  | 840 -> One (Sub (r265) :: r689)
  | 858 -> One (Sub (r265) :: r698)
  | 1014 -> One (Sub (r265) :: r796)
  | 1262 -> One (Sub (r265) :: r919)
  | 2019 -> One (Sub (r265) :: r1396)
  | 2034 -> One (Sub (r265) :: r1404)
  | 1477 -> One (Sub (r294) :: r1035)
  | 1560 -> One (Sub (r294) :: r1111)
  | 1276 -> One (Sub (r346) :: r925)
  | 379 -> One (Sub (r348) :: r351)
  | 397 -> One (Sub (r377) :: r379)
  | 429 -> One (Sub (r384) :: r400)
  | 439 -> One (Sub (r384) :: r406)
  | 467 -> One (Sub (r384) :: r423)
  | 494 -> One (Sub (r384) :: r439)
  | 531 -> One (Sub (r384) :: r449)
  | 550 -> One (Sub (r384) :: r457)
  | 563 -> One (Sub (r384) :: r463)
  | 567 -> One (Sub (r384) :: r466)
  | 577 -> One (Sub (r384) :: r472)
  | 761 -> One (Sub (r384) :: r637)
  | 1073 -> One (Sub (r384) :: r835)
  | 1994 -> One (Sub (r384) :: r1387)
  | 2007 -> One (Sub (r384) :: r1393)
  | 421 -> One (Sub (r392) :: r393)
  | 447 -> One (Sub (r412) :: r415)
  | 475 -> One (Sub (r427) :: r430)
  | 758 -> One (Sub (r427) :: r636)
  | 1033 -> One (Sub (r427) :: r813)
  | 1832 -> One (Sub (r427) :: r1308)
  | 1856 -> One (Sub (r427) :: r1319)
  | 583 -> One (Sub (r475) :: r477)
  | 591 -> One (Sub (r475) :: r482)
  | 660 -> One (Sub (r526) :: r549)
  | 615 -> One (Sub (r528) :: r529)
  | 687 -> One (Sub (r566) :: r568)
  | 1293 -> One (Sub (r566) :: r935)
  | 690 -> One (Sub (r570) :: r571)
  | 815 -> One (Sub (r570) :: r678)
  | 1574 -> One (Sub (r570) :: r1119)
  | 746 -> One (Sub (r629) :: r630)
  | 1290 -> One (Sub (r931) :: r933)
  | 1346 -> One (Sub (r945) :: r955)
  | 1357 -> One (Sub (r964) :: r965)
  | 1358 -> One (Sub (r973) :: r975)
  | 1746 -> One (Sub (r973) :: r1241)
  | 1766 -> One (Sub (r973) :: r1249)
  | 1774 -> One (Sub (r973) :: r1251)
  | 2109 -> One (Sub (r973) :: r1423)
  | 2052 -> One (Sub (r1065) :: r1409)
  | 2064 -> One (Sub (r1065) :: r1411)
  | 1581 -> One (Sub (r1093) :: r1120)
  | 1917 -> One (Sub (r1129) :: r1362)
  | 1941 -> One (Sub (r1129) :: r1371)
  | 1886 -> One (Sub (r1181) :: r1349)
  | 1873 -> One (Sub (r1253) :: r1332)
  | 1945 -> One (Sub (r1256) :: r1372)
  | 1797 -> One (Sub (r1274) :: r1276)
  | 1825 -> One (Sub (r1294) :: r1296)
  | 872 -> One (r0)
  | 871 -> One (r2)
  | 2151 -> One (r4)
  | 2150 -> One (r5)
  | 2149 -> One (r6)
  | 2148 -> One (r7)
  | 2147 -> One (r8)
  | 59 -> One (r9)
  | 54 -> One (r10)
  | 55 -> One (r12)
  | 58 -> One (r14)
  | 57 -> One (r15)
  | 1974 -> One (r16)
  | 1978 -> One (r18)
  | 2146 -> One (r20)
  | 2145 -> One (r21)
  | 61 -> One (r22)
  | 108 | 377 | 688 | 1307 -> One (r23)
  | 111 -> One (r25)
  | 231 -> One (r27)
  | 213 -> One (r29)
  | 223 -> One (r31)
  | 246 -> One (r33)
  | 1518 -> One (r35)
  | 2144 -> One (r37)
  | 2143 -> One (r38)
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
  | 124 -> One (r61)
  | 123 -> One (r62)
  | 120 -> One (r63)
  | 119 -> One (r64)
  | 2142 -> One (r65)
  | 2141 -> One (r66)
  | 127 -> One (r67)
  | 126 -> One (r68)
  | 125 -> One (r69)
  | 1785 -> One (r70)
  | 2140 -> One (r72)
  | 2139 -> One (r73)
  | 130 -> One (r74)
  | 2071 -> One (r75)
  | 2070 -> One (r76)
  | 2069 -> One (r77)
  | 165 | 241 -> One (r83)
  | 222 -> One (r85)
  | 413 -> One (r87)
  | 1396 -> One (r89)
  | 1773 -> One (r91)
  | 1772 -> One (r92)
  | 1771 | 2063 -> One (r93)
  | 2126 -> One (r95)
  | 2138 -> One (r97)
  | 2137 -> One (r98)
  | 2136 -> One (r99)
  | 2135 -> One (r100)
  | 2134 -> One (r101)
  | 2046 -> One (r105)
  | 180 -> One (r106)
  | 179 -> One (r107)
  | 2124 -> One (r111)
  | 2123 -> One (r112)
  | 2122 -> One (r113)
  | 2121 -> One (r114)
  | 2120 -> One (r115)
  | 146 -> One (r117)
  | 149 -> One (r119)
  | 145 -> One (r120)
  | 150 -> One (r122)
  | 152 -> One (r124)
  | 151 -> One (r125)
  | 148 -> One (r126)
  | 154 -> One (r127)
  | 1749 -> One (r128)
  | 2115 -> One (r130)
  | 2112 -> One (r131)
  | 1354 -> One (r132)
  | 1353 -> One (r133)
  | 162 -> One (r134)
  | 245 -> One (r135)
  | 2096 -> One (r137)
  | 2095 -> One (r138)
  | 2094 -> One (r139)
  | 164 -> One (r140)
  | 2088 -> One (r141)
  | 1370 -> One (r142)
  | 2080 -> One (r144)
  | 2079 -> One (r145)
  | 168 -> One (r146)
  | 2078 -> One (r147)
  | 2077 -> One (r148)
  | 170 -> One (r149)
  | 171 -> One (r150)
  | 172 -> One (r151)
  | 2059 -> One (r152)
  | 2076 -> One (r154)
  | 2075 -> One (r155)
  | 2074 -> One (r156)
  | 2073 -> One (r157)
  | 2072 -> One (r158)
  | 2056 -> One (r162)
  | 2055 -> One (r163)
  | 2049 -> One (r164)
  | 2048 -> One (r165)
  | 2047 -> One (r166)
  | 2045 -> One (r168)
  | 2044 -> One (r169)
  | 182 -> One (r170)
  | 1225 -> One (r171)
  | 1223 -> One (r172)
  | 741 -> One (r173)
  | 828 -> One (r175)
  | 2043 -> One (r177)
  | 2042 -> One (r178)
  | 2041 -> One (r179)
  | 185 -> One (r180)
  | 184 -> One (r181)
  | 2040 -> One (r182)
  | 2027 -> One (r183)
  | 2026 -> One (r184)
  | 289 -> One (r185)
  | 288 | 1032 -> One (r186)
  | 2025 -> One (r188)
  | 294 -> One (r189)
  | 293 -> One (r190)
  | 292 -> One (r191)
  | 188 -> One (r192)
  | 287 -> One (r193)
  | 271 -> One (r194)
  | 254 -> One (r196)
  | 281 -> One (r198)
  | 280 -> One (r199)
  | 192 -> One (r200)
  | 194 -> One (r201)
  | 193 -> One (r202)
  | 279 -> One (r203)
  | 278 -> One (r204)
  | 211 -> One (r205)
  | 210 -> One (r206)
  | 270 -> One (r208)
  | 259 -> One (r209)
  | 273 -> One (r211)
  | 272 -> One (r212)
  | 207 | 1700 -> One (r213)
  | 208 -> One (r215)
  | 206 -> One (r216)
  | 205 -> One (r217)
  | 198 -> One (r218)
  | 204 -> One (r220)
  | 201 -> One (r222)
  | 200 -> One (r223)
  | 203 -> One (r224)
  | 202 -> One (r225)
  | 256 -> One (r226)
  | 255 -> One (r227)
  | 252 -> One (r228)
  | 251 -> One (r229)
  | 217 -> One (r230)
  | 216 -> One (r231)
  | 250 -> One (r234)
  | 229 -> One (r236)
  | 239 -> One (r237)
  | 235 -> One (r238)
  | 234 -> One (r239)
  | 237 -> One (r240)
  | 244 -> One (r241)
  | 243 -> One (r242)
  | 249 -> One (r243)
  | 248 -> One (r244)
  | 258 -> One (r245)
  | 269 -> One (r246)
  | 266 -> One (r248)
  | 263 -> One (r249)
  | 262 -> One (r250)
  | 265 -> One (r251)
  | 268 -> One (r252)
  | 277 -> One (r253)
  | 276 -> One (r254)
  | 275 -> One (r255)
  | 286 -> One (r256)
  | 284 -> One (r258)
  | 283 -> One (r259)
  | 291 -> One (r260)
  | 300 -> One (r261)
  | 299 -> One (r262)
  | 298 -> One (r263)
  | 297 -> One (r264)
  | 1416 -> One (r266)
  | 2018 -> One (r267)
  | 2017 -> One (r268)
  | 2016 -> One (r269)
  | 305 -> One (r270)
  | 304 -> One (r271)
  | 2012 -> One (r272)
  | 2011 -> One (r273)
  | 307 -> One (r274)
  | 2009 -> One (r275)
  | 1999 -> One (r276)
  | 1998 -> One (r277)
  | 1996 -> One (r278)
  | 314 -> One (r279)
  | 313 -> One (r280)
  | 312 -> One (r281)
  | 311 -> One (r282)
  | 310 -> One (r283)
  | 321 -> One (r284)
  | 320 -> One (r285)
  | 319 -> One (r286)
  | 318 -> One (r287)
  | 317 -> One (r288)
  | 323 -> One (r289)
  | 326 -> One (r290)
  | 485 -> One (r291)
  | 484 -> One (r292)
  | 333 -> One (r293)
  | 336 -> One (r295)
  | 335 -> One (r296)
  | 332 -> One (r297)
  | 331 -> One (r298)
  | 1993 -> One (r299)
  | 1992 -> One (r300)
  | 1991 -> One (r301)
  | 341 -> One (r302)
  | 340 -> One (r303)
  | 339 -> One (r304)
  | 1990 -> One (r305)
  | 1989 -> One (r306)
  | 344 -> One (r307)
  | 1988 -> One (r308)
  | 1465 -> One (r309)
  | 1464 -> One (r310)
  | 1463 -> One (r311)
  | 1462 -> One (r312)
  | 1461 -> One (r313)
  | 1460 -> One (r314)
  | 352 -> One (r315)
  | 351 -> One (r316)
  | 673 -> One (r317)
  | 672 -> One (r318)
  | 1450 -> One (r319)
  | 1449 -> One (r320)
  | 355 -> One (r321)
  | 359 -> One (r322)
  | 365 -> One (r324)
  | 366 -> One (r326)
  | 358 -> One (r327)
  | 357 -> One (r328)
  | 363 -> One (r329)
  | 361 -> One (r330)
  | 362 -> One (r331)
  | 364 -> One (r332)
  | 1434 -> One (r333)
  | 1433 -> One (r334)
  | 1432 -> One (r335)
  | 1431 -> One (r336)
  | 1430 -> One (r337)
  | 1429 -> One (r338)
  | 373 -> One (r339)
  | 372 -> One (r340)
  | 1426 -> One (r341)
  | 1425 -> One (r342)
  | 1413 -> One (r343)
  | 1412 -> One (r344)
  | 581 -> One (r345)
  | 1278 -> One (r347)
  | 1275 -> One (r349)
  | 1274 -> One (r350)
  | 1273 -> One (r351)
  | 565 -> One (r352)
  | 555 -> One (r353)
  | 554 -> One (r354)
  | 533 -> One (r355)
  | 386 -> One (r356)
  | 385 -> One (r357)
  | 384 -> One (r358)
  | 383 -> One (r359)
  | 382 -> One (r360)
  | 393 -> One (r361)
  | 392 -> One (r362)
  | 391 -> One (r363)
  | 390 -> One (r364)
  | 389 -> One (r365)
  | 528 -> One (r366)
  | 396 -> One (r367)
  | 517 -> One (r368)
  | 516 -> One (r370)
  | 515 -> One (r371)
  | 398 -> One (r372)
  | 522 -> One (r374)
  | 404 -> One (r375)
  | 401 -> One (r376)
  | 400 -> One (r378)
  | 399 -> One (r379)
  | 403 -> One (r380)
  | 521 -> One (r381)
  | 417 | 1057 -> One (r383)
  | 418 -> One (r385)
  | 408 -> One (r386)
  | 407 -> One (r387)
  | 409 -> One (r388)
  | 411 -> One (r389)
  | 423 -> One (r391)
  | 422 -> One (r393)
  | 514 -> One (r394)
  | 513 -> One (r395)
  | 426 -> One (r396)
  | 428 -> One (r397)
  | 507 -> One (r398)
  | 431 -> One (r399)
  | 430 -> One (r400)
  | 438 -> One (r401)
  | 437 -> One (r402)
  | 436 -> One (r403)
  | 435 -> One (r404)
  | 434 -> One (r405)
  | 440 -> One (r406)
  | 443 -> One (r407)
  | 506 -> One (r408)
  | 446 -> One (r409)
  | 445 -> One (r410)
  | 448 | 724 -> One (r411)
  | 451 -> One (r413)
  | 450 -> One (r414)
  | 449 -> One (r415)
  | 455 -> One (r416)
  | 469 -> One (r417)
  | 466 -> One (r418)
  | 465 -> One (r419)
  | 464 -> One (r420)
  | 463 -> One (r421)
  | 462 -> One (r422)
  | 468 -> One (r423)
  | 472 -> One (r424)
  | 505 -> One (r425)
  | 476 -> One (r426)
  | 480 -> One (r428)
  | 479 -> One (r429)
  | 478 -> One (r430)
  | 483 -> One (r431)
  | 482 -> One (r432)
  | 496 -> One (r433)
  | 493 -> One (r434)
  | 492 -> One (r435)
  | 491 -> One (r436)
  | 490 -> One (r437)
  | 489 -> One (r438)
  | 495 -> One (r439)
  | 500 -> One (r440)
  | 499 -> One (r441)
  | 502 -> One (r442)
  | 504 -> One (r443)
  | 510 -> One (r444)
  | 509 -> One (r445)
  | 512 -> One (r446)
  | 526 -> One (r447)
  | 530 -> One (r448)
  | 532 -> One (r449)
  | 537 -> One (r450)
  | 552 -> One (r451)
  | 549 -> One (r452)
  | 548 -> One (r453)
  | 547 -> One (r454)
  | 546 -> One (r455)
  | 545 -> One (r456)
  | 551 -> One (r457)
  | 562 -> One (r458)
  | 561 -> One (r459)
  | 560 -> One (r460)
  | 559 -> One (r461)
  | 558 -> One (r462)
  | 564 -> One (r463)
  | 579 -> One (r464)
  | 569 -> One (r465)
  | 568 -> One (r466)
  | 576 -> One (r467)
  | 575 -> One (r468)
  | 574 -> One (r469)
  | 573 -> One (r470)
  | 572 -> One (r471)
  | 578 -> One (r472)
  | 589 -> One (r473)
  | 584 -> One (r474)
  | 588 -> One (r476)
  | 587 -> One (r477)
  | 586 -> One (r478)
  | 1406 -> One (r479)
  | 1405 -> One (r480)
  | 1404 -> One (r481)
  | 592 -> One (r482)
  | 1403 -> One (r483)
  | 1332 -> One (r484)
  | 1331 -> One (r485)
  | 1330 -> One (r486)
  | 1329 -> One (r487)
  | 1328 -> One (r488)
  | 595 -> One (r489)
  | 1029 -> One (r490)
  | 1402 -> One (r492)
  | 1401 -> One (r493)
  | 1400 -> One (r494)
  | 1398 -> One (r495)
  | 1397 -> One (r496)
  | 1960 -> One (r497)
  | 1327 -> One (r498)
  | 682 -> One (r499)
  | 681 -> One (r500)
  | 598 -> One (r501)
  | 597 -> One (r502)
  | 669 -> One (r503)
  | 667 -> One (r504)
  | 666 -> One (r505)
  | 600 -> One (r506)
  | 602 -> One (r507)
  | 665 -> One (r508)
  | 664 -> One (r509)
  | 604 -> One (r510)
  | 663 -> One (r511)
  | 662 -> One (r512)
  | 614 -> One (r513)
  | 612 -> One (r514)
  | 611 -> One (r515)
  | 608 -> One (r516)
  | 642 -> One (r517)
  | 641 -> One (r519)
  | 635 -> One (r521)
  | 634 -> One (r522)
  | 633 -> One (r523)
  | 632 -> One (r524)
  | 631 -> One (r525)
  | 658 -> One (r527)
  | 659 -> One (r529)
  | 622 -> One (r530)
  | 621 -> One (r531)
  | 618 -> One (r532)
  | 617 -> One (r533)
  | 625 -> One (r534)
  | 624 -> One (r535)
  | 629 -> One (r536)
  | 628 -> One (r537)
  | 627 -> One (r538)
  | 640 -> One (r539)
  | 645 -> One (r541)
  | 644 -> One (r542)
  | 647 -> One (r543)
  | 651 -> One (r544)
  | 654 -> One (r545)
  | 653 -> One (r546)
  | 655 | 694 -> One (r547)
  | 657 -> One (r548)
  | 661 -> One (r549)
  | 671 -> One (r550)
  | 676 -> One (r551)
  | 675 -> One (r552)
  | 1088 -> One (r553)
  | 1326 -> One (r555)
  | 1325 -> One (r556)
  | 1322 -> One (r557)
  | 1319 -> One (r558)
  | 685 -> One (r559)
  | 1318 -> One (r560)
  | 1299 -> One (r561)
  | 1298 -> One (r562)
  | 1297 -> One (r563)
  | 1302 -> One (r565)
  | 1313 -> One (r567)
  | 1312 -> One (r568)
  | 693 -> One (r571)
  | 1309 -> One (r572)
  | 700 -> One (r573)
  | 699 -> One (r574)
  | 1308 -> One (r575)
  | 703 -> One (r576)
  | 702 -> One (r577)
  | 706 -> One (r578)
  | 711 -> One (r579)
  | 710 -> One (r580)
  | 709 | 1306 -> One (r581)
  | 1305 -> One (r582)
  | 720 -> One (r583)
  | 719 -> One (r584)
  | 718 -> One (r585)
  | 717 -> One (r586)
  | 716 -> One (r587)
  | 715 -> One (r588)
  | 1269 -> One (r589)
  | 727 -> One (r590)
  | 726 -> One (r591)
  | 1268 -> One (r592)
  | 1255 -> One (r593)
  | 729 -> One (r594)
  | 731 -> One (r595)
  | 1092 | 1248 -> One (r596)
  | 1091 | 1247 -> One (r597)
  | 733 | 843 -> One (r598)
  | 732 | 842 -> One (r599)
  | 1240 -> One (r600)
  | 1229 -> One (r601)
  | 1228 -> One (r602)
  | 736 -> One (r603)
  | 735 -> One (r604)
  | 1227 -> One (r605)
  | 739 -> One (r606)
  | 738 -> One (r607)
  | 1226 -> One (r608)
  | 1222 -> One (r609)
  | 1221 -> One (r610)
  | 1220 -> One (r611)
  | 763 -> One (r612)
  | 765 -> One (r614)
  | 1056 -> One (r616)
  | 764 -> One (r618)
  | 1054 -> One (r620)
  | 1219 -> One (r622)
  | 771 -> One (r623)
  | 770 -> One (r624)
  | 767 -> One (r625)
  | 745 -> One (r626)
  | 744 -> One (r627)
  | 747 -> One (r628)
  | 756 -> One (r630)
  | 754 -> One (r631)
  | 753 -> One (r632)
  | 752 -> One (r633)
  | 751 -> One (r634)
  | 760 -> One (r635)
  | 759 -> One (r636)
  | 762 -> One (r637)
  | 769 -> One (r638)
  | 775 -> One (r639)
  | 774 -> One (r640)
  | 773 -> One (r641)
  | 1218 -> One (r642)
  | 782 -> One (r643)
  | 781 -> One (r644)
  | 780 -> One (r645)
  | 779 -> One (r646)
  | 784 -> One (r647)
  | 786 -> One (r648)
  | 1120 | 1195 -> One (r649)
  | 1119 | 1194 -> One (r650)
  | 788 | 1118 -> One (r651)
  | 787 | 1117 -> One (r652)
  | 1188 -> One (r653)
  | 1193 -> One (r655)
  | 1192 -> One (r656)
  | 1191 -> One (r657)
  | 1190 -> One (r658)
  | 1189 -> One (r659)
  | 1186 -> One (r660)
  | 793 -> One (r661)
  | 792 -> One (r662)
  | 791 -> One (r663)
  | 790 -> One (r664)
  | 797 -> One (r665)
  | 796 -> One (r666)
  | 795 -> One (r667)
  | 800 -> One (r668)
  | 799 -> One (r669)
  | 802 -> One (r670)
  | 804 -> One (r671)
  | 811 -> One (r672)
  | 818 -> One (r673)
  | 821 -> One (r675)
  | 820 -> One (r676)
  | 813 -> One (r677)
  | 817 -> One (r678)
  | 1185 -> One (r679)
  | 827 -> One (r680)
  | 826 -> One (r681)
  | 825 -> One (r682)
  | 835 -> One (r683)
  | 834 -> One (r684)
  | 833 -> One (r685)
  | 832 -> One (r686)
  | 837 -> One (r687)
  | 839 -> One (r688)
  | 841 -> One (r689)
  | 847 -> One (r690)
  | 846 -> One (r691)
  | 845 -> One (r692)
  | 1087 -> One (r693)
  | 857 -> One (r694)
  | 856 -> One (r695)
  | 855 -> One (r696)
  | 854 -> One (r697)
  | 859 -> One (r698)
  | 863 -> One (r699)
  | 862 -> One (r700)
  | 861 -> One (r701)
  | 865 -> One (r702)
  | 870 -> One (r703)
  | 869 -> One (r704)
  | 878 -> One (r705)
  | 877 -> One (r706)
  | 876 -> One (r707)
  | 875 -> One (r708)
  | 884 -> One (r709)
  | 883 -> One (r710)
  | 882 -> One (r711)
  | 881 -> One (r712)
  | 893 -> One (r713)
  | 892 -> One (r714)
  | 891 -> One (r715)
  | 890 -> One (r716)
  | 897 -> One (r717)
  | 896 -> One (r718)
  | 904 -> One (r719)
  | 903 -> One (r720)
  | 902 -> One (r721)
  | 901 -> One (r722)
  | 910 -> One (r723)
  | 909 -> One (r724)
  | 908 -> One (r725)
  | 907 -> One (r726)
  | 916 -> One (r727)
  | 915 -> One (r728)
  | 914 -> One (r729)
  | 913 -> One (r730)
  | 922 -> One (r731)
  | 921 -> One (r732)
  | 920 -> One (r733)
  | 919 -> One (r734)
  | 928 -> One (r735)
  | 927 -> One (r736)
  | 926 -> One (r737)
  | 925 -> One (r738)
  | 934 -> One (r739)
  | 933 -> One (r740)
  | 932 -> One (r741)
  | 931 -> One (r742)
  | 940 -> One (r743)
  | 939 -> One (r744)
  | 938 -> One (r745)
  | 937 -> One (r746)
  | 946 -> One (r747)
  | 945 -> One (r748)
  | 944 -> One (r749)
  | 943 -> One (r750)
  | 952 -> One (r751)
  | 951 -> One (r752)
  | 950 -> One (r753)
  | 949 -> One (r754)
  | 958 -> One (r755)
  | 957 -> One (r756)
  | 956 -> One (r757)
  | 955 -> One (r758)
  | 964 -> One (r759)
  | 963 -> One (r760)
  | 962 -> One (r761)
  | 961 -> One (r762)
  | 970 -> One (r763)
  | 969 -> One (r764)
  | 968 -> One (r765)
  | 967 -> One (r766)
  | 976 -> One (r767)
  | 975 -> One (r768)
  | 974 -> One (r769)
  | 973 -> One (r770)
  | 982 -> One (r771)
  | 981 -> One (r772)
  | 980 -> One (r773)
  | 979 -> One (r774)
  | 988 -> One (r775)
  | 987 -> One (r776)
  | 986 -> One (r777)
  | 985 -> One (r778)
  | 994 -> One (r779)
  | 993 -> One (r780)
  | 992 -> One (r781)
  | 991 -> One (r782)
  | 1000 -> One (r783)
  | 999 -> One (r784)
  | 998 -> One (r785)
  | 997 -> One (r786)
  | 1006 -> One (r787)
  | 1005 -> One (r788)
  | 1004 -> One (r789)
  | 1003 -> One (r790)
  | 1020 -> One (r791)
  | 1013 -> One (r792)
  | 1012 -> One (r793)
  | 1011 -> One (r794)
  | 1010 -> One (r795)
  | 1015 -> One (r796)
  | 1019 -> One (r797)
  | 1018 -> One (r798)
  | 1017 -> One (r799)
  | 1026 -> One (r800)
  | 1025 -> One (r801)
  | 1024 -> One (r802)
  | 1023 -> One (r803)
  | 1085 -> One (r804)
  | 1082 -> One (r805)
  | 1028 -> One (r806)
  | 1031 -> One (r807)
  | 1030 -> One (r808)
  | 1038 -> One (r809)
  | 1037 -> One (r810)
  | 1036 -> One (r811)
  | 1035 -> One (r812)
  | 1034 -> One (r813)
  | 1045 -> One (r814)
  | 1044 -> One (r815)
  | 1043 -> One (r816)
  | 1042 -> One (r817)
  | 1041 -> One (r818)
  | 1048 -> One (r819)
  | 1047 -> One (r820)
  | 1055 -> One (r821)
  | 1053 -> One (r822)
  | 1052 -> One (r823)
  | 1061 -> One (r824)
  | 1060 -> One (r825)
  | 1059 -> One (r826)
  | 1064 -> One (r827)
  | 1063 -> One (r828)
  | 1075 -> One (r829)
  | 1072 -> One (r830)
  | 1071 -> One (r831)
  | 1070 -> One (r832)
  | 1069 -> One (r833)
  | 1068 -> One (r834)
  | 1074 -> One (r835)
  | 1078 -> One (r836)
  | 1080 -> One (r837)
  | 1084 -> One (r838)
  | 1098 -> One (r839)
  | 1097 -> One (r840)
  | 1096 -> One (r841)
  | 1095 -> One (r842)
  | 1101 | 1251 -> One (r843)
  | 1100 | 1250 -> One (r844)
  | 1099 | 1249 -> One (r845)
  | 1107 -> One (r846)
  | 1106 -> One (r847)
  | 1105 -> One (r848)
  | 1104 -> One (r849)
  | 1110 | 1254 -> One (r850)
  | 1109 | 1253 -> One (r851)
  | 1108 | 1252 -> One (r852)
  | 1116 -> One (r853)
  | 1115 -> One (r854)
  | 1114 -> One (r855)
  | 1113 -> One (r856)
  | 1126 -> One (r857)
  | 1125 -> One (r858)
  | 1124 -> One (r859)
  | 1123 -> One (r860)
  | 1129 | 1198 -> One (r861)
  | 1128 | 1197 -> One (r862)
  | 1127 | 1196 -> One (r863)
  | 1135 -> One (r864)
  | 1134 -> One (r865)
  | 1133 -> One (r866)
  | 1132 -> One (r867)
  | 1138 | 1201 -> One (r868)
  | 1137 | 1200 -> One (r869)
  | 1136 | 1199 -> One (r870)
  | 1144 -> One (r871)
  | 1143 -> One (r872)
  | 1142 -> One (r873)
  | 1141 -> One (r874)
  | 1149 | 1206 -> One (r875)
  | 1148 | 1205 -> One (r876)
  | 1147 | 1204 -> One (r877)
  | 1146 | 1203 -> One (r878)
  | 1155 -> One (r879)
  | 1154 -> One (r880)
  | 1153 -> One (r881)
  | 1152 -> One (r882)
  | 1158 | 1209 -> One (r883)
  | 1157 | 1208 -> One (r884)
  | 1156 | 1207 -> One (r885)
  | 1164 -> One (r886)
  | 1163 -> One (r887)
  | 1162 -> One (r888)
  | 1161 -> One (r889)
  | 1167 | 1212 -> One (r890)
  | 1166 | 1211 -> One (r891)
  | 1165 | 1210 -> One (r892)
  | 1173 -> One (r893)
  | 1172 -> One (r894)
  | 1171 -> One (r895)
  | 1170 -> One (r896)
  | 1180 -> One (r897)
  | 1179 -> One (r898)
  | 1178 -> One (r899)
  | 1177 -> One (r900)
  | 1217 -> One (r901)
  | 1216 -> One (r902)
  | 1215 -> One (r903)
  | 1233 -> One (r904)
  | 1232 -> One (r905)
  | 1231 -> One (r906)
  | 1239 -> One (r907)
  | 1238 -> One (r908)
  | 1237 -> One (r909)
  | 1236 -> One (r910)
  | 1246 -> One (r911)
  | 1245 -> One (r912)
  | 1244 -> One (r913)
  | 1243 -> One (r914)
  | 1261 -> One (r915)
  | 1260 -> One (r916)
  | 1259 -> One (r917)
  | 1258 -> One (r918)
  | 1263 -> One (r919)
  | 1267 -> One (r920)
  | 1266 -> One (r921)
  | 1265 -> One (r922)
  | 1272 -> One (r923)
  | 1271 -> One (r924)
  | 1277 -> One (r925)
  | 1281 -> One (r926)
  | 1283 -> One (r927)
  | 1285 -> One (r928)
  | 1287 -> One (r929)
  | 1289 -> One (r930)
  | 1292 -> One (r932)
  | 1291 -> One (r933)
  | 1304 -> One (r934)
  | 1303 -> One (r935)
  | 1296 -> One (r936)
  | 1295 -> One (r937)
  | 1311 -> One (r938)
  | 1317 -> One (r939)
  | 1316 -> One (r940)
  | 1315 -> One (r941)
  | 1324 -> One (r942)
  | 1338 -> One (r943)
  | 1337 -> One (r944)
  | 1345 -> One (r946)
  | 1344 -> One (r947)
  | 1343 -> One (r948)
  | 1336 -> One (r949)
  | 1335 -> One (r950)
  | 1334 -> One (r951)
  | 1342 -> One (r952)
  | 1341 -> One (r953)
  | 1340 -> One (r954)
  | 1347 -> One (r955)
  | 1395 -> One (r956)
  | 1394 -> One (r957)
  | 1393 -> One (r958)
  | 1392 -> One (r959)
  | 1356 -> One (r960)
  | 1350 -> One (r961)
  | 1349 -> One (r962)
  | 1380 -> One (r963)
  | 1379 -> One (r965)
  | 1375 -> One (r972)
  | 1372 -> One (r974)
  | 1371 -> One (r975)
  | 1369 -> One (r976)
  | 1368 -> One (r977)
  | 1367 -> One (r978)
  | 1366 -> One (r979)
  | 1362 -> One (r980)
  | 1361 -> One (r981)
  | 1365 -> One (r982)
  | 1364 -> One (r983)
  | 1378 -> One (r984)
  | 1377 -> One (r985)
  | 1391 -> One (r986)
  | 1387 -> One (r987)
  | 1383 -> One (r988)
  | 1386 -> One (r989)
  | 1385 -> One (r990)
  | 1390 -> One (r991)
  | 1389 -> One (r992)
  | 1411 -> One (r993)
  | 1410 -> One (r994)
  | 1409 -> One (r995)
  | 1415 -> One (r996)
  | 1421 -> One (r997)
  | 1420 -> One (r998)
  | 1419 -> One (r999)
  | 1418 -> One (r1000)
  | 1424 -> One (r1001)
  | 1423 -> One (r1002)
  | 1428 -> One (r1003)
  | 1439 -> One (r1004)
  | 1438 -> One (r1005)
  | 1437 -> One (r1006)
  | 1436 -> One (r1007)
  | 1442 -> One (r1008)
  | 1441 -> One (r1009)
  | 1445 -> One (r1010)
  | 1444 -> One (r1011)
  | 1448 -> One (r1012)
  | 1447 -> One (r1013)
  | 1453 -> One (r1014)
  | 1452 -> One (r1015)
  | 1456 -> One (r1016)
  | 1455 -> One (r1017)
  | 1459 -> One (r1018)
  | 1458 -> One (r1019)
  | 1490 -> One (r1020)
  | 1489 -> One (r1021)
  | 1488 -> One (r1022)
  | 1476 -> One (r1023)
  | 1475 -> One (r1024)
  | 1474 -> One (r1025)
  | 1473 -> One (r1026)
  | 1470 -> One (r1027)
  | 1469 -> One (r1028)
  | 1468 -> One (r1029)
  | 1467 -> One (r1030)
  | 1472 -> One (r1031)
  | 1487 -> One (r1032)
  | 1480 -> One (r1033)
  | 1479 -> One (r1034)
  | 1478 -> One (r1035)
  | 1486 -> One (r1036)
  | 1485 -> One (r1037)
  | 1484 -> One (r1038)
  | 1483 -> One (r1039)
  | 1482 -> One (r1040)
  | 1984 -> One (r1041)
  | 1983 -> One (r1042)
  | 1492 -> One (r1043)
  | 1494 -> One (r1044)
  | 1496 -> One (r1045)
  | 1982 -> One (r1046)
  | 1981 -> One (r1047)
  | 1498 -> One (r1048)
  | 1503 -> One (r1049)
  | 1502 -> One (r1050)
  | 1501 -> One (r1051)
  | 1500 -> One (r1052)
  | 1514 -> One (r1053)
  | 1517 -> One (r1055)
  | 1516 -> One (r1056)
  | 1513 -> One (r1057)
  | 1512 -> One (r1058)
  | 1508 -> One (r1059)
  | 1507 -> One (r1060)
  | 1506 -> One (r1061)
  | 1505 -> One (r1062)
  | 1511 -> One (r1063)
  | 1510 -> One (r1064)
  | 1530 -> One (r1066)
  | 1529 -> One (r1067)
  | 1528 -> One (r1068)
  | 1523 -> One (r1069)
  | 1533 -> One (r1073)
  | 1532 -> One (r1074)
  | 1531 -> One (r1075)
  | 1591 -> One (r1076)
  | 1590 -> One (r1077)
  | 1589 -> One (r1078)
  | 1588 -> One (r1079)
  | 1527 -> One (r1080)
  | 1784 -> One (r1081)
  | 1783 -> One (r1082)
  | 1545 -> One (r1083)
  | 1544 -> One (r1084)
  | 1543 -> One (r1085)
  | 1542 -> One (r1086)
  | 1541 -> One (r1087)
  | 1540 -> One (r1088)
  | 1539 -> One (r1089)
  | 1538 -> One (r1090)
  | 1578 -> One (r1091)
  | 1577 -> One (r1092)
  | 1580 -> One (r1094)
  | 1579 -> One (r1095)
  | 1573 -> One (r1096)
  | 1555 -> One (r1097)
  | 1554 -> One (r1098)
  | 1553 -> One (r1099)
  | 1552 -> One (r1100)
  | 1551 -> One (r1101)
  | 1559 -> One (r1105)
  | 1558 -> One (r1106)
  | 1572 -> One (r1107)
  | 1564 -> One (r1108)
  | 1563 -> One (r1109)
  | 1562 -> One (r1110)
  | 1561 -> One (r1111)
  | 1571 -> One (r1112)
  | 1570 -> One (r1113)
  | 1569 -> One (r1114)
  | 1568 -> One (r1115)
  | 1567 -> One (r1116)
  | 1566 -> One (r1117)
  | 1576 -> One (r1118)
  | 1575 -> One (r1119)
  | 1582 -> One (r1120)
  | 1587 -> One (r1121)
  | 1586 -> One (r1122)
  | 1585 -> One (r1123)
  | 1584 -> One (r1124)
  | 1647 | 1701 -> One (r1126)
  | 1703 -> One (r1128)
  | 1717 -> One (r1130)
  | 1707 -> One (r1131)
  | 1706 -> One (r1132)
  | 1688 -> One (r1133)
  | 1687 -> One (r1134)
  | 1686 -> One (r1135)
  | 1685 -> One (r1136)
  | 1684 -> One (r1137)
  | 1683 -> One (r1138)
  | 1682 -> One (r1139)
  | 1672 -> One (r1140)
  | 1671 -> One (r1141)
  | 1603 -> One (r1142)
  | 1602 -> One (r1143)
  | 1601 -> One (r1144)
  | 1597 -> One (r1145)
  | 1595 -> One (r1146)
  | 1594 -> One (r1147)
  | 1600 -> One (r1148)
  | 1599 -> One (r1149)
  | 1665 -> One (r1150)
  | 1664 -> One (r1151)
  | 1609 -> One (r1152)
  | 1605 -> One (r1153)
  | 1608 -> One (r1154)
  | 1607 -> One (r1155)
  | 1620 -> One (r1156)
  | 1619 -> One (r1157)
  | 1618 -> One (r1158)
  | 1617 -> One (r1159)
  | 1616 -> One (r1160)
  | 1611 -> One (r1161)
  | 1631 -> One (r1162)
  | 1630 -> One (r1163)
  | 1629 -> One (r1164)
  | 1628 -> One (r1165)
  | 1627 -> One (r1166)
  | 1622 -> One (r1167)
  | 1656 -> One (r1168)
  | 1655 -> One (r1169)
  | 1633 -> One (r1170)
  | 1654 -> One (r1171)
  | 1653 -> One (r1172)
  | 1652 -> One (r1173)
  | 1651 -> One (r1174)
  | 1635 -> One (r1175)
  | 1649 -> One (r1176)
  | 1639 -> One (r1177)
  | 1638 -> One (r1178)
  | 1637 -> One (r1179)
  | 1646 | 1694 -> One (r1180)
  | 1643 -> One (r1182)
  | 1642 -> One (r1183)
  | 1641 -> One (r1184)
  | 1640 | 1693 -> One (r1185)
  | 1645 -> One (r1186)
  | 1661 -> One (r1187)
  | 1660 -> One (r1188)
  | 1659 -> One (r1189)
  | 1663 -> One (r1191)
  | 1662 -> One (r1192)
  | 1658 -> One (r1193)
  | 1667 -> One (r1194)
  | 1670 -> One (r1195)
  | 1681 -> One (r1196)
  | 1680 -> One (r1197)
  | 1679 -> One (r1198)
  | 1678 -> One (r1199)
  | 1677 -> One (r1200)
  | 1676 -> One (r1201)
  | 1675 -> One (r1202)
  | 1674 -> One (r1203)
  | 1705 -> One (r1204)
  | 1692 -> One (r1205)
  | 1691 -> One (r1206)
  | 1690 -> One (r1207)
  | 1704 -> One (r1208)
  | 1696 -> One (r1209)
  | 1702 -> One (r1210)
  | 1699 -> One (r1211)
  | 1698 -> One (r1212)
  | 1716 -> One (r1213)
  | 1715 -> One (r1214)
  | 1714 -> One (r1215)
  | 1713 -> One (r1216)
  | 1712 -> One (r1217)
  | 1711 -> One (r1218)
  | 1710 -> One (r1219)
  | 1709 -> One (r1220)
  | 1726 -> One (r1221)
  | 1728 -> One (r1222)
  | 1738 -> One (r1223)
  | 1737 -> One (r1224)
  | 1736 -> One (r1225)
  | 1735 -> One (r1226)
  | 1734 -> One (r1227)
  | 1733 -> One (r1228)
  | 1732 -> One (r1229)
  | 1731 -> One (r1230)
  | 1780 -> One (r1231)
  | 1760 -> One (r1232)
  | 1759 -> One (r1233)
  | 1758 -> One (r1234)
  | 1757 -> One (r1235)
  | 1744 -> One (r1236)
  | 1743 -> One (r1237)
  | 1742 -> One (r1238)
  | 1741 -> One (r1239)
  | 1748 -> One (r1240)
  | 1747 -> One (r1241)
  | 1753 -> One (r1242)
  | 1752 -> One (r1243)
  | 1751 | 2051 -> One (r1244)
  | 1755 | 2050 -> One (r1245)
  | 1777 -> One (r1246)
  | 1769 -> One (r1247)
  | 1768 -> One (r1248)
  | 1767 -> One (r1249)
  | 1776 -> One (r1250)
  | 1775 -> One (r1251)
  | 1896 -> One (r1252)
  | 1940 -> One (r1254)
  | 1793 -> One (r1255)
  | 1957 -> One (r1257)
  | 1948 -> One (r1258)
  | 1947 -> One (r1259)
  | 1792 -> One (r1260)
  | 1791 -> One (r1261)
  | 1790 -> One (r1262)
  | 1789 -> One (r1263)
  | 1788 -> One (r1264)
  | 1934 -> One (r1265)
  | 1933 -> One (r1266)
  | 1796 -> One (r1267)
  | 1795 -> One (r1268)
  | 1821 -> One (r1269)
  | 1820 -> One (r1270)
  | 1819 -> One (r1271)
  | 1818 -> One (r1272)
  | 1809 -> One (r1273)
  | 1808 -> One (r1275)
  | 1807 -> One (r1276)
  | 1803 -> One (r1277)
  | 1802 -> One (r1278)
  | 1801 -> One (r1279)
  | 1800 -> One (r1280)
  | 1799 -> One (r1281)
  | 1806 -> One (r1282)
  | 1805 -> One (r1283)
  | 1817 -> One (r1284)
  | 1816 -> One (r1285)
  | 1815 -> One (r1286)
  | 1824 -> One (r1287)
  | 1823 -> One (r1288)
  | 1865 -> One (r1290)
  | 1854 -> One (r1291)
  | 1853 -> One (r1292)
  | 1844 -> One (r1293)
  | 1843 -> One (r1295)
  | 1842 -> One (r1296)
  | 1841 -> One (r1297)
  | 1830 -> One (r1298)
  | 1829 -> One (r1299)
  | 1827 -> One (r1300)
  | 1840 -> One (r1301)
  | 1839 -> One (r1302)
  | 1838 -> One (r1303)
  | 1837 -> One (r1304)
  | 1836 -> One (r1305)
  | 1835 -> One (r1306)
  | 1834 -> One (r1307)
  | 1833 -> One (r1308)
  | 1852 -> One (r1309)
  | 1851 -> One (r1310)
  | 1850 -> One (r1311)
  | 1864 -> One (r1312)
  | 1863 -> One (r1313)
  | 1862 -> One (r1314)
  | 1861 -> One (r1315)
  | 1860 -> One (r1316)
  | 1859 -> One (r1317)
  | 1858 -> One (r1318)
  | 1857 -> One (r1319)
  | 1869 -> One (r1320)
  | 1868 -> One (r1321)
  | 1867 -> One (r1322)
  | 1928 -> One (r1323)
  | 1927 -> One (r1324)
  | 1926 -> One (r1325)
  | 1925 -> One (r1326)
  | 1924 -> One (r1327)
  | 1923 -> One (r1328)
  | 1920 -> One (r1329)
  | 1872 -> One (r1330)
  | 1916 -> One (r1331)
  | 1915 -> One (r1332)
  | 1910 -> One (r1333)
  | 1909 -> One (r1334)
  | 1908 -> One (r1335)
  | 1907 -> One (r1336)
  | 1881 -> One (r1337)
  | 1880 -> One (r1338)
  | 1879 -> One (r1339)
  | 1878 -> One (r1340)
  | 1877 -> One (r1341)
  | 1876 -> One (r1342)
  | 1906 -> One (r1343)
  | 1885 -> One (r1344)
  | 1884 -> One (r1345)
  | 1883 -> One (r1346)
  | 1889 -> One (r1347)
  | 1888 -> One (r1348)
  | 1887 -> One (r1349)
  | 1903 -> One (r1350)
  | 1893 -> One (r1351)
  | 1892 -> One (r1352)
  | 1905 -> One (r1354)
  | 1891 -> One (r1355)
  | 1900 -> One (r1356)
  | 1895 -> One (r1357)
  | 1914 -> One (r1358)
  | 1913 -> One (r1359)
  | 1912 -> One (r1360)
  | 1919 -> One (r1361)
  | 1918 -> One (r1362)
  | 1922 -> One (r1363)
  | 1932 -> One (r1364)
  | 1931 -> One (r1365)
  | 1930 -> One (r1366)
  | 1936 -> One (r1367)
  | 1939 -> One (r1368)
  | 1944 -> One (r1369)
  | 1943 -> One (r1370)
  | 1942 -> One (r1371)
  | 1946 -> One (r1372)
  | 1956 -> One (r1373)
  | 1955 -> One (r1374)
  | 1954 -> One (r1375)
  | 1953 -> One (r1376)
  | 1952 -> One (r1377)
  | 1951 -> One (r1378)
  | 1950 -> One (r1379)
  | 1966 -> One (r1380)
  | 1969 -> One (r1381)
  | 1971 -> One (r1382)
  | 1977 -> One (r1383)
  | 1976 -> One (r1384)
  | 1987 -> One (r1385)
  | 1986 -> One (r1386)
  | 1995 -> One (r1387)
  | 2006 -> One (r1388)
  | 2005 -> One (r1389)
  | 2004 -> One (r1390)
  | 2003 -> One (r1391)
  | 2002 -> One (r1392)
  | 2008 -> One (r1393)
  | 2015 -> One (r1394)
  | 2014 -> One (r1395)
  | 2020 -> One (r1396)
  | 2024 -> One (r1397)
  | 2023 -> One (r1398)
  | 2022 -> One (r1399)
  | 2033 -> One (r1400)
  | 2032 -> One (r1401)
  | 2031 -> One (r1402)
  | 2030 -> One (r1403)
  | 2035 -> One (r1404)
  | 2039 -> One (r1405)
  | 2038 -> One (r1406)
  | 2037 -> One (r1407)
  | 2054 -> One (r1408)
  | 2053 -> One (r1409)
  | 2066 -> One (r1410)
  | 2065 -> One (r1411)
  | 2082 -> One (r1412)
  | 2090 -> One (r1413)
  | 2093 -> One (r1414)
  | 2092 -> One (r1415)
  | 2098 -> One (r1416)
  | 2103 -> One (r1417)
  | 2100 -> One (r1418)
  | 2102 -> One (r1419)
  | 2105 -> One (r1420)
  | 2108 -> One (r1421)
  | 2111 -> One (r1422)
  | 2110 -> One (r1423)
  | 2119 -> One (r1424)
  | 2118 -> One (r1425)
  | 2117 -> One (r1426)
  | 2133 -> One (r1427)
  | 2132 -> One (r1428)
  | 2131 -> One (r1429)
  | 2153 -> One (r1430)
  | 2157 -> One (r1431)
  | 2162 -> One (r1432)
  | 2169 -> One (r1433)
  | 2168 -> One (r1434)
  | 2167 -> One (r1435)
  | 2166 -> One (r1436)
  | 2176 -> One (r1437)
  | 2180 -> One (r1438)
  | 2184 -> One (r1439)
  | 2187 -> One (r1440)
  | 2192 -> One (r1441)
  | 2196 -> One (r1442)
  | 2200 -> One (r1443)
  | 2204 -> One (r1444)
  | 2208 -> One (r1445)
  | 2211 -> One (r1446)
  | 2215 -> One (r1447)
  | 2220 -> One (r1448)
  | 2230 -> One (r1449)
  | 2232 -> One (r1450)
  | 2235 -> One (r1451)
  | 2234 -> One (r1452)
  | 2237 -> One (r1453)
  | 2247 -> One (r1454)
  | 2243 -> One (r1455)
  | 2242 -> One (r1456)
  | 2246 -> One (r1457)
  | 2245 -> One (r1458)
  | 2252 -> One (r1459)
  | 2251 -> One (r1460)
  | 2250 -> One (r1461)
  | 2254 -> One (r1462)
  | 425 -> Select (function
    | -1 -> [R 98]
    | _ -> S (T T_DOT) :: r396)
  | 708 -> Select (function
    | -1 -> [R 98]
    | _ -> r582)
  | 131 -> Select (function
    | -1 -> r82
    | _ -> R 124 :: r104)
  | 173 -> Select (function
    | -1 -> r82
    | _ -> R 124 :: r161)
  | 1519 -> Select (function
    | -1 -> r1079
    | _ -> R 124 :: r1072)
  | 1547 -> Select (function
    | -1 -> r1030
    | _ -> R 124 :: r1104)
  | 639 -> Select (function
    | -1 -> r224
    | _ -> [R 257])
  | 473 -> Select (function
    | -1 -> [R 819]
    | _ -> S (N N_pattern) :: r425)
  | 452 -> Select (function
    | -1 -> [R 820]
    | _ -> S (N N_pattern) :: r416)
  | 137 -> Select (function
    | -1 -> r110
    | _ -> R 926 :: r116)
  | 176 -> Select (function
    | -1 -> r110
    | _ -> R 926 :: r167)
  | 1524 -> Select (function
    | -1 -> S (T T_RPAREN) :: r134
    | _ -> S (T T_COLONCOLON) :: r432)
  | 328 -> Select (function
    | 379 | 723 | 1028 | 1276 | 1400 | 1878 | 1912 -> r47
    | -1 -> S (T T_RPAREN) :: r134
    | _ -> S (N N_pattern) :: r292)
  | 374 -> Select (function
    | -1 -> S (T T_RPAREN) :: r134
    | _ -> Sub (r3) :: r342)
  | 605 -> Select (function
    | -1 -> S (T T_RPAREN) :: r507
    | _ -> S (N N_module_type) :: r512)
  | 395 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r367
    | _ -> Sub (r369) :: r371)
  | 683 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r367
    | _ -> Sub (r554) :: r556)
  | 594 -> Select (function
    | 61 | 170 | 182 | 344 | 1492 | 1498 -> r497
    | _ -> S (T T_OPEN) :: r489)
  | 1526 -> Select (function
    | -1 -> r547
    | _ -> S (T T_LPAREN) :: r1080)
  | 219 -> Select (function
    | 1688 | 1692 | 1696 | 1699 | 1713 | 1917 | 1941 -> r218
    | -1 -> r232
    | _ -> S (T T_DOT) :: r235)
  | 637 -> Select (function
    | -1 -> r232
    | _ -> S (T T_DOT) :: r540)
  | 163 -> Select (function
    | -1 -> r83
    | _ -> S (T T_COLON) :: r140)
  | 114 -> Select (function
    | 1032 | 1381 -> r62
    | _ -> Sub (r59) :: r60)
  | 117 -> Select (function
    | 1032 | 1381 -> r61
    | _ -> r60)
  | 2068 -> Select (function
    | -1 -> r78
    | _ -> r83)
  | 2129 -> Select (function
    | -1 -> r78
    | _ -> r83)
  | 2128 -> Select (function
    | -1 -> r79
    | _ -> r102)
  | 2067 -> Select (function
    | -1 -> r79
    | _ -> r159)
  | 133 -> Select (function
    | -1 -> r80
    | _ -> r103)
  | 175 -> Select (function
    | -1 -> r80
    | _ -> r160)
  | 132 -> Select (function
    | -1 -> r81
    | _ -> r104)
  | 174 -> Select (function
    | -1 -> r81
    | _ -> r161)
  | 178 -> Select (function
    | -1 -> r108
    | _ -> r83)
  | 156 -> Select (function
    | -1 -> r108
    | _ -> r83)
  | 155 -> Select (function
    | -1 -> r109
    | _ -> r116)
  | 177 -> Select (function
    | -1 -> r109
    | _ -> r167)
  | 220 -> Select (function
    | 1688 | 1692 | 1696 | 1699 | 1713 | 1917 | 1941 -> r217
    | -1 -> r225
    | _ -> r235)
  | 638 -> Select (function
    | -1 -> r225
    | _ -> r540)
  | 1550 -> Select (function
    | -1 -> r1027
    | _ -> r1102)
  | 1549 -> Select (function
    | -1 -> r1028
    | _ -> r1103)
  | 1548 -> Select (function
    | -1 -> r1029
    | _ -> r1104)
  | 1522 -> Select (function
    | -1 -> r1076
    | _ -> r1070)
  | 1521 -> Select (function
    | -1 -> r1077
    | _ -> r1071)
  | 1520 -> Select (function
    | -1 -> r1078
    | _ -> r1072)
  | _ -> raise Not_found

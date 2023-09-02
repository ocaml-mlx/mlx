# 1 "mlx/jsx_helper.ml"
open Asttypes
open Longident
open Parsetree
open Ast_helper

let make_loc (startpos, endpos) =
  {
    Location.loc_start = startpos;
    Location.loc_end = endpos;
    Location.loc_ghost = false;
  }

let mkloc = Location.mkloc
let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d

let mkjsxexp ~loc e =
  let e = mkexp ~loc e in
  let attr =
    let loc = make_loc loc in
    Attr.mk ~loc { txt = "JSX"; loc } (PStr [])
  in
  { e with pexp_attributes = [ attr ] }

let rec equal_longindent a b =
  match a, b with
  | Longident.Lident a, Longident.Lident b -> String.equal a b
  | Ldot (pa, a), Ldot (pb, b) ->
      String.equal a b && equal_longindent pa pb
  | Lapply _, _ | _, Lapply _ -> assert false
  | _ -> false

let make_jsx_element ~raise:_ ~loc ~tag ~end_tag ~props ~children () =
  let () =
    match end_tag with
    | None -> ()
    | Some end_tag ->
        let eq =
          match tag, end_tag with
          | (`Module, _, s), (`Module, _, e) -> equal_longindent s e
          | (`Value, _, s), (`Value, _, e) -> equal_longindent s e
          | _ -> false
        in
        if not eq then
          let _, loc, _ = end_tag in
          let _, _, tag = tag in
          let tag = Longident.flatten tag |> String.concat "." in
          let msg = Printf.sprintf "JSX closing tag </%s>" tag in
          raise Syntaxerr.(Error(Expecting(make_loc loc, msg)))
  in
  let tag =
    match tag with
    | `Value, loc, txt ->
        mkexp ~loc (Pexp_ident { loc = make_loc loc; txt })
    | `Module, loc, txt ->
        let txt = Longident.Ldot (txt, "createElement") in
        mkexp ~loc (Pexp_ident { loc = make_loc loc; txt })
  in
  let props =
    List.map
      (function
        | loc, `Prop_punned name ->
            let id = mkloc (Lident name) (make_loc loc) in
            Labelled name, mkexp ~loc (Pexp_ident id)
        | _loc, `Prop (name, expr) -> Labelled name, expr)
      props
  in
  let unit =
    mkexp ~loc
      (Pexp_construct ({ txt = Lident "()"; loc = make_loc loc }, None))
  in
  let props = (Labelled "children", children) :: props in
  Pexp_apply (tag, (Nolabel, unit) :: props)

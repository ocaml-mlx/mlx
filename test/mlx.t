
  $ echo 'let _ = <div />' | ./mlx
  BATCH
  let _ = div () ~children:[] [@JSX]
  MERLIN
  let _ = div () ~children:[] [@JSX]

  $ echo 'let css = {|color: red;|}' | ./mlx
  BATCH
  let css = {|color: red;|}
  MERLIN
  let css = {|color: red;|}

  $ echo 'let _ = <div>hello world</div>' | ./mlx
  BATCH
  let _ = div () ~children:[ hello; world ] [@JSX]
  MERLIN
  let _ = div () ~children:[ hello; world ] [@JSX]

  $ echo 'let _ = <div attr with_value=1 />' | ./mlx
  BATCH
  let _ = div () ~children:[] ~attr ~with_value:1 [@JSX]
  MERLIN
  let _ = div () ~children:[] ~attr ~with_value:1 [@JSX]

  $ echo 'let _ = <div ?opt ?opt_value=some />' | ./mlx
  BATCH
  let _ = div () ~children:[] ?opt ?opt_value:some [@JSX]
  MERLIN
  let _ = div () ~children:[] ?opt ?opt_value:some [@JSX]

  $ echo 'let _ = <Hello attr with_value=1 />' | ./mlx
  BATCH
  let _ = Hello.createElement () ~children:[] ~attr ~with_value:1 [@JSX]
  MERLIN
  let _ = Hello.createElement () ~children:[] ~attr ~with_value:1 [@JSX]

  $ echo 'let render children = <Component children />' | ./mlx
  BATCH
  let render children = Component.createElement () ~children [@JSX]
  MERLIN
  let render children = Component.createElement () ~children [@JSX]

  $ echo 'let render children nested = <Component children>nested</Component>' | ./mlx
  BATCH
  let render children nested = Component.createElement () ~children [@JSX]
  MERLIN
  let render children nested = Component.createElement () ~children [@JSX]

  $ echo 'let _ = <Hello.ok attr with_value=1 />' | ./mlx
  BATCH
  let _ = Hello.ok () ~children:[] ~attr ~with_value:1 [@JSX]
  MERLIN
  let _ = Hello.ok () ~children:[] ~attr ~with_value:1 [@JSX]

  $ echo 'let _ = <Hello.Ok attr with_value=1 />' | ./mlx
  BATCH
  let _ = Hello.Ok.createElement () ~children:[] ~attr ~with_value:1 [@JSX]
  MERLIN
  let _ = Hello.Ok.createElement () ~children:[] ~attr ~with_value:1 [@JSX]

  $ echo 'let _ = <Hello>world</Hello>' | ./mlx
  BATCH
  let _ = Hello.createElement () ~children:[ world ] [@JSX]
  MERLIN
  let _ = Hello.createElement () ~children:[ world ] [@JSX]

  $ echo 'let _ = <Hello.ok>world</Hello.ok>' | ./mlx
  BATCH
  let _ = Hello.ok () ~children:[ world ] [@JSX]
  MERLIN
  let _ = Hello.ok () ~children:[ world ] [@JSX]

  $ echo 'let _ = <Hello.Ok>world</Hello.Ok>' | ./mlx
  BATCH
  let _ = Hello.Ok.createElement () ~children:[ world ] [@JSX]
  MERLIN
  let _ = Hello.Ok.createElement () ~children:[ world ] [@JSX]

Expected error (tag mismatch):

  $ echo 'let _ = <one>world</two>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 18-24:
  Error: Syntax error: </one> expected
  File "*stdin*", line 1, characters 8-12:
    This <one> might be unmatched
  
  MERLIN
  File "*stdin*", line 1, characters 18-24
  Error: Syntax error: </one> expected
    This <one> might be unmatched
  
Some tests for prop expressions:

  $ echo 'let _ = <element prop=`Some />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:`Some [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:`Some [@JSX]
  $ echo 'let _ = <element prop=Some.value />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:Some.value [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:Some.value [@JSX]
  $ echo 'let _ = <element prop=() />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:() [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:() [@JSX]
  $ echo 'let _ = <element prop=(1+2) />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:(1 + 2) [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:(1 + 2) [@JSX]
  $ echo 'let _ = <element prop=[] />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:[] [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:[] [@JSX]
  $ echo 'let _ = <element prop=name#obj />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:name#obj [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:name#obj [@JSX]
  $ echo 'let _ = <element prop=!ref />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:!ref [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:!ref [@JSX]
  $ echo 'let _ = <element prop=!?ref />' | ./mlx
  BATCH
  let _ = element () ~children:[] ~prop:!?ref [@JSX]
  MERLIN
  let _ = element () ~children:[] ~prop:!?ref [@JSX]

We have a lexer hack to parse [<element and [<Element as JSX:
  $ echo 'let _ = [<element />]' | ./mlx
  BATCH
  let _ = [ (element () ~children:[] [@JSX]) ]
  MERLIN
  let _ = [ (element () ~children:[] [@JSX]) ]

  $ echo 'let _ = [<M.element />]' | ./mlx
  BATCH
  let _ = [ (M.element () ~children:[] [@JSX]) ]
  MERLIN
  let _ = [ (M.element () ~children:[] [@JSX]) ]

  $ echo 'let _ = [<element> 1 </element>]' | ./mlx
  BATCH
  let _ = [ (element () ~children:[ 1 ] [@JSX]) ]
  MERLIN
  let _ = [ (element () ~children:[ 1 ] [@JSX]) ]

  $ echo 'let _ = [<M.element> 1 </M.element>]' | ./mlx
  BATCH
  let _ = [ (M.element () ~children:[ 1 ] [@JSX]) ]
  MERLIN
  let _ = [ (M.element () ~children:[ 1 ] [@JSX]) ]

The `>`-operator rule must give back the ">" when a JSX element closes
directly before "|]" inside an array literal (`[|<div>...</div>|]`), so that
"|]" still lexes as BARRBRACKET instead of being swallowed into an `>|`
operator token:

  $ echo 'let _ = [|<div>aa</div>|]' | ./mlx
  BATCH
  let _ = [| (div () ~children:[ aa ] [@JSX]) |]
  MERLIN
  let _ = [| (div () ~children:[ aa ] [@JSX]) |]

  $ echo 'let _ = [|<div>aa</div>; <div>bb</div>|]' | ./mlx
  BATCH
  let _ = [| (div () ~children:[ aa ] [@JSX]); (div () ~children:[ bb ] [@JSX]) |]
  MERLIN
  let _ = [| (div () ~children:[ aa ] [@JSX]); (div () ~children:[ bb ] [@JSX]) |]

  $ echo 'let _ = [<div>aa</div>]' | ./mlx
  BATCH
  let _ = [ (div () ~children:[ aa ] [@JSX]) ]
  MERLIN
  let _ = [ (div () ~children:[ aa ] [@JSX]) ]

  $ echo 'let _ = [|<div>aa</div>|]' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ = [| (div () ~children:[ aa ] [@JSX]) |]

Operator sanity: only the exact sequence ">|]" is special-cased, so ">|"
still lexes as an ordinary operator everywhere else, including right before
a closing "|]" that isn't immediately preceded by ">":

  $ echo 'let (>|) a b = a
  > let _ = 1>|2' | ./mlx
  BATCH
  let ( >| ) a b = a
  let _ = 1 >| 2
  MERLIN
  let ( >| ) a b = a
  let _ = 1 >| 2

  $ echo 'let (>|) a b = a
  > let _ = [|1>|2|]' | ./mlx
  BATCH
  let ( >| ) a b = a
  let _ = [| 1 >| 2 |]
  MERLIN
  let ( >| ) a b = a
  let _ = [| 1 >| 2 |]

  $ echo 'let (>|=) a b = a in 1 >|= 2' | ./mlx
  BATCH
  let ( >|= ) a b = a in
  1 >|= 2
  MERLIN
  let ( >|= ) a b = a in
  (1 >|= 2) [@merlin.loc]

The `>`-operator rule must also give back the ">" when a JSX element closes
directly before "}" inside a record/braced expression (`{x = <div>...</div>}`),
so that "}" still lexes as RBRACE instead of being swallowed into the
GREATERRBRACE object-override closer:

  $ echo 'let _ = {x = <div>a</div>}' | ./mlx
  BATCH
  let _ = { x = div () ~children:[ a ] [@JSX] }
  MERLIN
  let _ = { x = div () ~children:[ a ] [@JSX] }

  $ echo 'let _ = {x = <div>a</div>; y = 1}' | ./mlx
  BATCH
  let _ = { x = div () ~children:[ a ] [@JSX]; y = 1 }
  MERLIN
  let _ = { x = div () ~children:[ a ] [@JSX]; y = 1 }

  $ echo 'let r = {r with x = <div>a</div>}' | ./mlx
  BATCH
  let r = { r with x = div () ~children:[ a ] [@JSX] }
  MERLIN
  let r = { r with x = div () ~children:[ a ] [@JSX] }

  $ echo 'let _ = {x = <div>a</div>}' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ = { x = div () ~children:[ a ] [@JSX] }

Object override still works, both spaced and unspaced, since the grammar now
closes `{< ... >}` with GREATER RBRACE instead of the single GREATERRBRACE
token:

  $ echo 'let _ = object val x = 1 method m = {< x = 2 >} end' | ./mlx
  BATCH
  let _ =
    object
      val x = 1
      method m = {<x = 2>}
    end
  MERLIN
  let _ =
    object
      val x = 1
      method m = {<x = 2>}
    end

  $ echo 'let _ = object val x = 1 method m = {<x = 2>} end' | ./mlx
  BATCH
  let _ =
    object
      val x = 1
      method m = {<x = 2>}
    end
  MERLIN
  let _ =
    object
      val x = 1
      method m = {<x = 2>}
    end

Because the grammar now closes the override with two separate tokens
(GREATER RBRACE) instead of one, `{< x = 2 > }` (with a space before the
closing brace) is now newly accepted as well; that was rejected in stock
OCaml, where ">}" only ever lexed as one token. This is a harmless
relaxation of the grammar, pinned here:

  $ echo 'let _ = object val x = 1 method m = {< x = 2 > } end' | ./mlx
  BATCH
  let _ =
    object
      val x = 1
      method m = {<x = 2>}
    end
  MERLIN
  let _ =
    object
      val x = 1
      method m = {<x = 2>}
    end

Known regression: splitting ">}" into GREATER RBRACE means the final GREATER
of a `>}` sequence is grammatically indistinguishable from an infix ">"
comparison, so when an override field's value itself ends in an unparenthesized
"> expr" immediately before the closing brace, the parser now greedily shifts
that GREATER as a continuing comparison instead of reducing to close the
override, and fails where stock OCaml (and our own GREATERRBRACE token before
this change) would accept it. This requires parentheses as a workaround:

  $ echo 'let _ = object val x = true method m = {< x = (1 > 2) >} end' | ./mlx
  BATCH
  let _ =
    object
      val x = true
      method m = {<x = 1 > 2>}
    end
  MERLIN
  let _ =
    object
      val x = true
      method m = {<x = 1 > 2>}
    end

Conversion to the host merlin's AST — exercises the Obj.magic + ppxlib
migration bridge (Mlx_conv) that the reader uses to hand its parsetree to
merlin; a shape mismatch here segfaults or garbles the output. Constants are
the interesting case (their representation changed across AST versions):

  $ echo "let _ = <div attr with_value=1 ?opt>hello \"str\" 3.14 'c'</div>" | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ =
    div () ~children:[ hello; "str"; 3.14; 'c' ] ~attr ~with_value:1 ?opt [@JSX]

  $ echo 'let _ = <Hello.Ok ?opt_value=some attr><span>inner</span></Hello.Ok>' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ =
    Hello.Ok.createElement ()
      ~children:[ (span () ~children:[ inner ] [@JSX]) ]
      ?opt_value:some ~attr [@JSX]

  $ echo 'let f ?(y = 2) = function Some (a, b) -> [ a; b; y ] | None -> []' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let f ?(y = 2) = function
    | Some (a, b) -> [ a; b; y ] [@merlin.loc]
    | None -> [] [@merlin.loc]

Signature conversion:

  $ printf 'val f : int -> string\nmodule M : sig type t val x : t option end\n' | ./mlx_merlin.exe -conv -intf | ocamlformat - --intf --enable-outside-detected-project
  val f : int -> string
  
  module M : sig
    type t
  
    val x : t option
  end

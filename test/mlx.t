
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

A JSX element closing directly before "|]" inside an array literal must give back the ">" so "|]" still lexes as BARRBRACKET:

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

Operator sanity: only the exact sequence ">|]" is special-cased, so ">|" still lexes as an ordinary operator everywhere else:

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

A JSX element closing directly before "}" inside a record/braced expression must likewise give back the ">" so "}" still lexes as RBRACE:

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

Object override still works, both spaced and unspaced, with a distinct GREATER_BEFORE_RBRACE followed by RBRACE:

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

Whitespace before the final brace remains accepted:

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

An override comparison no longer needs parentheses to distinguish it from the closer:

  $ echo 'let _ = object val x = true method m = {< x = 1 > 2 >} end' | ./mlx
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

Object types written without a space after "<" (e.g. `<m : int>`) used to fail because the lexer fuses "<" with the following identifier into JSX_LIDENT; the grammar now accepts that too:

  $ echo 'let f (x : <m : int>) = x#m' | ./mlx
  BATCH
  let f (x : < m : int >) = x#m
  MERLIN
  let f (x : < m : int >) = x#m

  $ echo 'let f (x : <m : int; n : float>) = x#m' | ./mlx
  BATCH
  let f (x : < m : int ; n : float >) = x#m
  MERLIN
  let f (x : < m : int ; n : float >) = x#m

  $ echo "let f (x : <m : 'a. 'a -> 'a>) = x" | ./mlx
  BATCH
  let f (x : < m : 'a. 'a -> 'a >) = x
  MERLIN
  let f (x : < m : 'a. 'a -> 'a >) = x

  $ echo 'let f (x : <m : int>) = x#m' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let f (x : < m : int >) = x#m

  $ printf 'val f : <m : int> -> unit\n' | ./mlx_merlin.exe -intf | ocamlformat - --intf --enable-outside-detected-project
  val f : < m : int > -> unit

Regression guards: the spaced form, the open object type `< .. >`, and a JSX element whose tag looks like a method name must all still work:

  $ echo 'let f (x : < m : int >) = x#m' | ./mlx
  BATCH
  let f (x : < m : int >) = x#m
  MERLIN
  let f (x : < m : int >) = x#m

  $ echo 'let f (x : < .. >) = x' | ./mlx
  BATCH
  let f (x : < .. >) = x
  MERLIN
  let f (x : < .. >) = x

  $ echo 'let _ = <m />' | ./mlx
  BATCH
  let _ = m () ~children:[] [@JSX]
  MERLIN
  let _ = m () ~children:[] [@JSX]

Children spread syntax (`...expr` as the sole child) passes the expression directly as `~children`:

  $ echo 'let _ = <div> ...children </div>' | ./mlx
  BATCH
  let _ = div () ~children [@JSX]
  MERLIN
  let _ = div () ~children [@JSX]

  $ echo 'let _ = <Foo> ...(List.map f xs) </Foo>' | ./mlx
  BATCH
  let _ = Foo.createElement () ~children:(List.map f xs) [@JSX]
  MERLIN
  let _ = Foo.createElement () ~children:(List.map f xs) [@JSX]

  $ echo 'let _ = <div> ...children </div>' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ = div () ~children [@JSX]

The `>...` spelling (no space before the `...`) also works:

  $ echo 'let _ = <div>...children</div>' | ./mlx
  BATCH
  let _ = div () ~children [@JSX]
  MERLIN
  let _ = div () ~children [@JSX]

  $ echo 'let _ = <Foo>...(List.map f xs)</Foo>' | ./mlx
  BATCH
  let _ = Foo.createElement () ~children:(List.map f xs) [@JSX]
  MERLIN
  let _ = Foo.createElement () ~children:(List.map f xs) [@JSX]

  $ echo 'let _ = <div>...children</div>' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ = div () ~children [@JSX]

Mixed spacing around the spread works the same way:

  $ echo 'let _ = <div>...children </div>' | ./mlx
  BATCH
  let _ = div () ~children [@JSX]
  MERLIN
  let _ = div () ~children [@JSX]

  $ echo 'let _ = <div> ...children</div>' | ./mlx
  BATCH
  let _ = div () ~children [@JSX]
  MERLIN
  let _ = div () ~children [@JSX]

A non-`children` variable is spelled out explicitly as `~children:x`:

  $ echo 'let _ = <div>...x</div>' | ./mlx
  BATCH
  let _ = div () ~children:x [@JSX]
  MERLIN
  let _ = div () ~children:x [@JSX]

Because `>...` is reserved for the spread, infix operators whose lexeme
starts with `>...` are rejected by the dialect:

  $ echo 'let (>...) a b = a' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 6-9:
  Error: Syntax error: ) expected
  File "*stdin*", line 1, characters 4-5:
    This ( might be unmatched
  
  MERLIN
  File "*stdin*", line 1, characters 6-9
  Error: Syntax error after unclosed (, expecting `)'
  



  $ echo 'let _ = 1 >... 2' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 11-14:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 11-14
  Error: Syntax error, expecting fun_expr
  



Other operators starting with `>` (but not `>...`) are unaffected:

  $ echo 'let (>.) a b = a' | ./mlx
  BATCH
  let ( >. ) a b = a
  MERLIN
  let ( >. ) a b = a

  $ echo 'let _ = x >. y' | ./mlx
  BATCH
  let _ = x >. y
  MERLIN
  let _ = x >. y

  $ echo 'let (>>=) a f = f a in x >>= y' | ./mlx
  BATCH
  let ( >>= ) a f = f a in
  x >>= y
  MERLIN
  let ( >>= ) a f = f a in
  (x >>= y) [@merlin.loc]

Mixing a spread child with other children is a syntax error:

  $ echo 'let _ = <div> a ...b </div>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 16-19:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 16-19
  Error: Syntax error, expecting `JSX_LIDENT_E'
  

  $ echo 'let _ = <div> ...a b </div>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 19-20:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 2, characters 0-0
  Error: Syntax error, expecting fun_expr
  

Fragments lower to a list literal with the `[@JSX]` attribute; `<></>` is the empty list:

  $ echo 'let _ = <></>' | ./mlx
  BATCH
  let _ = [] [@JSX]
  MERLIN
  let _ = [] [@JSX]
  $ echo 'let _ = <>a</>' | ./mlx
  BATCH
  let _ = [ a ] [@JSX]
  MERLIN
  let _ = [ a ] [@JSX]
  $ echo 'let _ = <>a b</>' | ./mlx
  BATCH
  let _ = [ a; b ] [@JSX]
  MERLIN
  let _ = [ a; b ] [@JSX]
  $ echo 'let _ = <>(f x) "s" 1</>' | ./mlx
  BATCH
  let _ = [ f x; "s"; 1 ] [@JSX]
  MERLIN
  let _ = [ f x; "s"; 1 ] [@JSX]

Fragments nest inside tags and tags nest inside fragments:
  $ echo 'let _ = <div><>a b</></div>' | ./mlx
  BATCH
  let _ = div () ~children:[ [ a; b ] [@JSX] ] [@JSX]
  MERLIN
  let _ = div () ~children:[ [ a; b ] [@JSX] ] [@JSX]
  $ echo 'let _ = <><div>a</div><Foo>b</Foo></>' | ./mlx
  BATCH
  let _ =
    [
      (div () ~children:[ a ] [@JSX]);
      (Foo.createElement () ~children:[ b ] [@JSX]);
    ]
    [@JSX]
  MERLIN
  let _ =
    [
      (div () ~children:[ a ] [@JSX]);
      (Foo.createElement () ~children:[ b ] [@JSX]);
    ]
    [@JSX]
  $ echo 'let _ = <><div />c</>' | ./mlx
  BATCH
  let _ = [ (div () ~children:[] [@JSX]); c ] [@JSX]
  MERLIN
  let _ = [ (div () ~children:[] [@JSX]); c ] [@JSX]
  $ echo 'let _ = <><></></>' | ./mlx
  BATCH
  let _ = [ ([] [@JSX]) ] [@JSX]
  MERLIN
  let _ = [ ([] [@JSX]) ] [@JSX]
  $ echo 'let _ = <><>a</><>b</></>' | ./mlx
  BATCH
  let _ = [ [ a ] [@JSX]; [ b ] [@JSX] ] [@JSX]
  MERLIN
  let _ = [ [ a ] [@JSX]; [ b ] [@JSX] ] [@JSX]
  $ echo 'let _ = <div><></></div>' | ./mlx
  BATCH
  let _ = div () ~children:[ ([] [@JSX]) ] [@JSX]
  MERLIN
  let _ = div () ~children:[ ([] [@JSX]) ] [@JSX]
  $ echo 'let _ = [<>a</>; <>b</>]' | ./mlx
  BATCH
  let _ = [ [ a ] [@JSX]; [ b ] [@JSX] ]
  MERLIN
  let _ = [ [ a ] [@JSX]; [ b ] [@JSX] ]
  $ echo 'let _ = f (<>a</>)' | ./mlx
  BATCH
  let _ = f ([ a ] [@JSX])
  MERLIN
  let _ = f ([ a ] [@JSX])

Children spread works in a fragment, spaced and unspaced:
  $ echo 'let _ = <> ...xs </>' | ./mlx
  BATCH
  let _ = xs [@JSX]
  MERLIN
  let _ = xs [@JSX]
  $ echo 'let _ = <>...xs</>' | ./mlx
  BATCH
  let _ = xs [@JSX]
  MERLIN
  let _ = xs [@JSX]
  $ echo 'let _ = <>...xs </>' | ./mlx
  BATCH
  let _ = xs [@JSX]
  MERLIN
  let _ = xs [@JSX]
  $ echo 'let _ = <> ...xs</>' | ./mlx
  BATCH
  let _ = xs [@JSX]
  MERLIN
  let _ = xs [@JSX]
  $ echo 'let _ = <>...(List.map f xs)</>' | ./mlx
  BATCH
  let _ = List.map f xs [@JSX]
  MERLIN
  let _ = List.map f xs [@JSX]
  $ echo 'let _ = <><>...xs</></>' | ./mlx
  BATCH
  let _ = [ (xs [@JSX]) ] [@JSX]
  MERLIN
  let _ = [ (xs [@JSX]) ] [@JSX]
  $ echo 'let _ = <div>...xs</div>' | ./mlx
  BATCH
  let _ = div () ~children:xs [@JSX]
  MERLIN
  let _ = div () ~children:xs [@JSX]

  $ echo 'let _ = <>a b</>' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ = [ a; b ] [@JSX]

  $ echo 'let _ = <>...xs</>' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ = xs [@JSX]

The fragment delimiters are checked, so mismatched or missing closers are syntax errors:

  $ echo 'let _ = <>a</div>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 11-16:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 2, characters 0-0
  Error: Syntax error, expecting fun_expr
  
  $ echo 'let _ = <div>a</>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 14-17:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 14-17
  Error: Syntax error, expecting `JSX_LIDENT_E'
  
  $ echo 'let _ = <>a' | ./mlx
  BATCH
  File "*stdin*", line 2, characters 0-0:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 2, characters 0-0
  Error: Syntax error, expecting `</>'
  
  $ echo 'let _ = <>a b ...c</>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 14-17:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 14-17
  Error: Syntax error, expecting `</>'
  
  $ echo 'let _ = <>...a b</>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 15-16:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 16-19
  Error: Syntax error
  
  $ echo 'let _ = <>...a ...b</>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 15-18:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 19-22
  Error: Syntax error
  
  $ echo 'let _ = <>a</></>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 14-17:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 14-17
  Error: Syntax error
  

`<>` is still the structural inequality operator. Operators that start with
`<>` and continue with `<` or `...` are reserved for fragments, and `</>` is
reserved as a closing delimiter and cannot be used as an operator:

  $ echo 'let _ = a <> b' | ./mlx
  BATCH
  let _ = a <> b
  MERLIN
  let _ = a <> b
  $ echo 'let _ = a<>b' | ./mlx
  BATCH
  let _ = a <> b
  MERLIN
  let _ = a <> b
  $ echo 'let _ = a <>b' | ./mlx
  BATCH
  let _ = a <> b
  MERLIN
  let _ = a <> b
  $ echo 'let _ = a<> b' | ./mlx
  BATCH
  let _ = a <> b
  MERLIN
  let _ = a <> b
  $ echo 'let _ = if x <> y then 1 else 2' | ./mlx
  BATCH
  let _ = if x <> y then 1 else 2
  MERLIN
  let _ = if x <> y then (1 [@merlin.loc]) else (2 [@merlin.loc])
  $ echo 'let _ = f x <> g y' | ./mlx
  BATCH
  let _ = f x <> g y
  MERLIN
  let _ = f x <> g y
  $ echo 'let _ = a <> b <> c' | ./mlx
  BATCH
  let _ = a <> b <> c
  MERLIN
  let _ = a <> b <> c
  $ echo 'let (<>) a b = a' | ./mlx
  BATCH
  let ( <> ) a b = a
  MERLIN
  let ( <> ) a b = a
  $ echo 'let _ = (<>) a b' | ./mlx
  BATCH
  let _ = a <> b
  MERLIN
  let _ = a <> b
  $ echo 'let _ = ( <> ) a b' | ./mlx
  BATCH
  let _ = a <> b
  MERLIN
  let _ = a <> b
  $ echo 'let _ = List.filter ((<>) x) xs' | ./mlx
  BATCH
  let _ = List.filter (( <> ) x) xs
  MERLIN
  let _ = List.filter (( <> ) x) xs
  $ echo 'let _ = [a <> b; c]' | ./mlx
  BATCH
  let _ = [ a <> b; c ]
  MERLIN
  let _ = [ a <> b; c ]
  $ echo 'let _ = <div>(a <> b)</div>' | ./mlx
  BATCH
  let _ = div () ~children:[ a <> b ] [@JSX]
  MERLIN
  let _ = div () ~children:[ a <> b ] [@JSX]
  $ echo 'let _ = <>(a <> b)</>' | ./mlx
  BATCH
  let _ = [ a <> b ] [@JSX]
  MERLIN
  let _ = [ a <> b ] [@JSX]
  $ echo 'let _ = <>a</> <> <>b</>' | ./mlx
  BATCH
  let _ = [ a ] [@JSX] <> [ b ] [@JSX]
  MERLIN
  let _ = [ a ] [@JSX] <> [ b ] [@JSX]
  $ echo 'let f (x : [< `A | `B ]) = x' | ./mlx
  BATCH
  let f (x : [< `A | `B ]) = x
  MERLIN
  let f (x : [< `A | `B ]) = x
  $ echo 'let f (x : < a : int >) = x' | ./mlx
  BATCH
  let f (x : < a : int >) = x
  MERLIN
  let f (x : < a : int >) = x
  $ echo 'let _ = a <>- b' | ./mlx
  BATCH
  let _ = a <>- b
  MERLIN
  let _ = a <>- b
  $ echo 'let _ = a </> b' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 10-13:
  Error: Syntax error
  
  MERLIN
  File "*stdin*", line 1, characters 10-13
  Error: Syntax error
  
  $ echo 'let (</>) a b = a' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 5-8:
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  MERLIN
  File "*stdin*", line 1, characters 5-8
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  $ echo 'let _ = ( </> ) a b' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 10-13:
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  MERLIN
  File "*stdin*", line 1, characters 10-13
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  $ echo 'module type S = sig val (</>) : int -> int -> int end' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 25-28:
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  MERLIN
  File "*stdin*", line 1, characters 25-28
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  $ echo 'external (</>) : int -> int -> int = "x"' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 10-13:
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  MERLIN
  File "*stdin*", line 1, characters 10-13
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  $ echo 'let _ = M.(</>)' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 11-14:
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  MERLIN
  File "*stdin*", line 1, characters 11-14
  Error: Syntax error: operator "</>" (reserved for JSX fragments) not expected.
  
  $ echo 'let _ = (<>-) a b' | ./mlx
  BATCH
  let _ = a <>- b
  MERLIN
  let _ = a <>- b
  $ echo 'let (<>) a b = a' | ./mlx
  BATCH
  let ( <> ) a b = a
  MERLIN
  let ( <> ) a b = a

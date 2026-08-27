
  $ echo 'let _ = <div />' | ./mlx
  BATCH
  let _ = div () ~children:[] [@JSX]
  MERLIN
  let _ = div () ~children:[] [@JSX]

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

UTF-8 is accepted in source text:

  $ printf '%s\n' '(** Text — arrow → emoji 🌟 and Japanese 東京. *)' 'let message = "“Quoted” text isn’t ASCII"' | ./mlx
  BATCH
  let message = "\226\128\156Quoted\226\128\157 text isn\226\128\153t ASCII"
  [@@ocaml.doc
    " Text \226\128\148 arrow \226\134\146 emoji \240\159\140\159 and Japanese \
     \230\157\177\228\186\172. "]
  MERLIN
  let message = "\226\128\156Quoted\226\128\157 text isn\226\128\153t ASCII"

UTF-8 is accepted in quoted strings:

  $ printf '%s\n' 'let quoted = {|“Quoted” text 東京 🌟|}' 'let delimited = {utf|“Quoted” text 東京 🌟|utf}' | ./mlx
  BATCH
  let quoted = {|“Quoted” text 東京 🌟|}
  let delimited = {utf|“Quoted” text 東京 🌟|utf}
  MERLIN
  let quoted = {|“Quoted” text 東京 🌟|}
  let delimited = {utf|“Quoted” text 東京 🌟|utf}

Expected error (tag mismatch):

  $ echo 'let _ = <one>world</two>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 18-24:
  Error: Syntax error: '</one>' expected
  File "*stdin*", line 1, characters 8-12:
    This '<one>' might be unmatched
  
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

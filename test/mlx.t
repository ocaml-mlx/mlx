
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

  $ echo 'let _ = <one>world</two>' | ./mlx
  BATCH
  File "*stdin*", line 1, characters 18-24:
  Error: Syntax error: '</one>' expected
  File "*stdin*", line 1, characters 8-12:
    This '<one>' might be unmatched
  
  MERLIN
  File "*stdin*", line 1, characters 18-24
  Error: Syntax error: '</one>' expected
    This '<one>' might be unmatched
  

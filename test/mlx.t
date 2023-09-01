
  $ echo 'let _ = <div />' | ./mlx
  BATCH
  let _ = div () [@JSX]
  MERLIN
  let _ = div () [@JSX]

  $ echo 'let _ = <div>hello world</div>' | ./mlx
  BATCH
  let _ = div () ~children:[ hello; world ] [@JSX]
  MERLIN
  let _ = div () ~children:[ hello; world ] [@JSX]

  $ echo 'let _ = <div attr with_value=1 />' | ./mlx
  BATCH
  let _ = div () ~attr ~with_value:1 [@JSX]
  MERLIN
  let _ = div () ~attr ~with_value:1 [@JSX]

  $ echo 'let _ = <Hello attr with_value=1 />' | ./mlx
  BATCH
  let _ = Hello.createElement () ~attr ~with_value:1 [@JSX]
  MERLIN
  let _ = Hello.createElement () ~attr ~with_value:1 [@JSX]

  $ echo 'let _ = <Hello>world</Hello>' | ./mlx
  BATCH
  let _ = Hello.createElement () ~children:[ world ] [@JSX]
  MERLIN
  let _ = Hello.createElement () ~children:[ world ] [@JSX]

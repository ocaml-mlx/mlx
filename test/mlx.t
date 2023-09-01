
  $ echo 'let _ = <div />' | ./mlx
  let _ = div () [@JSX]

  $ echo 'let _ = <div>hello world</div>' | ./mlx
  let _ = div () ~children:[ hello; world ] [@JSX]

  $ echo 'let _ = <div attr with_value=1 />' | ./mlx
  let _ = div () ~attr ~with_value:1 [@JSX]

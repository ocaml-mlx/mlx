Override field expressions must not consume the closing angle as an infix operator:

  $ echo 'let _ = {<x = true && false>}' | ./mlx
  BATCH
  let _ = {<x = true && false>}
  MERLIN
  let _ = {<x = true && false>}

  $ echo 'let _ = {<x = if true then 1 else 2>}' | ./mlx
  BATCH
  let _ = {<x = if true then 1 else 2>}
  MERLIN
  let _ = {<x = if true then (1 [@merlin.loc]) else (2 [@merlin.loc])>}

  $ echo 'let _ = {<x = let a = 1 in a > 2>}' | ./mlx
  BATCH
  let _ =
    {<x = let a = 1 in
          a > 2>}
  MERLIN
  let _ =
    {<x = let a = 1 in
          (a > 2) [@merlin.loc]>}

  $ echo 'let _ = {<x = 1 > 2 && true; y = false || 3 > 4>}' | ./mlx
  BATCH
  let _ = {<x = 1 > 2 && true; y = false || 3 > 4>}
  MERLIN
  let _ = {<x = 1 > 2 && true; y = false || 3 > 4>}

The converted Merlin AST preserves the same expressions:

  $ echo 'let _ = {<x = true && false; y = if true then 1 else 2>}' | ./mlx_merlin.exe -conv | ocamlformat - --impl --enable-outside-detected-project
  let _ =
    {<x = true && false
     ; y = if true then (1 [@merlin.loc]) else (2 [@merlin.loc])>}

Empty and locally opened overrides, whitespace, and nested braces remain valid:

  $ echo 'let _ = {<>} let _ = M.{<x = 1 > 2>}' | ./mlx
  BATCH
  let _ = {<>}
  
  let _ =
    let open M in
    {<x = 1 > 2>}
  MERLIN
  let _ = {<>}
  
  let _ =
    let open M in
    {<x = 1 > 2>}

  $ printf 'let _ = {<x = true && false>\n}\n' | ./mlx
  BATCH
  let _ = {<x = true && false>}
  MERLIN
  let _ = {<x = true && false>}

  $ echo 'let _ = {x = {<y = true && false>}}' | ./mlx
  BATCH
  let _ = { x = {<y = true && false>} }
  MERLIN
  let _ = { x = {<y = true && false>} }

The distinct closer also closes JSX and object types immediately before braces:

  $ echo 'let _ = {x = <div>...xs</div>}' | ./mlx
  BATCH
  let _ = { x = div () ~children:xs [@JSX] }
  MERLIN
  let _ = { x = div () ~children:xs [@JSX] }

  $ echo 'let _ = a.{ <div>x</div>}' | ./mlx
  BATCH
  let _ = a.{div () ~children:[ x ] [@JSX]}
  MERLIN
  let _ = a.{div () ~children:[ x ] [@JSX]}

  $ echo 'type t = {x : <m : int>}' | ./mlx
  BATCH
  type t = { x : < m : int > }
  MERLIN
  type t = { x : < m : int > }

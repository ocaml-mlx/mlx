# mlx

An OCaml syntax dialect which adds to JSX expressions to the language.

```
let header title = 
  <header>
    <h1>title</h1>
  </header>

let page =
  <html>
    <body>
      (header "Hello, world!")
      <div>
        "Some content gere"
      </div>
    </body>
  </html>
```

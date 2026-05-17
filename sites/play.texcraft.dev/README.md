# Texcraft Playgound

The Texcraft Playground consists of three small pieces:

1. `src/`: A Texcraft TeX engine that is compiled to WebAssembly/WASM.

1. `ui/`: A TypeScript/React frontend app that embeds the WASM code to run TeX scripts.

1. `backend.go`: A backend service with two responsibilites: serving the static UI files
    and managing the share feature, which involves persisting data in Redis.
    Ideally this would have been written in Rust to keep the number of technologies in
    the repo to a minimum...but writing HTTP services in Rust just seems more complicated than Go!
    This is definitely open to change though.

The Dockerfile illustates how the pieces are chained together to build the playground.

## Working on the frontend

When working on the frontend it's easiest to work inside the `ui` directly and use `npm` commands:

- `npm start`: compile the Rust and TypeScript code and run the debug server.

- `npm builddev`: compile the Rust and TypeScript code in debug mode.

- `npm build`: compile the TypeScript code in production mode. This assumes the Rust code has been compiled separetely. 

- `npm check`: run the TypeScript typechecker.

To compile the Rust/WASM code manually from the `ui` directory, run:
```
wasm-pack build ../src
```
Include the `--release` flag if needed.

## Working on the backend

To compile the backend code you generally need to compile the UI first with `npm run builddev`.
The backend code won't compile otherwise because it assumes certain static assets have been generated.
A hack to get around this is to copy some static assets manually:

```
mkdir -p ui/dist && cp ui/static/index.html ui/dist
```

After either approach, you can play the usual game of `go run .` and `go test .`

## TODO

1. In a hash collision case, just increment the num views thing
1. When shared, change the URL
1. When click welcome, etc., then change the URL. Maybe need react router? 
1. Handle the case when the share link is for the welcome page - just return /welcom
1. Wire up the docs endpoint
1. Get docs for a specific command. Will want to sub `` in the docs for Tex , having `````` would be cool
1. Write the tutorial
1. It would be awesome to run Tex examples in the docs in the playground


# TIP-ocaml

See https://cs.au.dk/~amoeller/spa/ for more information.

# Dev Setup:

`opam switch create tip 4.14.0`
`make setup`

Check out the Semgrep repo, and in it, run:

`make dev-setup`
`make core`
`dune install`

To run:

`./_build/default/src/main/Main.exe`

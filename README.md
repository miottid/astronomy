# Astronomy

OCaml library for astronomy calculations.

Inspired from the book [Practical Astronomy with your Calculator or Spreadsheet](https://www.amazon.com/Practical-Astronomy-your-Calculator-Spreadsheet-ebook/dp/B00E3UR5FQ/ref=sr_1_1?dchild=1&keywords=Practical+Astronomy+with+your+Calculator+or+Spreadsheet&qid=1626079939&sr=8-1).

## Development

**Setup opam environment:**

```shell
opam switch create 5.0.0
eval $(opam env)
opam install --yes --deps-only --with-test . # Library dependencies
opam install alcotest merlin ocp-indent utop ocamlformat ocaml-lsp-server odoc # Development dependencies
```

**Commands:**

```shell
make test # Run tests
make docs # Build documentation
dune exec examples/main.exe # Run playground
make build # Build library
```

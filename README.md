# Astro

Ocaml library for astronomy calculations.

Inspired from the book [Practical Astronomy with your Calculator or Spreadsheet](https://www.amazon.com/Practical-Astronomy-your-Calculator-Spreadsheet-ebook/dp/B00E3UR5FQ/ref=sr_1_1?dchild=1&keywords=Practical+Astronomy+with+your+Calculator+or+Spreadsheet&qid=1626079939&sr=8-1).

## Development

**Requirements:**

```shell
opam switch create . ocaml-base-compiler.4.11.0
opam install .
```

**Run test:**

```shell
dune runtest
```

**Run playground:**

```shell
dune exec bin/astro.exe
```
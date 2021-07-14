(lang dune 2.8)

(name astronomy)

(generate_opam_files true)

(license MIT)

(authors "David Miotti <david.miotti@gmail.com>")

(maintainers "David Miotti <david.miotti@gmail.com>")

(source
 (github miottid/astronomy))

(bug_reports "https://github.com/miottid/astronomy/issues")

(homepage "https://github.com/miottid/astronomy")

(documentation "https://github.com/miottid/astronomy")

(package
 (name astronomy)
 (synopsis "OCaml astronomy library.")
 (description "Perform astronomy calculations.")
 (depends
  (ppx_inline_test (>= 0.14.1))
  (ocaml
   (>= 4.11.0))
  dune))
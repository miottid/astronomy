(lang dune 2.8)

(name astro)

(generate_opam_files true)

(license ISC)

(authors "David Miotti <david.miotti@gmail.com>")

(maintainers "David Miotti <david.miotti@gmail.com>")

(source
 (github miottid/astrol))

(bug_reports "https://github.com/miottid/astro/issues")

(homepage "https://github.com/miottid/astro")

(documentation "https://github.com/miottid/astro")

(package
 (name astro)
 (synopsis "An astronomy calculation library.")
 (description "All you need for astronomy calculations.")
 (depends
  (ppx_inline_test (>= 0.14.1))
  (ocaml
   (>= 4.11.0))
  dune))
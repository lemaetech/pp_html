(lang dune 2.7)

(name pp_html)

(version 0.0.1)

(generate_opam_files true)

(source
 (github bikallem/pp_html))

(license MPL-2.0)

(authors "Bikal Lem, <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name pp_html)
 (synopsis "Pretty prints html5")
 (depends
  (ocaml
   (>= 4.11.1))
  core
  reparse
  sexplib
  sexp_pretty
  ppx_jane))

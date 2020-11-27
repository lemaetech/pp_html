(lang dune 2.7)

(name pp_html)

(version 0.0.1)

(generate_opam_files true)

(source
 (github bikallem/gilung))

(license MPL-2.0)

(authors "Bikal Lem, <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name pp_html)
 (synopsis "Pretty prints html5")
 (depends
  (ocaml
   (>= 4.11.1))
  reparse
  sexplib
  sexp_pretty
  ppx_sexp_conv
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)))

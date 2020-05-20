(lang dune 2.0)
(name ez-cmdliner)
(allow_approximate_merlin)

(generate_opam_files true)

(source (github ocamlpro/ez-cmdliner))
(license GPL-3.0-or-later)
(authors
 "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>"
)
(maintainers "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>")

(package
 (name ez-cmdliner)
 (synopsis "Easy version of Cmdliner")
 (documentation "https://ocamlpro.github.io/ez-cmdliner/doc")
 (depends
  (cmdliner (>= 1.0.0)))
  )

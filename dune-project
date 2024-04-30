(lang dune 3.7)
(using menhir 2.1)

(name wacc)
(authors "jtm")
(maintainers "jtm")
(license ISC)
(homepage "https://github.com/jtmcx/wacc")
(source (uri "git+https://github.com/jtmcx/wacc"))
(bug_reports "https://github.com/jtmcx/wacc/issues")
(documentation "https://github.com/jtmcx/wacc")

(package
 (name wacc)
 (synopsis "A C Compiler")
 (description "A C Compiler")
 (depends
   ocaml
   dune
   menhir
   odoc
   ppx_deriving
   ppx_sexp_conv
   sexplib))

(generate_opam_files true)

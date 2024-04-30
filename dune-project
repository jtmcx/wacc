(lang dune 3.7)
(using menhir 2.1)

(name wacc)
(authors "jtm")
(maintainers "jtm")
(license ISC)
(homepage "https://github.com/jtmcx/wacc")
(source (uri "https://github.com/jtmcx/wacc"))
(documentation "https://github.com/jtmcx/wacc")

(package
 (name wacc)
 (synopsis "A C Compiler")
 (description "A C Compiler")
 (depends
   ocaml
   dune
   menhir
   ppx_deriving))

(generate_opam_files true)

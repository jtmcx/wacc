# Building

Install OCaml dependencies using opam,

    $ opam switch create wacc 4.13.0
    $ eval $(opam env --switch=wacc)
    $ opam install --deps-only -y .

and then,

    $ make


let usage = "wacc [--lex|--parse|--codegen]"

let mode_lex = ref false
let mode_parse = ref false
let mode_codegen = ref false

let argspec =
  [ ("--codegen", Arg.Set mode_codegen, "Compile an input file")
  ; ("--lex",     Arg.Set mode_lex,     "Lex an input file")
  ; ("--parse",   Arg.Set mode_parse,   "Parse an input file")
  ]

let () =
  Arg.parse argspec ignore usage

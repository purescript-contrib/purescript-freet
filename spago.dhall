{ name = "freet"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "exists"
  , "prelude"
  , "psci-support"
  , "tailrec"
  , "tuples"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

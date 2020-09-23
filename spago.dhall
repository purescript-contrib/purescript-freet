{ name = "freet"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "exists"
  , "free"
  , "prelude"
  , "psci-support"
  , "tailrec"
  , "tuples"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

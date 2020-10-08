{ name = "react"
, dependencies =
  [ "console"
  , "effect"
  , "exceptions"
  , "maybe"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

{ name = "gemini"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "deku"
  , "tuples"
  , "typelevel"
  , "enums"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

{ name = "gemini"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "deku"
  , "tuples"
  , "typelevel-prelude"
  , "typelevel"
  , "enums"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

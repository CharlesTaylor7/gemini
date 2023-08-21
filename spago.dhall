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
  , "hyrule"
  , "arrays"
  , "ordered-collections"
  , "sequences"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

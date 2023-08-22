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
  , "safe-coerce"
  , "lists"
  , "foldable-traversable"
  , "maybe"
  , "numbers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

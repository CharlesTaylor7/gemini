{ name = "gemini"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "deku"
  , "tuples"
  -- , "typelevel-prelude"
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
  , "type-equality"
  , "control"
  , "partial"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

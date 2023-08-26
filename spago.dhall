{ name = "gemini"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "deku"
  , "tuples"
  , "typelevel"
  , "enums"
  , "hyrule"
  , "arrays"
  , "ordered-collections"
  , "safe-coerce"
  , "lists"
  , "foldable-traversable"
  , "unfoldable"
  , "maybe"
  , "numbers"
  , "type-equality"
  , "control"
  , "partial"
  , "catenable-lists"
  , "strings"
  , "integers"
  , "random"
  , "web-uievents"
  , "debug"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

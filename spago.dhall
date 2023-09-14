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
  , "control"
  , "partial"
  -- , "catenable-lists"
  , "strings"
  , "integers"
  , "random"
  , "debug"
  , "web-uievents"
  , "web-events"
  , "web-dom"
  , "st"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

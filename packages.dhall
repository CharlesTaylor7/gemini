let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230820/packages.dhall
        sha256:f002e9182da135bb48f3f1937830c57a66a422e21fc03fa0922755e8b444de66

in  upstream
  with deku =
      { repo =
          "https://github.com/mikesol/purescript-deku.git"
      , version =
          "v0.9.24"
      , dependencies =
          [ "aff"
          , "arrays"
          , "bolson"
          , "catenable-lists"
          , "control"
          , "css"
          , "effect"
          , "either"
          , "fast-vect"
          , "filterable"
          , "foldable-traversable"
          , "foreign-object"
          , "free"
          , "hyrule"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "quickcheck"
          , "record"
          , "safe-coerce"
          , "st"
          , "strings"
          , "stringutils"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          , "web-dom"
          , "web-events"
          , "web-html"
          , "web-uievents"
          ]
      }

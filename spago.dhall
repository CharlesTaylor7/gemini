{ name = "hello-deku"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "deku"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

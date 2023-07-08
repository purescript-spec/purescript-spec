let config = ./spago.dhall

in config // {
  sources = config.sources # [ "test/**/*.purs" ],
  dependencies = config.dependencies #
    [ "console"
    , "node-buffer"
    , "node-child-process"
    , "node-fs"
    , "node-fs-aff"
    , "node-os"
    , "node-process"
    , "node-streams"
    , "refs"
    ]
}

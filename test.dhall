let config = ./spago.dhall

in config // {
  sources = config.sources # [ "test/**/*.purs" ],
  dependencies = config.dependencies #
    [ "console"
    , "node-buffer"
    , "node-child-process"
    , "node-event-emitter"
    , "node-fs"
    , "node-os"
    , "node-process"
    , "node-streams"
    , "refs"
    ]
}

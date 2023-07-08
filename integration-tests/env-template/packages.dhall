let packages =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.9-20230629/packages.dhall

    with
      spec = (SPEC_REPO_PATH/spago.dhall as Location)

in packages

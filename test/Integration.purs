module Test.Integration where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String as Str
import Data.Traversable (for_, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.ChildProcess as Proc
import Node.ChildProcess.Types as IO
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.FS.Aff as FS
import Node.FS.Stats (isDirectory)
import Node.OS (tmpdir)
import Node.Process (cwd)
import Node.Stream as Stream
import Test.Spec (SpecT, afterAll, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Reads the contents of `/integration-tests/cases` and turns each
-- | subdirectory into a test case. See `/integration-tests/cases/README` for
-- | more details.
integrationSpecs :: SpecT Aff Unit Aff Unit
integrationSpecs = do
  { runFile, cleanupEnvironment } <- liftEffect $ prepareEnvironment { debug: false }

  afterAll (\_ -> cleanupEnvironment) $
    describe "Integration tests" do
      cases <- lift $ FS.readdir "integration-tests/cases"

      for_ cases \testName -> do
        let testDir = "integration-tests/cases/" <> testName
        it testName do
          program <- FS.readTextFile UTF8 $ testDir // "Main.purs"
          goldenOutput <- FS.readTextFile UTF8 $ testDir // "output.txt"
          actualOutput <- runFile program
          Str.trim actualOutput `shouldEqual` Str.trim goldenOutput

prepareEnvironment :: { debug :: Boolean } -> Effect { runFile :: String -> Aff String, cleanupEnvironment :: Aff Unit }
prepareEnvironment { debug } =
  Ref.new Nothing <#> \envDirVar ->
    { runFile: \program -> do
        dir <- ensureEnvironmentInitialized envDirVar
        ensureDirExists $ dir // "test"
        FS.writeTextFile UTF8 (dir // "test/Main.purs") program
        run' dir "npx" ["spago", "build"]
        res <- run dir "npx" $ ["spago", "--quiet", "--no-psa", "test"]
        pure $
          -- Removing ESC characters (which are used for colors), because
          -- they're very inconvenient to include in the golden output files.
          S.replaceAll (S.Pattern "\x1B") (S.Replacement "") res

    , cleanupEnvironment:
        liftEffect (Ref.read envDirVar) >>= case _ of
          Just dir | debug -> do
            traceLog "Skipping environment cleanup due to debug=true flag"
            traceLog $ "Environment at: " <> dir
          Just dir ->
            rmdirRec dir
          Nothing ->
            pure unit
    }
  where
    traceLog
      | debug = log
      | otherwise = const $ pure unit

    ensureEnvironmentInitialized envDirVar =
      liftEffect (Ref.read envDirVar) >>= case _ of
        Just d ->
          pure d
        Nothing -> do
          dir <- liftEffect tmpdir >>= \tmp -> FS.mkdtemp $ tmp // "purescript-spec-test-env"
          liftEffect $ Ref.write (Just dir) envDirVar
          traceLog $ "Preparing environment in: " <> dir
          copyAllFiles { from: "integration-tests/env-template", to: dir }
          patchRepoPath $ dir // "packages.dhall"
          run' dir "npm" ["install", "purescript@0.15", "spago@0.21"]
          pure dir

    patchRepoPath file = do
      thisDir <- liftEffect cwd
      content <- FS.readTextFile UTF8 file
      let newContent = S.replaceAll (S.Pattern "SPEC_REPO_PATH") (S.Replacement thisDir) content
      FS.writeTextFile UTF8 file newContent

    copyAllFiles { from, to } = do
      ensureDirExists to
      FS.readdir from >>= traverse_ \f -> do
        stat <- FS.stat f
        if isDirectory stat then
          copyAllFiles { from: from // f, to: to // f }
        else
          FS.copyFile (from // f) (to // f)

    run' cwd cmd = void <<< run cwd cmd

    run cwd cmd args = do
      traceLog $ "Running: " <> S.joinWith " " ([cmd] <> args)
      makeAff \cb -> do
        output <- Ref.new ""
        let return = cb <<< Right =<< Ref.read output

        proc <- Proc.spawn' cmd args _ { cwd = Just cwd, appendStdio = Just [IO.ignore, IO.pipe, IO.pipe] }

        for_ [Proc.stdout, Proc.stderr] \pipe ->
          pipe proc # on_ Stream.dataH \buf -> do
            str <- Buffer.toString UTF8 buf
            void $ output # Ref.modify (_ <> str)

        proc # on_ Proc.exitH \_ -> return
        proc # on_ Proc.errorH \_ -> return
        proc # on_ Proc.disconnectH return
        proc # on_ Proc.closeH \_ -> return

        pure nonCanceler

    ensureDirExists dir =
      FS.access dir >>= case _ of
        Just _err -> FS.mkdir dir
        Nothing -> pure unit

    rmdirRec dir = do
      stat <- FS.stat dir
      if isDirectory stat then do
        FS.readdir dir >>= traverse_ \f -> rmdirRec $ dir // f
        FS.rmdir dir
      else
        FS.unlink dir

pathConcat :: String -> String -> String
pathConcat a b = a <> "/" <> b

infixl 5 pathConcat as //

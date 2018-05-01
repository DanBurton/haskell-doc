#!/usr/bin/env stack
{- stack
  script
  --trace
  --resolver lts-11.6
  --package process
  --package directory
-}

-- usage: snapshot=nightly-2018-04-28 package=tardis version=0.4.1.0 ./package-check.hs

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Control.Exception as Exception
import qualified System.Directory as Dir
import qualified System.IO as System
import System.Exit (ExitCode(..))
import qualified System.Process as Process
import qualified System.Environment as Env
import Data.Monoid ((<>))
import Control.Monad (forM_, when)
import Data.Typeable (Typeable)

type CommandName=FilePath
type Arg=String
type Question=String

data Command = Command
  { logDir :: FilePath
  , question :: Question
  , commandName :: CommandName
  , args :: [Arg]
  }

data PackageCheck = PackageCheck
  { snapshot :: String
  , package :: String
  , pkgVer :: String
  }

data DirectoryAlreadyExists = DirectoryAlreadyExists FilePath
  deriving (Show, Typeable)
instance Exception.Exception DirectoryAlreadyExists

data NoCabalFileFoundForPackage = NoCabalFileFoundForPackage String
  deriving (Show, Typeable)
instance Exception.Exception NoCabalFileFoundForPackage

packageCheckPrefixDir :: FilePath
packageCheckPrefixDir = ".package-check"

-- Run Command with Args, redirecting both stdin and stderr to FilePath
-- Also logs a bit of stuff
loggyRun :: Command -> IO ExitCode
loggyRun Command {logDir, question, commandName, args} = do
  putStrLn $ "[ " <> logDir <> " ]"
  putStr $ question <> " ... "

  Dir.createDirectory $ packageCheckPrefixDir <> "/" <> logDir

  let outLogFile = packageCheckPrefixDir <> "/" <> logDir <> "/out.txt"
  let errLogFile = packageCheckPrefixDir <> "/" <> logDir <> "/err.txt"
  let logLogFile = packageCheckPrefixDir <> "/" <> logDir <> "/log.txt"
  stdoutHandle <- System.openFile outLogFile System.WriteMode
  stderrHandle <- System.openFile errLogFile System.WriteMode
  let workingDir = Nothing
      env = Nothing
      stdin = Nothing
      stdout = Just stdoutHandle
      stderr = Just stderrHandle
  proc <- Process.runProcess commandName args workingDir env stdin stdout stderr
  exitCode <- Process.waitForProcess proc

  let log = System.appendFile logLogFile . (++ "\n")

  log ""
  log "# stack.yaml"
  log =<< System.readFile "stack.yaml"
  log "# end stack.yaml"

  log ""
  log $ Process.showCommandForUser commandName args

  log ""
  log $ "Exit: " <> show exitCode

  let answer =
        case exitCode of
          ExitSuccess -> "YES"
          _ -> "NO"

  log ""
  log $ question <> " ... " <> answer

  putStrLn answer

  return exitCode

commonBuildArgs :: [Arg]
commonBuildArgs = ["--haddock", "--profile"]

testArgs :: [Arg]
testArgs = ["--profile", "--coverage"]

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

getEnvOr :: String -> String -> IO String
getEnvOr envVar def = Env.lookupEnv envVar <&> \case
  Nothing -> def
  Just "" -> def
  Just value -> value


genCommands :: PackageCheck -> [Command]
genCommands PackageCheck {package, pkgVer, snapshot} =
  [ Command
    { logDir = "plan"
    , question = "Does " <> pkgVer <> " have a valid build plan with " <> snapshot <> "?"
    , commandName = "stack"
    , args = ["build", "--dry-run"]
    }
  , Command
    { logDir = "test-suites"
    , question = "Does " <> pkgVer <> " have any test suites?"
    , commandName = "grep"
    , args = ["-i", "test-suite", package <> ".cabal"]
    }
  , Command
    { logDir = "testPlan"
    , question = "Do the " <> pkgVer <> " test suites have a valid build plan with " <> snapshot <> "?"
    , commandName = "stack"
    , args = ["test", "--dry-run"]
    }
  , Command
    { logDir = "snapshot-deps"
    , question = "Do " <> pkgVer <> " snapshot deps build and haddock correctly?"
    , commandName = "stack"
    , args = ["build", "--only-snapshot"] ++ commonBuildArgs
    }
  , Command
    { logDir = "local-deps"
    , question = "Do " <> pkgVer <> " (other, local) deps build and haddock correctly?"
    , commandName = "stack"
    , args = ["build", "--only-dependencies"] ++ commonBuildArgs
    }
  , Command
    { logDir = "fast-warnings"
    , question = "Does " <> pkgVer <> " build with no errors/warnings?"
    , commandName = "stack"
    -- note the lack of commonBuildArgs here
    , args = ["build", "--fast", "--ghc-options", "-Werror -Weverything -fhide-source-paths"]
    }
  , Command
    { logDir = "build"
    , question = "Does " <> pkgVer <> " build?"
    , commandName = "stack"
    , args = ["build", "--ghc-options", "-Weverything -fhide-source-paths"] ++ commonBuildArgs
    }
  , Command
    { logDir = "test-snapshot-deps"
    , question = "Do " <> pkgVer <> " snapshot testing deps build and haddock correctly?"
    , commandName = "stack"
    , args = ["build", "--test", "--only-snapshot"] ++ commonBuildArgs
    }
  , Command
    { logDir = "test-local-deps"
    , question = "Do " <> pkgVer <> " (other, local) testing deps build and haddock correctly?"
    , commandName = "stack"
    , args = ["build", "--test", "--only-dependencies"] ++ commonBuildArgs
    }
  , Command
    { logDir = "tests"
    , question = "Does " <> pkgVer <> " pass its test suites?"
    , commandName = "stack"
    , args = ["test"] ++ testArgs
    }
  ]


unpackAndCd :: String -> Bool -> IO ()
unpackAndCd pkgVer force = do
  dirExists <- Dir.doesPathExist pkgVer
  when dirExists $ case force of
    True -> do
      putStrLn $ "Deleting existing directory: " <> pkgVer
      Dir.removeDirectoryRecursive pkgVer
    False -> Exception.throwIO $ DirectoryAlreadyExists pkgVer
  Process.rawSystem "stack" ["unpack", pkgVer]
  Dir.setCurrentDirectory pkgVer


main :: IO ()
main = do
  snapshot <- getEnvOr "snapshot" "nightly-2018-04-28"
  package <- getEnvOr "package" "tardis"
  version <- getEnvOr "version" "0.4.1.0"
  let pkgVer = package <> "-" <> version

  args <- Env.getArgs
  let local = "--local" `elem` args
      force = "--force" `elem` args

  putStrLn $ "snapshot = " <> snapshot
  putStrLn $ "pkgVer = " <> pkgVer
  putStrLn $ "local = " <> show local
  putStrLn $ "force = " <> show force

  when (not local) $ unpackAndCd pkgVer force

  Dir.doesFileExist (package <> ".cabal") >>= \case
    False -> Exception.throwIO (NoCabalFileFoundForPackage package)
    True -> return ()

  when force $ do
    Dir.doesPathExist packageCheckPrefixDir >>= \case
      True -> do
        putStrLn $ "Deleting existing directory: " <> packageCheckPrefixDir
        Dir.removeDirectoryRecursive packageCheckPrefixDir
      False -> return ()

  Dir.createDirectory packageCheckPrefixDir
  System.writeFile "stack.yaml" $ "resolver: " <> snapshot

  let commands = genCommands PackageCheck
        { snapshot = snapshot
        , package = package
        , pkgVer = pkgVer
        }

  forM_ commands loggyRun

  return ()

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- This main contains a daemon that updates to https://github.com/smelc/miso-darkcraw
-- HEAD every 5 minutes. If the head has changed since the last update,
-- it tries to update the balance data (see 'Balance' and 'Weight'). If the
-- update yields a change, this daemon creates a commit for it and tries
-- to push it (without forcing). This daemon hereby needs to run in a context
-- where it has write access to the remote repo.
--
-- This daemon is in the main repo to KISS. It needs to be compiled from
-- within a @nix-shell@ with @cabal build judge@ (or with @nix-shell --run
-- 'cabal build judge') and then should be executed from outside the @nix-shell@.
-- This is mandatory, because this daemon issues commands of the form
-- @nix-shell --run '...'@ and nix-shell nesting is erroneous.
--
-- This daemon supposes that it executes from the @app/@ directory.
module Main where

import Control.Concurrent
import Control.Exception
import Data.Functor ((<&>))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Shelly
import Prelude hiding (FilePath)

newtype GitRev = GitRev Text
  deriving (Eq)

main :: IO ()
main = do
  loop Nothing

loop :: (Maybe GitRev) -> IO ()
loop (mrev :: Maybe GitRev) = do
  either <- try (ioStep mrev) :: IO (Either SomeException (Maybe GitRev))
  nextRev <- case either of
    Left err
      | isJust (asyncExceptionFromException err :: Maybe SomeAsyncException) ->
        throw err
    Left err -> do
      putStrLn $ show err -- Print error
      return mrev -- Keep previous rev
    Right newRev -> pure $ newRev -- Use new rev obtained
  threadDelay (1000000 * 60 * 15) -- Wait 15 minutes
  loop nextRev

-- | Like 'step' but in the IO monad instead of the 'Sh' monad.
ioStep :: Maybe GitRev -> IO (Maybe GitRev)
ioStep mrev = shelly $ verbosely $ carefulStep mrev

carefulStep :: Maybe GitRev -> Sh (Maybe GitRev)
carefulStep mrev = do
  diffs <- gitDiff
  if null diffs
    then -- Can proceed
      Just <$> (step mrev)
    else do
      inspect "Not proceeding, because local repo has unexpected changes:"
      mapM_ (\l -> inspect ("  " <> l)) diffs
      return mrev

-- | Given the last head tested, update from remote, and if there is a change,
-- try to fix the balance. Returns the current head.
step :: Maybe GitRev -> Sh GitRev
step mrev = do
  knownRev <- case mrev of Nothing -> gitHead; Just rev -> pure rev
  run_ "git" ["fetch"]
  run_ "git" ["reset", "--hard", "@{u}"] -- Update to remote HEAD
  currentRev <- gitHead
  if currentRev == knownRev
    then pure currentRev -- No change
    else do
      -- A change, try to update the balance
      change <- update
      lastRev <- if change then gitHead else pure currentRev -- Retrieve new head
      return lastRev

update :: Sh Bool
update = do
  run_ "nix-shell" ["--run", "cabal build"] -- Build, if this fails shelly throws an exception;
  -- which is fine we don't want to proceed in this case.
  run_ "nix-shell" ["--run", "cabal run updb"]
  diffs <- gitDiff <&> (filter ((==) weightFile)) <&> listToMaybe
  case diffs of
    Nothing -> pure False
    Just _ -> do
      run_ "git" ["add", weightFile]
      run_ "git" ["commit", "-m", "[judge] Update balance", "--no-verify"]
      run_ "git" ["push"]
      pure True

weightFile :: Text
weightFile = "test/Weight.hs"

-- Now comes various functions to query the state of the git repo

-- | Returns the current head of the local repo
gitHead :: Sh GitRev
gitHead = do
  res :: Text <- run "git" ["rev-parse", "HEAD"]
  return (GitRev (rstripln res))

-- | Returns the list of modified files, i.e. unstaged files with a diff
gitDiff :: Sh [Text]
gitDiff = do
  files :: Text <- run "git" ["diff", "--name-only", "--diff-filter=ACMR"]
  return $ filter (not . Text.null) (Text.lines files)

-- Various boring helper functions

-- | Remove the trailing newlines of a 'Text'
rstripln :: Text -> Text
rstripln (s :: Text) =
  if "\n" `Text.isSuffixOf` s
    then rstripln (Text.take (Text.length s - 2) s)
    else s

#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , cradle
ghc-options: -threaded
-}

import Cradle (addArgs, cmd, run, (&))
import System.Environment (getArgs)

main :: IO ()
main = do
  cmdLineArgs <- getArgs
  run $ cmd "cabal" & addArgs (defaultArgs <> fmap testOption cmdLineArgs)
  where
    defaultArgs =
      [ "test",
        "--test-show-details=direct",
        testOption "--color",
        testOption "--unicode",
        testOption "--pretty",
        testOption "--format=specdoc",
        testOption "--failure-report=dist-newstyle/hspec.log"
      ]
    testOption = mappend "--test-option="
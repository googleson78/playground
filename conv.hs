{-# LANGUAGE BlockArguments #-}

import System.Environment
import System.Directory
import System.Process
import Data.Bifunctor
import Data.List
import System.Exit
import Data.Foldable
import Data.Maybe
import Data.List.NonEmpty (nonEmpty)
import Control.Concurrent.Async

-- TODO!:
-- handle folders in input folders - copyFile crashes
-- TODO:
-- use path (don't have to think so much (+ can probably copy recursively))
-- TODO:
-- better errors?

main :: IO ()
main = do
  xs <- getArgs
  case fmap (first nonEmpty) $ unsnoc xs of
    Nothing -> print usage
    Just (Nothing, _) -> print usage
    Just (Just fromDirs, toDir) -> do
      putStrLn $ "creating " ++ toDir
      createDirectoryIfMissing True toDir
      putStrLn $ "created " ++ toDir
      for_ fromDirs \dir -> do
        files <- listDirectory dir
        let target = toDir ++ "/" ++ dir
        putStrLn $ "creating " ++ target
        createDirectoryIfMissing True target
        putStrLn $ "created " ++ target
        withCurrentDirectory dir $ convDir target files


convDir :: FilePath -> [FilePath] -> IO ()
convDir toDir files = do
  let (toConvert, others) = partition (isSuffixOf ".flac") files
      conv filename = do
        let fromTarget = filename ++ ".flac"
            toTarget = toDir ++ "/" ++ filename ++ ".mp3"
        (code, _, err) <- readProcessWithExitCode "ffmpeg" ["-i", fromTarget, "-codec:a", "libmp3lame", "-qscale:a", "0", toTarget] ""
        case code of
          ExitSuccess -> pure ()
          n@(ExitFailure _) -> do
            print "err while doing ffmpeg:"
            print err
            exitWith n

  forConcurrently_ others \file -> copyFile file $ toDir ++ "/" ++ file
  mapConcurrently_ conv $ map (\x -> fromMaybe (error $ "called me with non-flac!" ++ x) $ dropFlac x) toConvert

-- TODO: use path things for this
dropFlac :: String -> Maybe String
dropFlac = fmap reverse . stripPrefix "calf." . reverse

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x:xs) =
  case unsnoc xs of
    Nothing -> Just ([], x)
    Just (ys, y) -> Just (x:ys, y)

usage :: String
usage =
  "Usage: conv.hs from0 from1 ... fromn to"

module Main where

import Lib
import Text.Regex.Posix
import System.Environment
import Options.Applicative
import Data.List
import Data.Semigroup ((<>))
import System.Directory
import System.Directory.PathWalk
import Control.Monad
import System.FilePath.Posix

import Utils

data HrepOptions = HrepOptions
  { pattern         :: Regex 
  , path            :: [FilePath] }

data HrepMatch = HrepMatch
  { text :: String
  , line :: Int
  , column :: Int
  , filePath :: String }

instance Show HrepMatch where
  show (HrepMatch t l c f) = intercalate ":" [f, (show l), (show c), t]

hrepParser :: Parser HrepOptions
hrepParser = HrepOptions
  <$> argument regexOpt (metavar "PATTERN")
  <*> many (argument str (metavar "PATH..."))

explodePaths :: [FilePath] -> Maybe Regex -> IO [FilePath]
explodePaths path _ = return path
  
hrep :: HrepOptions -> IO ()
hrep (HrepOptions pattern paths) = do 
  forM_ paths $ \path -> do
    isFile <- doesFileExist path
    if isFile
      then hrepFile pattern path
      else pathWalk path $ \dir subdirs files -> do
        forM_ files $ \file -> do
          hrepFile pattern $ joinPath [dir, file]
          hrep $ HrepOptions pattern $ map (\s -> joinPath [dir, s]) subdirs

hrepFile :: Regex -> FilePath -> IO ()
hrepFile pattern fileName = do
  contents <- readFile fileName
  forM_ (zip (lines contents) [1..]) $ \(line,lineNo) -> do
    case matchOnce pattern line of
      Nothing      -> return ()
      Just matches -> forM_ matches $ \(offset,_) -> do
        printMatch $ HrepMatch {text=line, line=lineNo, column=(offset+1), filePath=fileName}

printMatch :: HrepMatch -> IO ()
printMatch match = putStrLn $ show match

main :: IO ()
main = hrep =<< execParser opts
  where
    opts = info (helper <*> hrepParser)
      (fullDesc
      <> progDesc "hrep - grep tool for vim"
      <> header "hrep - grep tool for vim")

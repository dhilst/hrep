module Main where

import Lib
import Text.Regex.Posix
import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))

import Utils

data HrepOptions = HrepOptions
  { pattern         :: Regex 
  , path            :: [FilePath] 
  , fileSearchRegex :: Maybe Regex }


data Match = Match
  { text :: String
  , line :: Int
  , column :: Int
  , filePath :: String }


hrepParser :: Parser HrepOptions
hrepParser = HrepOptions
  <$> argument regexOpt (metavar "PATTERN")
  <*> many (argument str (metavar "PATH..."))
  <*> option maybeRegexOpt 
    (short 'G' 
    <> long "file-search-regex" 
    <> metavar "PATTERN"
    <> help "Only search file whose name match PATTERN"
    <> value Nothing)

explodePaths :: [FilePath] Regex -> [FilePath]
explodePaths path "" = path
  
hrep :: HrepOptions -> IO ()
hrep (HrepOptions pattern [] Nothing) = hrepFile pattern "-"
hrep (HrepOptions pattern paths Nothing) = mapM_ (hrepFile pattern) (explodePaths paths "")
hrep (HrepOptions pattern paths (Just filePattern)) = mapM_ (hrepFile pattern) (explodePaths paths filePattern))

hrepFile :: Regex -> FilePath -> IO ()
hrepFile pattern fileName = do
  contents <- if fileName == "-" then getContents else readFile fileName
  mapM_ printMatch $ filter (match pattern) (lines contents)

printMatch :: String -> IO ()
printMatch match = putStrLn match

main :: IO ()
main = hrep =<< execParser opts
  where
    opts = info (helper <*> hrepParser)
      (fullDesc
      <> progDesc "hrep - grep tool for vim"
      <> header "hrep - grep tool for vim")

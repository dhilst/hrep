module Utils 
  ( regexOpt
  , maybeRegexOpt
  )
where

import Text.Regex.Posix
import Options.Applicative

eitherRegex :: String -> Either String (Maybe Regex)
eitherRegex pattern
  | pattern == "" = Left  $ "No pattern provided"
  | otherwise     = Right $ Just (makeRegex pattern)

maybeRegexOpt :: ReadM (Maybe Regex)
maybeRegexOpt = eitherReader eitherRegex

regexOpt :: ReadM Regex
regexOpt = eitherReader $ Right . makeRegex

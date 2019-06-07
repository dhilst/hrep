module Utils 
  ( regexOpt
  )
where

import Text.Regex.Posix
import Options.Applicative

regexOpt :: ReadM Regex
regexOpt = eitherReader $ Right . makeRegex

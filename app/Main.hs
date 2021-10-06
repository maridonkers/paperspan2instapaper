{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Options.Applicative as OA
import Paperspan2Instapaper

-- data Args = Args String String
newtype Args = Args String

args :: OA.Parser Args
args =
  Args
    <$> OA.strOption
      ( OA.long "input"
          <> OA.help "Input is a Paperspan export file (HTML)."
      )

-- <*> OA.strOption
--   ( OA.long "output"
--       <> OA.help "Output is an Instapaper import file (CSV)."
--   )

argsInfo :: OA.ParserInfo Args
argsInfo = OA.info args OA.fullDesc

main :: IO ()
main = do
  -- Args fiPath foPath <- OA.execParser argsInfo
  Args fiPath <- OA.execParser argsInfo
  -- processFile fiPath foPath
  processFile fiPath

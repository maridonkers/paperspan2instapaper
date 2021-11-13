{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Options.Applicative  as OA
import           Paperspan2Instapaper

newtype Args = Args String

args :: OA.Parser Args
args = Args <$> OA.strArgument
  (OA.metavar "HTMLFILE" <> OA.help "Input is a Paperspan export file (HTML).")

argsInfo :: OA.ParserInfo Args
argsInfo = OA.info args OA.fullDesc

main :: IO ()
main = do
  Args fiPath <- OA.execParser argsInfo
  processFile fiPath

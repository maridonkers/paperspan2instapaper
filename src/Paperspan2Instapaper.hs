{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Paperspan2Instapaper where

import Data.Aeson.Types
import qualified Data.Char as C
import Data.List
import Data.Yaml
import GHC.Generics
import qualified System.IO as I
import qualified Text.Printf as TP
import Text.Regex.PCRE
import Text.XML.HXT.Core

configFile :: String
configFile = "folders.yaml"

timeStampZero :: String
timeStampZero = "0000000000000"

type FolderName = String

type FolderPath = String

folderNameEmpty :: String
folderNameEmpty = ""

folderPathEmpty :: String
folderPathEmpty = ""

folderPathDefault :: String
folderPathDefault = "Paperspan"

data Folder = Folder
  { folderName :: FolderName,
    folderPath :: FolderPath
  }
  deriving (Generic, Read, Show, FromJSON)

folderEmpty :: Folder
folderEmpty = Folder folderNameEmpty folderPathEmpty

conditionSourceUrl :: String
conditionSourceUrl = "url"

conditionSourceText :: String
conditionSourceText = "text"

data Condition = Condition
  { conditionRegExp :: String,
    conditionSource :: String,
    conditionFolderName :: String
  }
  deriving (Generic, Read, Show, FromJSON)

type Folders = [Folder]

type Conditions = [Condition]

data Selectors = Selectors
  { selectorsFolders :: Folders,
    selectorsConditions :: Conditions
  }
  deriving (Generic, Read, Show, FromJSON)

processFile :: String -> IO ()
processFile fiPath = do
  (res :: Either ParseException Selectors) <-
    decodeFileEither configFile
  case res of
    Left err -> print err
    Right val -> processFile' fiPath val

processFile' :: String -> Selectors -> IO ()
processFile' fiPath selectors = do
  let (folders, conditions) =
        ( selectorsFolders selectors,
          selectorsConditions selectors
        )
  putStrLn "URL,Title,Selection,Folder,Timestamp"
  h <- I.openFile fiPath I.ReadMode
  I.hSetEncoding h I.utf8
  contents <- I.hGetContents h
  let doc = readString [withParseHTML yes, withWarnings no] contents
  links <-
    runX $
      doc //> hasName "a"
        -- >>> withTraceLevel 5 traceTree
        >>> ( getAttrValue "href"
                &&& (deep getText `orElse` getAttrValue "href")
                &&& getAttrValue "time_added"
            )
  let putStrLnLink = putStrLnLinkFactory folders conditions
  mapM_ putStrLnLink links
  where
    putStrLnLinkFactory folders conditions =
      \link -> do
        let (url, (txt, ts)) = link
            url' = toLowerString url
            txt' = toLowerString txt
            fon = getFolderNameBySelector url' txt' conditions
            fop = getFolderPathByName folders fon
            ts' = timestampStr ts
            str = TP.printf "%s,\"%s\",%s,\"%s\",%s" url txt url fop ts'
        putStrLn str
      where
        getFolderPathByName fos fon = do
          let fos' = find (\a -> folderName a == fon) fos
          maybe folderPathDefault folderPath fos'
        timestampStr ts =
          if null ts then timeStampZero else ts
        toLowerString :: [Char] -> [Char]
        toLowerString = map C.toLower

getFolderNameBySelector :: String -> String -> Conditions -> FolderName
getFolderNameBySelector url txt conditions = do
  getFolderNameBySelector' conditions
  where
    getFolderNameBySelector' :: Conditions -> FolderName
    getFolderNameBySelector' [] = folderNameEmpty
    getFolderNameBySelector' (c : cs)
      | conditionSource c == conditionSourceUrl = do
        let regExp = conditionRegExp c
            folder = conditionFolderName c
        if url =~ regExp
          then folder
          else getFolderNameBySelector' cs
      | conditionSource c == conditionSourceText = do
        let regExp = conditionRegExp c
            folder = conditionFolderName c
        if txt =~ regExp
          then folder
          else getFolderNameBySelector' cs
      | otherwise = folderNameEmpty

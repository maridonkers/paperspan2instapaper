{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Paperspan2Instapaper where

import Data.Aeson.Types
import qualified Data.Char as C
import Data.Yaml
import GHC.Generics
import qualified System.IO as I
import qualified Text.Printf as TP
import Text.Regex.PCRE
import Text.XML.HXT.Core

configFile :: String
configFile = "folders.yaml"

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
  deriving (Generic, Read, Show, Eq, FromJSON, ToJSON)

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
  deriving (Generic, Read, Show, FromJSON, ToJSON)

type Folders = [Folder]

type Conditions = [Condition]

data Selectors = Selectors
  { selectorsFolders :: Folders,
    selectorsConditions :: Conditions
  }
  deriving (Generic, Read, Show, FromJSON, ToJSON)

-- processFile :: String -> String -> IO ()
processFile :: String -> IO ()
-- processFile fiPath foPath = do
processFile fiPath = do
  (res :: Either ParseException Selectors) <-
    decodeFileEither configFile
  case res of
    Left err -> print err
    -- Right val -> processFile' fiPath foPath val
    Right val -> processFile' fiPath val

-- processFile' :: String -> String -> Selectors -> IO ()
processFile' :: String -> Selectors -> IO ()
-- processFile' fiPath foPath selectors = do
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
        >>> ( getAttrValue "href"
                &&& (deep getText `orElse` getAttrValue "href")
                &&& getAttrValue "time_added"
            )
  let putStrLnLink = putStrLnLinkFactory folders conditions
  mapM_ putStrLnLink links
  where
    putStrLnLinkFactory folders conditions =
      \link -> do
        let (url, (text, timestamp)) = link
            url' = toLowerString url
            text' = toLowerString text
            fon = getFolderBySelector url' text' conditions
            fop = getFolderPathByName folders fon
            timestamp' = timestampStr timestamp
            str = TP.printf "%s,\"%s\",%s,\"%s\",%s" url text url fop timestamp'
        putStrLn str
      where
        getFolderPathByName fos fon = do
          let fos' = filter (\a -> folderName a == fon) fos
          if null fos'
            then folderPathDefault
            else folderPath $ head fos'
        timestampStr ts =
          if null ts
            then "0000000000000"
            else ts
        toLowerString :: [Char] -> [Char]
        toLowerString = map C.toLower

getFolderBySelector :: String -> String -> Conditions -> FolderName
getFolderBySelector url text conditions = do
  getFolderBySelector' conditions
  where
    getFolderBySelector' :: Conditions -> FolderName
    getFolderBySelector' [] = folderNameEmpty
    getFolderBySelector' (c : cs)
      | conditionSource c == conditionSourceUrl = do
        let regExp = conditionRegExp c
            folder = conditionFolderName c
        if url =~ regExp
          then folder
          else getFolderBySelector' cs
      | conditionSource c == conditionSourceText = do
        let regExp = conditionRegExp c
            folder = conditionFolderName c
        if text =~ regExp
          then folder
          else getFolderBySelector' cs
      | otherwise = folderNameEmpty

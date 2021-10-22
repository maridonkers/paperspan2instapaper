{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Paperspan2Instapaper where

import Data.Aeson.Types
import qualified Data.Char as C
import Data.List
import qualified Data.List.Split as S
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

folderPaperspanNone :: String
folderPaperspanNone = "Read Later"

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
  putStrLn "URL,Title,Selection,Folder,Timestamp"
  h <- I.openFile fiPath I.ReadMode
  I.hSetEncoding h I.utf8
  contents <- I.hGetContents h
  let doc = readString [withParseHTML yes, withWarnings no] contents
      anchorFields =
        [ getAttrValue "href",
          getAllText,
          getAttrValue "time_added"
        ]
  links <-
    runX $
      doc
        //> (isElem >>> hasName "body")
          /> (isElem >>> hasName "ul")
          /> ( (isElem >>> hasName "h2")
                 <+> multi (isElem >>> hasName "a")
             )
        >>> ( getName
                &&& catA anchorFields
            )
  let partitionedLinks = S.chunksOf (length anchorFields) links
  putStrLnLinks folderPaperspanNone partitionedLinks
  where
    (folders, conditions) =
      ( selectorsFolders selectors,
        selectorsConditions selectors
      )
    putStrLnLinks _ [] = pure ()
    putStrLnLinks folder (l : ls) = do
      let [(tag, url), (_, txt), (_, ts)] = l
          url' = toLowerString url
          txt' = rstrip $ toLowerString txt
          fop
            | tag == "h2" = txt
            | folder == folderPaperspanNone =
              getFolderPathByName folders $
                getFolderNameBySelector url' txt' conditions
            | otherwise = folder
          ts' = timestampStr ts
          str =
            TP.printf
              "%s,\"%s\",%s,\"%s\",%s"
              url
              txt
              url
              fop
              ts'
      if tag == "a" then putStrLn str else pure ()
      putStrLnLinks fop ls
      where
        getFolderPathByName fos fon = do
          let fos' = find (\a -> folderName a == fon) fos
          maybe folderPathDefault folderPath fos'
        timestampStr ts =
          if null ts then timeStampZero else ts
        toLowerString = map C.toLower
        rstrip = reverse . dropWhile C.isSpace . reverse

-- There may be several sub tags with text (e.g. <i>); combine them.
getAllText =
  listA
    ( multi getText
        `orElse` getAttrValue "href"
    )
    >>> arr concat

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

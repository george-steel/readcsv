{-# LANGUAGE LambdaCase #-}

{-|
Module: Text.Read.CSV
Description: CSV parser/emitter based on ReadP
Copyright: © 2017 George Steel
License: MIT
Maintainer: george.steel@gmail.com

Implementation of RFC 4180-compliant CSV parser and emitter.
Works with both LF (Unix) and CRLF (DOS) linebreaks and allows varianble-length records.
The header line (if one is used) is the first item in the parsed list, and can be separated with a pattern match.
-}

module Text.Read.CSV (
    readCSV, writeCSV, writeCSVstrict, csvTableP
) where

import Text.ParserCombinators.ReadP
import Control.Monad
--import Control.Applicative
import Data.List
import Data.Maybe

munchSpaces :: ReadP ()
munchSpaces =
    do  s <- look
        skip s
    where
        skip (' ':s) = get >> skip s
        skip _       = return ()

unescaped :: ReadP String
unescaped = munch (`notElem` ",\"\r\n\t")

notquote :: ReadP String
notquote = munch (/= '"')

singlequoted :: ReadP String
singlequoted = do
    char '"'
    s <- notquote
    char '"'
    return s

fullquoted :: ReadP String
fullquoted = fmap (intercalate "\"") (many1 singlequoted)

csvcell :: ReadP String
csvcell = munchSpaces >> (unescaped +++ fullquoted)

csvrow :: ReadP [String]
csvrow = sepBy1 csvcell (char ',')

crlf :: ReadP ()
crlf = optional (char '\r') >> char '\n' >> return ()

-- | ReadP parser for CSV.
csvTableP :: ReadP [[String]]
csvTableP = sepBy1 csvrow crlf

csvfile :: ReadP [[String]]
csvfile = do
    table <- csvTableP
    optional crlf
    eof
    return table

-- | Parse CSV from a string.
readCSV :: String -> Maybe [[String]]
readCSV = fmap fst . listToMaybe . readP_to_S csvfile


escapecell :: String -> String
escapecell cs = "\"" ++ (cs >>= \case '"' -> "\"\""; x -> [x]) ++ "\""

escapeifneeded :: String -> String
escapeifneeded cs = if any (`elem` ",\"\r\n\t") cs then escapecell cs else cs

-- | Emit CSV, escaping fields as necessary. Uses Unix linebreaks and a trailing newline.
writeCSV :: [[String]] -> String
writeCSV = join . fmap ((++ "\n") . intercalate "," . fmap escapeifneeded)

-- | Emit CSV with CRLF line breaks ans no trailing newline, as specified by RFC 4180.
writeCSVstrict :: [[String]] -> String
writeCSVstrict = intercalate "\r\n" . fmap (intercalate "," . fmap escapeifneeded)

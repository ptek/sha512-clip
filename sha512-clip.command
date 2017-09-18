#!/usr/bin/env stack
{- stack script
     --install-ghc
     --resolver=lts-9.5
     --package=SHA
     --package=text
     --package=turtle
     --package=foldl
-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Digest.Pure.SHA (sha512, showDigest)

import           Control.Foldl        (list)
import           Data.Maybe           (maybe)
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Turtle

main :: IO ()
main =
  readClipboard
  >>= maybe errorMissingInput calculateSha512
  >>= printAndWriteClipboard

calculateSha512 :: Text -> IO Text
calculateSha512 t = do
  echo ("Calculating SHA512 of '" <> unsafeTextToLine t <> "'")
  return . T.pack . showDigest . sha512 . fromString $ T.unpack t

errorMissingInput :: IO Text
errorMissingInput = do
  stderr (return "Sorry, clipboard contains no usable text.")
  sleep 5.0
  exit (ExitFailure 1)

printAndWriteClipboard :: Text -> IO ()
printAndWriteClipboard token = do
  let tokenLine = unsafeTextToLine token
  shell "pbcopy" (return tokenLine)
  echo tokenLine
  echo "Above token has been copied to the clipboard"

readClipboard :: IO (Maybe Text)
readClipboard = do
  clip <- inshell "pbpaste" empty `fold` list
  return $ case (map lineToText clip) of
    [t] -> Just t
    _   -> Nothing

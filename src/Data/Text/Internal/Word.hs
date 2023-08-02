{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE BangPatterns, CPP #-}

----------------------------------------------------------------
--                                                  ~ 2019.04.11
-- |
-- Module      :  Data.Trie.Text.Convenience
-- Copyright   :  Copyright (c) 2019 michael j. klein
-- License     :  BSD3
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
--
-- Methods for accessing `Text` in terms of its constituent words
----------------------------------------------------------------

module Data.Text.Internal.Word where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Array as TA

#if MIN_VERSION_text(2,0,0)

import Data.Word (Word8)
type TextElem = Word8

dropElem :: Int -> Text -> Text
dropElem = TU.dropWord8

#else

import Data.Word (Word16)
type TextElem = Word16

dropElem :: Int -> Text -> Text
dropElem = TU.dropWord16

#endif

headElem :: Text -> TextElem
{-# INLINE [0] headElem #-}
headElem (TI.Text xs i0 _) = xs `TA.unsafeIndex` i0

tailElem :: Text -> Maybe Text
{-# INLINE [1] tailElem #-}
tailElem xs =
  if T.null xs
     then Nothing
     else Just $ dropElem 1 xs

toListElem :: Text -> [TextElem]
toListElem xs =
  case tailElem xs of
    Nothing -> []
    Just ys -> headElem xs : toListElem ys

-- | Length of `Text` in `TextElem`'s
-- lengthElem xs == length (toListElem xs)
lengthElem :: Text -> Int
{-# INLINE [0] lengthElem #-}
lengthElem (TI.Text _ off len) = len - off


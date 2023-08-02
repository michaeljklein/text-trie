{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE BangPatterns #-}

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
import Data.Word (Word16)

type TextElem = Word16

head16 :: Text -> TextElem
{-# INLINE [0] head16 #-}
head16 (TI.Text xs i0 _) = xs `TA.unsafeIndex` i0

tail16 :: Text -> Maybe Text
{-# INLINE [1] tail16 #-}
tail16 xs =
  if T.null xs
     then Nothing
     else Just $ TU.dropWord16 1 xs

toList16 :: Text -> [TextElem]
toList16 xs =
  case tail16 xs of
    Nothing -> []
    Just ys -> head16 xs : toList16 ys

-- | Length of `Text` in `TextElem`'s
-- length16 xs == length (toList16 xs)
length16 :: Text -> Int
{-# INLINE [0] length16 #-}
length16 (TI.Text _ off len) = len - off


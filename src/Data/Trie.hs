-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}
----------------------------------------------------------------
--                                                  ~ 2014.10.09
-- |
-- Module      :  Data.Trie
-- Copyright   :  Copyright (c) 2008--2015 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- An efficient implementation of finite maps from strings to values.
-- The implementation is based on /big-endian patricia trees/, like
-- "Data.IntMap". We first trie on the elements of "Data.ByteString"
-- and then trie on the big-endian bit representation of those
-- elements. For further details on the latter, see
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--    Workshop on ML, September 1998, pages 77-86,
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve/
--    /Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--    October 1968, pages 514-534.
--
-- This module aims to provide an austere interface, while being
-- detailed enough for most users. For an extended interface with
-- many additional functions, see "Data.Trie.Convenience". For
-- functions that give more detailed (potentially abstraction-breaking)
-- access to the data strucuture, or for experimental functions
-- which aren't quite ready for the public API, see "Data.Trie.Internal".
----------------------------------------------------------------

module Data.Trie
    (
    -- * Data type
      Trie()
    -- , TrieText()

    -- * Basic functions
    , empty, null, singleton, size
    -- , emptyText, nullText, singletonText, sizeText

    -- * Conversion functions
    , fromList, toListBy, toList, keys, elems
    -- , fromListText, toListByText, toListText, keysText, elemsText

    -- * Query functions
    , lookupBy, lookup, member, submap, match, matches
    -- , lookupByText, lookupText, memberText, submapText, matchText, matchesText

    -- * Single-value modification
    , alterBy, insert, adjust, delete
    -- , alterByText, insertText, adjustText, deleteText

    -- * Combining tries
    , mergeBy, unionL, unionR
    -- , mergeByText, unionLText, unionRText

    -- * Mapping functions
    -- , mapByText, filterMapText
    ) where

import Prelude hiding     (null, lookup)
import qualified Prelude  (null, lookup)

import Data.Trie.Internal
import Data.Trie.Errors   (impossible)
import Data.ByteString    (ByteString)
import qualified Data.ByteString as S

-- import Data.Text          (Text)
-- import qualified Data.Text as T

import Data.Maybe         (isJust)
import Control.Monad      (liftM)
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- Conversion functions
---------------------------------------------------------------}

-- | Convert association list into a trie. On key conflict, values
-- earlier in the list shadow later ones.
fromList :: [(ByteString,a)] -> Trie a
{-# INLINE fromList #-}
fromList = foldr (uncurry insert) empty

-- fromListText :: [(Text,a)] -> TrieText a
-- {-# INLINE fromListText #-}
-- fromListText = foldr (uncurry insertText) emptyText


-- | Convert trie into association list. Keys will be in sorted order.
toList :: Trie a -> [(ByteString,a)]
{-# INLINE toList #-}
toList  = toListBy (,)

-- toListText :: TrieText a -> [(Text,a)]
-- {-# INLINE toListText #-}
-- toListText  = toListByText (,)

-- FIX? should 'keys' and 'elems' move to Data.Trie.Convenience instead?

-- | Return all keys in the trie, in sorted order.
keys :: Trie a -> [ByteString]
{-# INLINE keys #-}
keys  = toListBy const

-- keysText :: TrieText a -> [Text]
-- {-# INLINE keysText #-}
-- keysText = toListByText const

-- | Return all values in the trie, in sorted order according to the keys.
elems :: Trie a -> [a]
{-# INLINE elems #-}
elems  = toListBy (flip const)

-- elemsText :: TrieText a -> [a]
-- {-# INLINE elemsText #-}
-- elemsText = toListByText (flip const)

{---------------------------------------------------------------
-- Query functions (just recurse)
---------------------------------------------------------------}

-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix.
lookupBy :: (Maybe a -> Trie a -> b) -> ByteString -> Trie a -> b
{-# INLINE lookupBy #-}
lookupBy f = lookupBy_ f (f Nothing empty) (f Nothing)


-- lookupByText :: (Maybe a -> TrieText a -> b) -> Text -> TrieText a -> b
-- {-# INLINE lookupByText #-}
-- lookupByText f = lookupByText_ f (f Nothing emptyText) (f Nothing)

-- | Return the value associated with a query string if it exists.
lookup :: ByteString -> Trie a -> Maybe a
{-# INLINE lookup #-}
lookup = lookupBy_ const Nothing (const Nothing)

-- lookupText :: Text -> TrieText a -> Maybe a
-- {-# INLINE lookupText #-}
-- lookupText = lookupByText_ const Nothing (const Nothing)


-- TODO? move to "Data.Trie.Convenience"?
-- | Does a string have a value in the trie?
member :: ByteString -> Trie a -> Bool
{-# INLINE member #-}
member q = isJust . lookup q

-- memberText :: Text -> TrieText a -> Bool
-- {-# INLINE memberText #-}
-- memberText q = isJust . lookupText q

-- | Given a query, find the longest prefix with an associated value
-- in the trie, returning that prefix, it's value, and the remaining
-- string.
match :: Trie a -> ByteString -> Maybe (ByteString, a, ByteString)
match t q =
    case match_ t q of
    Nothing    -> Nothing
    Just (n,x) ->
        case S.splitAt n q of
        (p,q') -> Just (p, x, q')

-- matchText :: TrieText a -> Text -> Maybe (Text, a, Text)
-- matchText t q =
--     case matchText_ t q of
--     Nothing    -> Nothing
--     Just (n,x) ->
--         case T.splitAt n q of
--         (p,q') -> Just (p, x, q')

-- | Given a query, find all prefixes with associated values in the
-- trie, returning the prefixes, their values, and their remaining
-- strings. This function is a good producer for list fusion.
matches :: Trie a -> ByteString -> [(ByteString, a, ByteString)]
{-# INLINE matches #-}
matches t q = map f (matches_ t q)
    where
    f (n,x) =
        case S.splitAt n q of
        (p,q') -> (p, x, q')

-- matchesText :: TrieText a -> Text -> [(Text, a, Text)]
-- {-# INLINE matchesText #-}
-- matchesText t q = map f (matchesText_ t q)
--     where
--     f (n,x) =
--         case T.splitAt n q of
--         (p,q') -> (p, x, q')

{---------------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
---------------------------------------------------------------}

-- | Insert a new key. If the key is already present, overrides the
-- old value
insert :: ByteString -> a -> Trie a -> Trie a
{-# INLINE insert #-}
insert = alterBy (\_ x _ -> Just x)

-- insertText :: Text -> a -> TrieText a -> TrieText a
-- {-# INLINE insertText #-}
-- insertText = alterByText (\_ x _ -> Just x)

-- | Apply a function to the value at a key.
adjust :: (a -> a) -> ByteString -> Trie a -> Trie a
{-# INLINE adjust #-}
adjust f q = adjustBy (\_ _ -> f) q (impossible "adjust")
-- TODO: benchmark vs the definition with alterBy/liftM

-- adjustText :: (a -> a) -> Text -> TrieText a -> TrieText a
-- {-# INLINE adjustText #-}
-- adjustText f q = adjustByText (\_ _ -> f) q (impossible "adjust")


-- | Remove the value stored at a key.
delete :: ByteString -> Trie a -> Trie a
{-# INLINE delete #-}
delete q = alterBy (\_ _ _ -> Nothing) q (impossible "delete")

-- deleteText :: Text -> TrieText a -> TrieText a
-- {-# INLINE deleteText #-}
-- deleteText q = alterByText (\_ _ _ -> Nothing) q (impossible "delete")

{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- | Combine two tries, resolving conflicts by choosing the value
-- from the left trie.
unionL :: Trie a -> Trie a -> Trie a
{-# INLINE unionL #-}
unionL = mergeBy (\x _ -> Just x)

-- unionLText :: TrieText a -> TrieText a -> TrieText a
-- {-# INLINE unionLText #-}
-- unionLText = mergeByText (\x _ -> Just x)

-- | Combine two tries, resolving conflicts by choosing the value
-- from the right trie.
unionR :: Trie a -> Trie a -> Trie a
{-# INLINE unionR #-}
unionR = mergeBy (\_ y -> Just y)

-- unionRText :: TrieText a -> TrieText a -> TrieText a
-- {-# INLINE unionRText #-}
-- unionRText = mergeByText (\_ y -> Just y)

----------------------------------------------------------------
----------------------------------------------------------- fin.

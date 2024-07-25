{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-name-shadowing #-}

-- The MagicHash is for unboxed primitives (-fglasgow-exts also works)
{-# LANGUAGE CPP, MagicHash #-}
{-# LANGUAGE BangPatterns #-}

----------------------------------------------------------------
--                                                  ~ 2019.04.03
-- |
-- Module      :  Data.Trie.BitTwiddle
-- Copyright   :  Copyright (c) 2002 Daan Leijen, 2019 michael j. klein
-- License     :  BSD3
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
--
-- Functions to treat 'Word' as a bit-vector for big-endian patricia
-- trees. This code is duplicated from "Data.IntMap". The only
-- differences are that some of the conversion functions are
-- specialized to 'Word8' for bytestrings, instead of being specialized
-- to 'Int'.
----------------------------------------------------------------

module Data.Trie.Text.BitTwiddle
    ( Prefix, Mask

    , zero, nomatch

    , mask, shorter, branchMask
    ) where

import Data.Text.Internal.Word (TextElem)

import Data.Bits

#if MIN_VERSION_text(2,0,0)
import GHC.Exts  (Int(..), uncheckedShiftRLWord8# )
import GHC.Word (Word8(..))
#elif __GLASGOW_HASKELL__ >= 902
import GHC.Exts  (Int(..), uncheckedShiftRLWord16# )
import GHC.Word (Word16(..))
#elif __GLASGOW_HASKELL__ >= 503
import GHC.Exts  (Int(..), uncheckedShiftRL# )
import GHC.Word (Word16(..))
#elif __GLASGOW_HASKELL__
import GlaExts   (Int(..), uncheckedShiftRL# )
import GHC.Word (Word16(..))
#else
import Data.Word (Word16(..))
#endif

----------------------------------------------------------------

type KeyElem = TextElem
type Prefix  = KeyElem
type Mask    = KeyElem


uncheckedShiftRL :: TextElem -> Int -> TextElem
{-# INLINE [0] uncheckedShiftRL #-}
-- GHC: use unboxing to get @uncheckedShiftRL@ inlined.
#if MIN_VERSION_text(2,0,0)
uncheckedShiftRL (W8# x) (I# i) = W8# (uncheckedShiftRLWord8# x i)
#elif __GLASGOW_HASKELL__ >= 902
uncheckedShiftRL (W16# x) (I# i) = W16# (uncheckedShiftRLWord16# x i)
#elif __GLASGOW_HASKELL__
uncheckedShiftRL (W16# x) (I# i) = W16# (uncheckedShiftRL# x i)
#else
uncheckedShiftRL x i = shiftR x i
#endif


{---------------------------------------------------------------
-- Endian independent bit twiddling (Trie endianness, not architecture)
---------------------------------------------------------------}

-- | Is the value under the mask zero?
zero :: KeyElem -> Mask -> Bool
{-# INLINE [0] zero #-}
zero !i !m = i .&. m == 0


-- | Does a value /not/ match some prefix, for all the bits preceding
-- a masking bit? (Hence a subtree matching the value doesn't exist.)
nomatch :: KeyElem -> Prefix -> Mask -> Bool
{-# INLINE [0] nomatch #-}
nomatch !i !p !m = mask i m /= p

mask :: KeyElem -> Mask -> Prefix
{-# INLINE [0] mask #-}
mask !i !m = maskW i m


{---------------------------------------------------------------
-- Big endian operations (Trie endianness, not architecture)
---------------------------------------------------------------}

-- | Get mask by setting all bits higher than the smallest bit in
-- @m@. Then apply that mask to @i@.
maskW :: TextElem -> TextElem -> Prefix
{-# INLINE [0] maskW #-}
maskW !i !m = i .&. (complement (m-1) `xor` m)
-- TODO: try the alternatives mentioned in the Containers paper:
-- \i m -> natToElem (i .&. (negate m - m))
-- \i m -> natToElem (i .&. (m * complement 1))
-- N.B. these return /all/ the low bits, and therefore they are not equal functions for all m. They are, however, equal when only one bit of m is set.


-- | Determine whether the first mask denotes a shorter prefix than
-- the second.
shorter :: Mask -> Mask -> Bool
{-# INLINE [0] shorter #-}
shorter !m1 !m2 = m1 > m2



-- | Determine first differing bit of two prefixes.
branchMask :: Prefix -> Prefix -> Mask
{-# INLINE [0] branchMask #-}
branchMask !p1 !p2
    = highestBitMask (p1 `xor` p2)


{---------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently
  in three ways:
  * convert to a floating point value and the mantissa tells us the
    [log2(x)] that corresponds with the highest bit position. The
    mantissa is retrieved either via the standard C function [frexp]
    or by some bit twiddling on IEEE compatible numbers (float).
    Note that one needs to use at least [double] precision for an
    accurate mantissa of 32 bit numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and
    shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an
  AMD Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC
  instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even
  faster than a single CISC instruction (BSR)!
---------------------------------------------------------------}

{---------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is
  set. It is found by first setting all bits in lower positions
  than the highest bit and than taking an exclusive or with the
  original value. Allthough the function may look expensive, GHC
  compiles this into excellent C code that subsequently compiled
  into highly efficient machine code. The algorithm is derived from
  Jorg Arndt's FXT library.
---------------------------------------------------------------}
-- highestBitMask !x
--     = case (x .|. uncheckedShiftRL x 1) of
--        !x -> case (x .|. uncheckedShiftRL x 2) of
--         !x -> case (x .|. uncheckedShiftRL x 4) of
--          !x -> case (x .|. uncheckedShiftRL x 8) of
--           -- !x -> case (x .|. uncheckedShiftRL x 16) of
--            -- !x -> case (x .|. uncheckedShiftRL x 32) of   -- for 64 bit platforms
--             !x -> (x `xor` uncheckedShiftRL x 1)
highestBitMask :: TextElem -> TextElem
{-# INLINE [0] highestBitMask #-}
highestBitMask !x0 =
  let !x1 = x0 .|. uncheckedShiftRL x0 1 in
  let !x2 = x1 .|. uncheckedShiftRL x1 2 in
  let !x3 = x2 .|. uncheckedShiftRL x2 4 in
  let !x4 = x3 .|. uncheckedShiftRL x3 8 in
  (x4 `xor` uncheckedShiftRL x4 1)


----------------------------------------------------------------
----------------------------------------------------------- fin.


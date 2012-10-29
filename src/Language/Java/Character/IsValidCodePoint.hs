module Language.Java.Character.IsValidCodePoint
(
  IsValidCodePoint(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

class Enum c => IsValidCodePoint c where
  isValidCodePoint ::
    c
    -> Bool
  isNotValidCodePoint ::
    c
    -> Bool
  isNotValidCodePoint =
    not . isValidCodePoint

instance IsValidCodePoint Char where
  isValidCodePoint c =
    ord c `S.member` isValidCodePointSet

instance IsValidCodePoint Int where
  isValidCodePoint c =
    c `S.member` isValidCodePointSet

instance IsValidCodePoint Integer where
  isValidCodePoint c =
    c `S.member` isValidCodePointSet

instance IsValidCodePoint Word8 where
  isValidCodePoint c =
    c `S.member` isValidCodePointSet

instance IsValidCodePoint Word16 where
  isValidCodePoint c =
    c `S.member` isValidCodePointSet

instance IsValidCodePoint Word32 where
  isValidCodePoint c =
    c `S.member` isValidCodePointSet

instance IsValidCodePoint Word64 where
  isValidCodePoint c =
    c `S.member` isValidCodePointSet

instance HasResolution a => IsValidCodePoint (Fixed a) where
  isValidCodePoint c =
    c `S.member` isValidCodePointSet

isValidCodePointSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isValidCodePointSet =
  let r = [
            [0..200000]
          ]
  in S.fromList . concat $ r
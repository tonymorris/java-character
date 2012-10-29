-- | Simulates the @isValidCodePoint@ Java method. <http://docs.oracle.com/javase/6/docs/api/java/lang/Character.html#isValidCodePoint%28int%29>
module Language.Java.Character.IsValidCodePoint
(
  IsValidCodePoint(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

-- | Instances simulate Java characters and provide a decision on simulating @isValidCodePoint@.
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
            [0..1114111]
          ]
  in S.fromList . concat $ r
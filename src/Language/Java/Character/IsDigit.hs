module Language.Java.Character.IsDigit
(
  IsDigit(..)
) where

import Data.Char hiding (isDigit)
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

class Enum c => IsDigit c where
  isDigit ::
    c
    -> Bool
  isNotDigit ::
    c
    -> Bool
  isNotDigit =
    not . isDigit

instance IsDigit Char where
  isDigit c =
    ord c `S.member` isDigitSet

instance IsDigit Int where
  isDigit c =
    c `S.member` isDigitSet

instance IsDigit Integer where
  isDigit c =
    c `S.member` isDigitSet

instance IsDigit Word8 where
  isDigit c =
    c `S.member` isDigitSet

instance IsDigit Word16 where
  isDigit c =
    c `S.member` isDigitSet

instance IsDigit Word32 where
  isDigit c =
    c `S.member` isDigitSet

instance IsDigit Word64 where
  isDigit c =
    c `S.member` isDigitSet

instance HasResolution a => IsDigit (Fixed a) where
  isDigit c =
    c `S.member` isDigitSet

isDigitSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isDigitSet =
  let r = [
            [48..57]
          , [1632..1641]
          , [1776..1785]
          , [2406..2415]
          , [2534..2543]
          , [2662..2671]
          , [2790..2799]
          , [2918..2927]
          , [3047..3055]
          , [3174..3183]
          , [3302..3311]
          , [3430..3439]
          , [3664..3673]
          , [3792..3801]
          , [3872..3881]
          , [4160..4169]
          , [4969..4977]
          , [6112..6121]
          , [6160..6169]
          , [6470..6479]
          , [65296..65305]
          , [66720..66729]
          , [120782..120831]
          ]
  in S.fromList . concat $ r
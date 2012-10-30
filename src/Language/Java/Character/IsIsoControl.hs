-- | Simulates the @isISOControl@ Java method. <http://docs.oracle.com/javase/6/docs/api/java/lang/Character.html#isISOControl%28int%29>
module Language.Java.Character.IsIsoControl
(
  IsIsoControl(..)
) where

import Data.Char
import Data.Word
import Data.Set.Diet(Diet)
import qualified Data.Set.Diet as S

-- | Instances simulate Java characters and provide a decision on simulating @isISOControl@.
class Enum c => IsIsoControl c where
  isIsoControl ::
    c
    -> Bool
  isNotIsoControl ::
    c
    -> Bool
  isNotIsoControl =
    not . isIsoControl

instance IsIsoControl Char where
  isIsoControl c =
    ord c `S.member` isIsoControlSet

instance IsIsoControl Int where
  isIsoControl c =
    c `S.member` isIsoControlSet

instance IsIsoControl Integer where
  isIsoControl c =
    c `S.member` isIsoControlSet

instance IsIsoControl Word8 where
  isIsoControl c =
    c `S.member` isIsoControlSet

instance IsIsoControl Word16 where
  isIsoControl c =
    c `S.member` isIsoControlSet

instance IsIsoControl Word32 where
  isIsoControl c =
    c `S.member` isIsoControlSet

instance IsIsoControl Word64 where
  isIsoControl c =
    c `S.member` isIsoControlSet

isIsoControlSet ::
  (Num a, Enum a, Ord a) =>
  Diet a
isIsoControlSet =
  let r = [
            [0..31]
          , [127..159]
          ]
  in S.fromList . concat $ r
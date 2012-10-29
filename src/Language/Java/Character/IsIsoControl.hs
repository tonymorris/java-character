module Language.Java.Character.IsIsoControl
(
  IsIsoControl(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

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

instance HasResolution a => IsIsoControl (Fixed a) where
  isIsoControl c =
    c `S.member` isIsoControlSet

isIsoControlSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isIsoControlSet =
  let r = [
            [0..31]
          , [127..159]
          ]
  in S.fromList . concat $ r
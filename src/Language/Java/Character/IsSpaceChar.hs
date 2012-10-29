module Language.Java.Character.IsSpaceChar
(
  IsSpaceChar(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

class Enum c => IsSpaceChar c where
  isSpaceChar ::
    c
    -> Bool
  isNotSpaceChar ::
    c
    -> Bool
  isNotSpaceChar =
    not . isSpaceChar

instance IsSpaceChar Char where
  isSpaceChar c =
    ord c `S.member` isSpaceCharSet

instance IsSpaceChar Int where
  isSpaceChar c =
    c `S.member` isSpaceCharSet

instance IsSpaceChar Integer where
  isSpaceChar c =
    c `S.member` isSpaceCharSet

instance IsSpaceChar Word8 where
  isSpaceChar c =
    c `S.member` isSpaceCharSet

instance IsSpaceChar Word16 where
  isSpaceChar c =
    c `S.member` isSpaceCharSet

instance IsSpaceChar Word32 where
  isSpaceChar c =
    c `S.member` isSpaceCharSet

instance IsSpaceChar Word64 where
  isSpaceChar c =
    c `S.member` isSpaceCharSet

instance HasResolution a => IsSpaceChar (Fixed a) where
  isSpaceChar c =
    c `S.member` isSpaceCharSet

isSpaceCharSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isSpaceCharSet =
  let r = [
            [32]
          , [160]
          , [5760]
          , [6158]
          , [8192..8203]
          , [8232..8233]
          , [8239]
          , [8287]
          , [12288]
          ]
  in S.fromList . concat $ r
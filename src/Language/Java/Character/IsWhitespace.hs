-- | Simulates the @isDefined@ Java method. <http://docs.oracle.com/javase/6/docs/api/java/lang/Character.html#isWhitespace%28int%29>
module Language.Java.Character.IsWhitespace
(
  IsWhitespace(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

-- | Instances simulate Java characters and provide a decision on simulating @isWhitespace@.
class Enum c => IsWhitespace c where
  isWhitespace ::
    c
    -> Bool
  isNotWhitespace ::
    c
    -> Bool
  isNotWhitespace =
    not . isWhitespace

instance IsWhitespace Char where
  isWhitespace c =
    ord c `S.member` isWhitespaceSet

instance IsWhitespace Int where
  isWhitespace c =
    c `S.member` isWhitespaceSet

instance IsWhitespace Integer where
  isWhitespace c =
    c `S.member` isWhitespaceSet

instance IsWhitespace Word8 where
  isWhitespace c =
    c `S.member` isWhitespaceSet

instance IsWhitespace Word16 where
  isWhitespace c =
    c `S.member` isWhitespaceSet

instance IsWhitespace Word32 where
  isWhitespace c =
    c `S.member` isWhitespaceSet

instance IsWhitespace Word64 where
  isWhitespace c =
    c `S.member` isWhitespaceSet

instance HasResolution a => IsWhitespace (Fixed a) where
  isWhitespace c =
    c `S.member` isWhitespaceSet

isWhitespaceSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isWhitespaceSet =
  let r = [
            [9..13]
          , [28..32]
          , [5760]
          , [6158]
          , [8192..8198]
          , [8200..8203]
          , [8232..8233]
          , [8287]
          , [12288]
          ]
  in S.fromList . concat $ r
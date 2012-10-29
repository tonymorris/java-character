module Language.Java.Character.IsTitleCase
(
  IsTitleCase(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

class Enum c => IsTitleCase c where
  isTitleCase ::
    c
    -> Bool
  isNotTitleCase ::
    c
    -> Bool
  isNotTitleCase =
    not . isTitleCase

instance IsTitleCase Char where
  isTitleCase c =
    ord c `S.member` isTitleCaseSet

instance IsTitleCase Int where
  isTitleCase c =
    c `S.member` isTitleCaseSet

instance IsTitleCase Integer where
  isTitleCase c =
    c `S.member` isTitleCaseSet

instance IsTitleCase Word8 where
  isTitleCase c =
    c `S.member` isTitleCaseSet

instance IsTitleCase Word16 where
  isTitleCase c =
    c `S.member` isTitleCaseSet

instance IsTitleCase Word32 where
  isTitleCase c =
    c `S.member` isTitleCaseSet

instance IsTitleCase Word64 where
  isTitleCase c =
    c `S.member` isTitleCaseSet

instance HasResolution a => IsTitleCase (Fixed a) where
  isTitleCase c =
    c `S.member` isTitleCaseSet

isTitleCaseSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isTitleCaseSet =
  let r = [
            [453]
          , [456]
          , [459]
          , [498]
          , [8072..8079]
          , [8088..8095]
          , [8104..8111]
          , [8124]
          , [8140]
          , [8188]
          ]
  in S.fromList . concat $ r
-- | Simulates the @isIdentifierIgnorable@ Java method. <http://docs.oracle.com/javase/6/docs/api/java/lang/Character.html#isIdentifierIgnorable%28int%29>
module Language.Java.Character.IsIdentifierIgnorable
(
  IsIdentifierIgnorable(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

-- | Instances simulate Java characters and provide a decision on simulating @isIdentifierIgnorable@.
class Enum c => IsIdentifierIgnorable c where
  isIdentifierIgnorable ::
    c
    -> Bool
  isNotIdentifierIgnorable ::
    c
    -> Bool
  isNotIdentifierIgnorable =
    not . isIdentifierIgnorable

instance IsIdentifierIgnorable Char where
  isIdentifierIgnorable c =
    ord c `S.member` isIdentifierIgnorableSet

instance IsIdentifierIgnorable Int where
  isIdentifierIgnorable c =
    c `S.member` isIdentifierIgnorableSet

instance IsIdentifierIgnorable Integer where
  isIdentifierIgnorable c =
    c `S.member` isIdentifierIgnorableSet

instance IsIdentifierIgnorable Word8 where
  isIdentifierIgnorable c =
    c `S.member` isIdentifierIgnorableSet

instance IsIdentifierIgnorable Word16 where
  isIdentifierIgnorable c =
    c `S.member` isIdentifierIgnorableSet

instance IsIdentifierIgnorable Word32 where
  isIdentifierIgnorable c =
    c `S.member` isIdentifierIgnorableSet

instance IsIdentifierIgnorable Word64 where
  isIdentifierIgnorable c =
    c `S.member` isIdentifierIgnorableSet

instance HasResolution a => IsIdentifierIgnorable (Fixed a) where
  isIdentifierIgnorable c =
    c `S.member` isIdentifierIgnorableSet

isIdentifierIgnorableSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isIdentifierIgnorableSet =
  let r = [
            [0..8]
          , [14..27]
          , [127..159]
          , [173]
          , [1536..1539]
          , [1757]
          , [1807]
          , [6068..6069]
          , [8204..8207]
          , [8234..8238]
          , [8288..8291]
          , [8298..8303]
          , [65279]
          , [65529..65531]
          , [119155..119162]
          ]
  in S.fromList . concat $ r
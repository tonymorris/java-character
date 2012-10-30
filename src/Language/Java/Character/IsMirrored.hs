-- | Simulates the @isMirrored@ Java method. <http://docs.oracle.com/javase/6/docs/api/java/lang/Character.html#isMirrored%28int%29>
module Language.Java.Character.IsMirrored
(
  IsMirrored(..)
) where

import Data.Char
import Data.Word
import Data.Set.Diet(Diet)
import qualified Data.Set.Diet as S

-- | Instances simulate Java characters and provide a decision on simulating @isMirrored@.
class Enum c => IsMirrored c where
  isMirrored ::
    c
    -> Bool
  isNotMirrored ::
    c
    -> Bool
  isNotMirrored =
    not . isMirrored

instance IsMirrored Char where
  isMirrored c =
    ord c `S.member` isMirroredSet

instance IsMirrored Int where
  isMirrored c =
    c `S.member` isMirroredSet

instance IsMirrored Integer where
  isMirrored c =
    c `S.member` isMirroredSet

instance IsMirrored Word8 where
  isMirrored c =
    c `S.member` isMirroredSet

instance IsMirrored Word16 where
  isMirrored c =
    c `S.member` isMirroredSet

instance IsMirrored Word32 where
  isMirrored c =
    c `S.member` isMirroredSet

instance IsMirrored Word64 where
  isMirrored c =
    c `S.member` isMirroredSet

isMirroredSet ::
  (Num a, Enum a, Ord a) =>
  Diet a
isMirroredSet =
  let r = [
            [40..41]
          , [60]
          , [62]
          , [91]
          , [93]
          , [123]
          , [125]
          , [171]
          , [187]
          , [8249..8250]
          , [8261..8262]
          , [8317..8318]
          , [8333..8334]
          , [8512]
          , [8705..8708]
          , [8712..8717]
          , [8721]
          , [8725..8726]
          , [8730..8733]
          , [8735..8738]
          , [8740]
          , [8742]
          , [8747..8755]
          , [8761]
          , [8763..8780]
          , [8786..8789]
          , [8799..8800]
          , [8802]
          , [8804..8811]
          , [8814..8844]
          , [8847..8850]
          , [8856]
          , [8866..8867]
          , [8870..8888]
          , [8894..8895]
          , [8905..8909]
          , [8912..8913]
          , [8918..8941]
          , [8944..8959]
          , [8968..8971]
          , [8992..8993]
          , [9001..9002]
          , [10088..10101]
          , [10195..10198]
          , [10204..10206]
          , [10210..10219]
          , [10627..10648]
          , [10651..10671]
          , [10680]
          , [10688..10693]
          , [10697]
          , [10702..10706]
          , [10708..10709]
          , [10712..10716]
          , [10721]
          , [10723..10725]
          , [10728..10729]
          , [10740..10745]
          , [10748..10749]
          , [10762..10780]
          , [10782..10785]
          , [10788]
          , [10790]
          , [10793]
          , [10795..10798]
          , [10804..10805]
          , [10812..10814]
          , [10839..10840]
          , [10852..10853]
          , [10858..10861]
          , [10863..10864]
          , [10867..10868]
          , [10873..10915]
          , [10918..10925]
          , [10927..10966]
          , [10972]
          , [10974]
          , [10978..10982]
          , [10988..10990]
          , [10995]
          , [10999..11003]
          , [11005]
          , [12296..12305]
          , [12308..12315]
          , [65288..65289]
          , [65308]
          , [65310]
          , [65339]
          , [65341]
          , [65371]
          , [65373]
          , [65375..65376]
          , [65378..65379]
          ]
  in S.fromList . concat $ r
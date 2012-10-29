module Language.Java.Character.IsLetter
(
  IsLetter(..)
) where

import Data.Char hiding (isLetter)
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

class Enum c => IsLetter c where
  isLetter ::
    c
    -> Bool
  isNotLetter ::
    c
    -> Bool
  isNotLetter =
    not . isLetter

instance IsLetter Char where
  isLetter c =
    ord c `S.member` isLetterSet

instance IsLetter Int where
  isLetter c =
    c `S.member` isLetterSet

instance IsLetter Integer where
  isLetter c =
    c `S.member` isLetterSet

instance IsLetter Word8 where
  isLetter c =
    c `S.member` isLetterSet

instance IsLetter Word16 where
  isLetter c =
    c `S.member` isLetterSet

instance IsLetter Word32 where
  isLetter c =
    c `S.member` isLetterSet

instance IsLetter Word64 where
  isLetter c =
    c `S.member` isLetterSet

instance HasResolution a => IsLetter (Fixed a) where
  isLetter c =
    c `S.member` isLetterSet

isLetterSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isLetterSet =
  let r = [
            [65..90]
          , [97..122]
          , [170]
          , [181]
          , [186]
          , [192..214]
          , [216..246]
          , [248..566]
          , [592..705]
          , [710..721]
          , [736..740]
          , [750]
          , [890]
          , [902]
          , [904..906]
          , [908]
          , [910..929]
          , [931..974]
          , [976..1013]
          , [1015..1019]
          , [1024..1153]
          , [1162..1230]
          , [1232..1269]
          , [1272..1273]
          , [1280..1295]
          , [1329..1366]
          , [1369]
          , [1377..1415]
          , [1488..1514]
          , [1520..1522]
          , [1569..1594]
          , [1600..1610]
          , [1646..1647]
          , [1649..1747]
          , [1749]
          , [1765..1766]
          , [1774..1775]
          , [1786..1788]
          , [1791]
          , [1808]
          , [1810..1839]
          , [1869..1871]
          , [1920..1957]
          , [1969]
          , [2308..2361]
          , [2365]
          , [2384]
          , [2392..2401]
          , [2437..2444]
          , [2447..2448]
          , [2451..2472]
          , [2474..2480]
          , [2482]
          , [2486..2489]
          , [2493]
          , [2524..2525]
          , [2527..2529]
          , [2544..2545]
          , [2565..2570]
          , [2575..2576]
          , [2579..2600]
          , [2602..2608]
          , [2610..2611]
          , [2613..2614]
          , [2616..2617]
          , [2649..2652]
          , [2654]
          , [2674..2676]
          , [2693..2701]
          , [2703..2705]
          , [2707..2728]
          , [2730..2736]
          , [2738..2739]
          , [2741..2745]
          , [2749]
          , [2768]
          , [2784..2785]
          , [2821..2828]
          , [2831..2832]
          , [2835..2856]
          , [2858..2864]
          , [2866..2867]
          , [2869..2873]
          , [2877]
          , [2908..2909]
          , [2911..2913]
          , [2929]
          , [2947]
          , [2949..2954]
          , [2958..2960]
          , [2962..2965]
          , [2969..2970]
          , [2972]
          , [2974..2975]
          , [2979..2980]
          , [2984..2986]
          , [2990..2997]
          , [2999..3001]
          , [3077..3084]
          , [3086..3088]
          , [3090..3112]
          , [3114..3123]
          , [3125..3129]
          , [3168..3169]
          , [3205..3212]
          , [3214..3216]
          , [3218..3240]
          , [3242..3251]
          , [3253..3257]
          , [3261]
          , [3294]
          , [3296..3297]
          , [3333..3340]
          , [3342..3344]
          , [3346..3368]
          , [3370..3385]
          , [3424..3425]
          , [3461..3478]
          , [3482..3505]
          , [3507..3515]
          , [3517]
          , [3520..3526]
          , [3585..3632]
          , [3634..3635]
          , [3648..3654]
          , [3713..3714]
          , [3716]
          , [3719..3720]
          , [3722]
          , [3725]
          , [3732..3735]
          , [3737..3743]
          , [3745..3747]
          , [3749]
          , [3751]
          , [3754..3755]
          , [3757..3760]
          , [3762..3763]
          , [3773]
          , [3776..3780]
          , [3782]
          , [3804..3805]
          , [3840]
          , [3904..3911]
          , [3913..3946]
          , [3976..3979]
          , [4096..4129]
          , [4131..4135]
          , [4137..4138]
          , [4176..4181]
          , [4256..4293]
          , [4304..4344]
          , [4352..4441]
          , [4447..4514]
          , [4520..4601]
          , [4608..4614]
          , [4616..4678]
          , [4680]
          , [4682..4685]
          , [4688..4694]
          , [4696]
          , [4698..4701]
          , [4704..4742]
          , [4744]
          , [4746..4749]
          , [4752..4782]
          , [4784]
          , [4786..4789]
          , [4792..4798]
          , [4800]
          , [4802..4805]
          , [4808..4814]
          , [4816..4822]
          , [4824..4846]
          , [4848..4878]
          , [4880]
          , [4882..4885]
          , [4888..4894]
          , [4896..4934]
          , [4936..4954]
          , [5024..5108]
          , [5121..5740]
          , [5743..5750]
          , [5761..5786]
          , [5792..5866]
          , [5888..5900]
          , [5902..5905]
          , [5920..5937]
          , [5952..5969]
          , [5984..5996]
          , [5998..6000]
          , [6016..6067]
          , [6103]
          , [6108]
          , [6176..6263]
          , [6272..6312]
          , [6400..6428]
          , [6480..6509]
          , [6512..6516]
          , [7424..7531]
          , [7680..7835]
          , [7840..7929]
          , [7936..7957]
          , [7960..7965]
          , [7968..8005]
          , [8008..8013]
          , [8016..8023]
          , [8025]
          , [8027]
          , [8029]
          , [8031..8061]
          , [8064..8116]
          , [8118..8124]
          , [8126]
          , [8130..8132]
          , [8134..8140]
          , [8144..8147]
          , [8150..8155]
          , [8160..8172]
          , [8178..8180]
          , [8182..8188]
          , [8305]
          , [8319]
          , [8450]
          , [8455]
          , [8458..8467]
          , [8469]
          , [8473..8477]
          , [8484]
          , [8486]
          , [8488]
          , [8490..8493]
          , [8495..8497]
          , [8499..8505]
          , [8509..8511]
          , [8517..8521]
          , [12293..12294]
          , [12337..12341]
          , [12347..12348]
          , [12353..12438]
          , [12445..12447]
          , [12449..12538]
          , [12540..12543]
          , [12549..12588]
          , [12593..12686]
          , [12704..12727]
          , [12784..12799]
          , [13312..19893]
          , [19968..40869]
          , [40960..42124]
          , [44032..55203]
          , [63744..64045]
          , [64048..64106]
          , [64256..64262]
          , [64275..64279]
          , [64285]
          , [64287..64296]
          , [64298..64310]
          , [64312..64316]
          , [64318]
          , [64320..64321]
          , [64323..64324]
          , [64326..64433]
          , [64467..64829]
          , [64848..64911]
          , [64914..64967]
          , [65008..65019]
          , [65136..65140]
          , [65142..65276]
          , [65313..65338]
          , [65345..65370]
          , [65382..65470]
          , [65474..65479]
          , [65482..65487]
          , [65490..65495]
          , [65498..65500]
          , [65536..65547]
          , [65549..65574]
          , [65576..65594]
          , [65596..65597]
          , [65599..65613]
          , [65616..65629]
          , [65664..65786]
          , [66304..66334]
          , [66352..66377]
          , [66432..66461]
          , [66560..66717]
          , [67584..67589]
          , [67592]
          , [67594..67637]
          , [67639..67640]
          , [67644]
          , [67647]
          , [119808..119892]
          , [119894..119964]
          , [119966..119967]
          , [119970]
          , [119973..119974]
          , [119977..119980]
          , [119982..119993]
          , [119995]
          , [119997..120003]
          , [120005..120069]
          , [120071..120074]
          , [120077..120084]
          , [120086..120092]
          , [120094..120121]
          , [120123..120126]
          , [120128..120132]
          , [120134]
          , [120138..120144]
          , [120146..120483]
          , [120488..120512]
          , [120514..120538]
          , [120540..120570]
          , [120572..120596]
          , [120598..120628]
          , [120630..120654]
          , [120656..120686]
          , [120688..120712]
          , [120714..120744]
          , [120746..120770]
          , [120772..120777]
          , [131072..173782]
          , [194560..195101]
          ]
  in S.fromList . concat $ r
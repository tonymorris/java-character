-- | Simulates the @isJavaIdentifierPart@ Java method. <http://docs.oracle.com/javase/6/docs/api/java/lang/Character.html#isJavaIdentifierPart%28int%29>
module Language.Java.Character.IsJavaIdentifierPart
(
  IsJavaIdentifierPart(..)
) where

import Data.Char
import Data.Word
import Data.Set.Diet(Diet)
import qualified Data.Set.Diet as S

-- | Instances simulate Java characters and provide a decision on simulating @isJavaIdentifierPart@.
class Enum c => IsJavaIdentifierPart c where
  isJavaIdentifierPart ::
    c
    -> Bool
  isNotJavaIdentifierPart ::
    c
    -> Bool
  isNotJavaIdentifierPart =
    not . isJavaIdentifierPart

instance IsJavaIdentifierPart Char where
  isJavaIdentifierPart c =
    ord c `S.member` isJavaIdentifierPartSet

instance IsJavaIdentifierPart Int where
  isJavaIdentifierPart c =
    c `S.member` isJavaIdentifierPartSet

instance IsJavaIdentifierPart Integer where
  isJavaIdentifierPart c =
    c `S.member` isJavaIdentifierPartSet

instance IsJavaIdentifierPart Word8 where
  isJavaIdentifierPart c =
    c `S.member` isJavaIdentifierPartSet

instance IsJavaIdentifierPart Word16 where
  isJavaIdentifierPart c =
    c `S.member` isJavaIdentifierPartSet

instance IsJavaIdentifierPart Word32 where
  isJavaIdentifierPart c =
    c `S.member` isJavaIdentifierPartSet

instance IsJavaIdentifierPart Word64 where
  isJavaIdentifierPart c =
    c `S.member` isJavaIdentifierPartSet

isJavaIdentifierPartSet ::
  (Num a, Enum a, Ord a) =>
  Diet a
isJavaIdentifierPartSet =
  let r = [
            [0..8]
          , [14..27]
          , [36]
          , [48..57]
          , [65..90]
          , [95]
          , [97..122]
          , [127..159]
          , [162..165]
          , [170]
          , [173]
          , [181]
          , [186]
          , [192..214]
          , [216..246]
          , [248..566]
          , [592..705]
          , [710..721]
          , [736..740]
          , [750]
          , [768..855]
          , [861..879]
          , [890]
          , [902]
          , [904..906]
          , [908]
          , [910..929]
          , [931..974]
          , [976..1013]
          , [1015..1019]
          , [1024..1153]
          , [1155..1158]
          , [1162..1230]
          , [1232..1269]
          , [1272..1273]
          , [1280..1295]
          , [1329..1366]
          , [1369]
          , [1377..1415]
          , [1425..1441]
          , [1443..1465]
          , [1467..1469]
          , [1471]
          , [1473..1474]
          , [1476]
          , [1488..1514]
          , [1520..1522]
          , [1536..1539]
          , [1552..1557]
          , [1569..1594]
          , [1600..1624]
          , [1632..1641]
          , [1646..1747]
          , [1749..1757]
          , [1759..1768]
          , [1770..1788]
          , [1791]
          , [1807..1866]
          , [1869..1871]
          , [1920..1969]
          , [2305..2361]
          , [2364..2381]
          , [2384..2388]
          , [2392..2403]
          , [2406..2415]
          , [2433..2435]
          , [2437..2444]
          , [2447..2448]
          , [2451..2472]
          , [2474..2480]
          , [2482]
          , [2486..2489]
          , [2492..2500]
          , [2503..2504]
          , [2507..2509]
          , [2519]
          , [2524..2525]
          , [2527..2531]
          , [2534..2547]
          , [2561..2563]
          , [2565..2570]
          , [2575..2576]
          , [2579..2600]
          , [2602..2608]
          , [2610..2611]
          , [2613..2614]
          , [2616..2617]
          , [2620]
          , [2622..2626]
          , [2631..2632]
          , [2635..2637]
          , [2649..2652]
          , [2654]
          , [2662..2676]
          , [2689..2691]
          , [2693..2701]
          , [2703..2705]
          , [2707..2728]
          , [2730..2736]
          , [2738..2739]
          , [2741..2745]
          , [2748..2757]
          , [2759..2761]
          , [2763..2765]
          , [2768]
          , [2784..2787]
          , [2790..2799]
          , [2801]
          , [2817..2819]
          , [2821..2828]
          , [2831..2832]
          , [2835..2856]
          , [2858..2864]
          , [2866..2867]
          , [2869..2873]
          , [2876..2883]
          , [2887..2888]
          , [2891..2893]
          , [2902..2903]
          , [2908..2909]
          , [2911..2913]
          , [2918..2927]
          , [2929]
          , [2946..2947]
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
          , [3006..3010]
          , [3014..3016]
          , [3018..3021]
          , [3031]
          , [3047..3055]
          , [3065]
          , [3073..3075]
          , [3077..3084]
          , [3086..3088]
          , [3090..3112]
          , [3114..3123]
          , [3125..3129]
          , [3134..3140]
          , [3142..3144]
          , [3146..3149]
          , [3157..3158]
          , [3168..3169]
          , [3174..3183]
          , [3202..3203]
          , [3205..3212]
          , [3214..3216]
          , [3218..3240]
          , [3242..3251]
          , [3253..3257]
          , [3260..3268]
          , [3270..3272]
          , [3274..3277]
          , [3285..3286]
          , [3294]
          , [3296..3297]
          , [3302..3311]
          , [3330..3331]
          , [3333..3340]
          , [3342..3344]
          , [3346..3368]
          , [3370..3385]
          , [3390..3395]
          , [3398..3400]
          , [3402..3405]
          , [3415]
          , [3424..3425]
          , [3430..3439]
          , [3458..3459]
          , [3461..3478]
          , [3482..3505]
          , [3507..3515]
          , [3517]
          , [3520..3526]
          , [3530]
          , [3535..3540]
          , [3542]
          , [3544..3551]
          , [3570..3571]
          , [3585..3642]
          , [3647..3662]
          , [3664..3673]
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
          , [3757..3769]
          , [3771..3773]
          , [3776..3780]
          , [3782]
          , [3784..3789]
          , [3792..3801]
          , [3804..3805]
          , [3840]
          , [3864..3865]
          , [3872..3881]
          , [3893]
          , [3895]
          , [3897]
          , [3902..3911]
          , [3913..3946]
          , [3953..3972]
          , [3974..3979]
          , [3984..3991]
          , [3993..4028]
          , [4038]
          , [4096..4129]
          , [4131..4135]
          , [4137..4138]
          , [4140..4146]
          , [4150..4153]
          , [4160..4169]
          , [4176..4185]
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
          , [4969..4977]
          , [5024..5108]
          , [5121..5740]
          , [5743..5750]
          , [5761..5786]
          , [5792..5866]
          , [5870..5872]
          , [5888..5900]
          , [5902..5908]
          , [5920..5940]
          , [5952..5971]
          , [5984..5996]
          , [5998..6000]
          , [6002..6003]
          , [6016..6099]
          , [6103]
          , [6107..6109]
          , [6112..6121]
          , [6155..6157]
          , [6160..6169]
          , [6176..6263]
          , [6272..6313]
          , [6400..6428]
          , [6432..6443]
          , [6448..6459]
          , [6470..6509]
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
          , [8204..8207]
          , [8234..8238]
          , [8255..8256]
          , [8276]
          , [8288..8291]
          , [8298..8303]
          , [8305]
          , [8319]
          , [8352..8369]
          , [8400..8412]
          , [8417]
          , [8421..8426]
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
          , [8544..8579]
          , [12293..12295]
          , [12321..12335]
          , [12337..12341]
          , [12344..12348]
          , [12353..12438]
          , [12441..12442]
          , [12445..12447]
          , [12449..12543]
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
          , [64285..64296]
          , [64298..64310]
          , [64312..64316]
          , [64318]
          , [64320..64321]
          , [64323..64324]
          , [64326..64433]
          , [64467..64829]
          , [64848..64911]
          , [64914..64967]
          , [65008..65020]
          , [65024..65039]
          , [65056..65059]
          , [65075..65076]
          , [65101..65103]
          , [65129]
          , [65136..65140]
          , [65142..65276]
          , [65279]
          , [65284]
          , [65296..65305]
          , [65313..65338]
          , [65343]
          , [65345..65370]
          , [65381..65470]
          , [65474..65479]
          , [65482..65487]
          , [65490..65495]
          , [65498..65500]
          , [65504..65505]
          , [65509..65510]
          , [65529..65531]
          , [65536..65547]
          , [65549..65574]
          , [65576..65594]
          , [65596..65597]
          , [65599..65613]
          , [65616..65629]
          , [65664..65786]
          , [66304..66334]
          , [66352..66378]
          , [66432..66461]
          , [66560..66717]
          , [66720..66729]
          , [67584..67589]
          , [67592]
          , [67594..67637]
          , [67639..67640]
          , [67644]
          , [67647]
          , [119141..119145]
          , [119149..119170]
          , [119173..119179]
          , [119210..119213]
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
          , [120782..120831]
          , [131072..173782]
          , [194560..195101]
          ]
  in S.fromList . concat $ r
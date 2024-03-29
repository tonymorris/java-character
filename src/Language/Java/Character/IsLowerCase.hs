-- | Simulates the @IsLowerCase@ Java method. <http://docs.oracle.com/javase/6/docs/api/java/lang/Character.html#IsLowerCase%28int%29>
module Language.Java.Character.IsLowerCase
(
  IsLowerCase(..)
) where

import Data.Char
import Data.Word
import Data.Set.Diet(Diet)
import qualified Data.Set.Diet as S

-- | Instances simulate Java characters and provide a decision on simulating @IsLowerCase@.
class Enum c => IsLowerCase c where
  isLowerCase ::
    c
    -> Bool
  isNotLowerCase ::
    c
    -> Bool
  isNotLowerCase =
    not . isLowerCase

instance IsLowerCase Char where
  isLowerCase c =
    ord c `S.member` isLowerCaseSet

instance IsLowerCase Int where
  isLowerCase c =
    c `S.member` isLowerCaseSet

instance IsLowerCase Integer where
  isLowerCase c =
    c `S.member` isLowerCaseSet

instance IsLowerCase Word8 where
  isLowerCase c =
    c `S.member` isLowerCaseSet

instance IsLowerCase Word16 where
  isLowerCase c =
    c `S.member` isLowerCaseSet

instance IsLowerCase Word32 where
  isLowerCase c =
    c `S.member` isLowerCaseSet

instance IsLowerCase Word64 where
  isLowerCase c =
    c `S.member` isLowerCaseSet

isLowerCaseSet ::
  (Num a, Enum a, Ord a) =>
  Diet a
isLowerCaseSet =
  let r = [
            [97..122]
          , [170]
          , [181]
          , [186]
          , [223..246]
          , [248..255]
          , [257]
          , [259]
          , [261]
          , [263]
          , [265]
          , [267]
          , [269]
          , [271]
          , [273]
          , [275]
          , [277]
          , [279]
          , [281]
          , [283]
          , [285]
          , [287]
          , [289]
          , [291]
          , [293]
          , [295]
          , [297]
          , [299]
          , [301]
          , [303]
          , [305]
          , [307]
          , [309]
          , [311..312]
          , [314]
          , [316]
          , [318]
          , [320]
          , [322]
          , [324]
          , [326]
          , [328..329]
          , [331]
          , [333]
          , [335]
          , [337]
          , [339]
          , [341]
          , [343]
          , [345]
          , [347]
          , [349]
          , [351]
          , [353]
          , [355]
          , [357]
          , [359]
          , [361]
          , [363]
          , [365]
          , [367]
          , [369]
          , [371]
          , [373]
          , [375]
          , [378]
          , [380]
          , [382..384]
          , [387]
          , [389]
          , [392]
          , [396..397]
          , [402]
          , [405]
          , [409..411]
          , [414]
          , [417]
          , [419]
          , [421]
          , [424]
          , [426..427]
          , [429]
          , [432]
          , [436]
          , [438]
          , [441..442]
          , [445..447]
          , [454]
          , [457]
          , [460]
          , [462]
          , [464]
          , [466]
          , [468]
          , [470]
          , [472]
          , [474]
          , [476..477]
          , [479]
          , [481]
          , [483]
          , [485]
          , [487]
          , [489]
          , [491]
          , [493]
          , [495..496]
          , [499]
          , [501]
          , [505]
          , [507]
          , [509]
          , [511]
          , [513]
          , [515]
          , [517]
          , [519]
          , [521]
          , [523]
          , [525]
          , [527]
          , [529]
          , [531]
          , [533]
          , [535]
          , [537]
          , [539]
          , [541]
          , [543]
          , [545]
          , [547]
          , [549]
          , [551]
          , [553]
          , [555]
          , [557]
          , [559]
          , [561]
          , [563..566]
          , [592..687]
          , [912]
          , [940..974]
          , [976..977]
          , [981..983]
          , [985]
          , [987]
          , [989]
          , [991]
          , [993]
          , [995]
          , [997]
          , [999]
          , [1001]
          , [1003]
          , [1005]
          , [1007..1011]
          , [1013]
          , [1016]
          , [1019]
          , [1072..1119]
          , [1121]
          , [1123]
          , [1125]
          , [1127]
          , [1129]
          , [1131]
          , [1133]
          , [1135]
          , [1137]
          , [1139]
          , [1141]
          , [1143]
          , [1145]
          , [1147]
          , [1149]
          , [1151]
          , [1153]
          , [1163]
          , [1165]
          , [1167]
          , [1169]
          , [1171]
          , [1173]
          , [1175]
          , [1177]
          , [1179]
          , [1181]
          , [1183]
          , [1185]
          , [1187]
          , [1189]
          , [1191]
          , [1193]
          , [1195]
          , [1197]
          , [1199]
          , [1201]
          , [1203]
          , [1205]
          , [1207]
          , [1209]
          , [1211]
          , [1213]
          , [1215]
          , [1218]
          , [1220]
          , [1222]
          , [1224]
          , [1226]
          , [1228]
          , [1230]
          , [1233]
          , [1235]
          , [1237]
          , [1239]
          , [1241]
          , [1243]
          , [1245]
          , [1247]
          , [1249]
          , [1251]
          , [1253]
          , [1255]
          , [1257]
          , [1259]
          , [1261]
          , [1263]
          , [1265]
          , [1267]
          , [1269]
          , [1273]
          , [1281]
          , [1283]
          , [1285]
          , [1287]
          , [1289]
          , [1291]
          , [1293]
          , [1295]
          , [1377..1415]
          , [7424..7467]
          , [7522..7531]
          , [7681]
          , [7683]
          , [7685]
          , [7687]
          , [7689]
          , [7691]
          , [7693]
          , [7695]
          , [7697]
          , [7699]
          , [7701]
          , [7703]
          , [7705]
          , [7707]
          , [7709]
          , [7711]
          , [7713]
          , [7715]
          , [7717]
          , [7719]
          , [7721]
          , [7723]
          , [7725]
          , [7727]
          , [7729]
          , [7731]
          , [7733]
          , [7735]
          , [7737]
          , [7739]
          , [7741]
          , [7743]
          , [7745]
          , [7747]
          , [7749]
          , [7751]
          , [7753]
          , [7755]
          , [7757]
          , [7759]
          , [7761]
          , [7763]
          , [7765]
          , [7767]
          , [7769]
          , [7771]
          , [7773]
          , [7775]
          , [7777]
          , [7779]
          , [7781]
          , [7783]
          , [7785]
          , [7787]
          , [7789]
          , [7791]
          , [7793]
          , [7795]
          , [7797]
          , [7799]
          , [7801]
          , [7803]
          , [7805]
          , [7807]
          , [7809]
          , [7811]
          , [7813]
          , [7815]
          , [7817]
          , [7819]
          , [7821]
          , [7823]
          , [7825]
          , [7827]
          , [7829..7835]
          , [7841]
          , [7843]
          , [7845]
          , [7847]
          , [7849]
          , [7851]
          , [7853]
          , [7855]
          , [7857]
          , [7859]
          , [7861]
          , [7863]
          , [7865]
          , [7867]
          , [7869]
          , [7871]
          , [7873]
          , [7875]
          , [7877]
          , [7879]
          , [7881]
          , [7883]
          , [7885]
          , [7887]
          , [7889]
          , [7891]
          , [7893]
          , [7895]
          , [7897]
          , [7899]
          , [7901]
          , [7903]
          , [7905]
          , [7907]
          , [7909]
          , [7911]
          , [7913]
          , [7915]
          , [7917]
          , [7919]
          , [7921]
          , [7923]
          , [7925]
          , [7927]
          , [7929]
          , [7936..7943]
          , [7952..7957]
          , [7968..7975]
          , [7984..7991]
          , [8000..8005]
          , [8016..8023]
          , [8032..8039]
          , [8048..8061]
          , [8064..8071]
          , [8080..8087]
          , [8096..8103]
          , [8112..8116]
          , [8118..8119]
          , [8126]
          , [8130..8132]
          , [8134..8135]
          , [8144..8147]
          , [8150..8151]
          , [8160..8167]
          , [8178..8180]
          , [8182..8183]
          , [8305]
          , [8319]
          , [8458]
          , [8462..8463]
          , [8467]
          , [8495]
          , [8500]
          , [8505]
          , [8509]
          , [8518..8521]
          , [64256..64262]
          , [64275..64279]
          , [65345..65370]
          , [66600..66639]
          , [119834..119859]
          , [119886..119892]
          , [119894..119911]
          , [119938..119963]
          , [119990..119993]
          , [119995]
          , [119997..120003]
          , [120005..120015]
          , [120042..120067]
          , [120094..120119]
          , [120146..120171]
          , [120198..120223]
          , [120250..120275]
          , [120302..120327]
          , [120354..120379]
          , [120406..120431]
          , [120458..120483]
          , [120514..120538]
          , [120540..120545]
          , [120572..120596]
          , [120598..120603]
          , [120630..120654]
          , [120656..120661]
          , [120688..120712]
          , [120714..120719]
          , [120746..120770]
          , [120772..120777]
          ]
  in S.fromList . concat $ r
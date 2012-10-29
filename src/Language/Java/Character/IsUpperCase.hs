module Language.Java.Character.IsUpperCase
(
  IsUpperCase(..)
) where

import Data.Char
import Data.Fixed
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S

class Enum c => IsUpperCase c where
  isUpperCase ::
    c
    -> Bool
  isNotUpperCase ::
    c
    -> Bool
  isNotUpperCase =
    not . isUpperCase

instance IsUpperCase Char where
  isUpperCase c =
    ord c `S.member` isUpperCaseSet

instance IsUpperCase Int where
  isUpperCase c =
    c `S.member` isUpperCaseSet

instance IsUpperCase Integer where
  isUpperCase c =
    c `S.member` isUpperCaseSet

instance IsUpperCase Word8 where
  isUpperCase c =
    c `S.member` isUpperCaseSet

instance IsUpperCase Word16 where
  isUpperCase c =
    c `S.member` isUpperCaseSet

instance IsUpperCase Word32 where
  isUpperCase c =
    c `S.member` isUpperCaseSet

instance IsUpperCase Word64 where
  isUpperCase c =
    c `S.member` isUpperCaseSet

instance HasResolution a => IsUpperCase (Fixed a) where
  isUpperCase c =
    c `S.member` isUpperCaseSet

isUpperCaseSet ::
  (Num a, Enum a, Ord a) =>
  Set a
isUpperCaseSet =
  let r = [
            [65..90]
          , [192..214]
          , [216..222]
          , [256]
          , [258]
          , [260]
          , [262]
          , [264]
          , [266]
          , [268]
          , [270]
          , [272]
          , [274]
          , [276]
          , [278]
          , [280]
          , [282]
          , [284]
          , [286]
          , [288]
          , [290]
          , [292]
          , [294]
          , [296]
          , [298]
          , [300]
          , [302]
          , [304]
          , [306]
          , [308]
          , [310]
          , [313]
          , [315]
          , [317]
          , [319]
          , [321]
          , [323]
          , [325]
          , [327]
          , [330]
          , [332]
          , [334]
          , [336]
          , [338]
          , [340]
          , [342]
          , [344]
          , [346]
          , [348]
          , [350]
          , [352]
          , [354]
          , [356]
          , [358]
          , [360]
          , [362]
          , [364]
          , [366]
          , [368]
          , [370]
          , [372]
          , [374]
          , [376..377]
          , [379]
          , [381]
          , [385..386]
          , [388]
          , [390..391]
          , [393..395]
          , [398..401]
          , [403..404]
          , [406..408]
          , [412..413]
          , [415..416]
          , [418]
          , [420]
          , [422..423]
          , [425]
          , [428]
          , [430..431]
          , [433..435]
          , [437]
          , [439..440]
          , [444]
          , [452]
          , [455]
          , [458]
          , [461]
          , [463]
          , [465]
          , [467]
          , [469]
          , [471]
          , [473]
          , [475]
          , [478]
          , [480]
          , [482]
          , [484]
          , [486]
          , [488]
          , [490]
          , [492]
          , [494]
          , [497]
          , [500]
          , [502..504]
          , [506]
          , [508]
          , [510]
          , [512]
          , [514]
          , [516]
          , [518]
          , [520]
          , [522]
          , [524]
          , [526]
          , [528]
          , [530]
          , [532]
          , [534]
          , [536]
          , [538]
          , [540]
          , [542]
          , [544]
          , [546]
          , [548]
          , [550]
          , [552]
          , [554]
          , [556]
          , [558]
          , [560]
          , [562]
          , [902]
          , [904..906]
          , [908]
          , [910..911]
          , [913..929]
          , [931..939]
          , [978..980]
          , [984]
          , [986]
          , [988]
          , [990]
          , [992]
          , [994]
          , [996]
          , [998]
          , [1000]
          , [1002]
          , [1004]
          , [1006]
          , [1012]
          , [1015]
          , [1017..1018]
          , [1024..1071]
          , [1120]
          , [1122]
          , [1124]
          , [1126]
          , [1128]
          , [1130]
          , [1132]
          , [1134]
          , [1136]
          , [1138]
          , [1140]
          , [1142]
          , [1144]
          , [1146]
          , [1148]
          , [1150]
          , [1152]
          , [1162]
          , [1164]
          , [1166]
          , [1168]
          , [1170]
          , [1172]
          , [1174]
          , [1176]
          , [1178]
          , [1180]
          , [1182]
          , [1184]
          , [1186]
          , [1188]
          , [1190]
          , [1192]
          , [1194]
          , [1196]
          , [1198]
          , [1200]
          , [1202]
          , [1204]
          , [1206]
          , [1208]
          , [1210]
          , [1212]
          , [1214]
          , [1216..1217]
          , [1219]
          , [1221]
          , [1223]
          , [1225]
          , [1227]
          , [1229]
          , [1232]
          , [1234]
          , [1236]
          , [1238]
          , [1240]
          , [1242]
          , [1244]
          , [1246]
          , [1248]
          , [1250]
          , [1252]
          , [1254]
          , [1256]
          , [1258]
          , [1260]
          , [1262]
          , [1264]
          , [1266]
          , [1268]
          , [1272]
          , [1280]
          , [1282]
          , [1284]
          , [1286]
          , [1288]
          , [1290]
          , [1292]
          , [1294]
          , [1329..1366]
          , [4256..4293]
          , [7680]
          , [7682]
          , [7684]
          , [7686]
          , [7688]
          , [7690]
          , [7692]
          , [7694]
          , [7696]
          , [7698]
          , [7700]
          , [7702]
          , [7704]
          , [7706]
          , [7708]
          , [7710]
          , [7712]
          , [7714]
          , [7716]
          , [7718]
          , [7720]
          , [7722]
          , [7724]
          , [7726]
          , [7728]
          , [7730]
          , [7732]
          , [7734]
          , [7736]
          , [7738]
          , [7740]
          , [7742]
          , [7744]
          , [7746]
          , [7748]
          , [7750]
          , [7752]
          , [7754]
          , [7756]
          , [7758]
          , [7760]
          , [7762]
          , [7764]
          , [7766]
          , [7768]
          , [7770]
          , [7772]
          , [7774]
          , [7776]
          , [7778]
          , [7780]
          , [7782]
          , [7784]
          , [7786]
          , [7788]
          , [7790]
          , [7792]
          , [7794]
          , [7796]
          , [7798]
          , [7800]
          , [7802]
          , [7804]
          , [7806]
          , [7808]
          , [7810]
          , [7812]
          , [7814]
          , [7816]
          , [7818]
          , [7820]
          , [7822]
          , [7824]
          , [7826]
          , [7828]
          , [7840]
          , [7842]
          , [7844]
          , [7846]
          , [7848]
          , [7850]
          , [7852]
          , [7854]
          , [7856]
          , [7858]
          , [7860]
          , [7862]
          , [7864]
          , [7866]
          , [7868]
          , [7870]
          , [7872]
          , [7874]
          , [7876]
          , [7878]
          , [7880]
          , [7882]
          , [7884]
          , [7886]
          , [7888]
          , [7890]
          , [7892]
          , [7894]
          , [7896]
          , [7898]
          , [7900]
          , [7902]
          , [7904]
          , [7906]
          , [7908]
          , [7910]
          , [7912]
          , [7914]
          , [7916]
          , [7918]
          , [7920]
          , [7922]
          , [7924]
          , [7926]
          , [7928]
          , [7944..7951]
          , [7960..7965]
          , [7976..7983]
          , [7992..7999]
          , [8008..8013]
          , [8025]
          , [8027]
          , [8029]
          , [8031]
          , [8040..8047]
          , [8120..8123]
          , [8136..8139]
          , [8152..8155]
          , [8168..8172]
          , [8184..8187]
          , [8450]
          , [8455]
          , [8459..8461]
          , [8464..8466]
          , [8469]
          , [8473..8477]
          , [8484]
          , [8486]
          , [8488]
          , [8490..8493]
          , [8496..8497]
          , [8499]
          , [8510..8511]
          , [8517]
          , [65313..65338]
          , [66560..66599]
          , [119808..119833]
          , [119860..119885]
          , [119912..119937]
          , [119964]
          , [119966..119967]
          , [119970]
          , [119973..119974]
          , [119977..119980]
          , [119982..119989]
          , [120016..120041]
          , [120068..120069]
          , [120071..120074]
          , [120077..120084]
          , [120086..120092]
          , [120120..120121]
          , [120123..120126]
          , [120128..120132]
          , [120134]
          , [120138..120144]
          , [120172..120197]
          , [120224..120249]
          , [120276..120301]
          , [120328..120353]
          , [120380..120405]
          , [120432..120457]
          , [120488..120512]
          , [120546..120570]
          , [120604..120628]
          , [120662..120686]
          , [120720..120744]
          ]
  in S.fromList . concat $ r
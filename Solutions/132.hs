import Euler (factorTD)
import Time
import Data.List (delete, nub, sort, (\\))

{-
n -> factorTD (r n)
1 -> []
2 -> [11]
3 -> [3,37]
4 -> [11,101]
5 -> [41,271]
6 -> [3,7,11,13,37]
7 -> [239,4649]
8 -> [11,73,101,137]
9 -> [3,3,37,333667]
10 -> [11,41,271,9091]
11 -> [21649,513239]
12 -> [3,7,11,13,37,101,9901]
13 -> [53,79,265371653]
14 -> [11,239,4649,909091]
15 -> [3,31,37,41,271,2906161]
16 -> [11,17,73,101,137,5882353]
17 -> [2071723,5363222357]
18 -> [3,3,7,11,13,19,37,52579,333667]
19 -> [1111111111111111111]
20 -> [11,41,101,271,3541,9091,27961]

the factors of (r a) are a subset of the factors of (r (a*b)).

These sites helped:
http://homepage2.nifty.com/m_kamada/math/11111.htm
http://www.alpertron.com.ar/ECM.HTM
-}

r :: Int -> Integer
r n = read $ replicate n '1'

fs = [f100k, f78125, f64k, f80k, f62500]
fMerge = sort $ nub $ concat fs

-- the elements in (f) that aren't in the other factor groups (not necessary)
uniq f = f \\ concat (delete f fs)

main = timePrint $ sum $ take 40 fMerge

{-
http://homepage2.nifty.com/m_kamada/math/11111.htm

64000 = 2^9 * 5^3
80000 = 2^7 * 5^4
100000 = 2^5 * 5^5
62500 = 2^2 * 5^6
78125 = 5^7
-}
f78125 = [41,271,751,21401,25601,32500001,532525001,1797655751,182521213001,176144543406001,3852075247993751,42051775804956304559810859008305819975199677041099230574273451704628125001]
f100k = [11,17,41,73,101,137,251,271,353,401,449,641,751,1201,1409,1601,3541,4001,4801,5051,9091,16001,21001,21401,24001,25601,27961,37501,43201,60101,69857,76001,160001,162251,544001,670001,952001,980801,1378001,1610501,1634881,1676321,5070721,5882353,6600001,7019801,12600001,18453761,21408001,130290001,188080001,245832001,421144001,532525001,756100001,1797655751,5964848081,10256250001,10893295001,20600850001,24592788001,24657552001,66489400001,170774437501,182521213001,469620340001,604991362501,630240900001,947147262401,14103673319201,26216896777501,37750052632001,50779597795001,78875943472201,176144543406001,1680588011350901,2336633447080001,3701291672812501,3852075247993751,9432470260080001,21597204466600001,128372635774581251,339320469104212501,19721061166646717498359681,226780642946014381162024001,3277301266213756376603140001,11727340168618688631353490001,485632702361659071076259666970001,349954396040122577928041596214187605761,3475700719926956233659563848134955052440801,129694419029057750551385771184564274499075700947656757821537291527196801,42051775804956304559810859008305819975199677041099230574273451704628125001,269409792871731627664586194662281233853701011108906726055753272681082282441709251,32886082501657187247904557195788749020689459942486319578774270656490822624175142646095920001,6209247929687718293872530970176361270188593487368651121607499902204345107547278766048583639501,116990301476747139063183899107829995503054414640437569739696739532637081068627241615756780789663364521811201]
f64k = [11,17,41,73,101,137,251,257,271,353,401,449,641,751,1201,1409,1601,3541,4001,4801,5051,9091,10753,15361,16001,19841,21001,21401,24001,25601,27961,40961,43201,60101,69857,76001,76801,162251,453377,524801,544001,952001,976193,980801,1378001,1610501,1634881,1676321,3072001,5070721,5882353,6187457,6576001,7019801,8253953,18453761,21408001,40960001,69913601,245832001,421144001,727912961,1265011073,1797655751,5964848081,9524994049,10893295001,24592788001,24657552001,73171503617,182521213001,947147262401,14103673319201,37750052632001,54789630784001,78875943472201,84941592144001,176144543406001,643467603532801,834427406578561,1680588011350901,541568088183015318401,15343168188889137818369,19721061166646717498359681,46557699107280397718952961,226780642946014381162024001,336490113384347841803678799361,515217525265213267447869906815873,349954396040122577928041596214187605761,8036845650724384999755715242923368926721,3475700719926956233659563848134955052440801,129694419029057750551385771184564274499075700947656757821537291527196801,42051775804956304559810859008305819975199677041099230574273451704628125001,269409792871731627664586194662281233853701011108906726055753272681082282441709251,32886082501657187247904557195788749020689459942486319578774270656490822624175142646095920001,6209247929687718293872530970176361270188593487368651121607499902204345107547278766048583639501,116990301476747139063183899107829995503054414640437569739696739532637081068627241615756780789663364521811201,55871187633753621225794775009016131346430842253464047463157158784732544216230781165223702155223678309562822667655169,99999999999999999999999999999999000000000000000000000000000000009999999999999999999999999999999900000000000000000000000000000001]
f80k = [11,17,41,73,101,137,251,271,353,401,449,641,751,1201,1409,1601,3541,4001,4801,5051,9091,16001,19841,21001,21401,24001,25601,27961,43201,60101,69857,76001,76801,160001,162251,524801,544001,670001,952001,976193,980801,1378001,1610501,1634881,1676321,5070721,5882353,6187457,6576001,7019801,12600001,18453761,21408001,40960001,69913601,130290001,188080001,245832001,421144001,1265011073,1797655751,1843840001,5964848081,10893295001,24592788001,24657552001,170774437501,182521213001,469620340001,630240900001,947147262401,14103673319201,20248874160001,26216896777501,37750052632001,50779597795001,78875943472201,84941592144001,176144543406001,834427406578561,1680588011350901,2336633447080001,9432470260080001,21597204466600001,128372635774581251,541568088183015318401,15343168188889137818369,19721061166646717498359681,226780642946014381162024001,3277301266213756376603140001,11727340168618688631353490001,485632702361659071076259666970001,515217525265213267447869906815873,349954396040122577928041596214187605761,3475700719926956233659563848134955052440801,129694419029057750551385771184564274499075700947656757821537291527196801,42051775804956304559810859008305819975199677041099230574273451704628125001,269409792871731627664586194662281233853701011108906726055753272681082282441709251,32886082501657187247904557195788749020689459942486319578774270656490822624175142646095920001,6209247929687718293872530970176361270188593487368651121607499902204345107547278766048583639501,116990301476747139063183899107829995503054414640437569739696739532637081068627241615756780789663364521811201,99999999999999999999999999999999000000000000000000000000000000009999999999999999999999999999999900000000000000000000000000000001]
f62500 = [11,41,101,251,271,751,3541,4001,5051,9091,21001,21401,25601,27961,37501,60101,62501,76001,160001,162251,670001,1610501,7019801,89625001,532525001,756100001,1797655751,10893295001,15508687501,170774437501,182521213001,604991362501,630240900001,14103673319201,26216896777501,50779597795001,78875943472201,176144543406001,1680588011350901,3701291672812501,3852075247993751,128372635774581251,339320469104212501,11727340168618688631353490001,42051775804956304559810859008305819975199677041099230574273451704628125001,269409792871731627664586194662281233853701011108906726055753272681082282441709251,32886082501657187247904557195788749020689459942486319578774270656490822624175142646095920001,6209247929687718293872530970176361270188593487368651121607499902204345107547278766048583639501]
{-
f125 = [41,271,751,21401,25601,1797655751,182521213001,176144543406001,42051775804956304559810859008305819975199677041099230574273451704628125001]
f250 = [11,41,251,271,751,5051,9091,21001,21401,25601,162251,1797655751,10893295001,182521213001,78875943472201,176144543406001,42051775804956304559810859008305819975199677041099230574273451704628125001,269409792871731627664586194662281233853701011108906726055753272681082282441709251]
f256 = [11,17,73,101,137,257,353,449,641,1409,15361,19841,69857,453377,976193,5882353,6187457,1265011073,834427406578561,15343168188889137818369,515217525265213267447869906815873,55871187633753621225794775009016131346430842253464047463157158784732544216230781165223702155223678309562822667655169]
f200 = [11,41,73,101,137,251,271,401,1201,1601,3541,5051,9091,21401,25601,27961,60101,1676321,7019801,5964848081,182521213001,14103673319201,78875943472201,1680588011350901,129694419029057750551385771184564274499075700947656757821537291527196801]
f512 = [11,17,73,101,137,257,353,449,641,1409,10753,15361,19841,69857,453377,976193,5882353,6187457,8253953,1265011073,9524994049,73171503617,834427406578561,15343168188889137818369,515217525265213267447869906815873,55871187633753621225794775009016131346430842253464047463157158784732544216230781165223702155223678309562822667655169,161659663356434944948942201164163009493717089102370771373121362150985544514761379133487997023996012149425048654486737380370333511296921220558813648612791137845552210697266256120930676972710885926127946416909582894897995807233]
f500 = [11,41,101,251,271,751,3541,4001,5051,9091,21001,21401,25601,27961,60101,76001,162251,1610501,7019801,1797655751,10893295001,182521213001,14103673319201,78875943472201,176144543406001,1680588011350901,42051775804956304559810859008305819975199677041099230574273451704628125001,269409792871731627664586194662281233853701011108906726055753272681082282441709251,32886082501657187247904557195788749020689459942486319578774270656490822624175142646095920001,6209247929687718293872530970176361270188593487368651121607499902204345107547278766048583639501]
-}
import Debug.Trace (trace)
import Control.Monad (guard)
import qualified Data.MultiSet as MultiSet
import Data.Char (digitToInt)
import Data.Array ((!))
import qualified Data.Array as Array
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import Data.Set (Set)
import qualified Data.Set as Set

-- problem 1 -- multiples of 3 and 5 below 1000
-----------------------------------------------

isDivisibleBy :: Integer -> Integer -> Bool
m `isDivisibleBy` n = 0 == m `mod` n

isDivisibleByAllOf :: Integer -> [Integer] -> Bool
m `isDivisibleByAllOf` ns = all (m `isDivisibleBy`) ns

isDivisibleByAnyOf :: Integer -> [Integer] -> Bool
m `isDivisibleByAnyOf` ns = any (m `isDivisibleBy`) ns

problem1 :: Integer
problem1 = sum $ filter (`isDivisibleByAnyOf` [3,5]) [1..999]

-- problem 2 -- even fibonacci numbers
--------------------------------------

fibs12 :: [Integer]
fibs12 = 1 : 2 : zipWith (+) fibs12 (tail fibs12)

problem2 :: Integer
problem2 = sum $ filter even $ takeWhile (< 4000000) fibs12

-- problem 3 -- largest prime factor
------------------------------------

primes :: [Integer]
primes = filterPrimes [2..]
  where
    filterPrimes :: [Integer] -> [Integer]
    filterPrimes (x:xs) = x : filterPrimes (filter (not . (`isDivisibleBy` x)) xs)

primeFactorOf :: Integer -> Integer
primeFactorOf n = head $ filter (n `isDivisibleBy`) $  primes

removeFactorsFrom :: Integer -> Integer -> Integer
removeFactorsFrom n m = if n `isDivisibleBy` m
                        then removeFactorsFrom (n `div` m) m
                        else n

primeFactorsOf :: Integer -> [Integer]
primeFactorsOf 1 = []
primeFactorsOf n = m : primeFactorsOf (removeFactorsFrom n m)
  where m = primeFactorOf n

problem3 :: Integer
problem3 = maximum $ primeFactorsOf 600851475143

-- problem 4 -- largest palindrome project
------------------------------------------

isPalindrome :: Integer -> Bool
isPalindrome i = ip (show i)
  where ip s = s == reverse s

problem4 :: Integer
problem4 = maximum $ do
  x <- [100..999]
  y <- [100..999]
  let z = x * y
  guard $ isPalindrome z
  return z

-- problem 5 -- smallest multiple
---------------------------------

primeFactorsRepeated :: Integer -> [Integer]
primeFactorsRepeated 1 = []
primeFactorsRepeated n = m : primeFactorsRepeated (n `div` m)
  where m = primeFactorOf n

multiFactors :: Integer -> MultiSet.MultiSet Integer
multiFactors = MultiSet.fromList . primeFactorsRepeated

problem5 :: Integer
problem5 = product $ MultiSet.toList $ foldr MultiSet.maxUnion MultiSet.empty $ map multiFactors [1..20]

-- problem 6 -- sum square difference
--------------------------------------

square :: Integer -> Integer
square x = x * x

sumOfSquares :: [Integer] -> Integer
sumOfSquares = sum . map square

squareOfSum :: [Integer] -> Integer
squareOfSum = square . sum

problem6 :: Integer
problem6 = squareOfSum [1..100] - sumOfSquares [1..100]

-- problem 7 -- 10001st prime
-----------------------------

problem7 = primes !! 10000

-- problem 8 -- largest product in a series

integerToDigits :: Integer -> [Int]
integerToDigits = map digitToInt . show

productsOfSize :: Int -> [Int] -> [Int]
productsOfSize n xs
  | length xs >= n = (product $ take n xs) : productsOfSize n (tail xs)
  | otherwise = []

bigInt :: Integer
bigInt = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

problem8 :: Int
problem8 = maximum $ productsOfSize 13 $ integerToDigits bigInt

-- problem 9 -- special pythagorean triplet

isPythagoreanTriplet :: Integer -> Integer -> Integer -> Bool
isPythagoreanTriplet a b c = a * a + b * b - c * c == 0

problem9 :: [Integer]
problem9 = do
  a <- [1..1000]
  b <- [a..(1000-a)]
  let c = 1000 - a - b
  guard $ b <= c
  guard $ isPythagoreanTriplet a b c
  return $ product [a, b, c]

-- problem 10 -- summation of primes

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

candidatePrimeFactors :: Integer -> [Integer]
candidatePrimeFactors x = takeWhile (<= s) primes
  where s = isqrt x

isPrime :: Integer -> Bool
isPrime x | x < 2 = False
isPrime x = isEmpty $ filter (x `isDivisibleBy`) $ candidatePrimeFactors x
  where
    isEmpty [] = True
    isEmpty _ = False

problem10 :: Integer
problem10 = sum $ filter isPrime [2..2000000]

-- problem 11 -- largest product in a grid

gridRows :: [String]
gridRows = ["08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08",
            "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00",
            "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65",
            "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66",
            "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91",
            "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80",
            "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50",
            "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70",
            "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21",
            "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72",
            "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95",
            "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92",
            "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57",
            "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58",
            "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40",
            "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69",
            "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36",
            "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16",
            "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54",
            "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"]

grid :: [[Integer]]
grid = map (map read . words) gridRows

type Coord = (Int, Int)

onBoard :: Int -> [Coord] -> Bool
onBoard size positions = minCoord >= 0 && maxCoord < size
  where
    allCoords = (map fst positions) ++ (map snd positions)
    minCoord = minimum allCoords
    maxCoord = maximum allCoords

type Offset = (Int, Int)

delta :: (Int -> Offset) -> Int -> [Offset]
delta f size = map f [0..(size-1)]

rightDelta :: Int -> [Offset]
rightDelta = delta (\x -> (0, x))

downDelta :: Int -> [Offset]
downDelta = delta (\x -> (x, 0))

upRightDelta :: Int -> [Offset]
upRightDelta = delta (\x -> (-x, x))

downRightDelta :: Int -> [Offset]
downRightDelta = delta (\x -> (x, x))

newCoord :: Coord -> Offset -> Coord
newCoord (x, y) (dx, dy) = (x + dx, y + dy)

newCoords :: Coord -> [Offset] -> [Coord]
newCoords coord offsets = map (newCoord coord) offsets

gridValue :: [Coord] -> Integer
gridValue coords = product $ map f coords
  where
    f :: Coord -> Integer
    f (x, y) = grid !! y !! x

problem11 :: Integer
problem11 = maximum $ do
  x <- [0..19]
  y <- [0..19]
  ds <- [(rightDelta 4), (downDelta 4), (upRightDelta 4), (downRightDelta 4)]
  let zs = newCoords (x, y) ds
  guard $ onBoard 20 zs
  return $ gridValue zs

-- problem 12 - highly divisible triangular number

triangulars :: [Integer]
triangulars = List.unfoldr tri (0, 1)
  where
    tri (x, y) = Just $ (y, (x+1, y+x+2))

numDivisors :: Integer -> Integer
numDivisors n = product $ map (+ 1) counts
  where
    factors = multiFactors n
    counts = map (toInteger . snd) $ MultiSet.toOccurList factors

problem12 :: Integer
problem12 = head $ filter (\n -> (numDivisors n) >= 500) triangulars

-- problem 13 - large sum

largeNumbers :: [Integer]
largeNumbers = [37107287533902102798797998220837590246510135740250,
                46376937677490009712648124896970078050417018260538,
                74324986199524741059474233309513058123726617309629,
                91942213363574161572522430563301811072406154908250,
                23067588207539346171171980310421047513778063246676,
                89261670696623633820136378418383684178734361726757,
                28112879812849979408065481931592621691275889832738,
                44274228917432520321923589422876796487670272189318,
                47451445736001306439091167216856844588711603153276,
                70386486105843025439939619828917593665686757934951,
                62176457141856560629502157223196586755079324193331,
                64906352462741904929101432445813822663347944758178,
                92575867718337217661963751590579239728245598838407,
                58203565325359399008402633568948830189458628227828,
                80181199384826282014278194139940567587151170094390,
                35398664372827112653829987240784473053190104293586,
                86515506006295864861532075273371959191420517255829,
                71693888707715466499115593487603532921714970056938,
                54370070576826684624621495650076471787294438377604,
                53282654108756828443191190634694037855217779295145,
                36123272525000296071075082563815656710885258350721,
                45876576172410976447339110607218265236877223636045,
                17423706905851860660448207621209813287860733969412,
                81142660418086830619328460811191061556940512689692,
                51934325451728388641918047049293215058642563049483,
                62467221648435076201727918039944693004732956340691,
                15732444386908125794514089057706229429197107928209,
                55037687525678773091862540744969844508330393682126,
                18336384825330154686196124348767681297534375946515,
                80386287592878490201521685554828717201219257766954,
                78182833757993103614740356856449095527097864797581,
                16726320100436897842553539920931837441497806860984,
                48403098129077791799088218795327364475675590848030,
                87086987551392711854517078544161852424320693150332,
                59959406895756536782107074926966537676326235447210,
                69793950679652694742597709739166693763042633987085,
                41052684708299085211399427365734116182760315001271,
                65378607361501080857009149939512557028198746004375,
                35829035317434717326932123578154982629742552737307,
                94953759765105305946966067683156574377167401875275,
                88902802571733229619176668713819931811048770190271,
                25267680276078003013678680992525463401061632866526,
                36270218540497705585629946580636237993140746255962,
                24074486908231174977792365466257246923322810917141,
                91430288197103288597806669760892938638285025333403,
                34413065578016127815921815005561868836468420090470,
                23053081172816430487623791969842487255036638784583,
                11487696932154902810424020138335124462181441773470,
                63783299490636259666498587618221225225512486764533,
                67720186971698544312419572409913959008952310058822,
                95548255300263520781532296796249481641953868218774,
                76085327132285723110424803456124867697064507995236,
                37774242535411291684276865538926205024910326572967,
                23701913275725675285653248258265463092207058596522,
                29798860272258331913126375147341994889534765745501,
                18495701454879288984856827726077713721403798879715,
                38298203783031473527721580348144513491373226651381,
                34829543829199918180278916522431027392251122869539,
                40957953066405232632538044100059654939159879593635,
                29746152185502371307642255121183693803580388584903,
                41698116222072977186158236678424689157993532961922,
                62467957194401269043877107275048102390895523597457,
                23189706772547915061505504953922979530901129967519,
                86188088225875314529584099251203829009407770775672,
                11306739708304724483816533873502340845647058077308,
                82959174767140363198008187129011875491310547126581,
                97623331044818386269515456334926366572897563400500,
                42846280183517070527831839425882145521227251250327,
                55121603546981200581762165212827652751691296897789,
                32238195734329339946437501907836945765883352399886,
                75506164965184775180738168837861091527357929701337,
                62177842752192623401942399639168044983993173312731,
                32924185707147349566916674687634660915035914677504,
                99518671430235219628894890102423325116913619626622,
                73267460800591547471830798392868535206946944540724,
                76841822524674417161514036427982273348055556214818,
                97142617910342598647204516893989422179826088076852,
                87783646182799346313767754307809363333018982642090,
                10848802521674670883215120185883543223812876952786,
                71329612474782464538636993009049310363619763878039,
                62184073572399794223406235393808339651327408011116,
                66627891981488087797941876876144230030984490851411,
                60661826293682836764744779239180335110989069790714,
                85786944089552990653640447425576083659976645795096,
                66024396409905389607120198219976047599490197230297,
                64913982680032973156037120041377903785566085089252,
                16730939319872750275468906903707539413042652315011,
                94809377245048795150954100921645863754710598436791,
                78639167021187492431995700641917969777599028300699,
                15368713711936614952811305876380278410754449733078,
                40789923115535562561142322423255033685442488917353,
                44889911501440648020369068063960672322193204149535,
                41503128880339536053299340368006977710650566631954,
                81234880673210146739058568557934581403627822703280,
                82616570773948327592232845941706525094512325230608,
                22918802058777319719839450180888072429661980811197,
                77158542502016545090413245809786882778948721859617,
                72107838435069186155435662884062257473692284509516,
                20849603980134001723930671666823555245252804609722,
                53503534226472524250874054075591789781264330331690]

problem13 :: Integer
problem13 = read $ take 10 $ show $ sum largeNumbers


-- problem 14 - longest collatz sequence

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSeq :: Integer -> [Integer]
collatzSeq n = takeWhile (> 1) $ iterate collatz n

chainLength :: Integer -> Int
chainLength = length . collatzSeq

problem14 = maximum $ do
  n <- [1..1000000]
  let cl = chainLength n
  return (cl, n)

-- problem 15 -- lattice paths
numPaths :: Int -> Int -> Integer
numPaths h w = countPaths h w
  where
    countPaths :: Int -> Int -> Integer
    countPaths 0 w = 1
    countPaths h 0 = 1
    countPaths h w = (cp ! (h - 1, w)) + (cp ! (h, w - 1))
    cp = Array.listArray bounds
                         [countPaths x y | (x, y) <- Array.range (bounds)]
    bounds = ((0,0), (h, w))

problem15 :: Integer
problem15 = numPaths 20 20

-- problem 16 -- power digit sum

problem16 :: Integer
problem16 = sum $ map toInteger $ integerToDigits (2 ^ 1000)

-- problem 17 -- number letter counts

digitWord :: Int -> String
digitWord i = ["", "One", "Two", "Three", "Four",
               "Five", "Six", "Seven", "Eight", "Nine"] !! i

thousandWord :: Int -> String
thousandWord n
  | n < 1000 = ""
  | otherwise = (digitWord t) ++ " thousand"
      where t = n `div` 1000

hundredWord :: Int -> String
hundredWord n
  | n < 100 = ""
  | otherwise = (digitWord h) ++ " hundred"
      where h = n `div` 100

tensWord :: Int -> String
tensWord n
  | n < 20 = ""
  | otherwise = ["twenty", "thirty", "forty", "fifty", "sixty",
                 "seventy", "eighty", "ninety"] !! t
      where t = (n `div` 10) - 2

teensWord :: Int -> String
teensWord n
  | n < 10 || n >= 20 = ""
  | otherwise = ["ten", "eleven", "twelve", "thirteen", "fourteen",
                 "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! t
      where t = n - 10

singleWord :: Int -> String
singleWord n
  | n < 10 || n >= 20 = digitWord (n `mod` 10)
  | otherwise = ""

andWord :: Int -> String
andWord n
  | n < 100 || n `mod` 100 == 0 = ""
  | otherwise = " and "

numberWord :: Int -> String
numberWord n = (thousandWord thou) ++ (hundredWord hund) ++
               (andWord hund) ++
               (tensWord ten) ++
               (teensWord ten) ++ (singleWord ten)
  where
    thou = n `mod` 10000
    hund = n `mod` 1000
    ten = n `mod` 100

numLetters :: String -> Int
numLetters = length . filter Char.isLetter

problem17 :: Int
problem17 = sum $ map (numLetters . numberWord) [1..1000]

-- problem 18 - maximum path sum i

triangle18 :: [[Int]]
triangle18 = map (map read . words)
             ["75",
              "95 64",
              "17 47 82",
              "18 35 87 10",
              "20 04 82 47 65",
              "19 01 23 75 03 34",
              "88 02 77 73 07 63 67",
              "99 65 04 28 06 16 70 92",
              "41 41 26 56 83 40 80 70 33",
              "41 48 72 33 47 32 37 16 94 29",
              "53 71 44 65 25 43 91 52 97 51 14",
              "70 11 33 28 77 73 17 78 39 68 17 57",
              "91 71 52 38 17 14 91 43 58 50 27 29 48",
              "63 66 04 68 89 53 67 30 73 16 69 87 40 31",
              "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"]

maxPathSum :: [[Int]] -> Int
maxPathSum triangle = mps 0 0
  where
    mps row col
      | row == lastRow = triangle !! row !! col
      | row < col = 0
      | otherwise = triangle !! row !! col +
                    max (mpsLookup ! (row + 1, col))
                        (mpsLookup ! (row + 1, col + 1))
    mpsLookup = Array.listArray bounds
                                [mps r c | (r, c) <- Array.range (bounds)]
    bounds = ((0,0), (lastRow, lastRow))
    lastRow = length triangle - 1

problem18 :: Int
problem18 = maxPathSum triangle18

-- problem 19 - counting Sundays

isLeapYear :: Integer -> Bool
isLeapYear y
  | y `isDivisibleBy` 400 = True
  | y `isDivisibleBy` 100 = False
  | y `isDivisibleBy` 4 = True
  | otherwise = False

daysPerMonth :: Integer -> [Int]
daysPerMonth y
  | isLeapYear y = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  | otherwise =    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

monthDays :: [Int]
monthDays = do
  year <- [1900 .. 2000]
  days <- daysPerMonth year
  return days

daysOfTheWeek :: Int -> [Int] -> [Int]
daysOfTheWeek d [_] = [d]
daysOfTheWeek d (x:xs) = d : daysOfTheWeek dow xs
  where dow = (d + x) `mod` 7

dows :: [Int]
dows = daysOfTheWeek 1 monthDays

problem19 :: Int
problem19 = length $ filter (== 0) $ drop 12 dows

-- problem 20

factorial :: Integer -> Integer
factorial n = loop n 1
  where
    loop :: Integer -> Integer -> Integer
    loop 0 result = result
    loop x result = loop (x - 1) (result * x)

sumDigits :: Integer -> Integer
sumDigits = sum . (map toInteger) . integerToDigits

problem20 :: Integer
problem20 = sumDigits $ factorial 100

-- problem 21 - amicable numbers

recursiveProducts :: [(Integer, MultiSet.Occur)] -> [Integer]
recursiveProducts [] = []
recursiveProducts [(i, c)] = map (i ^) [0..c]
recursiveProducts ((i, c):rest) = do
  exponent <- [0 .. c]
  remainingProduct <- recursiveProducts rest
  return $ remainingProduct * (i ^ exponent)

allDivisors :: Integer -> [Integer]
allDivisors = recursiveProducts . MultiSet.toOccurList . multiFactors

properDivisors :: Integer -> [Integer]
properDivisors n = filter (< n) $ allDivisors n

amicables :: Integer -> [Integer]
amicables n = filter isAmicable $ [1 .. n]
  where
    isAmicable :: Integer -> Bool
    isAmicable i = j <= n && j /= i && spds ! j == i
      where j = spds ! i
    spds :: Array.Array Integer Integer
    spds = Array.listArray (0, n) $ 0:[sum $ properDivisors i | i <- [1..n]]

problem21 :: Integer
problem21 = sum $ amicables 10000

-- problem22 - names scores

charValue :: Char -> Int
charValue c
  | Char.isAlpha c = (Char.ord $ Char.toLower c) - (Char.ord 'a') + 1
  | otherwise = 0

wordValue :: String -> Integer
wordValue = sum . map (toInteger . charValue)

prepNames :: String -> [String]
prepNames text = prepNamesHelper "" text []
  where
    prepNamesHelper :: String -> String -> [String] -> [String]
    prepNamesHelper w "" names = names ++ [w]
    prepNamesHelper w (c:cs) names
      | c == '"' = prepNamesHelper w cs names
      | c == ',' = prepNamesHelper "" cs (names ++ [w])
      | otherwise = prepNamesHelper (w ++ [c]) cs names

problem22 :: IO Integer
problem22 = do
  text <- readFile "p022_names.txt"
  let names = prepNames text
  let points = map wordValue $ List.sort names
  return $ sum $ zipWith (*) points [1..]

-- problem 23 - non-abundant sums

abundants :: [Integer]
abundants = filter isAbundant [1..]
  where isAbundant n = n < (sum $ properDivisors n)

problem23 :: Integer
problem23 = sum notAbundantSums
  where
    abunds = takeWhile (< 28123) abundants
    abundSet = Set.fromList abunds
    notAbundantSums = filter nas [1..28123]
    nas n = null $ do
      i <- takeWhile (< n) abunds
      let j = n - i
      guard $ j `Set.member` abundSet

-- problem 24 - lexicographic permutations

perms :: [a] -> [[a]]
perms [] = []
perms [x] = [[x]]
perms (x:xs) = do
    p <- perms xs
    i <- [0 .. (length p)]
    return $ (take i p) ++ [x] ++ (drop i p)

problem24 :: [Int]
problem24 = lexico !! 999999
  where lexico = List.sort $ perms [0..9]

-- problem 25 - 1000-digit fibonacci number

fibs11 :: [Integer]
fibs11 = 1 : 1 : zipWith (+) fibs11 (tail fibs11)


numDigits :: Integer -> Int
numDigits n = length $ show n

problem25 :: Int
problem25 = fst $ head $ filter enoughDigits $ zip [1..] fibs11
  where
    enoughDigits :: (Int, Integer) -> Bool
    enoughDigits (_, n) = 1000 <= numDigits n

-- problem 26 - reciprocal cycles

reciprocalDigits :: Int -> [Int]
reciprocalDigits n = rd 1
  where
    rd x = q : rd (10 * r)
      where
        q = x `div` n
        r = x `mod` n

reciprocalNumerators :: Int -> [Int]
reciprocalNumerators n = rn 1
  where
    rn x = x : rn (10 * r)
      where r = x `mod` n

findPeriod :: [Int] -> Int
findPeriod xs = fp Map.empty (zip [0..] xs)
  where
    fp m ((i, x):ixs) = case Map.lookup x m of
      (Just j) -> i - j
      Nothing -> fp (Map.insert x i m) ixs


problem26 = snd $
            maximum $
            zip (map (findPeriod . reciprocalNumerators) [1..999]) [1..999]

-- problem 27 - quadratic primes

numPrimesProduced :: Int -> Int -> Int
numPrimesProduced a b = length $
                        takeWhile (\x -> (x > 1) && (isPrime $ toInteger x)) $
                        map (\i -> i * i + i * a + b) [0..]

problem27 = snd $ maximum $ do
  a <- [-999 .. 999]
  b <- [-999 .. 999]
  return (numPrimesProduced a b, a * b)

-- problem 28 -

startingNumber :: Int -> Int
startingNumber n = 4 * n * n - 2 * n + 1

increment :: Int -> Int
increment n = 2 * n

diagonalValues :: Int -> [Int]
diagonalValues n
  | n <= 0 = [1]
  | otherwise = [startingNumber n + k * (increment n) | k <- [0..3]]

problem28 = sum $ do
  n <- [0..500]
  d <- diagonalValues n
  return d

-- problem 29 -- distinct powers

problem29 = length $ List.nub $ do
  let nums = [2..100] :: [Integer]
  a <- nums
  b <- nums
  return $ a ^ b

-- problem 30 - fifth powers of digits

sumFifth :: Int -> Int
sumFifth = sum . map (^5) . integerToDigits . toInteger

problem30 = sum $ filter (\x -> x == sumFifth x) [2 .. 999999]

-- problem 31 coin sums

coinValues :: [Int]
coinValues = [1, 2, 5, 10, 20, 50, 100, 200]

numberOfWaysToMake :: [Int] -> Int -> Int
numberOfWaysToMake [] _ = 0
numberOfWaysToMake cs@(coin:coins) value
  | coin > value = 0
  | coin == value = 1
  | otherwise = useCoin + dontUseCoin
    where
      useCoin = numberOfWaysToMake cs (value - coin)
      dontUseCoin = numberOfWaysToMake coins value

problem31 = numberOfWaysToMake coinValues 200

-- problem 32 - pandigital products

areDistinct :: (Eq a) => [a] -> Bool
areDistinct xs = nubLen == len
  where
    len = length xs
    nubLen = length $ List.nub xs

isPandigital :: [Int] -> Bool
isPandigital xs = (length xs == 9) && (List.sort xs == [1..9])

problem32 = sum $ List.nub $ do
  a <- [1 .. 9876]
  let aDigits = integerToDigits a
  guard $ areDistinct aDigits
  b <- [a .. 9876]
  let bDigits = integerToDigits b
  guard $ areDistinct bDigits
  let c = a * b
  let cDigits = integerToDigits c
  guard $ isPandigital (aDigits ++ bDigits ++ cDigits)
  return c

-- problem 33 - digit cancelling fractions

type Fraction = (Int, Int)

fractionEqual :: Fraction -> Fraction -> Bool
fractionEqual (a, b) (c, d) = a * d == b * c

fakeCancel :: Fraction -> Maybe Fraction
fakeCancel (a, b) = case (aOnly, bOnly) of
                      ([x], [y]) -> Just $ (x, y)
                      _ -> Nothing
  where
    aDigits = integerToDigits $ toInteger a
    bDigits = integerToDigits $ toInteger b
    commonDigits = filter (\x -> x `elem` aDigits && x > 0) bDigits
    aOnly = filter (not . (`elem` commonDigits)) aDigits
    bOnly = filter (not . (`elem` commonDigits)) bDigits


reduceFraction :: Fraction -> Fraction
reduceFraction (a, b) = (a `div` c, b `div` c)
  where c = gcd a b

fractionProduct :: Fraction -> Fraction -> Fraction
fractionProduct (a, b) (c, d) = reduceFraction (a * c, b * d)

problem33 :: Int
problem33 = snd $ foldr fractionProduct (1, 1) $ do
  num <- [10 .. 99]
  denom <- [10 .. 99]
  guard $ num < denom
  let f1 = (num, denom)
  let f2 = fakeCancel f1
  guard $ case f2 of
    Just f -> fractionEqual f f1
    _ -> False
  return f1

-- problem 34 -

digitFactorials :: [Integer]
digitFactorials = map factorial [0..9]

isCurious :: Integer -> Bool
isCurious n = n == (sum $ map (digitFactorials !!) $ integerToDigits n)

problem34 :: Integer
problem34 = sum $ filter isCurious [3 .. m]
  where m = factorial 10


-- problem 35 : circular primes

intFromDigits :: [Int] -> Int
intFromDigits = foldr (\digit num -> 10 * num + digit) 0 . reverse

rotations :: [a] -> [[a]]
rotations xs = do
  let n = length xs
  i <- [1..n]
  return $ (drop i xs) ++ (take i xs)

isCircularPrime :: Int -> Bool
isCircularPrime x = all isPrime $ xs
  where
    xs = map (toInteger . intFromDigits) $ rotations digits
    digits = integerToDigits $ toInteger x

problem35 :: Int
problem35 = length $ filter isCircularPrime [2..1000000]

-- problem 36 - double base palindrome

integerFromDigits :: [Integer] -> Integer
integerFromDigits = foldr (\digit num -> 10 * num + digit) 0 . reverse

toBinary :: Integer -> Integer
toBinary n = integerFromDigits $ reverse $ List.unfoldr nextDigit n
  where
    nextDigit :: Integer -> Maybe (Integer, Integer)
    nextDigit 0 = Nothing
    nextDigit i = Just (i `mod` 2, i `div` 2)

problem36 :: Integer
problem36 = sum $ do
  n <- [1..1000000]
  guard $ isPalindrome n
  guard $ isPalindrome $ toBinary n
  return n

-- problem 37 -- truncatable primes

removeDigitsFromRight :: Integer -> Integer
removeDigitsFromRight = (`div` 10)

removeDigitsFromLeft :: Integer -> Integer
removeDigitsFromLeft n = (n `mod` size)
  where
    numDigits = length $ show n
    size = 10 ^ (numDigits - 1)

truncations :: Integer -> [Integer]
truncations n = n : (leftTruncations ++ rightTruncations)
  where
    numDigits = length $ show n
    leftTruncations = tail $ take numDigits $ iterate removeDigitsFromLeft n
    rightTruncations = tail $ take numDigits $ iterate removeDigitsFromRight n

problem37 = sum $ map (!! 0) $ take 11 $ filter (all isPrime) $ map truncations (drop 4 primes)

-- problem 38 -- pandigital multiples

multiplesOf :: Integer -> [Integer]
multiplesOf n = map (* n) [1 ..]

makesPandigital :: [Integer] -> Maybe [Int]
makesPandigital xs = loop [] xs
  where
    loop :: [Int] -> [Integer] -> Maybe [Int]
    loop digits (n:ns)
      | isPandigital digits = Just digits
      | 9 < length digits = Nothing
      | otherwise = loop (digits ++ integerToDigits n) ns

problem38 :: [Int]
problem38 = maximum $ do
  n <- [2 .. 9999]
  guard $ areDistinct $ integerToDigits n
  let pd = makesPandigital $ multiplesOf n
  guard $ Maybe.isJust pd
  return $ Maybe.fromJust pd

-- problem 39 -- integer right triangles

isPythagorean :: Int -> Int -> Int -> Bool
isPythagorean a b c = c * c - a * a - b * b == 0

mostFrequent :: (Ord a) => [a] -> a
mostFrequent = head . List.maximumBy (Ord.comparing length) . List.group . List.sort

problem39 :: Int
problem39 = mostFrequent $ do
  a <- [1..333]
  b <- [a..500]
  c <- [b..(1000-a-b)]
  guard $ isPythagorean a b c
  return $ a + b + c

-- problem 40 :: Champernowne's constant

extractNthDigits :: [Int] -> [Int] -> [Int]
extractNthDigits ns digits = loop [] (List.sort ns) (zip [1..] digits)
  where
    loop result [] _ = result
    loop result xxs@(x:xs) ((i,d):ds)
      | x == i = loop (result ++ [d]) xs ds
      | otherwise = loop result xxs ds

concatenateTheIntegers :: [Int]
concatenateTheIntegers = do
  n <- [1..]
  digit <- integerToDigits n
  return digit

problem40 :: Integer
problem40 = product $ map toInteger $ extractNthDigits powersOf10 concatenateTheIntegers
  where powersOf10 = map (10 ^) [0..6]

-- problem 41

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = do
  let n = length xs
  subPerm <- permutations xs
  i <- [0 .. n]
  return $ (take i subPerm) ++ [x] ++ (drop i subPerm)

pandigitals :: Int -> [Integer]
pandigitals n = do
  digits <- permutations [1..n]
  return $ toInteger $ intFromDigits digits

problem41 :: Integer
problem41 = maximum $ do
  i <- [1..9]
  n <- pandigitals i
  guard $ isPrime n
  return n

-- problem 42

triangleNumbers :: [Integer]
triangleNumbers = map (\i -> (i * (i + 1) `div` 2)) [1..]

isTriangleNumber :: Integer -> Bool
isTriangleNumber n = not $ null $ filter (== n) $ takeWhile (<= n) triangleNumbers

problem42 :: IO Int
problem42 = do
  text <- readFile "p042_words.txt"
  let words = prepNames text
  return $ length $ filter isTriangleNumber $ map wordValue words

-- problem 43

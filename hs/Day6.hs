import Utils ((<!>), splitByChar)
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)

main =
  do
    test
    realData <- readFile "../data/day6.data.txt"

    let races = parse $ lines realData
    print $ part1 races

    let bigRace = parseCompact $ lines realData
    print $ part2 bigRace

-- test

test =
  do
    []         <!> polyroots 1.0 2.0 3.0
    [0.0]      <!> polyroots 1.0 0.0 0.0
    [1.0, 0.0] <!> polyroots 1.0 (-1.0) 0.0

    Just (2, 5)   <!> improvementsRange 7 9
    Just (4, 11)  <!> improvementsRange 15 40
    Just (11, 19) <!> improvementsRange 30 200

    margin 7 9    <!> 4
    margin 15 40  <!> 8
    margin 30 200 <!> 9

    288 <!> part1 [(7, 9), (15, 40), (30, 200)]

    let testData = ["Time:      7  15   30", "Distance:  9  40  200"]
    let races = parse testData
    races <!> [(7, 9), (15, 40), (30, 200)]
    288 <!> part1 races

    let bigRace = parseCompact testData
    bigRace <!> (71530, 940200)
    margin 71530 940200 <!> 71503
    part2 bigRace <!> 71503

    putStrLn "Tests completed"

-- polyroots
-- real roots of a degree 2 polynomial

polyroots :: Double -> Double -> Double -> [Double]
polyroots a b c =
  let
    square x = x * x
    z = (square b) - (4 * a * c)
    root op = ((-b) `op` (sqrt z)) / (2 * a)
  in
    case signum(z) of
      -- no real roots
      -1 -> []
      -- one real root
      0 -> [root (+)]
      -- two real roots
      1 -> [root (+), root (-)]

-- raceroots
-- real roots of a race
-- a race is equivalent to a polynomial f(x, y) = x * (y - x) = y * x - x^2
-- finding the points equal to the current record z is equivalent to
-- finding the roots of g(x, y, z) = f(x, y) - z <=> -x^2 + y*x - z = 0
-- the polynomial is then defined as (a = -1, b = y, c = -z)

raceroots :: Double -> Double -> [Double]
raceroots y z = polyroots (-1) y (-z)

g :: Double -> Double -> Double -> Double
g y z x = -(x * x) + y * x - z

-- improvementsRange

improvementsRange :: Double -> Double -> Maybe (Int, Int)
improvementsRange y z =
  case raceroots y z of
    -- single real root:
    -- the current record has to be the best achievable
    -- no improvement possible
    r:[] -> Nothing

    -- two real roots:
    -- need to convert the real roots to integer roots
    -- then build the range
    r1:r2:[] ->
      let
        left  = integerRoot y z Up   $ min r1 r2
        right = integerRoot y z Down $ max r1 r2
      in
        if left <= right
          then Just (left, right)
          else Nothing

    -- Note: there must be a real root, as
    -- (a) the record is a value that was computed by applying f to a valid input x
    -- (b) the edges of the range both result in f returning 0

data Direction = Up | Down

integerRoot :: Double -> Double -> Direction -> Double -> Int
integerRoot y z d r =
  let
    n =
      case d of
        Up   -> ceiling r
        Down -> floor r
    -- The current record could be a "co-maximum" in integer values
    -- Patch that case now
    patched =
      case g y z (fromIntegral n) of
        0.0 ->
          case d of
            Up   -> n + 1
            Down -> n - 1
        _ -> n
  in
    patched

-- margin
-- In how many ways can we beat the current record?

margin :: Double -> Double -> Int
margin y z = fromMaybe 0 $ fmap (\(lo, hi) -> hi - lo + 1) $ improvementsRange y z

-- part1

part1 :: [(Int, Int)] -> Int
part1 = product . map (\(duration, record) -> margin (fromIntegral duration) (fromIntegral record))

-- parse

parse :: [String] -> [(Int, Int)]
parse (tl:dl:[]) = (parseLine tl) `zip` (parseLine dl)

parseLine :: String -> [Int]
parseLine = map read . tail . splitByChar ' '

-- parseCompact

parseCompact :: [String] -> (Int, Int)
parseCompact (tl:dl:[]) = (,) (parseLineCompact tl) (parseLineCompact dl)

parseLineCompact :: String -> Int
parseLineCompact = read . fold . tail . splitByChar ' '

-- part2

part2 :: (Int, Int) -> Int
part2 (duration, record) = margin (fromIntegral duration) (fromIntegral record)

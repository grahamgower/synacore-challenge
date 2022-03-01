{- Find a path to the vault. -}

-- containers
import qualified Data.IntMap.Strict as IntMap
-- random
import System.Random


data Mosaic a = Add | Sub | Mul | Num a
  deriving Show

rooms :: IntMap.IntMap (Mosaic Int, [Int])
rooms = IntMap.fromList
  [(0x0a3f, (Num 22, [0x0a2b, 0x0a44]))
  ,(0x0a44, (Sub,    [0x0a30, 0x0a49]))
  ,(0x0a49, (Num 9,  [0x0a44, 0x0a35, 0x0a4e]))
  ,(0x0a4e, (Mul,    [0x0a49, 0x0a3a]))
  ,(0x0a2b, (Add,    [0x0a17, 0x0a30]))
  ,(0x0a30, (Num 4,  [0x0a2b, 0x0a44, 0x0a35, 0x0a1c]))
  ,(0x0a35, (Sub,    [0x0a30, 0x0a49, 0x0a3a, 0x0a21]))
  ,(0x0a3a, (Num 18, [0x0a35, 0x0a4e, 0x0a26]))
  ,(0x0a17, (Num 4,  [0x0a2b, 0x0a1c, 0x0a03]))
  ,(0x0a1c, (Mul,    [0x0a17, 0x0a30, 0x0a21, 0x0a08]))
  ,(0x0a21, (Num 11, [0x0a1c, 0x0a35, 0x0a26, 0x0a0d]))
  ,(0x0a26, (Mul,    [0x0a21, 0x0a3a, 0x0a12]))
  ,(0x0a03, (Mul,    [0x0a17, 0x0a08]))
  ,(0x0a08, (Num 8,  [0x0a03, 0x0a1c, 0x0a0d]))
  ,(0x0a0d, (Sub,    [0x0a08, 0x0a21, 0x0a12]))
  ,(0x0a12, (Num 1,  []))
  ]

-- Random walk from a given location to an absorbing state.
walk :: RandomGen t => Int -> [Mosaic Int] -> t -> [Mosaic Int]
walk location path rng =
  let (mosaic, doors) = rooms IntMap.! location
      path' = mosaic:path
      n = length doors
      (i, rng') = uniformR (0, n - 1) rng
      location' = doors !! i
  in
    if n == 0 then
      reverse path'
    else
      walk location' path' rng'

-- Calculate the weight of the orb given a path.
weigh :: [Mosaic Int] -> Int
weigh path =
  let m = head path
  in case m of
    Num w -> weigh' w (tail path)
    _ -> error $ "Initial path component not a number: " <> show m
  where
  weigh' :: Int -> [Mosaic Int] -> Int
  weigh' w _ | w < 0 = 0
  weigh' w [] = w
  weigh' w (m:n:path') =
    let w' =
          case (m, n) of
            (Add, Num o) -> w + o
            (Sub, Num o) -> w - o
            (Mul, Num o) -> w * o
            _ -> error $ show m <> " connected to " <> show n
    in weigh' w' path'
  weigh' _ _ = error $ "path length must be even"

mkPath :: Int -> Int -> (Int, [Mosaic Int], Int, Int)
mkPath start seed =
  let path = walk start [] (mkStdGen seed)
      weight = weigh path
      len = length path
  in (len, path, weight, seed)

main :: IO ()
main =
  let start = 0x0a3f
      seeds = [0..]
      lpws = map (mkPath start) seeds
      paths = filter (\(l, _, w, _) -> w == 30 && l < 15) lpws
  in do
    print $ head paths

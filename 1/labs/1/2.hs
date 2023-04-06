max3 :: Int -> Int -> Int -> Int
max3 x y z = max x (max y z)

min3 :: Int -> Int -> Int -> Int
min3 x y z = min x (min y z)

sort2 :: Int -> Int -> (Int, Int)
sort2 x y = if x < y then (x, y) else (y, x)


-- pattern matching. вызывается та часть функции, которая подходит под заданный шаблон , сверху вниз
bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _ _ = False

solve2 :: Double -> Double -> (Bool, Double)
solve2 a b =
  if a == 0
    then (False, 0.0)
    else (True, -b / a)

isParallel :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
  (x2 - x1) * (y4 - y3) == (x4 - x3) * (y2 - y1)

isIncluded :: (Double, Double, Double) -> (Double, Double, Double) -> Bool
isIncluded (x1, y1, r1) (x2, y2, r2) =
  let d = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
   in r1 >= r2 + d

isRectangular :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectangular (x1, y1) (x2, y2) (x3, y3) =
  let a = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
      b = sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
      c = sqrt ((x1 - x3) ^ 2 + (y1 - y3) ^ 2)
  in a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || c^2 + a^2 == b^2

isTriangle :: Double -> Double -> Double -> Bool
isTriangle x y z =
  let a = maximum [x, y, z]
      b = sum [x, y, z] - a
   in a < b + a

isSorted :: Double -> Double -> Double -> Bool
isSorted a b c = (a <= b && b <= c) || (a >= b && b >= c)

main :: IO ()
main = do
  putStrLn "max3 1 2 3:"
  print $ max3 1 2 3 -- 3

  putStrLn "max3 5 5 5:"
  print $ max3 5 5 5 -- 5

  putStrLn "min3 1 2 3:"
  print $ min3 1 2 3 -- 1
  putStrLn "min3 5 5 5:"
  print $ min3 5 5 5 -- 5

  putStrLn "sort2 1 2:"
  print $ sort2 1 2 -- (1, 2)
  putStrLn "sort2 5 3:"
  print $ sort2 5 3 -- (3, 5)

  putStrLn "bothTrue True True:"
  print $ bothTrue True True -- True
  putStrLn "bothTrue True False:"
  print $ bothTrue True False -- False

  putStrLn "solve2 2 4:"
  print $ solve2 2 4 -- (True, -2.0)
  putStrLn "solve2 0 4:"
  print $ solve2 0 4 -- (False, 0.0)

  putStrLn "isParallel (1, 1) (2, 2) (2, 0) (4, 2):"
  print $ isParallel (1, 1) (2, 2) (2, 0) (4, 2) -- True
  putStrLn "isParallel (1, 1) (2, 2) (2, 0) (3, 3):"
  print $ isParallel (1, 1) (2, 2) (2, 0) (3, 3) -- False

  putStrLn "isIncluded (1, 1, 4) (2, 2, 2):"
  print $ isIncluded (1, 1, 4) (2, 2, 2) -- False
  putStrLn "isIncluded (1, 1, 5) (2, 2, 2):"
  print $ isIncluded (1, 1, 5) (2, 2, 2) -- True


-- 1) Функция вычисления арифметического среднего элементов списка вещественных чисел с использованием функции foldr. Функция должна осуществлять только один проход по списку.
average :: [Double] -> Double
average xs =
  let (sum, count) = foldr (\x (s, c) -> (s + x, c + 1)) (0, 0) xs
   in sum / count


dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = foldr (+) 0 $ zipWith (*) xs ys


countEven :: Integral a => [a] -> Int
countEven xs = foldr (\x acc -> if even x then acc + 1 else acc) 0 xs


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
  where left = filter (< x) xs
        right = filter (>= x) xs

quicksortWith :: (a -> a -> Bool) -> [a] -> [a]
quicksortWith _ [] = []
quicksortWith cmp (x : xs) = quicksortWith cmp left ++ [x] ++ quicksortWith cmp right
  where
    left = filter (\y -> cmp y x) xs
    right = filter (\y -> not (cmp y x)) xs

main :: IO ()
main = do
  let numbers = [1.0, 2.0, 3.0, 4.0, 5.0]
  print $ average numbers -- 3.0
  
  let xs = [1, 2, 3, 4]
      ys = [5, 6, 7, 8]
  print $ dotProduct xs ys -- 70

  let integers = [1, 2, 3, 4, 5, 6]
  print $ countEven integers -- 3
  
  let unsortedList = [5, 1, 9, 4, 6, 8, 2, 3, 7]
  print $ quicksort unsortedList -- [1,2,3,4,5,6,7,8,9]

  let strings = ["abc", "ab", "a", "abcd", "abcde"]
  print $ quicksortWith (\x y -> length x < length y) strings -- ["a","ab","abc","abcd","abcde"]
  
  let emptyList = [] :: [Double]
      singleElementList = [1.0]
  print $ average emptyList -- 0.0
  print $ average singleElementList -- 1.0

  let emptyList' = [] :: [Int]
      singleElementList' = [5]
  print $ countEven emptyList' -- 0
  print $ countEven singleElementList' -- 0

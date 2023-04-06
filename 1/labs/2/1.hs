-- Функция, принимающая на вход целое число n и возвращающая список, содержащий n элементов, упорядоченных по возрастанию.
ascendingList :: Int -> [Int]
ascendingList n = [1..n]
-- ascendingList n = take n [1..]
-- функция тейк принимает первым аргом кол-во элтов, а вторым список
-- оператор список принимает в себя элты, среди которых 1 и .. 
-- .. оператор диапазона, используется для создания списков с определегнным шагом (изначально шаг =1) . В данном случае максимум 


-- Список натуральных чисел.
naturalNumbers :: [Int]
naturalNumbers = [1..]

-- Список нечетных натуральных чисел.
oddNumbers :: [Int]
oddNumbers = [1,3..]

-- Список четных натуральных чисел.
evenNumbers :: [Int]
evenNumbers = [2,4..]

-- Список квадратов натуральных чисел.
squares :: [Int]
squares = map (^2) [1..]
-- map -- функция применяющая функцию к элтам заданногр списка и возвращающая значения после применения
-- в данном случае функция ^2

-- Список факториалов.
factorials :: [Integer]
factorials = scanl (*) 1 [1..]
-- scanl -- функция, которая принимает функцию, начальное значение и список и возвращает список, составленный из промежуточных значений

-- Список степеней двойки.
powersOfTwo :: [Int]
powersOfTwo = iterate (*2) 1
-- iterate -- функция, которая принимает функцию и начальное значение и возвращает список, составленный из промежуточных значений
-- можно было сделать тоже из scanl и repeat
-- powersOfTwo :: [Int]
-- powersOfTwo = scanl (*) 1 (repeat 2)

-- Список треугольных чисел.
triangularNumbers :: [Int]
triangularNumbers = scanl (+) 0 [1..]

-- Список пирамидальных чисел.
pyramidalNumbers :: Int -> [Int]
pyramidalNumbers n = take n $ scanl (+) 0 [1,3..]
-- список с 1, по бесконечность с шагом 2 (1,3,6,9...)

-- Функция, принимающая на входе список вещественных чисел и вычисляющая их арифметическое среднее.
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)
-- fromIngeral -- функция, которая принимает целое число и возвращает вещественное. Гарантирует плавующую точку
-- нужно, потмоу что / использует только тип дабл

-- Функция вычленения n-го элемента из заданного списка.
nthElement :: Int -> [a] -> a
nthElement n xs = xs !! (n - 1)
--  !! -- оператор индексации списка, нужен для доступа к элту по индексу (слева список справа индукс)

-- Функция сложения элементов двух списков. Возвращает список, составленный из сумм элементов списков-параметров.
addLists :: Num a => [a] -> [a] -> [a]
addLists [] ys = ys
addLists xs [] = xs
addLists (x:xs) (y:ys) = x + y : addLists xs ys
-- оператор (:) -- добавление элта в начало списка
-- первые и второй патерн для отлова максимальной глубины рекурсии -- когда в исходных масивах не осталось элтов
-- третья определяет х и хс, у и ус как первые элты и всё остальное (деструктуризирует для использования в функции) и вызывает рекурсивно 

-- Функция перестановки местами соседних четных и нечетных элементов в заданном списке.
swapNeighbors :: [Int] -> [Int]
swapNeighbors [] = []
swapNeighbors [x] = [x]
swapNeighbors (x:y:zs) | even x && odd y = y:x:swapNeighbors zs
                       | odd x && even y = y:x:swapNeighbors zs
                       | otherwise = x : swapNeighbors (y:zs)

-- | -- условный оператор (guards)
-- otherwise -- ключ слово "в ином случае" по русски
-- 

-- Функция twopow n, которая вычисляет 2^n.
twopow :: Int -> Int
twopow 0 = 1
twopow n | even n = let x = twopow (n `div` 2) in x * x
         | otherwise = 2 * twopow (n - 1)


-- Функция removeOdd, которая удаляет из заданного списка целых чисел все нечетные числа.
removeOdd :: [Int] -> [Int]
removeOdd [] = []
removeOdd (x:xs) = if odd x then removeOdd xs else x : removeOdd xs


-- Функция removeEmpty, которая удаляет пустые строки из заданного списка строк
removeEmpty :: [String] -> [String]
removeEmpty = filter (not . null)

-- концепция point-free, применяем переданное значение к другой функции, поээтому описание атрибутов опущено

-- Функция countTrue :: [Bool] -> Integer, возвращающая количество элементов списка, равных True
countTrue :: [Bool] -> Integer
countTrue = toInteger . length . filter id
-- filter id -- id просто возвращает элт без изменений, если там фолс то фильтр отфильтрует

-- Функция makePositive, которая меняет знак всех отрицательных элементов списка чисел
makePositive :: [Int] -> [Int]
makePositive = map abs

-- Функция delete :: Char -> String -> String, которая принимает на вход строку и символ и возвращает строку, в которой удалены все вхождения символа
delete :: Char -> String -> String
delete c = filter (/= c)
-- /= -- оператор сравнения на неравенство

-- Функция substitute :: Char -> Char -> String -> String, которая заменяет в строке указанный символ на заданный
substitute :: Char -> Char -> String -> String
substitute from to = map (\c -> if c == from then to else c)
-- \c лямбда функция с аргом с


main :: IO ()
main = do
  print $ ascendingList 5 -- [1,2,3,4,5]
  
  print $ take 5 naturalNumbers -- [1,2,3,4,5]
  
  print $ take 5 oddNumbers -- [1,3,5,7,9]
  
  print $ take 5 evenNumbers -- [2,4,6,8,10]
  
  print $ take 5 squares -- [1,4,9,16,25]
  
  print $ take 5 factorials -- [1,1,2,6,24]
  
  print $ take 5 powersOfTwo -- [1,2,4,8,16]
  
  print $ take 5 triangularNumbers -- [0,1,3,6,10]
  
  print $ pyramidalNumbers 4 -- [1,4,10,20]
  
  print $ average [1.0, 2.0, 3.0] -- 2.0
  
  print $ nthElement 2 [1, 2, 3] -- 2
  
  print $ addLists [1, 2, 3] [4, 5, 6] -- [5,7,9]
  
  print $ swapNeighbors [1, 2, 3, 4, 5, 6] -- [2,1,4,3,6,5]
  
  print $ twopow 4 -- 16
  
  print $ removeOdd [1,4,5,6,10] -- [4,6,10]
  
  print $ removeEmpty ["", "Hello", "", "", "World!"] -- ["Hello","World!"]
  
  print $ countTrue [True, False, True, True, False] -- 3
  
  print $ makePositive [-1, 0, 5, -10, -20] -- [1,0,5,10,20]
  
  print $ delete 'l' "Hello world!" -- "Heo word!"
  
  print $ substitute 'e' 'i' "eigenvalue" -- "iiginvalui"

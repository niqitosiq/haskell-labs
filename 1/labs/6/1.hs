
main :: IO ()

-- Считывание двух чисел и их суммирование:
main = do
  putStrLn "Введите первое число:"
  xStr <- getLine
  putStrLn "Введите второе число:"
  yStr <- getLine
  let x = read xStr :: Int
      y = read yStr :: Int
      sum = x + y
  putStrLn $ "Сумма: " ++ show sum

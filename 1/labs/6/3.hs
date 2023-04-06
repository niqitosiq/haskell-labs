import System.Environment (getArgs)

-- Вывод имени файла, переданного через командную строку:
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> putStrLn $ "Имя файла: " ++ filename
    _ -> putStrLn "Пожалуйста, укажите имя файла в качестве аргумента командной строки."

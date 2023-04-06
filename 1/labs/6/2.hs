import System.Environment (getArgs)

-- Распечатывание аргументов командной строки:
main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Аргументы командной строки: " ++ show args

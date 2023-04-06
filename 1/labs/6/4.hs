
import System.Environment (getArgs)
import System.IO (readFile)

-- Вывод первых n строк файла, указанного в командной строке:˚
main :: IO ()
main = do
  args <- getArgs
  case args of
    [nStr, filename] -> do
      fileContents <- readFile filename
      let n = read nStr :: Int
          linesToPrint = take n $ lines fileContents
      putStrLn $ unlines linesToPrint
    _ -> putStrLn "Пожалуйста, укажите число n и имя файла в качестве аргументов командной строки."

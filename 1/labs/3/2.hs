import Data.List (find, nub)

-- вариант 8
-- В электронной записной книжке хранятся записи следующих ви-дов: напоминания о днях рождения знакомых, телефоны знакомых и назначенные встречи. Напоминание состоит из имени знакомого и даты (день и месяц). Запись о телефоне должна содержать имя человека и его телефон. Информация о назначенной встрече содержит дату встречи (день, месяц, год) и краткое описание (мож-но представлять строкой). Разработайте тип данных, представляющий такую запись. Записная книжка является списком записей.
-- Определите следующие функции:
-- 1) getByName, возвращающая информацию о человеке с указан-ным именем (его телефон и дагу рождения).
-- 2) getByLetter, возвращающая список людей, о которых есть информация в записной книжке и чье имя начинается на указан-ную букву.
-- 3) getAssignment, возвращающая по указанной дате список дел (информация о назначенных встречах и телефоны друзей, которых нужно поздравить в этот день).

data Date = Date
  { day :: Int,
    month :: Int,
    year :: Maybe Int
  }
  deriving (Eq, Show)

type Notebook = [Entry]

data Entry
  = BirthdayReminder {entryName :: String, date :: Date}
  | PhoneNumber {entryName :: String, phone :: String}
  | Appointment {date :: Date, description :: String}
  deriving (Eq, Show) -- существенно облегчаем работу....

getByName :: String -> Notebook -> (Maybe String, Maybe Date)
getByName personName notebook =
  let phoneEntry = find (\e -> case e of PhoneNumber n _ -> n == personName; _ -> False) notebook
      birthdayEntry = find (\e -> case e of BirthdayReminder n _ -> n == personName; _ -> False) notebook
   in (phoneEntry >>= (\(PhoneNumber _ p) -> Just p), birthdayEntry >>= (\(BirthdayReminder _ d) -> Just d))

extractName :: Entry -> String
extractName (PhoneNumber n _) = n
extractName (BirthdayReminder n _) = n
extractName _ = ""

getByLetter :: Char -> Notebook -> [String]
getByLetter letter notebook = nub filteredNames
  where
    names = map extractName notebook
    startsWithLetter name = (not . null) name && head name == letter
    filteredNames = filter startsWithLetter names

getAssignment :: Date -> Notebook -> ([String], [String])
getAssignment date notebook =
  let events =
        filter
          ( \e -> case e of
              Appointment {date = d} -> d == date
              BirthdayReminder {date = d} -> d == date {year = Nothing}
              _ -> False
          )
          notebook
      appointments = [desc | Appointment {description = desc} <- events]
      friendsToCongratulate = [name | BirthdayReminder {entryName = name} <- events]
   in (appointments, friendsToCongratulate)

main :: IO ()
main = do
  let notebook =
        [ BirthdayReminder "Ivan" (Date 15 4 Nothing),
          PhoneNumber "Ivan" "555-1234",
          Appointment (Date 15 4 (Just 2023)) "Празднование дня рождения Ивана",
          BirthdayReminder "Anna" (Date 20 4 Nothing),
          PhoneNumber "Anna" "555-5678",
          Appointment (Date 20 4 (Just 2023)) "Празднование дня рождения Анны"
        ]

  print $ getByName "Ivan" notebook
  print $ getByLetter 'I' notebook
  print $ getAssignment (Date 15 4 (Just 2023)) notebook

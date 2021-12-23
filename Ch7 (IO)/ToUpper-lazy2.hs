import Data.Char (toUpper)

main = interact ((++) "You data in uppercase, is: \n\n" . map toUpper)
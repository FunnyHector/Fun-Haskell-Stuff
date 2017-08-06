import Data.List
import Data.Char

strong :: String -> Bool
strong pswd = any isDigit pswd && any isUpper pswd && any isLower pswd && length pswd > 15

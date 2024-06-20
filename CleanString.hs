module CleanString (cleanString, cleanChar) where

-- Function to clean the string, removing escape characters and surrounding quotes
cleanString :: String -> String
cleanString str = read str :: String

-- Function to clean the character, removing surrounding single quotes
cleanChar :: String -> Char
cleanChar str = read str :: Char
module Main where
import Control.Applicative
import Data.Char (isDigit, isSpace)
import Language.Haskell.TH.PprLib (sep)

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer -- NOTE: no support for float
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', x) <- p2 input'
            Just (input'', f x)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    Parser(p1) <|> Parser(p2) = Parser $ \input -> p1 input <|> p2 input


charP :: Char -> Parser Char
charP x = Parser $ f
    where
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \input ->
    let (token, rest) = span p input
        in Just(rest, token)


jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where f "true"  = JsonBool True
          f "false" = JsonBool False
          -- should not happen
          f _       = undefined

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs then Nothing else Just (input', xs)


jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where
        f ds = JsonNumber $ (read ds :: Integer)

stringLiteral :: Parser String
stringLiteral = spanP (/= '"')

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"')

spaces :: Parser String
spaces = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> elements <* charP ']')
    where elements = sepBy (spaces *> charP ',' <* spaces) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> spaces *> objects <* spaces <* charP '}')
    where
        objects = sepBy (spaces *> charP ',' <* spaces) objectParser
        objectParser = (\(JsonString key) _ value -> (key,value))
            <$> jsonString <*> (spaces *> charP ':' <* spaces) <*> (jsonValue <* spaces)


-- NOTE: No proper error reporting
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO ()
main = undefined




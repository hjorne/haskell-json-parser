{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Data.Char
import           Control.Applicative

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer -- TODO Add support for floats
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

-- TODO: Add error reporting?
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (input', a) <- p input
        return (input', f a)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input' , f) <- p1 input
        (input'', a) <- p2 input'
        return (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldr (\x acc -> (:) <$> x <*> acc) (pure [])

charP :: Char -> Parser Char
charP c = Parser $ \case
    x : xs | x == c -> Just (xs, x)
    _               -> Nothing

stringP :: String -> Parser String
stringP = sequenceA' . fmap charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
    Parser $ \input -> let (token, rest) = span f input in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ p >=> \(input', xs) ->
    if null xs then Nothing else Just (input', xs)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true"  = JsonBool True
    f "false" = JsonBool False
    f _       = error "Fell through true/false bool parsing"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNull (spanP isDigit)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
    where elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
    JsonObject
        <$> (  charP '{'
            *> ws
            *> sepBy (ws *> charP ',' *> ws) pairs
            <* ws
            <* charP '}'
            )
  where
    pairs =
        (\key _ value -> (key, value))
            <$> stringLiteral
            <*> (ws *> charP ':' *> ws)
            <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue =
    jsonNull
        <|> jsonBool
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject

main :: IO ()
main = undefined

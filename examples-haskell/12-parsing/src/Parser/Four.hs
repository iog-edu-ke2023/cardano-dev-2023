{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use void" #-}

module Parser.Four where

import           Control.Applicative
import           Data.List           (foldl')
import           Numeric.Natural     (Natural)

newtype Parser a = Parser {runParser :: String -> [(a, String)]}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \s ->
        [(f a, t) | (a, t) <- runParser p s]

instance Applicative Parser where

    pure :: a -> Parser a
    pure a = Parser $ \s -> [(a, s)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p <*> q = Parser $ \s -> do
        (f, t) <- runParser p s
        (a, u) <- runParser q t
        pure (f a, u)

instance Alternative Parser where

    empty :: Parser a
    empty = Parser $ const []

    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = Parser $ \s -> runParser p s ++ runParser q s

eof :: Parser ()
eof = Parser $ \s -> case s of
    []    -> [((), [])]
    _ : _ -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    []              -> []
    c : cs
        | p c       -> [(c, cs)]
        | otherwise -> []

digit :: Parser Char
digit = satisfy (`elem` "0123456789")

letter :: Parser Char
letter = satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'])

char :: Char -> Parser ()
char c = () <$ satisfy (== c)

digit' :: Parser Natural
digit' = charToNat <$> digit

nzDigit :: Parser Natural
nzDigit = charToNat <$> satisfy (`elem` "123456789")

charToNat :: Char -> Natural
charToNat c = fromIntegral (fromEnum c) - 48

natural :: Parser Natural
natural =
        (0 <$  char '0')
    <|> (f <$> nzDigit <*> many digit')
  where
    f :: Natural -> [Natural] -> Natural
    f = foldl' (\acc d -> 10 * acc + d)

integer :: Parser Integer
integer = f <$> optional sign <*> natural
  where
    f :: Maybe (Natural -> Integer) -> Natural -> Integer
    f Nothing  = toInteger
    f (Just s) = s

    sign :: Parser (Natural -> Integer)
    sign =
            (toInteger          <$ char '+')
        <|> (negate . toInteger <$ char '-')

module Parser.Two where

type Parser = String -> Maybe String

satisfy :: (Char -> Bool) -> Parser
satisfy _ []       = Nothing
satisfy p (c : cs)
    | p c          = Just cs
    | otherwise    = Nothing

digit :: Parser
digit = satisfy (`elem` "0123456789")

letter :: Parser
letter = satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'])

char :: Char -> Parser
char c = satisfy (== c)

many :: Parser -> Parser
many p s = case p s of
    Nothing -> Just s
    Just t  -> many p t

eof :: Parser
eof [] = Just []
eof _  = Nothing

combine :: Parser -> Parser -> Parser
combine p q s = do
    t <- p s
    q t

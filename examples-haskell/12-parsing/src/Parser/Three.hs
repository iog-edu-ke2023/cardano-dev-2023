module Parser.Three where

type Parser = String -> [String]

satisfy :: (Char -> Bool) -> Parser
satisfy _ []       = []
satisfy p (c : cs)
    | p c          = [cs]
    | otherwise    = []

digit :: Parser
digit = satisfy (`elem` "0123456789")

letter :: Parser
letter = satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'])

char :: Char -> Parser
char c = satisfy (== c)

many :: Parser -> Parser
many p s = s : (p `combine` many p) s

eof :: Parser
eof [] = [[]]
eof _  = []

combine :: Parser -> Parser -> Parser
combine p q s = do
    t <- p s
    q t

choose :: Parser -> Parser -> Parser
choose p q s = p s ++ q s

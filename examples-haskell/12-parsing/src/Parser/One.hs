module Parser.One where

type Parser = String -> Bool

digit :: Parser
digit [c]
    | c `elem` "0123456789" = True
digit _                     = False

eof :: Parser
eof = null

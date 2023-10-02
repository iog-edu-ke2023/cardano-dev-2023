{-# LANGUAGE OverloadedStrings #-}

import Language.Marlowe.Extended.V1

main :: IO ()
main = printJSON $ example [Role "Alice", Role "Bob"] (Role "Charlie")

example :: [Party] -> Party -> Contract
example parties oracle = 
    When
        (map (\p -> deposit p $ filter (/= p) parties) parties)
        (TimeParam "payment_deadline")
        Close 
  where
    cid :: ChoiceId
    cid = ChoiceId "winner" oracle

    choice :: [(Party, Integer)] -> Contract
    choice []            = Close
    choice ((p, n) : xs) =
        If
            (ValueEQ (ChoiceValue cid) (Constant n))
            (pay p $ filter (/= p) parties)
            (choice xs)

    pay :: Party -> [Party] -> Contract 
    pay _ []       = Close
    pay p (q : qs) = Pay q (Account p) ada (ConstantParam "amount") $ pay p qs

    deposit :: Party -> [Party] -> Case
    deposit p ps = 
        Case
            (Deposit p p ada (ConstantParam "amount"))
            (case ps of
                [] ->
                    When
                        [Case
                            (Choice cid [Bound 1 $ toInteger $ length parties])
                            (choice (zip parties [1..]))
                        ]
                        (TimeParam "choice_deadline")
                        Close 
                    
                qs ->
                    When
                        (map (\q -> deposit q $ filter (/= q) qs) qs)
                        (TimeParam "payment_deadline")
                        Close)
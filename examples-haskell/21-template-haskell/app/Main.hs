import           Criterion.Main
import           Power
import           UsePower

main :: IO ()
main = defaultMain
    [ bench "pow 10 42" $ nf (pow 10) 42
    , bench "power 10 42" $ nf (power 10) 42
    , bench "power10 42" $ nf power10 42
    , bench "naivePower10 42" $ nf (naivePower 10) 42
    , bench "spower10 42" $ nf spower10 42
    , bench "spower10' 42" $ nf spower10' 42
    ]

pow :: Int -> Int -> Int
pow = flip (^)

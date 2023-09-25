{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE QuasiQuotes    #-}

module UseQuasi where

import           Quasi
import           Text.Blaze.Html          (Markup)
import           Text.Blaze.Renderer.Text (renderMarkup)
import           Text.Hamlet

fortyTwo2, fortyTwo3, fortyTwo8, fortyTwo16 :: Double
fortyTwo2  = 0b101010
fortyTwo3  = [ternary|1120|]
fortyTwo8  = 0o52
fortyTwo16 = 0x2A

weird :: Int -> String
weird 37              = "hmmm - 37!"
weird [ternary|1120|] = "42"
weird _               = "something else"

myHtml :: Markup
myHtml = [shamlet|
    <html>
        <head>
            <title>Haskell
        <body>
            <h1>Haskell Course
            <p>Haskell is great!
|]

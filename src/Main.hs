module Main
where

import Text.Pandoc.JSON as J
import Text.Pandoc.Walk
import Control.Monad
import Data.Char
import System.IO
import qualified Data.ByteString.Lazy as BL
import Data.List


import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Data.Aeson

prefixAndReplacement :: [([Inline],String)]
prefixAndReplacement =
  [ ( [Str "[", J.Space, Str "]"] , "☐")
  , ( [Str "[.]"]                 , "▄⃞▄")
  , ( [Str "[o]"]                 , "▆⃞▆")
  , ( [Str "[O]"]                 , "▇⃞▇")
  , ( [Str "[X]"]                 , "☑")
  ]

-- Replace the first matching prefix
replacePrefixes :: [Inline] -> [([Inline],String)] -> [Inline]
replacePrefixes s [] = s
replacePrefixes s ((p,r):pars) =
  if p `isPrefixOf` s
     then Str r : drop (length p) s
     else replacePrefixes s pars

inlineItems :: Inline -> Inline
inlineItems (Emph s)    = Emph $ replacePrefixes s prefixAndReplacement
inlineItems (Strong s)  = Strong $ replacePrefixes s prefixAndReplacement
inlineItems (Strikeout s)  = Strikeout $ replacePrefixes s prefixAndReplacement
inlineItems (Superscript s)  = Superscript $ replacePrefixes s prefixAndReplacement
inlineItems (Subscript s)  = Subscript $ replacePrefixes s prefixAndReplacement
inlineItems (SmallCaps s)  = SmallCaps $ replacePrefixes s prefixAndReplacement
inlineItems x = x

inlineBlocks :: Block -> IO Block
inlineBlocks (Plain s) = return $ Plain $ replacePrefixes (map inlineItems s) prefixAndReplacement
inlineBlocks x = return x

main :: IO ()
main = do
  input <- BL.getContents
  filtered <- (walkM inlineBlocks :: Pandoc -> IO Pandoc) $ either error id $ eitherDecode' input
  BL.putStr $ encode filtered


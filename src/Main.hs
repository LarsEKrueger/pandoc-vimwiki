{-# LANGUAGE BangPatterns #-}
module Main
where

import Text.Pandoc.JSON as J
import Text.Pandoc.Walk
import Control.Monad
import Data.Char
import System.IO
import System.IO.Error
import Control.Exception.Base
import qualified Data.ByteString.Lazy as BL
import Data.List

import Text.Pandoc
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

inlineItems :: Inline -> IO Inline
inlineItems (Emph s)    = return $ Emph $ replacePrefixes s prefixAndReplacement
inlineItems (Strong s)  = return $ Strong $ replacePrefixes s prefixAndReplacement
inlineItems (Strikeout s)  = return $ Strikeout $ replacePrefixes s prefixAndReplacement
inlineItems (Superscript s)  = return $ Superscript $ replacePrefixes s prefixAndReplacement
inlineItems (Subscript s)  = return $ Subscript $ replacePrefixes s prefixAndReplacement
inlineItems (SmallCaps s)  = return $ SmallCaps $ replacePrefixes s prefixAndReplacement
inlineItems x = return x

inlineBlocks :: Block -> IO Block
inlineBlocks (Plain s) = do
  ili <- mapM inlineItems s
  return $ Plain $ replacePrefixes ili prefixAndReplacement
inlineBlocks x = return x

isTransclusion :: String -> Bool
isTransclusion s = isPrefixOf "{{" s && isSuffixOf "}}" s

extractLink :: String -> String
extractLink s = drop 2 $ take (length s - 2) s

extensions :: [String]
extensions =
  [ ".md"
  , ".markdown"
  , ".wiki"
  , ".mdwiki"
  ]


tryFirst :: [a] -> (a -> IO (Maybe b)) -> IO (Maybe b)
tryFirst [] _ = return Nothing
tryFirst (a:as) f = do
  mbOk <- f a
  case mbOk of
       Just _ -> return mbOk
       Nothing -> tryFirst as f

justReadFile :: String -> IO (Maybe [Block])
justReadFile fn = bracket (openFile fn ReadMode) hClose $ \handle -> do
  hSetEncoding handle utf8
  !cont <- hGetContents handle
  case readMarkdown def cont of
      Left _ -> return Nothing
      Right (Pandoc _ blocks) -> return $ Just blocks

handleTransclusion :: String -> IO [Block]
handleTransclusion s = do
  let filename = extractLink s
  mbBlocks <- tryFirst extensions $ \ext -> do
    hPutStrLn stderr $ "Trying to load file «" ++ filename ++ ext ++ "»"
    catchIOError (justReadFile $ filename ++ ext) (\_ -> return Nothing)
  case mbBlocks of
       Nothing -> return [Para [Str $ "Error: Could not transclude «" ++ filename ++ "»"]]
       Just blocks -> return blocks

processLinks :: Block -> IO [Block]
processLinks (Para [Str s]) =
  if isTransclusion s
     then handleTransclusion s
     else return [Para [Str s]]
processLinks x = return [x]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = do
  bbs <- mapM f as
  return $ concat bbs

transclude :: Pandoc -> IO Pandoc
transclude (Pandoc m bs) = do
  walked <- concatMapM processLinks bs
  return $ Pandoc m walked

main :: IO ()
main = BL.getContents >>=
  (walkM inlineBlocks :: Pandoc -> IO Pandoc) . either error id . eitherDecode' >>=
  transclude >>=
  BL.putStr . encode


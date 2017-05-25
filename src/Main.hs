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
import Data.IORef
import Data.Map.Lazy as M

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

justReadFile :: IORef Meta -> String -> IO (Maybe [Block])
justReadFile metaRef fn = bracket (openFile fn ReadMode) hClose $ \handle -> do
  hSetEncoding handle utf8
  cont <- hGetContents handle
  case readMarkdown def cont of
      Left _ -> return Nothing
      Right (Pandoc meta blocks) -> do
        oldMeta <- readIORef metaRef
        let newMeta = Meta { unMeta = M.union (unMeta oldMeta) (unMeta meta)}
        writeIORef metaRef newMeta
        return $ Just blocks

handleTransclusion :: IORef Meta -> String -> IO [Block]
handleTransclusion metaRef s = do
  let filename = extractLink s
  mbBlocks <- tryFirst extensions $ \ext -> do
    hPutStrLn stderr $ "Trying to load file «" ++ filename ++ ext ++ "»"
    catchIOError (justReadFile metaRef $ filename ++ ext) (\_ -> return Nothing)
  case mbBlocks of
       Nothing -> return [Para [Str $ "Error: Could not transclude «" ++ filename ++ "»"]]
       Just blocks -> return blocks

processLinks :: IORef Meta -> Block -> IO [Block]
processLinks metaRef (Para [Str s]) =
  if isTransclusion s
     then handleTransclusion metaRef s
     else return [Para [Str s]]
processLinks _ x = return [x]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = do
  bbs <- mapM f as
  return $ concat bbs

transclude :: Pandoc -> IO Pandoc
transclude (Pandoc m bs) = do
  metaRef <- newIORef m
  walked <- concatMapM (processLinks metaRef) bs
  finalMeta <- readIORef metaRef
  return $ Pandoc finalMeta walked

main :: IO ()
main = do
  txt <- BL.getContents
  let input = either error id $ eitherDecode' txt
  transcluded <- transclude input
  filtered <- walkM inlineBlocks transcluded
  BL.putStr $ encode filtered


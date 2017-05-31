{-
Copyright (c) 2017 Lars Krueger

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

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
import Text.Pandoc.Shared
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


data Global = Global
  { gMeta :: Meta
  , gLink :: M.Map String (Int,[Block])
  , gNextLinkId :: Int
  }

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

isTransclusion :: Inline -> Bool
isTransclusion (Str s) = isPrefixOf "{{" s && isSuffixOf "}}" s
isTransclusion _ = False

isLink :: String -> Bool
isLink s = isPrefixOf "[[" s && isSuffixOf "]]" s

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

justReadFile :: IORef Global -> String -> IO (Maybe [Block])
justReadFile metaRef fn = bracket (openFile fn ReadMode) hClose $ \handle -> do
  hSetEncoding handle utf8
  cont <- hGetContents handle
  case readMarkdown def cont of
      Left _ -> return Nothing
      Right (Pandoc meta blocks) -> do
        oldGlobal <- readIORef metaRef
        let newMeta = Meta { unMeta = M.union (unMeta $ gMeta oldGlobal) (unMeta meta)}
        writeIORef metaRef oldGlobal { gMeta = newMeta }
        return $ Just blocks

handleTransclusion :: IORef Global -> String -> IO [Block]
handleTransclusion metaRef s = do
  let filename = extractLink s
  mbBlocks <- tryFirst extensions $ \ext ->
    catchIOError (justReadFile metaRef $ filename ++ ext) (\_ -> return Nothing)
  case mbBlocks of
       Nothing -> return [Para [Str $ "Error: Could not transclude «" ++ filename ++ "»"]]
       Just blocks -> return blocks

handleTransclusionOrOther :: IORef Global -> [Inline] -> IO [Block]
handleTransclusionOrOther metaRef is@[i@(Str s)] =
  if isTransclusion i
     then handleTransclusion metaRef s
     else return [Para is]
handleTransclusionOrOther _ is = return [Para is]

processTransclusions :: IORef Global -> Block -> IO [Block]
processTransclusions metaRef p@(Para inlines) = do
  let groups = groupBy (\a b -> isTransclusion a == isTransclusion b) inlines
  bob <- mapM (handleTransclusionOrOther metaRef) groups
  return $ concat bob
processTransclusions _ x = return [x]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = do
  bbs <- mapM f as
  return $ concat bbs

handleLink :: IORef Global -> String -> IO Inline
handleLink metaRef s = do
  global <- readIORef metaRef
  let (filename,local) = break (== '#') $ extractLink s
      ident = inlineListToIdentifier [Str filename]
      links = gLink global
  unless (M.member filename links) $ do
    mbBlocks <- tryFirst extensions $ \ext ->
      catchIOError (justReadFile metaRef $ filename ++ ext) (\_ -> return Nothing)
    case mbBlocks of
        Nothing -> hPutStrLn stderr $ "Failed to load link file «" ++ filename ++ "»"
        Just blocks -> do
          let oldLinkId = gNextLinkId global
              newLinks = M.insert filename (oldLinkId, Div (ident,[],[]) [] : blocks) links
          writeIORef metaRef global { gLink = newLinks, gNextLinkId = oldLinkId + 1 }
  return $ Link nullAttr [Str $ filename ++ local] ("#" ++ ident, filename ++ local)

processLinks :: IORef Global -> Inline -> IO Inline
processLinks metaRef i@(Str s) =
  if isLink s
     then handleLink metaRef s
     else return i
processLinks _ i = return i

transclude :: Pandoc -> IO Pandoc
transclude (Pandoc m bs) = do
  metaRef <- newIORef Global { gMeta = m, gLink = M.empty, gNextLinkId = 0 }
  transcluded <- concatMapM (processTransclusions metaRef) bs
  linked <- walkM (processLinks metaRef) transcluded
  finalGlobal <- readIORef metaRef
  let sortedFiles = sortOn fst $ M.elems $ gLink finalGlobal
      sortedBlocks = concatMap snd sortedFiles
  return $ Pandoc (gMeta finalGlobal) (linked ++ sortedBlocks)

main :: IO ()
main = do
  txt <- BL.getContents
  let input = either error id $ eitherDecode' txt
  transcluded <- transclude input
  filtered <- walkM inlineBlocks transcluded
  BL.putStr $ encode filtered


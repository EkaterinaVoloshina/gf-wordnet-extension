{-# LANGUAGE OverloadedStrings #-}

module Main where

import PGF2
import qualified Data.Map as Map
import Data.Text(replace, pack, unpack)
import Data.List.Split(splitOn)
import Data.Char (intToDigit, digitToInt, isDigit)
import Data.Maybe
import System.Environment
import Data.List
import System.IO.Unsafe
import System.IO.Error (tryIOError)
import Control.Exception (try, SomeException, evaluate)
import Distribution.Compat.Graph (keysSet)
import Distribution.Simple.Command (OptDescr(BoolOpt))


parseOne :: String -> [[String]]
parseOne blk = [[k, abs, repAll abs k] | k <- keys]
  where keys = parseKey (words (last (lines blk)))
        abs = drop 5 (head (lines blk))
        repAll abs key = unpack (replace (pack key) (pack "MASK") (pack abs))

parseKey :: [String] -> [String]
parseKey (s:x:ss) | (take 3 s == "key") && (isDigit (head x))  = take (digitToInt (head x)) ss
                  | otherwise                                  = []


readExamples :: IO [[String]]
readExamples = do
  examples <- readFile "examples.txt"
  let blks = splitOn "\n\n" examples
  let sentences = concatMap parseOne blks
  return sentences

lin :: Concr -> Expr -> Maybe String
lin lang ex = unsafePerformIO $ do 
  result <- try (evaluate (linearize lang ex)) :: IO (Either SomeException String) 
  case result of
      Left _  -> return Nothing 
      Right s -> return (Just s)

linAll :: Map.Map String Bool -> Concr -> [String] -> IO (String)
linAll wn lang abs = do
  let lins = map (\y -> maybe "" (\x -> "\t" ++ (fromMaybe "?" (lin lang x))) (readExpr y)) abs
  let k = head abs
  return (unwords (k : "\t" : show (fromMaybe True (Map.lookup k wn)) : lins))



readWN :: String -> Map.Map String Bool
readWN wn = Map.fromList (map (parseWN . words) (filter (\x -> isPrefixOf "lin" x) (lines wn)))
    where
      parseWN (x:key:rest) = (key, isSuffixOf "--guessed" (last rest))


main :: IO ()
main = do
  code <- getArgs
  let filecode = "Parse" ++ (head code)
  let wnFile = "../gf-wordnet/WordNet" ++ (head code) ++ ".gf"
  putStrLn filecode
  sentences <- readExamples
  pgf <- readPGF "../gf-wordnet/Parse.pgf"
  wordnet <- readFile wnFile
  let wn = readWN wordnet
  let Just lang = Map.lookup filecode (languages pgf)
  ls <- sequence (map (\x -> linAll wn lang x) sentences)
  writeFile ("examples" ++ (head code) ++ ".tsv") (unlines ls)



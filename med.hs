import Prelude
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import Data.Text as Tx (Text, pack, unpack, take, takeEnd, drop)
import Data.Text.Internal.Search
import Data.List
import Data.Maybe
import Control.Monad

splitchars = " ()[]{};\n"
--getIndexesHelper
gIH :: String -> Char -> [Int]
gIH text delim = indices (Tx.pack $ (:[]) delim) (Tx.pack text)
getIndexes :: String -> String -> [Int]
getIndexes _ "" = []
getIndexes text delims = gIH text (head delims) ++ getIndexes text (tail delims)

--https://stackoverflow.com/questions/48369242/in-haskell-how-can-i-get-a-substring-of-a-text-between-two-indices
substring :: Int -> Int -> Text -> Text
substring start end text = Tx.take (end - start) (Tx.drop start text)

splitByIndexes :: String -> [Int] -> [String]
splitByIndexes _ [] = []
splitByIndexes _ [x] = []
splitByIndexes str lst = Tx.unpack (substring (head lst + 1) (lst !! 1) (Tx.pack str)) : Tx.unpack (substring (lst !! 1) ((lst !! 1) + 1) (Tx.pack str))  : splitByIndexes str (tail lst)

parseCode :: String -> [String]
parseCode "" = []
parseCode str = splitByIndexes str ((-1) : sort (getIndexes str splitchars) ++ [length str])

itToEngDict = Map.fromList[
  ("se", "if"), 
  ("per", "for")]

translateItToEng :: String -> String
translateItToEng x = if Map.member x itToEngDict
    then itToEngDict Map.! x
	else x

main = do
  args <- getArgs
  when (length args /= 1) $ 
      do 
	    putStrLn "Need a file to compile"
	    exitFailure 
  let filename = head args
  contents <- readFile filename
  let output = concatMap translateItToEng $ parseCode contents
  writeFile (filename ++ ".c") output
  --If you reach this point, exit successfully
  putStrLn ("Successfully compiled " ++ filename ++ "!")
  exitSuccess 

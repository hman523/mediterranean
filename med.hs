import Prelude hiding (lookup)
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import Data.Text as Tx (Text, pack, unpack, take, takeEnd)
import Data.Text.Internal.Search
import Data.List
import Data.Maybe

splitchars = " ()[]{};"
--getIndexesHelper
gIH text delim = indices (Tx.pack delim) (Tx.pack text)
getIndexes :: String -> String -> [Int]
getIndexes _ "" = []
getIndexes text delims = (union (gIH text (head delims)) (getIndexes text (tail delims)))

--https://stackoverflow.com/questions/48369242/in-haskell-how-can-i-get-a-substring-of-a-text-between-two-indices
slice :: Int -> Int -> Text -> Text
slice a b text = takeEnd a (Data.Text.take b text)

splitByIndexes :: String -> [Int] -> [String]
splitByIndexes _ ([]) = []
splitByIndexes _ (x:[]) = []
splitByIndexes str lst = (Tx.unpack (slice (lst !! 0) (lst !! 1) str)) ++ splitByIndexes str (tail (tail lst))

parseCode :: String -> [String]
parseCode "" = []
parseCode str = splitByIndexes str ([0] ++ (sort (getIndexes str splitchars)) ++ [(length str)])

itToEngDict = Map.fromList([
  ("se", "if"), 
  ("per", "for")])

translateItToEng :: String -> String
translateItToEng x = if Map.member x itToEngDict
    then itToEngDict Map.! x
	else x

main = do
  args <- getArgs
  if length args /= 1 
    then exitFailure 
	else putStrLn $ "Compiling file " ++ head args
  let filename = head args
  contents <- readFile filename
  let output = join (map translateItToEng (parseCode contents))
  writeFile (filename ++ ".c") output
  --If you reach this point, exit successfully
  exitSuccess 

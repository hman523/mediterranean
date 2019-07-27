import Prelude hiding (lookup)
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import Data.Text as Tx(Text, drop, take, pack, unpack)
import Data.List
import Data.Maybe

containsParens :: String -> Bool
containsParens str = elem '(' str

textBeforeParen :: String -> String
textBeforeParen str = Tx.unpack (Tx.take (fromMaybe 0 (elemIndex '(' str)) (Tx.pack str))

textAfterParen :: String -> String
textAfterParen str = Tx.unpack (Tx.drop (fromMaybe 0 (elemIndex '(' str)) (Tx.pack str))

itToEngDict = Map.fromList([
  ("se", "if"), 
  ("per", "for")])

translateItToEng :: String -> String
translateItToEng x = if containsParens x 
  then (translateItToEng (textBeforeParen x) ++ textAfterParen x)
  else if Map.member x itToEngDict
    then itToEngDict Map.! x
	else x

main = do
  args <- getArgs
  if length args /= 1 
    then exitFailure 
	else putStrLn $ "Compiling file " ++ head args
  let filename = head args
  contents <- readFile filename
  let output = unwords (map translateItToEng (words contents))
  writeFile (filename ++ ".c") output
  --If you reach this point, exit successfully
  exitSuccess 

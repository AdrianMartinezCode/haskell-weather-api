module Config
    ( 
    initConfig,
    findByKey
    ) where
-- https://stackoverflow.com/questions/7867723/haskell-file-reading
-- https://stackoverflow.com/questions/16811376/simulate-global-variable

import Data.IORef
import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO  
import Control.Monad
import Text.Regex
import Text.Regex.Base

{-# NOINLINE funcs #-}
funcs :: IORef (Map.Map String String)
funcs = unsafePerformIO $ newIORef Map.empty

initConfig :: IO ()
initConfig =  getContentFile "resources/config.txt" >>= insertKeyPairValuesIntoConfig

findByKey :: String -> IO (Maybe String)
findByKey k = readIORef funcs >>= \m -> return (Map.lookup k m)


-- IMPURE FUNCTION
insertKeyPairValuesIntoConfig :: [(String, String)] -> IO ()
insertKeyPairValuesIntoConfig [] = return ()
insertKeyPairValuesIntoConfig ((key, value):xs) = 
  atomicModifyIORef funcs (\m -> (Map.insert key value m, ())) >> insertKeyPairValuesIntoConfig xs

getContentFile :: String -> IO [(String, String)]
getContentFile path = readFile path >>= return . transformToKeyPair
  
transformToKeyPair :: String -> [(String, String)]
transformToKeyPair contents = 
  map createTwoTupleFromList (
    map (splitRegex $ makeRegex "=#=") (splitRegex (makeRegex "\n") contents)
  )
  
createTwoTupleFromList :: [String] -> (String, String)
createTwoTupleFromList [] = ("", "")
createTwoTupleFromList s = ( head s, last $ take 2 s )
--getApiKey ::
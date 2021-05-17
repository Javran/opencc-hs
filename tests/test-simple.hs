module Main where

import Text.OpenCC
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString.UTF8 (toString, fromString)

check (input, truth) = do
  output <- convert input
  let ok = (fromString truth == output)
  when (not ok) (liftIO $ putStrLn ("Bad case:" ++ show (input, output, truth)))
  return ok

test :: String -> [(String, String)] -> IO (Maybe Bool)
test cfg pairs = runMaybeT $ withOpenCC cfg $ do
  results <- traverse check pairs
  return $ all (==True) results

assertNothing :: Maybe a -> IO ()
assertNothing Nothing = lastError >>= BS.putStrLn
assertNothing (Just _) = do
  putStrLn "Supposed to fail but it didn't!"
  exitFailure

assertOk :: Maybe Bool -> IO ()
assertOk (Just True) = return ()
assertOk _ = do
  putStrLn "Supposed to succeed but it didn't!"
  exitFailure

main = do
  test "notexist" undefined >>= assertNothing
  test "s2t" pairsS2T >>= assertOk
  test "t2s" pairsT2S >>= assertOk

pairsS2T :: [(String, String)]
pairsS2T = 
  [ ("你好👋", "你好👋")
  , ("海内存知己", "海內存知己")
  ]

pairsT2S = map (\(x,y) -> (y,x)) pairsS2T

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.OpenCC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Maybe

check :: (T.Text, T.Text) -> OpenCCM Bool
check (input, truth) = do
  output <- convert input
  let ok = (truth == output)
  when (not ok) (liftIO $ putStrLn ("Bad case:" ++ show (input, output, truth)))
  return ok

test :: String -> [(T.Text, T.Text)] -> IO (Maybe Bool)
test cfg pairs = runMaybeT $ withOpenCC cfg $ do
  results <- traverse check pairs
  return $ all (==True) results

assertNothing :: Maybe a -> IO ()
assertNothing Nothing = lastError >>= T.putStrLn
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

pairsS2T :: [(T.Text, T.Text)]
pairsS2T = 
  [ ("你好👋", "你好👋")
  , ("海内存知己", "海內存知己")
  ]

pairsT2S = map (\(x,y) -> (y,x)) pairsS2T

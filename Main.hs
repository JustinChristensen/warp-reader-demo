module Main where

import qualified Data.Map as M
import System.Environment (getEnvironment)
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Maybe (fromMaybe)

type Env = M.Map String String
type SimpleReader = ReaderT Env Identity
type IOReader = ReaderT Env IO

readNameFromEnv :: SimpleReader String
readNameFromEnv = do
    name <- asks $ M.lookup "NAME"
    return $ fromMaybe "" name

readAndGreet :: IOReader ()
readAndGreet = do
    name <- asks $ M.lookup "NAME"
    liftIO $ putStrLn $ "Good day, " ++ fromMaybe "" name

getEnvMap :: IO Env
getEnvMap = M.fromList <$> getEnvironment

simpleReaderTest :: IO ()
simpleReaderTest = do
    putStrLn "simpleReaderTest"
    env <- getEnvMap
    let name = runIdentity $ runReaderT readNameFromEnv env
    putStrLn $ "Hello: " ++ name

ioReaderTest :: IO ()
ioReaderTest = do
    putStrLn "ioReaderTest"
    env <- getEnvMap
    runReaderT readAndGreet env

main :: IO ()
main = do
    simpleReaderTest
    ioReaderTest

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import System.Exit (exitFailure)
import System.Environment (getEnvironment, getArgs, getProgName)
import Control.Monad.Reader
import Control.Monad.Identity
import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Data.Aeson (encode, ToJSON, FromJSON)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C

import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (
    Settings,
    defaultSettings,
    setHost,
    setBeforeMainLoop,
    setServerName,
    runSettings)
import Network.HTTP.Types (status200, status404, hContentType)

type Env = M.Map String String
type SimpleReader = ReaderT Env Identity
type IOReader = ReaderT Env IO

readNameFromEnv :: SimpleReader String
readNameFromEnv = do
    n <- asks $ M.lookup "NAME"
    return $ fromMaybe "" n

readAndGreet :: IOReader ()
readAndGreet = do
    n <- asks $ M.lookup "NAME"
    liftIO $ putStrLn $ "Good day, " ++ fromMaybe "" n

getEnvMap :: IO Env
getEnvMap = M.fromList <$> getEnvironment

simpleReaderTest :: IO ()
simpleReaderTest = do
    putStrLn "simpleReaderTest"
    env <- getEnvMap
    let n = runIdentity $ runReaderT readNameFromEnv env
    putStrLn $ "Hello: " ++ n

ioReaderTest :: IO ()
ioReaderTest = do
    putStrLn "ioReaderTest"
    env <- getEnvMap
    runReaderT readAndGreet env

baseSettings :: IO () -> Settings
baseSettings before = setHost "!4" $
                       setServerName "" $
                       setBeforeMainLoop before
                       defaultSettings

helloApp :: Application
helloApp req res = let
        parts = pathInfo req
        greeting str = C.pack $ "Hello " ++ T.unpack str ++ "!\n"
        respond404 = responseLBS status404 [] ""
    in
        res $ fromMaybe respond404 $ uncons parts >>= \(p1, rest1) -> case p1 of 
            "greet" -> uncons rest1 >>= \(n, rest2) -> case rest2 of
                [] -> return $ responseLBS status200 []  $ greeting n
                _ -> Nothing
            _ -> Nothing

runHelloApp :: IO ()
runHelloApp = 
    let preamble = putStrLn "Hello App listening..."
    in runSettings (baseSettings preamble) helloApp

data Person = Person {
        name :: String,
        age :: Maybe Int
    } deriving (Generic, Show)

instance ToJSON Person
instance FromJSON Person

jsonApp :: Application
jsonApp req res = let
        parts = pathInfo req
        respond404 = responseLBS status404 [] ""
        person n a = let
                n' = T.unpack n
                a' = readMaybe (T.unpack a) :: Maybe Int
            in Person n' a'
    in
        res $ fromMaybe respond404 $ uncons parts >>= \(p1, rest1) -> case p1 of 
            "greet" -> uncons rest1 >>= \(n, rest2) -> case rest2 of
                [] -> Nothing
                _ -> uncons rest2 >>= \(a, rest3) -> case rest3 of
                    [] -> let headers = [(hContentType, "application/json")]
                          in return $ responseLBS status200 headers $ encode (person n a)
                    _ -> Nothing
            _ -> Nothing

runJsonApp :: IO ()
runJsonApp = 
    let preamble = putStrLn "JSON App listening..."
    in runSettings (baseSettings preamble) jsonApp

main :: IO ()
main = let
        apps = [
            ("simpleReaderTest", simpleReaderTest),
            ("ioReaderTest", ioReaderTest),
            ("runHelloApp", runHelloApp),
            ("jsonApp", runJsonApp)]
        usage = do
            progName <- getProgName
            putStrLn $ "usage: " ++ progName ++ " [options]"
            putStrLn "-h\tPrint help"
            putStrLn "-N\tRun application #N"
            putStrLn "\nChoices:"
            putStrLn $ unlines $ 
                zipWith (\i a -> show i ++ ". " ++ a) [1..] $ map fst apps
    in do
        args <- getArgs
        if "-h" `elem` args then usage
        else case uncons args of
            Just (s, _) -> do
                let app = pred $ read $ tail s :: Int
                if app `elem` [0..pred $ length apps] then
                    snd $ apps !! app
                else do
                    usage
                    exitFailure
            _ -> do
                usage
                exitFailure
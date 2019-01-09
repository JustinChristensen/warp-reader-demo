{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import System.Exit (exitFailure)
import System.Environment (getEnvironment, getArgs, getProgName)
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C

import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (
    defaultSettings,
    setHost,
    setBeforeMainLoop,
    setServerName,
    runSettings)
import Network.HTTP.Types (status200, status404)

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

helloApp :: Application
helloApp req res = let
        parts = pathInfo req
        greeting str = C.pack $ "Hello " ++ T.unpack str ++ "!\n"
        send404 = res $ responseLBS status404 [] ""
    in
        fromMaybe send404 $ uncons parts >>= \("greet", rest) ->
            uncons rest >>= \(str, []) ->
                return $ res $ responseLBS status200 []  $ greeting str

runHelloApp :: IO ()
runHelloApp = let
            preamble = putStrLn "HelloApp listening..."
            settings =
                setHost "!4" $
                setServerName "" $
                setBeforeMainLoop preamble
                defaultSettings
    in runSettings settings helloApp

main :: IO ()
main = let
        apps = [
            ("simpleReaderTest", simpleReaderTest),
            ("ioReaderTest", ioReaderTest),
            ("runHelloApp", runHelloApp)]
        usage = do
            name <- getProgName
            putStrLn $ "usage: " ++ name ++ " [options]"
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
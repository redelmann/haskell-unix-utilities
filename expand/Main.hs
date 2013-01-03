{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Prelude hiding (catch)
import Options.Applicative
import Control.Error (assertErr, readErr, readMay)
import Control.Monad (unless)
import Control.Monad.Error.Class (Error, strMsg)
import Data.List.Split (splitOn)
import Data.Monoid
import System.IO
import System.IO.Error hiding (catch)
import Control.Exception (catch, handle)
import System.Exit (exitFailure)

data Options = Options {
    tabs  :: [Int],
    files :: [FilePath]
}

readTabs :: Error e => String -> Either e [Int]
readTabs s = do
    ts <- mapM (readErr $ strMsg "Supplied argument is not an integer") $
        splitOn "," s
    assertErr (strMsg "Supplied list of tabs not in order") $ sorted ts
    return $ listToTabs ts

options :: Parser Options
options = Options <$> nullOption (short 't'
                    <> long "tabs"
                    <> reader readTabs
                    <> metavar "tablist"
                    <> help "Specify the tab stops"
                    <> value (listToTabs []))
                  <*> arguments str (metavar "files...")

listToTabs :: [Int] -> [Int]
listToTabs [] = listToTabs [8]
listToTabs [x] = map (* x) [1 .. ]
listToTabs xs = xs ++ map (+ last xs) [1 .. ]

sorted :: Ord a => [a] -> Bool
sorted (x : y : ys) = x < y && sorted (y : ys)
sorted _ = True

expandLine :: [Int] -> String -> String
expandLine = go 0
    where
        go :: Int -> [Int] -> String -> String
        go p css s = let (c : cs) = dropWhile (<= p) css in case s of
            ""          -> ""
            ('\t' : xs) -> replicate (c - p) ' ' ++ go c cs xs
            (x : xs)    -> x : go (p + 1) css xs

mainWithOptions :: Options -> IO ()
mainWithOptions opts = handle (\ e -> errorHandler e >> exitFailure) $
    if null fs then expandHandle stdin else expandFiles
    where
        fs :: [FilePath]
        fs = files opts

        ts :: [Int]
        ts = tabs opts

        expandFiles :: IO ()
        expandFiles = mapM_ expandFile fs

        expandFile :: FilePath -> IO ()
        expandFile f = withFile f ReadMode expandHandle

        expandHandle :: Handle -> IO ()
        expandHandle h = do
                eof <- hIsEOF h
                unless eof $ do
                    line <- hGetLine h
                    putStrLn $ expandLine ts line
                    expandHandle h

        errorHandler :: IOError -> IO ()
        errorHandler e
            | isDoesNotExistError e, Just f <- ioeGetFileName e =
                hPutStrLn stderr $ "The file " ++ f ++ " does not exist."
            | isPermissionError e, Just f <- ioeGetFileName e =
                hPutStrLn stderr $
                "Not enough permission to read file " ++ f ++ "."
            | otherwise = hPutStrLn stderr "Unknown error."

main :: IO ()
main = execParser (info (options <**> helper) mempty) >>= mainWithOptions

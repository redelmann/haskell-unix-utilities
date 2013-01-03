{-# LANGUAGE PatternGuards #-}

module Main(main) where

import Prelude hiding (catch)
import Options.Applicative
import Control.Error (assertErr, readErr, readMay)
import Control.Monad (unless)
import Control.Monad.Error.Class (Error, strMsg)
import Data.List.Split (splitOn)
import Data.Monoid
import System.IO
import System.IO.Error hiding (catch)
import Control.Exception (catch)
import System.Exit (exitFailure)

data Options = Options {
    tabs     :: [Int],
    encoding :: String,
    files    :: [String]
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
                  <*> strOption (short 'e'
                    <> long "enc"
                    <> metavar "encoding"
                    <> help "Specify the encoding"
                    <> value (show localeEncoding))
                  <*> arguments str (metavar "files...")

listToTabs :: [Int] -> [Int]
listToTabs [] = listToTabs [8]
listToTabs [x] = map (*x) [1..] 
listToTabs xs = xs ++ map (+ last xs) [1..]

sorted :: Ord a => [a] -> Bool
sorted (x:y:ys) = x < y && sorted (y:ys) 
sorted _ = True

expandLine :: [Int] -> String -> String
expandLine = go 0
    where
        go :: Int -> [Int] -> String -> String
        go p css s = let (c:cs) = dropWhile (<=p) css in case s of
            ""        -> ""
            ('\t':xs) -> replicate (c - p) ' ' ++ go c cs xs
            (x:xs)    -> x : go (p+1) css xs

mainWithOptions :: Options -> IO ()
mainWithOptions opts = (do
    enc <- mkTextEncoding (encoding opts)
    hSetEncoding stdin enc  -- Dirty trick to fail when the encoding doesn't exist.
    if null fs then expandStdIn enc else expandFiles enc) `catch` 
        (\e -> errorHandler e >> exitFailure)
    where
        fs :: [String]
        fs = files opts 

        ts :: [Int]
        ts = tabs opts

        expandStdIn :: TextEncoding -> IO ()
        expandStdIn enc = expandHandle enc stdin

        expandFiles :: TextEncoding -> IO ()
        expandFiles enc = mapM_ (expandFile enc) fs

        expandFile :: TextEncoding -> String -> IO ()
        expandFile enc f = withFile f ReadMode (expandHandle enc)
        
        expandHandle :: TextEncoding -> Handle -> IO ()
        expandHandle enc h = (hSetEncoding h enc >> go h) `catch` errorHandler
            where
            go :: Handle -> IO ()
            go h = do
                eof <- hIsEOF h
                unless eof $ do 
                    line <- hGetLine h
                    putStrLn $ expandLine ts line
                    go h

        errorHandler :: IOError -> IO ()
        errorHandler e
            | isDoesNotExistError e, Just f <- ioeGetFileName e = 
                hPutStrLn stderr $ "The file " ++ f ++ " does not exist."
            | isPermissionError e, Just f <- ioeGetFileName e = 
                hPutStrLn stderr $ "Not enough permission to read file " ++ f ++ "."
            | otherwise = hPutStrLn stderr "Can not decode with specified encoding."

main :: IO ()
main = execParser (info (options <**> helper) mempty) >>= mainWithOptions
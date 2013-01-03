{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Options.Applicative
import Control.Monad (join, unless)
import Data.Monoid
import System.IO
import System.IO.Error hiding (catch)
import Control.Exception (catch, handle)
import System.Exit (exitWith, ExitCode (..))
import Data.Bits ((.|.))
import Data.IORef

data Options = Options {
    numberAllLines      :: Bool,
    numberNonBlankLines :: Bool,
    files               :: [FilePath]
}

options :: Parser Options
options = Options <$> switch (short 'n'
                    <> help "Number the output lines, starting at 1.")
                  <*> switch (short 'b'
                    <> help "Number the non-blank output lines, starting at 1.")
                  <*> arguments str (metavar "files...")

main :: IO ()
main = join $ mainWithOptions <$> execParser (info (options <**> helper) mempty)
                              <*> newIORef 0

mainWithOptions :: Options -> IORef Int -> IO ()
mainWithOptions opts ref = do
    case files opts of
        [] -> processHandle stdin
        fs -> mapM_ processFile fs
    fmap mkExitCode (readIORef ref) >>= exitWith
    where
        processFile :: FilePath -> IO ()
        processFile f = handle (\ e -> errorHandler e >> registerError 1) $
            withFile f ReadMode processHandle

        addNumber :: Int -> String -> (String, Int)
        addNumber
            | numberNonBlankLines opts = \ n s ->
                if null s then (s, n) else (prependLineNumber n s, n + 1)
            | numberAllLines opts = \ n s ->
                (prependLineNumber n s, n + 1)
            | otherwise = \ n s -> (s, n)

        prependLineNumber :: Int -> String -> String
        prependLineNumber n s = replicate (6 - length r) ' ' ++ r ++ "  " ++ s
            where
                r = show n

        processHandle :: Handle -> IO ()
        processHandle = go 1
            where
                go :: Int -> Handle -> IO ()
                go n h = do
                    eof <- hIsEOF h
                    unless eof $ do
                        (line, n') <- fmap (addNumber n) $ hGetLine h
                        putStrLn line
                        go n' h

        errorHandler :: IOError -> IO ()
        errorHandler e
            | isDoesNotExistError e, Just f <- ioeGetFileName e =
                hPutStrLn stderr $ "The file " ++ f ++ " does not exist."
            | isPermissionError e, Just f <- ioeGetFileName e =
                hPutStrLn stderr $
                "Not enough permission to read file " ++ f ++ "."
            | otherwise = hPutStrLn stderr "Unknown error."

        registerError :: Int -> IO ()
        registerError err = modifyIORef ref (err .|.)

        mkExitCode :: Int -> ExitCode
        mkExitCode 0 = ExitSuccess
        mkExitCode n = ExitFailure n

{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Options.Applicative
import Control.Monad (join, (>=>))
import Data.Monoid
import System.IO
import System.IO.Error hiding (catch)
import Control.Exception (catch, handle)
import System.Exit (exitWith, ExitCode (..))
import Data.Bits ((.|.))
import Data.IORef

data Options = Options {
    files :: [FilePath]
}

options :: Parser Options
options = Options <$> arguments str (metavar "files...")

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

        processHandle :: Handle -> IO ()
        processHandle = hGetContents >=> putStr

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

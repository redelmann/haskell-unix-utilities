{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Options.Applicative
import Control.Monad (join, unless, when)
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
    disableOutputBuff   :: Bool,
    displayAndDollar    :: Bool,
    squeezeEmptyLines   :: Bool,
    files               :: [FilePath]
}

options :: Parser Options
options = Options <$> switch (short 'n'
                    <> help "Number the output lines, starting at 1")
                  <*> switch (short 'b'
                    <> help "Number the non-blank output lines, starting at 1")
                  <*> switch (short 'u'
                    <> help "Disable output buffering")
                  <*> switch (short 'e'
                    <> help ("Display non-printing characters " ++
                             "(see the -v option), " ++
                             "and display a dollar sign (`$') at " ++
                             "the end of each line"))
                  <*> switch (short 's'
                    <> help ("Squeeze multiple adjacent empty lines, " ++
                             "causing the output to be single spaced"))
                  <*> arguments str (metavar "files...")

main :: IO ()
main = join $ mainWithOptions <$> execParser (info (options <**> helper) mempty)
                              <*> newIORef 0

mainWithOptions :: Options -> IORef Int -> IO ()
mainWithOptions opts ref = do
    when (disableOutputBuff opts) $ hSetBuffering stdout NoBuffering
    case files opts of
        [] -> processHandle stdin
        fs -> mapM_ processFile fs
    fmap mkExitCode (readIORef ref) >>= exitWith
    where
        processFile :: FilePath -> IO ()
        processFile f = handle (\ e -> errorHandler e >> registerError 1) $
            withFile f ReadMode processHandle

        addEndDollar :: IO ()
        addEndDollar
            | displayAndDollar opts = putStr "$"
            | otherwise             = return ()

        addNumber :: Int -> Bool -> IO Int
        addNumber
            | numberNonBlankLines opts = \ n e ->
                if e
                    then return n
                    else putStr (showLineNumber n) >> return (n + 1)
            | numberAllLines opts = \ n _ ->
                putStr (showLineNumber n) >> return (n + 1)
            | otherwise = \ n _ -> return n

        showLineNumber :: Int -> String
        showLineNumber n = replicate (6 - length r) ' ' ++ r ++ "  "
            where
                r = show n

        mustIgnore :: Bool -> Bool -> Bool
        mustIgnore
            | squeezeEmptyLines opts = (&&)
            | otherwise              = const $ const False

        processHandle :: Handle -> IO ()
        processHandle = go 1 False
            where
                go :: Int -> Bool -> Handle -> IO ()
                go n e h = do
                    eof <- hIsEOF h
                    unless eof $ do
                        line <- hGetLine h
                        let isEmpty = null line
                        unless (mustIgnore e isEmpty) $ do
                            n' <- addNumber n isEmpty
                            putStr line
                            addEndDollar
                            putStrLn ""
                            go n' isEmpty h
                        go n isEmpty h

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

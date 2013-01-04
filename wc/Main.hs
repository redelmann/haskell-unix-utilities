{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Options.Applicative
import Control.Monad (join, unless, when, void)
import Control.Exception (catch, handle)
import System.IO
import System.IO.Error hiding (catch)
import System.Exit (exitWith, ExitCode (..))
import Data.Monoid
import Data.Bits ((.|.))
import Data.IORef
import Data.Char (isSpace)
import Data.List (foldl')

data Options = Options {
    countLines :: Bool,
    countWords :: Bool,
    countChars :: Bool,
    files      :: [FilePath]
}

options :: Parser Options
options = Options <$> switch (short 'l'
                    <> help "Count the number of lines")
                  <*> switch (short 'w'
                    <> help "Count the number of words")
                  <*> switch (short 'c'
                    <> help "Count the number of characters")
                  <*> arguments str (metavar "files...")

data Infos = Infos {
    lineCount :: !Int,
    wordCount :: !Int,
    charCount :: !Int
}

start :: Infos
start = Infos 0 0 0

infosSums :: [Infos] -> Infos
infosSums = foldl' infoPlus start
    where
        infoPlus :: Infos -> Infos -> Infos
        infoPlus a b = Infos (lineCount a + lineCount b)
                             (wordCount a + wordCount b)
                             (charCount a + charCount b)

mainWithOptions :: Options -> IORef Int -> IO ()
mainWithOptions o ref = do
    case files opts of
        [] -> void $ processHandle Nothing stdin
        fs -> processFiles fs
    fmap mkExitCode (readIORef ref) >>= exitWith
    where
        opts :: Options
        opts = case o of
            Options False False False fs -> Options True True True fs
            x -> x

        processFiles :: [FilePath] -> IO ()
        processFiles fs = do
            total <- infosSums <$> mapM processFile fs
            unless (null $ drop 1 fs) $
                putStrLn $ showInfos (Just "total") total

        processFile :: FilePath -> IO Infos
        processFile f = handle (\ e -> errorHandler e
                                    >> registerError 1
                                    >> return start) $
            withFile f ReadMode (processHandle (Just f))

        processHandle :: Maybe String -> Handle -> IO Infos
        processHandle n h = do
            infos <- countHandle h
            putStrLn $ showInfos n infos
            return infos

        showInfos :: Maybe String -> Infos -> String
        showInfos m i = showField countLines (lineCount i) ++
                        showField countWords (wordCount i) ++
                        showField countChars (charCount i) ++
                        case m of
                            Just s  -> ' ' : s
                            Nothing -> ""

        showField :: (Options -> Bool) -> Int -> String
        showField field
            | field opts = fillRight 8 . show
            | otherwise = const ""

        fillRight :: Int -> String -> String
        fillRight n s = replicate (n - length s) ' ' ++ s

        countHandle :: Handle -> IO Infos
        countHandle h = countAll <$> hGetContents h

        countAll :: String -> Infos
        countAll = fst . foldl' go (start, True)
            where
                go :: (Infos, Bool) -> Char -> (Infos, Bool)
                go (i, _) '\n' = (i { lineCount = lineCount i + 1,
                                      charCount = charCount i + 1}, True)
                go (i, True) c = let s  = isSpace c in
                                 let i' = if s
                                        then i
                                        else i { wordCount = wordCount i + 1 }
                                    in
                                 (i' {charCount = charCount i + 1}, s)
                go (i, False) c = (i {charCount = charCount i + 1}, isSpace c)

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

main :: IO ()
main = join $ mainWithOptions <$> execParser (info (options <**> helper) mempty)
                              <*> newIORef 0

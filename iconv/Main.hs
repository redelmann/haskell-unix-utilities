{-# LANGUAGE PatternGuards #-}

module Main(main) where

import Prelude hiding (mapM_, catch)
import Data.Foldable (mapM_)
import Options.Applicative
import Data.Monoid
import System.IO
import System.IO.Error hiding (catch)
import Control.Exception (handle, catch)
import System.Exit (exitFailure)

data Options = Options {
    fromEnc :: Maybe String,
    toEnc   :: Maybe String,
    inFiles :: [String]
}

options :: Parser Options
options = Options <$> optional (strOption (short 'f' 
                    <> long "from-code" 
                    <> help "Specify the encoding of the input" 
                    <> metavar "encoding"))
                  <*> optional (strOption (short 't' 
                    <> long "to-code" 
                    <> help "Specify the encoding of the output" 
                    <> metavar "encoding"))
                  <*> arguments str (metavar "input files...")

mainWithOptions :: Options -> IO ()
mainWithOptions opts = handle errorHandler $ do
    mapM_ (setEnc stdout) (toEnc opts)
    case inFiles opts of
        [] -> convertHandle stdin
        fs -> do
            mapM_ (setEnc stdin) (fromEnc opts)  -- Dirty trick to fail only once when input encoding doesn't exist.
            mapM_ convertFile fs
    where
        convertHandle :: Handle -> IO ()
        convertHandle h = handle errorHandler $ do
            mapM_ (setEnc h) (fromEnc opts)
            hGetContents h >>= putStr

        convertFile :: String -> IO ()
        convertFile f = withFile f ReadMode convertHandle

        setEnc :: Handle -> String -> IO ()
        setEnc h enc = (mkTextEncoding enc >>= hSetEncoding h) `catch` encodingHandler

        encodingHandler :: IOError -> IO ()
        encodingHandler = const $ hPutStrLn stderr "Unsupported encoding." >> exitFailure

        errorHandler :: IOError -> IO ()
        errorHandler e
            | isDoesNotExistError e, Just f <- ioeGetFileName e = 
                hPutStrLn stderr $ "The file " ++ f ++ " does not exist."
            | isPermissionError e, Just f <- ioeGetFileName e = 
                hPutStrLn stderr $ "Not enough permission to read file " ++ f ++ "."
            | otherwise = hPutStrLn stderr "Encoding error."

main :: IO ()
main = execParser (info (options <**> helper) mempty) >>= mainWithOptions
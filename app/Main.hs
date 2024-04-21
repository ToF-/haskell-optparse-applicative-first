module Main (main) where

import Options.Applicative
import Control.Monad
import System.Environment

data Sample = Sample
    {   hello :: String
    ,   quiet :: Bool
    ,   enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
        <$> strOption
            ( long "hello" <> metavar "TARGET" <> help "Target for the greeting" )
        <*> switch
            ( long "quiet" <> short 'q' <> help "Whether to be quiet" )
        <*> option auto
            ( long "enthusiasm" <> help "how enthusiastically to greet" <> showDefault <> value 1 <> metavar "INT" )


start :: String -> IO ()
start s = putStrLn $ "starting " ++ s ++ " now"

stop :: String -> IO ()
stop s = putStrLn $ "stopping " ++ s ++ " now"

-- command :: String -> ParserInfo a -> Mod CommandFields a
-- Add a command to a subparser option.
-- 
-- Suggested usage for multiple commands is to add them to a single subparser. e.g.
-- 
-- sample :: Parser Sample
-- sample = subparser
--        ( command "hello"
--          (info hello (progDesc "Print greeting"))
--       <> command "goodbye"
--          (info goodbye (progDesc "Say goodbye"))
--        )
-- info :: Parser a -> InfoMod a -> ParserInfo a
-- Create a ParserInfo given a Parser and a modifier.

-- argument :: ReadM a -> Mod ArgumentFields a -> Parser
-- Builder for an argument parser.
--
-- str :: IsString s => ReadM s
-- String Option reader.
--
-- idm :: Monoid m => m
-- Trivial option modifier. (mempty)


opts :: Parser (IO ())
opts = subparser
    ( command "start" (info (start <$> argument str (metavar "PROCESS")) (progDesc "start a process"))
    <> command "stop" (info (stop <$> option str (long "process" <> short 'p')) (progDesc "stop the process")) )


stopCommand :: String -> String
stopCommand s = "stop command (" ++ s ++ ")"

options :: Parser String
options = subparser
    ( command "start" (info (return "start" <$> argument (str::ReadM String) (metavar "PROCESS")) (progDesc "start a process"))
    <> command "stop" (info (stopCommand <$> option (str::ReadM String) (long "process" <> short 'p')) (progDesc "stop the process"))
    <> command "view" (info (pure "return return view") (progDesc "view processes")))

data Command = Accounts | Version deriving (Eq, Show)

commands :: Parser Command
commands = subparser
    ( command "accounts" (info (pure Accounts) (progDesc "give accounts information"))
    <> command "version" (info (pure Version) (progDesc "give version number")))

main :: IO ()
main = do 
        -- join (customExecParser (prefs disambiguate) (info opts idm))
        args <- getArgs
        print $ execParserPure (prefs disambiguate) (info commands idm) args

-- main :: IO ()
-- main = greet =<< execParser opts
--     where
--         opts = info (sample <**> helper)
--             ( fullDesc
--             <> progDesc "Print a greeting for TARGET"
--             <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()


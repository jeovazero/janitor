module CLI (CLI(..),parseCLICmd,helpCLI,helpCLINotCmdError,helpCLINotCmdError) where

data CLI
    = CLIRead
    | CLIDeleteAll
    | CLIVerify
    | CLIHelp
    | CLICmdNoArgsError String
    | CLINotCmdError String
    deriving (Eq, Show)

parseCLICmd :: [String] -> CLI
parseCLICmd [] = CLIHelp 
parseCLICmd (cmd:xs)
    | isCmdNoArgsError = CLICmdNoArgsError (head xs)
    | otherwise                   = cli 
    where
        isCmdNoArgsError = not (cli `elem` [CLIHelp, CLINotCmdError cmd]) && xs /= []
        cli = case cmd of
                "read"      -> CLIRead
                "verify"    -> CLIVerify
                "help"      -> CLIHelp
                "deleteall" -> CLIDeleteAll
                _           -> CLINotCmdError cmd

helpCLINotCmdError arg =
    unlines
        [ "\"janitor " ++ arg ++ "\" accepts no arguments."
        , "See 'janitor help'"
        ]

helpCmdErrorCLI arg =
    unlines
        [ "\"" ++ arg ++ "\" is not a janitor command."
        , "See 'janitor help'"
        ]

helpCLI :: String
helpCLI =
    unlines
        [ "Usage: janitor COMMAND"
        , ""
        , "  First, with a Twitter Dev account you need to export 4 tokens/keys"
        , ""
        , "        export OAUTH_TOKEN=..."
        , "        export OAUTH_SECRET=..."
        , "        export CONSUMER_KEY=..."
        , "        export CONSUMER_SECRET=..."
        , ""
        , "  Commands:"
        , ""
        , "    verify         Verify your credentials"
        , "    read           Show some of yours Tweets"
        , "    deleteall      Delete all of yours Tweets"
        , "    help           Show this help text"
        , ""
        , ""
        , "  Eg:"
        , ""
        , "    janitor verify"
        , ""
        , "Source code: https://github.com/jeovazero/janitor"
        ]
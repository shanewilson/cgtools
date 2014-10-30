import Options.Applicative -- Provided by optparse-applicative

type App = String
type CommitMsg = String

data LogOptions = LogOptions
  { version :: String
--  , from :: String
  }

data Command
  = Log LogOptions
  | Msg CommitMsg

data Options = Options App Command

logOptions :: Parser String
logOptions = strOption (long "version" <> metavar "VERSION" <> help "version help")
--         <*> strOption (long "from" <> metavar "FROM" <> help "from help")


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseLog :: Parser Command
parseLog = Log . LogOptions <$> logOptions
--             <*> strOption (
--               long "version" <>
--               metavar "VERSION" <>
--               help "version help")
--             <*> strOption (
--               long "from" <>
--               metavar "FROM" <>
--               help "from help")

parseMsg :: Parser Command
parseMsg = Msg <$> argument str (metavar "COMMIT_MSG")

parseCommand :: Parser Command
parseCommand = subparser $
    command "log" (parseLog `withInfo` "Generate a changelog from git history") <>
    command "msg" (parseMsg `withInfo` "Validate commit message structure")

parseApp :: Parser App
parseApp = strOption $
    short 'a' <> long "app" <> metavar "COMPILE-APP" <>
    help "Heroku app on which to compile"

parseOptions :: Parser Options
parseOptions = Options
               <$> parseApp
               <*> parseCommand

withOptions :: (Options -> IO ()) -> IO ()
withOptions f = f =<< execParser
    (parseOptions `withInfo` "Interact with the heroku build API")

run :: Options -> IO ()
run (Options app cmd) = do
  case cmd of
   Log s -> putStrLn $ "log"
   Msg msg -> putStrLn $ "msg"

main :: IO ()
main = withOptions run

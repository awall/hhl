module Main where

import Control.Lens
import Data.Maybe(fromJust)
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as WD
import qualified Data.Text as T (Text, pack, unpack)
import Data.Aeson.Lens(key, _String, values)
import Network.Wreq

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Base64 as BS (encode)
import qualified Data.ByteString as BS hiding (pack)

import Network.Connection      (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)

import System.IO (hFlush, stdout)

import qualified Data.ByteString.Lazy as BL

--
-- Types
--
type Team = T.Text
type IsoDate = T.Text
type Url = String

data WLOT = W | L | OT
data Record = Record {
    wins :: Int
  , losses :: Int
  , overtimeWins :: Int
} deriving (Show, Read)

data MultiSeasonRecord = MultiSeasonRecord {
    lastSeason :: Record
  , thisSeason  :: Record
}

data Game = Game {
    when :: IsoDate
  , home :: Team
  , away :: Team
} deriving (Show, Read)

data Injury = Injury {
    player :: T.Text
  , team :: Team
  , description :: T.Text
} deriving (Show, Read)

--
-- Resources
-- 
allTeams :: [Team]
allTeams = [ "COL", "CHI", "CBJ", "STL", "BOS", "MTL", "VAN"
  , "WSH", "ARI", "NJD", "ANA", "CGY", "PHI", "CAR", "NYI", "WPJ"
  , "LAK", "TBL", "TOR", "EDM", "FLO", "PIT", "NSH", "NYR", "DET"
  , "BUF", "OTT", "SJS", "DAL", "MIN", "VGK"]

lastSeasonGamesUrl, thisSeasonGamesUrl, scheduleUrl, injuriesUrl :: Url
scheduleUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2017-2018-regular/full_game_schedule.json"
lastSeasonGamesUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2016-2017-regular/team_gamelogs.json"
thisSeasonGamesUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2017-2018-regular/team_gamelogs.json"
injuriesUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2017-2018-regular/player_injuries.json"

credentialsPath, schedulePath, lastSeasonPath, thisSeasonPath, injuriesPath :: FilePath
credentialsPath = "./data/credentials.b64"
schedulePath = "./data/schedule.dat"
lastSeasonPath = "./data/lastSeason.dat"
thisSeasonPath = "./data/thisSeason.dat"
injuriesPath = "./data/injuries.dat"

--
-- Utils
--
(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) a b = BS.concat [a, b]

--
-- Updating/Retrieving credentials
--
requestCredentials :: IO (BS.ByteString, BS.ByteString)
requestCredentials = do
  putStr "User: " >> hFlush stdout
  user <- getLine
  putStr "Password: " >> hFlush stdout
  pass <- getLine
  return (BS.pack user, BS.pack pass)

writeCredentials :: (BS.ByteString, BS.ByteString) -> IO ()
writeCredentials (user, pass) =
  BS.writeFile credentialsPath encoded
  where encoded = BS.encode (user +++ ":" +++ pass)

getCredentials :: IO BS.ByteString
getCredentials = BS.readFile credentialsPath

--
-- HTTPS requests
--
addAuth :: BS.ByteString -> Options -> Options
addAuth b64 opts =
  opts & header "Authorization" .~ ["Basic " +++ b64]

getWith' :: Options -> Url -> IO (Response (BL.ByteString))
getWith' opts url = do
  -- Ignore the fact that the we don't recognize the Certificate Authority
  -- see https://gist.github.com/j-keck/4f025ea39d6da259c1dc
  let fixedOpts = opts & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  b64 <- getCredentials  
  getWith (addAuth b64 fixedOpts) url

--
-- Extracting Data from MySportsFeeds
--
record :: [WLOT] -> Record
record games =
  foldl joinRecords (Record { wins = 0, losses = 0, overtimeWins = 0 }) games
  where joinRecords (Record ws ls ots) wlo = 
          case wlo of W  -> Record { wins = ws + 1, losses = ls, overtimeWins = ots }
                      L  -> Record { wins = ws, losses = ls + 1, overtimeWins = ots }
                      OT -> Record { wins = ws, losses = ls, overtimeWins = ots + 1 }

requestInjuries :: Url -> IO [Injury]
requestInjuries url = do
  resp <- getWith' defaults url
  let entries = resp ^.. responseBody . key "playerinjuries" . key "playerentry" . values
  return $ map extract entries
  where extract g = Injury { player = g ^. key "player" . key "LastName" . _String
                           , team = g ^. key "team" . key "Abbreviation" . _String
                           , description = g ^. key "injury" . _String }

requestSchedule :: Url -> IO [Game]
requestSchedule url = do  
  resp <- getWith' defaults url
  let entries = resp ^.. responseBody . key "fullgameschedule" . key "gameentry" . values
  return $ map extract entries
  where extract g = Game { when = g ^. key "date" . _String
                         , home = g ^. key "homeTeam" . key "Abbreviation" . _String
                         , away = g ^. key "awayTeam" . key "Abbreviation" . _String }

requestGamesForTeam :: Url -> Team -> IO [WLOT]
requestGamesForTeam url teamid = do
  let opts = defaults & param "team" .~ [teamid]
  resp <- getWith' opts url
  let gamelogs = resp ^.. responseBody . key "teamgamelogs" . key "gamelogs" . values
  return $ map (toGameLog . extract) gamelogs
  where 
    extract g = 
          let stat x = g ^. key "stats" . key x . key "#text" . _String
          in (stat "Wins", stat "Losses")
    toGameLog (ws, ls) = if ws == "1" then W else if ls == "1" then L else OT      

requestRecord :: Url -> Team -> IO Record
requestRecord url t =
  record <$> requestGamesForTeam url t

requestRecords :: Url -> IO [(Team, Record)]
requestRecords url = mapM (\t -> (\x -> (t, record x)) <$> requestGamesForTeam url t) allTeams

getSchedule :: IO [Game]
getSchedule = read <$> readFile schedulePath

getRecords :: FilePath -> IO [(Team, Record)]
getRecords path = read <$> readFile path

getInjuries :: IO [Injury]
getInjuries = read <$> readFile injuriesPath

getMultiSeasonRecords :: IO [(Team, MultiSeasonRecord)]
getMultiSeasonRecords = do
  this <- getRecords thisSeasonPath
  last <- getRecords lastSeasonPath
  return $ map (\(t, r) -> (t, MultiSeasonRecord { thisSeason = r, lastSeason = fromJust (lookup t last)})) this

nextSaturday :: Time.Day -> Time.Day
nextSaturday d =
  case (WD.toWeekDate d) of
    (_, _, 6) -> d
    _ -> nextSaturday (Time.addDays 1 d)  

run :: String -> IO ()
run "updateSchedule" = requestSchedule scheduleUrl >>= writeFile schedulePath . show
run "updateCredentials" = requestCredentials >>= writeCredentials
run "updateLastSeason" = requestRecords lastSeasonGamesUrl >>= writeFile lastSeasonPath . show
run "updateThisSeason" = requestRecords thisSeasonGamesUrl >>= writeFile thisSeasonPath . show
run "updateInjuries" = requestInjuries injuriesUrl >>= writeFile injuriesPath . show
run "show" = do
  now <- Time.getCurrentTime
  let today = Time.utctDay now 
  let sat = nextSaturday today
  let satstr = T.pack (show sat)

  allgames <- getSchedule  
  this <- getRecords thisSeasonPath
  last <- getRecords lastSeasonPath
  injuries <- getInjuries

  let scheduledGames = filter (\g -> when g == satstr) allgames

  let findRecord x t = fromJust (lookup t x)
  let showBothSeasons t = "[" ++ showRecords (findRecord this t) (findRecord last t) ++ "]"
  let showInjuries t = "\n" ++ T.unpack t ++ " Injuries" ++ concatMap (\i -> "\n    " ++ (T.unpack (player i)) ++ ": " ++ (T.unpack (description i))) (filter (\i -> team i == t) injuries)
  let showGame g = showMatchup g ++ " " ++ showBothSeasons (away g) ++ " " ++ showBothSeasons (home g) ++ showInjuries (away g) ++ showInjuries (home g) ++ "\n\n"
  let toPrint = map showGame scheduledGames  
  putStrLn ("Games for: " ++ show sat)
  mapM_ putStrLn toPrint
  
  where showMatchup g = (T.unpack (away g)) ++ " @ " ++ (T.unpack (home g))
        showRecords ts ls = showRecord ts ++ " / " ++ showRecord ls
        showRecord r = show (wins r) ++ "-" ++ show (losses r) ++ "-" ++ show (overtimeWins r)
    

run cmd = putStrLn ("'" ++ cmd ++ "' not understood.")

loop :: IO ()
loop = do
  putStr ">"
  hFlush stdout
  cmd <- getLine
  if cmd == "exit" || cmd == "quit" 
    then putStrLn "Bye."
    else run cmd >> putStrLn "Done." >> loop

main :: IO ()
main = loop  

module Main where

import Control.Lens
import qualified Data.Text as T (pack, unpack)
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
-- Utils
--
(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) a b = BS.concat [a, b]

--
-- Updating/Retrieving credentials
--
credentials :: FilePath
credentials = 
  "./data/credentials.b64"

requestCredentials :: IO (BS.ByteString, BS.ByteString)
requestCredentials = do
  putStr "User: " >> hFlush stdout
  user <- getLine
  putStr "Password: " >> hFlush stdout
  pass <- getLine
  return (BS.pack user, BS.pack pass)

writeCredentials :: (BS.ByteString, BS.ByteString) -> IO ()
writeCredentials (user, pass) =
  BS.writeFile credentials encoded
  where encoded = BS.encode (user +++ ":" +++ pass)

getCredentials :: IO BS.ByteString
getCredentials = BS.readFile credentials

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
-- URLs
--
type Url = String

lastSeasonGamesUrl, scheduleUrl :: Url
scheduleUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2016-2017-regular/full_game_schedule.json"
lastSeasonGamesUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2016-2017-regular/team_gamelogs.json"
currentSeasonGamesUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2017-2018-regular/team_gamelogs.json"

--
-- Extracting Data from MySportsFeeds
--
data HA = H | A
data WLOT = W | L | OT
data GameLog = GameLog {
    team :: String
  , wlot :: WLOT
}

data Record = Record {
    wins :: Int
  , losses :: Int
  , overtimeWins :: Int
} deriving (Show, Read)

data MultiSeasonRecord = MultiSeasonRecord {
    lastSeason :: Record
  , currentSeason  :: Record
} deriving (Show, Read)

record :: [GameLog] -> (String, Record)
record games =
  foldl joinRecords ("", Record { wins = 0, losses = 0, overtimeWins = 0}) games
  where joinRecords (_, (Record ws ls ots))  (GameLog tm wlo) = (tm, 
          case wlo of W  -> Record { wins = ws + 1, losses = ls, overtimeWins = ots }
                      L  -> Record { wins = ws, losses = ls + 1, overtimeWins = ots }
                      OT -> Record { wins = ws, losses = ls, overtimeWins = ots + 1 })

requestGamesForTeam :: Url -> Int -> IO [GameLog]
requestGamesForTeam url teamid = do
  let opts = defaults & param "team" .~ [T.pack $ show teamid]
  resp <- getWith' opts url
  let gamelogs = resp ^.. responseBody . key "teamgamelogs" . key "gamelogs" . values
  return $ map (toGameLog . extract) gamelogs
  where 
    extract g = 
          let source = g ^. key "team" . key "Abbreviation" . _String
              stat x = g ^. key "stats" . key x . key "#text" . _String
          in (source, stat "Wins", stat "Losses")
    toGameLog (tm, ws, ls) =
      GameLog {
          team = T.unpack tm
        , wlot = if ws == "1" then W else if ls == "1" then L else OT
      }

requestRecord :: Url -> Int -> IO (String, Record)
requestRecord url t =
  record <$> requestGamesForTeam url t

requestMultiSeasonRecord :: Int -> IO (String, MultiSeasonRecord)
requestMultiSeasonRecord t = do
  (_, ls) <- requestRecord lastSeasonGamesUrl t
  (tm, cs) <- requestRecord currentSeasonGamesUrl t
  return $ (tm, MultiSeasonRecord {
      lastSeason = ls
    , currentSeason = cs
  })

run :: String -> IO ()
run "updateCredentials" = requestCredentials >>= writeCredentials
run "showRecords" = do
  allRecords <- mapM requestMultiSeasonRecord [1 .. 31]
  mapM_ (putStrLn . showRecord) allRecords  
  where showRecord (t, x) = show (t, x)

run _ = return ()

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

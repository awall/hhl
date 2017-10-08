module Main where

import Control.Lens
import Data.Text as Text (Text, pack, unpack)
import Data.Aeson.Lens(key, _String, values)
import Network.Wreq

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Base64 as BS (encode)
import qualified Data.ByteString as BS hiding (pack)
import qualified Data.ByteString.Lazy as BL

-- Ignore the fact that the we don't recognize the Certificate Authority
-- see https://gist.github.com/j-keck/4f025ea39d6da259c1dc

-- import Network.Connection      (TLSSettings (..))
-- import Network.HTTP.Client.TLS (mkManagerSettings)
-- defaults & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

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

promptCredentials :: IO (BS.ByteString, BS.ByteString)
promptCredentials = do
  putStr "User: "
  user <- getLine
  putStr "Password: "
  pass <- getLine
  return (BS.pack user, BS.pack pass)

writeCredentials :: (BS.ByteString, BS.ByteString) -> IO ()
writeCredentials (user, pass) =
  BS.writeFile credentials encoded
  where encoded = BS.encode (user +++ ":" +++ pass)

getCredentials :: IO BS.ByteString
getCredentials = BS.readFile credentials

updateCredentials :: IO ()
updateCredentials = promptCredentials >>= writeCredentials

--
-- HTTPS requests
--
addAuth :: BS.ByteString -> Options -> Options
addAuth b64 opts =
  opts & header "Authorization" .~ ["Basic " +++ b64]

getWith' :: Options -> Url -> IO (Response (BL.ByteString))
getWith' opts url = do
  b64 <- getCredentials  
  getWith (addAuth b64 opts) url

--
-- URLs
--
type Url = String

oldUrl, gamelogsUrl :: Url
oldUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2016-2017-regular/full_game_schedule.json"
gamelogsUrl = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2016-2017-regular/team_gamelogs.json"

--
-- Extracting Data from MySportsFeeds
--
data HA = H | A deriving (Show, Read)
data WLOT = W | L | OT deriving (Show, Read)
data Game = Game {
    me :: String
  , opp :: String
  , ha :: HA
  , wlot :: WLOT
  , gf :: Int
  , ga :: Int
} deriving (Show, Read)

downloadGamesForTeam :: Int -> IO [Game]
downloadGamesForTeam teamid = do
  let opts = defaults & param "team" .~ [Text.pack $ show teamid]
  resp <- getWith' opts gamelogsUrl
  let gamelogs = resp ^.. responseBody . key "teamgamelogs" . key "gamelogs" . values
  return $ map (toGame . extract) gamelogs
  where 
    extract g = 
          let source = g ^. key "team" . key "City" . _String
              city x = g ^. key "game" . key x . key "City" . _String
              stat x = g ^. key "stats" . key x . key "#text" . _String
          in (source, city "homeTeam", city "awayTeam", stat "Wins", stat "Losses", stat "GoalsFor", stat "GoalsAgainst")
    toGame (tm, home, away, wins, losses, gfor, gagainst) =
      Game {
          me = Text.unpack tm
        , opp = Text.unpack (if home == tm then away else home)
        , ha = if home == tm then H else A
        , wlot = if wins == "1" then W else if losses == "1" then L else OT
        , gf = read (Text.unpack gfor)
        , ga = read (Text.unpack gagainst)
      }
  
downloadAllGames :: IO [Game]
downloadAllGames =
  concat <$> mapM downloadGamesForTeam allTeams
  where allTeams = [1..31]

main :: IO ()
main = do
  resp <- getWith' defaults oldUrl
  return ()

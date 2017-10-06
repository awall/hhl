module Main where

import Control.Lens
import Network.Wreq
import Network.Connection      (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Data.ByteString.Char8   (pack)

opts user pass = 
  defaults 
    & auth ?~ basicAuth (pack user) (pack pass)
    -- Ignore the fact that the we don't recognize the Certificate Authority
    -- see https://gist.github.com/j-keck/4f025ea39d6da259c1dc
    & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

oldURL = "https://api.mysportsfeeds.com/v1.1/pull/nhl/2016-2017-regular/full_game_schedule.json"

run = do 
  putStr "User: "
  user <- getLine
  putStr "Password: "
  pass <- getLine
  let o = opts user pass
  r <- getWith o oldURL
  return r

main :: IO ()
main = do
  run
  return ()

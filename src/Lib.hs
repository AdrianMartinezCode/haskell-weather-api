module Lib
    ( 
    getWeather
    ) where

{-# LANGUAGE OverloadedStrings #-}
--import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString as S (ByteString, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import Network.HTTP.Client
import Data.Aeson
import Config
import Control.Exception

--completeUrl :: String
--completeUrl = protocol ++ "://" ++ weatherHost ++ apiPath ++ queryParams

completeUrl :: IO (Maybe String)
completeUrl = sequence [
    findByKey "apiToken",
    findByKey "protocol",
    findByKey "weatherHost",
    findByKey "apiPath",
    findByKey "location"
  ] >>= sequence >>= \l -> return (Just $ buildUrl l)
  
--    case Nothing = error Exception("Any param not found.")
    
--handleMaybeVars :: Maybe 

buildUrl :: [String] -> String
buildUrl (apiToken:protocol:host:path:location:_) = protocol ++ "://" ++ host ++ path ++ "?q=" ++ location ++ "&APPID=" ++ apiToken
buildUrl _ = error "Incorrect number of params"

strToBS :: String -> S.ByteString
strToBS = C8.pack

bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack


-- https://stackoverflow.com/questions/33983629/basic-way-of-sending-http-post-in-haskell-using-http-conduit

buildRequest :: IO Request
buildRequest = do
  nakedRequest <- parseRequest completeUrl
  return (nakedRequest { method = strToBS "POST" })
  
send :: Request -> IO ()
send req = do
  manager <- newManager defaultManagerSettings
  response <- httpLbs req manager
  let Just obj = decode (responseBody response)
  print (obj :: Object)
  
  
getWeather :: IO ()
getWeather = do
  req <- buildRequest
  send req

-- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md

--getWeather :: IO ()
--getWeather = do
--  initReq <- parseUrl completeUrl
--  let req = initReq
--      { method = "POST"
--      }
--
--getWeather :: IO ()
--getWeather = do
--  response <- httpLBS "http://httpbin.org/get"
--  putStrLn $ "The status code was: " ++
--                 show (getResponseStatusCode response)
--      print $ getResponseHeader "Content-Type" response
--      L8.putStrLn $ getResponseBody response
--getWeather = do
--               rsp <- simpleHTTP (getRequest completeUrl)
--               fmap (take 100) (getResponseBody rsp)

--getWeather = do
--  (_, rsp)
--     <- Network.Browser.browse $ do
--           setAllowRedirects True -- handle HTTP redirects
--           request $ getRequest "http://www.haskell.org/"
--  return (take 100 (rspBody rsp))



  


--   do
--     rsp <- Network.HTTP.simpleHTTP (getRequest "http://www.haskell.org/")
--             -- fetch document and return it (as a 'String'.)
--     fmap (take 100) (getResponseBody rsp)
--

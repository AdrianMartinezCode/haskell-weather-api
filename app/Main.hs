{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Experimental.Auth (AuthProtect)
import Auth.Middleware (jwtAuthMiddleware, jwtAuthContext)
import Api.User (UserAPI, userServer)
import Database.Schema (migrateAll)
import Database.Connection (runMigrations)

type API = (UserAPI :<|> AuthProtect "jwt-auth" :> NoteAPI)

apiProxy :: Proxy API
apiProxy = Proxy

server :: Server API
server = userServer --:<|> noteServer

app :: Application
app = serveWithContext apiProxy jwtAuthContext server

main :: IO ()
main = do
  runMigrations migrateAll
  putStrLn "Server running on port 8080"
  run 8080 (jwtAuthMiddleware app)
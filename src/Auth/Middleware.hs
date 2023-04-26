{-# LANGUAGE OverloadedStrings #-}

module Auth.Middleware
  ( jwtAuthMiddleware
  ) where

import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types.Status (unauthorized401)
import qualified Data.Text.Encoding as T
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant (throwError, err401, addHeader)
import Models.User (User(..))
import Auth.Jwt (verifyToken)

type instance Servant.Auth.Server.AuthTypes.Auth (AuthProtect "jwt-auth") = User

-- Middleware to check for JWT token in the Authorization header
jwtAuthMiddleware :: Middleware
jwtAuthMiddleware = logStdout

-- AuthHandler to verify the JWT token and extract the user data
authHandler :: AuthHandler Request User
authHandler = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) >>= extractToken of
      Nothing -> throwError err401 { errBody = "Missing or invalid JWT token" }
      Just token -> case verifyToken (T.decodeUtf8 token) of
        Nothing -> throwError err401 { errBody = "Invalid JWT token" }
        Just user -> return user

    extractToken headerValue = case T.splitOn " " headerValue of
      ["Bearer", token] -> Just (T.encodeUtf8 token)
      _ -> Nothing

-- Combines the jwtAuthMiddleware and authHandler
-- To be used in the Servant API context
jwtAuthContext :: Context (AuthHandler Request User ': '[])
jwtAuthContext = authHandler :. EmptyContext
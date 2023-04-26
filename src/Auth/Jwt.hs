{-# LANGUAGE OverloadedStrings #-}

module Auth.Jwt
  ( generateToken
  , verifyToken
  ) where

import Web.JWT
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
--import Models.User (User(..))
import qualified Models.User as User
import qualified Data.Map as Map
import Data.Aeson (Value (Object, String), FromJSON (..), ToJSON (..), withObject, (.=), (.:), withObject, decode, fromJSON)
--import Data.Aeson (ToJSON(..), FromJSON(..))
--import Web.JWT (encodeSigned, hmacSecret, JWTClaimsSet(..), JWTHeader(..), JOSEHeader(..), Algorithm(..))
--let secretKey = secret "your_secret_key"

secretKey :: Text
secretKey = "SECRET_KEY"

-- Generate a JWT token for the authenticated user
generateToken :: User.User -> IO Text
generateToken user = do
  currentTime <- getPOSIXTime
  let payload = Map.fromList
        [ ("user_id", toJSON $ User.id user)
        , ("username", toJSON $ User.username user)
        , ("email", toJSON $ User.email user)
        , ("iat", toJSON currentTime)
        ]
--  let jwt = encodeSigned HS256 (hmacSecret secretKey) (jwtClaimsSet payload)
--  let jwt = encodeSigned HS256 (hmacSecret secretKey) (JWTClaimsSet {iss = Nothing, sub = Nothing, aud = Nothing, exp = Nothing, nbf = Nothing, iat = Nothing, jti = Nothing, unregisteredClaims = payload})
  let jwt = encodeSigned (hmacSecret secretKey) (JWTClaimsSet {iss = Nothing, sub = Nothing, aud = Nothing, exp = Nothing, nbf = Nothing, iat = Nothing, jti = Nothing, unregisteredClaims = payload})
  return jwt

-- Verify a JWT token and extract the user data if valid
verifyToken :: Text -> Maybe User.User
verifyToken token =
  case decodeAndVerifySignature (hmacSecret secretKey) token of
    Nothing -> Nothing
    Just verifiedJWT -> do
--      let claims = claimsMap verifiedJWT
      let claims = (JWTClaimsSet {iss = Nothing, sub = Nothing, aud = Nothing, exp = Nothing, nbf = Nothing, iat = Nothing, jti = Nothing, unregisteredClaims = verifiedJWT})
      userId <- Map.lookup "user_id" claims >>= fromJSON
      uname <- Map.lookup "username" claims >>= fromJSON
      uemail <- Map.lookup "email" claims >>= fromJSON
      return User.User { id = userId, username = uname, email = uemail }
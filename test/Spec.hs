{-# LANGUAGE OverloadedStrings #-}

import           Application
import           Control.Monad.IO.Class
import           Data.ByteString.Char8
import           Data.DateTime
import qualified Data.Map               as M
import           Data.Maybe
import           Database
import           Database.Redis         hiding (get)
import           Environment            (loadRedisConnectInfo)
import           GoogleDrive.Types
import           Snap.Core
import           Snap.Snaplet
import           Snap.Test
import           System.Environment
import           Test.Hspec

getSnap :: IO (Snap ())
getSnap = do
  (_, s, _) <- runSnaplet Nothing appInit
  return s

setupRedis :: IO Connection
setupRedis = do
  setEnv "REDIS_URL" "redis://h:supersecret@localhost:6379"
  info <- loadRedisConnectInfo
  conn <- connect info { connectDatabase = 5 }
  runRedis conn flushdb
  return conn

io :: MonadIO m => IO a -> m a
io = liftIO

main :: IO ()
main = hspec $ do
  describe "castmin bot server" $ do

    it "serves index route correctly" $ do
      resp <- getSnap >>= runHandler (get "/" M.empty)
      assertSuccess resp
      assertBodyContains "Visit /google-drive/sign-in to link google drive with slack" resp

    it "serves static assets correctly" $ do
      resp <- getSnap >>= runHandler (get "/google96f859d812b7c0c5.html" M.empty)
      assertSuccess resp
      assertBodyContains "google96f859d812b7c0c5.html" resp

    it "redirects on sign in handler" $ do
      resp <- getSnap >>= runHandler (get "/google-drive/sign-in" M.empty)
      assertRedirect resp
      let redirectAddress = fromMaybe "" $ getHeader "location" resp
      redirectAddress `shouldSatisfy` isInfixOf "https://accounts.google.com/o/oauth2/v2/auth"

    it "refuses requests to internal routes without internal secret" $ do
      resp <- getSnap >>= runHandler (get "/google-drive/check-files" M.empty)
      rspStatus resp `shouldBe` 401
      assertBodyContains "unauthorized" resp

    it "rejects notifications requests not from slack" $ do
      resp <- getSnap >>= runHandler (postUrlEncoded "/google-drive/notifications" M.empty)
      rspStatus resp `shouldBe` 401
      assertBodyContains "unauthorized" resp

    it "switches notifications on with correct payload and verification token" $ do
      token <- pack <$> getEnv "SLACK_VERIFICATION_TOKEN"
      let payload = M.fromList [ ("token", [token])
                               , ("text", ["on"])
                               ]
      resp <- getSnap >>= runHandler (postUrlEncoded "/google-drive/notifications" payload)
      assertSuccess resp
      assertBodyContains "notifications switched on" resp

    it "switches notifications off with correct payload and verification token" $ do
      token <- pack <$> getEnv "SLACK_VERIFICATION_TOKEN"
      let payload = M.fromList [ ("token", [token])
                               , ("text", ["off"])
                               ]
      resp <- getSnap >>= runHandler (postUrlEncoded "/google-drive/notifications" payload)
      assertSuccess resp
      assertBodyContains "notifications switched off" resp

    it "replies with correct message if unknown payload is sent" $ do
      token <- pack <$> getEnv "SLACK_VERIFICATION_TOKEN"
      let payload = M.fromList [ ("token", [token])
                               , ("text", ["aaksdjhaskjdhaskjd"])
                               ]
      resp <- getSnap >>= runHandler (postUrlEncoded "/google-drive/notifications" payload)
      assertSuccess resp
      assertBodyContains "Sorry I didn't quite get that" resp

  describe "database module" $ do

    it "parses the connection info correctly from an env var" $ do
      setEnv "REDIS_URL" "redis://h:supersecret@myhost:48921"
      info <- loadRedisConnectInfo
      connectPort info `shouldBe` PortNumber 48921
      connectHost info `shouldBe` "myhost"
      connectAuth info `shouldBe` Just "supersecret"
      unsetEnv "REDIS_URL"

    it "sets and retrieves a token correctly" $ do
      conn <- setupRedis
      runRedis conn $ do
        status <- setRefreshToken (RefreshToken "abc123")
        io $ status `shouldBe` Right Ok
        res    <- getRefreshToken
        io $ res `shouldBe` Right (Just $ RefreshToken "abc123")

    it "sets and retrieves the last_checked time correctly" $ do
      conn <- setupRedis
      now  <- getCurrentTime
      runRedis conn $ do
        status <- setLastChecked now
        io $ status `shouldBe` Right Ok
        let fallbackTime = startOfTime
        (Right (Just retrieved)) <- getLastChecked fallbackTime
        io $ diffSeconds now retrieved `shouldBe` 0

    it "sets and retrieves the polling status correctly" $ do
      conn <- setupRedis
      runRedis conn $ do
        status <- setPolling True
        io $ status `shouldBe` Right Ok
        res    <- getPolling
        io $ res `shouldBe` Right (Just True)

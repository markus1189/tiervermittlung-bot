#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell
#!nix-shell --pure
#!nix-shell --keep TELEGRAM_BOT_TOKEN
#!nix-shell --keep TELEGRAM_CHAT_ID
#!nix-shell shell.nix

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.TokenBucket (TokenBucket, newTokenBucket, tokenBucketWait)
import           Control.Lens
  ( filtered,
    filteredBy,
    only,
    preview,
    to,
    toListOf,
    universe,
    view,
    _Just,
  )
import           Control.Lens.Operators ((<&>), (^.), (^..), (^?))
import           Control.Lens.TH (makeClassy)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable (for_)
import           Data.Maybe (isJust, mapMaybe, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import           Data.Text.Lazy.Encoding (decodeLatin1)
import           Data.Time (Day, defaultTimeLocale, getZonedTime, localDay, parseTimeM, zonedTimeToLocalTime)
import           Data.Traversable (for)
import           Network.Wreq (responseBody)
import qualified Network.Wreq as Wreq
import           System.Environment (getEnv)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Taggy.Lens
  ( Element,
    HasContent (contents),
    HasElement (element),
    allNamed,
    attr,
    html,
    named,
  )
import System.Log.Logger (saveGlobalLogger, getRootLogger, Priority (..), setLevel, setHandlers, logM)
import Data.Text.Encoding (decodeUtf8)
import Control.Retry (limitRetries, fullJitterBackoff, recovering)
import Network.HTTP.Client
    ( HttpException(HttpExceptionRequest),
      HttpExceptionContent(StatusCodeException),
      responseStatus )
import Network.HTTP.Types.Status (Status(statusCode))
import System.Log.Handler.Simple (streamHandler)
import System.IO (stderr)
import System.Log.Handler (setFormatter)
import System.Log.Formatter (simpleLogFormatter)
import qualified Network.Wreq.Session as Sess

uri :: String
uri = "https://www.tiervermittlung.de/cgi-bin/haustier/db.cgi?db=hunde5&uid=default&ID=&Tierart=Hund&Rasse=&Groesse=&Geschlecht=weiblich&Alter-gt=3&Alter-lt=15.1&Zeitwert=Monate&Titel=&Name=&Staat=&Land=&PLZ=&PLZ-gt=&PLZ-lt=&Ort=&Grund=&Halter=&Notfall=&Chiffre=&keyword=&Date=&referer=&Nachricht=&E1=&E2=&E3=&E4=&E5=&E6=&E7=&E8=&E9=&E10=&mh=150&sb=0&so=descend&ww=&searchinput=&layout=&session=kNWVQkHlAVH5axV0HJs5&Bild=&video_only=&String_Rasse=&view_records=Suchen"

newtype Token = Token Text deriving (Show, Eq, Ord)

newtype ChatId = ChatId String deriving (Show, Eq, Ord)

data MyEnv = MyEnv { _envToken :: Token
                   , _envChatId :: ChatId
                   , _envTelegramBucket :: TokenBucket
                   , _envTelegramSession :: Sess.Session
                   , _envTiervermittlungSession :: Sess.Session
                   }
makeClassy ''MyEnv

data Entry = Entry {entryDay :: Day, entryLink :: Text} deriving (Show, Eq, Ord)

data Details = Details { detailsUri :: Text
                       , detailsTitle :: Maybe Text
                       , detailsPics :: [Text]
                       , detailsVideos :: [Video]
                       , detailsRace :: Maybe Text
                       } deriving (Show, Eq, Ord)

data Video = YoutubeVideo Text | DirectVideo Text deriving (Show, Eq, Ord)

telegramSendMessage :: (MonadIO m, MonadReader e m, HasMyEnv e, MonadMask m) => ChatId -> Text -> m ()
telegramSendMessage (ChatId chatId) text = withRetry . withToken $ do
  s <- view envTelegramSession
  theUri <- buildTelegramUri "sendMessage"
  liftIO $ void $ Sess.post s theUri
            (toJSON (object ["chat_id" .= chatId, "text" .= text, "disable_notification" .= True]))

telegramSendMediaGroup :: (MonadIO m, MonadReader e m, HasMyEnv e, MonadMask m) => [Wreq.Part] -> m ()
telegramSendMediaGroup parts = withRetry . withToken $ do
  s <- view envTelegramSession
  theUri <- buildTelegramUri "sendMediaGroup"
  liftIO $ void $ Sess.post s theUri parts

buildTelegramUri :: (MonadReader e m, HasMyEnv e) => String -> m String
buildTelegramUri op = do
  Token token <- view envToken
  pure $ "https://api.telegram.org/bot" <> Text.unpack token <> "/" <> op

sendPics :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Details -> m ()
sendPics (Details dUri mTitle pics _ _) = withSystemTempDirectory "tiervermittlung-photos" $ \tmpDir -> do
  ChatId chatId <- view envChatId
  liftIO . logM "dogbot.telegram.sendPics" INFO . Text.unpack $ "Sending pictures for " <> dUri
  picParts <- for pics $ \pic -> do
    let name = mediaName pic
        fp = tmpDir </> Text.unpack name
    dl <- downloadImage (Text.unpack pic)
    _ <- liftIO $ BS.writeFile (tmpDir </> Text.unpack name) dl
    pure $ Wreq.partFile name fp
  let mediaJson = toJSON $ map ((\n -> object ([ "type" .= ("photo" :: String)
                                               , "media" .= ("attach://" <> n)
                                               ] ++ map ("caption" .=) (maybeToList mTitle))) . mediaName) pics
      parts = [ Wreq.partString "chat_id" chatId
              , Wreq.partText "disable_notification" "true"
              , Wreq.partLBS "media" (Aeson.encode mediaJson)
              ] ++ picParts
  liftIO $ do
    logM "dogbot.telegram.pics" DEBUG (Text.unpack (decodeUtf8 (BS.toStrict (Aeson.encode mediaJson))))
    logM "dogbot.telegram.pics" DEBUG ("Number of parts: " <> show (length parts))
  telegramSendMediaGroup parts

sendVideos :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Details -> m ()
sendVideos (Details dUri mTitle _ videos _) = do
  ChatId chatId <- view envChatId
  let filteredVideos = filter (\case YoutubeVideo _ -> False
                                     DirectVideo _ -> True) videos
  unless (null filteredVideos) $ do
    withSystemTempDirectory "tiervermittlung-videos" $ \tmpDir -> do
      liftIO . logM "dogbot.telegram.sendVideos" INFO . Text.unpack $ "Sending videos for " <> dUri <> "\n"
      let videoNames = concatMap (\case YoutubeVideo _ -> []
                                        DirectVideo u -> [mediaName u]) videos
      videoParts <- for filteredVideos $ \video -> do
        case video of
          YoutubeVideo videoUri -> do
            let name = mediaName videoUri
            pure $ Wreq.partText name videoUri
          DirectVideo videoUri -> do
            let name = mediaName videoUri
                fp = tmpDir </> Text.unpack name
            dl <- downloadImage (Text.unpack videoUri)
            _ <- liftIO $ BS.writeFile (tmpDir </> Text.unpack name) dl
            pure $ Wreq.partFile name fp
      let mediaJson = toJSON $ map ((\n -> object ([ "type" .= ("video" :: String)
                                                   , "media" .= ("attach://" <> n)
                                                   ] ++ map ("caption" .=) (maybeToList mTitle))) . mediaName) videoNames
          parts = [ Wreq.partString "chat_id" chatId
                  , Wreq.partText "disable_notification" "true"
                  , Wreq.partLBS "media" (Aeson.encode mediaJson)
                  ] ++ videoParts
      liftIO $ logM "dogbot.telegram.videos" DEBUG (Text.unpack (decodeUtf8 (BS.toStrict (Aeson.encode mediaJson))))
      telegramSendMediaGroup parts

sendLink d = do
  chatId <- view envChatId
  liftIO . logM "dogbot.telegram.sendLink" INFO . Text.unpack $ "Sending link for " <> detailsUri d <> "\n"
  telegramSendMessage chatId (maybe "" (<> ": ") (detailsTitle d) <> detailsUri d)

main = runBot

runBot :: IO ()
runBot = do
  logger <- getRootLogger
  h <- flip setFormatter (simpleLogFormatter "[$time] $prio [$loggername] $msg") <$> streamHandler stderr DEBUG
  saveGlobalLogger (setLevel DEBUG $ setHandlers [h] logger)
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  chatId <- getEnv "TELEGRAM_CHAT_ID"
  bucket <- newTokenBucket
  s1 <- Sess.newAPISession
  s2 <- Sess.newAPISession
  let theEnv = MyEnv (Token (Text.pack token)) (ChatId chatId) bucket s1 s2
  runReaderT loadAndProcessEntries theEnv

loadAndProcessEntries :: (MonadReader e m, MonadIO m, MonadMask m, HasMyEnv e) => m ()
loadAndProcessEntries = do
  yesterday <- liftIO getYesterday
  chatId <- view envChatId
  telegramSendMessage chatId $ "Hunde vom " <> Text.pack (show yesterday)
  es <- filter (\(Entry eDay _) -> eDay == yesterday) <$> loadEntries
  liftIO . logM "dogbot" INFO $ "Processing started for day=" <> show yesterday <> " with " <> show (length es) <> " entries"
  for_ es processEntry

processEntry :: forall m e. (MonadIO m, MonadCatch m, MonadMask m, MonadReader e m, HasMyEnv e) => Entry -> m ()
processEntry (Entry _ eLink) = do
  r <- try @m @SomeException $ do
    liftIO . logM "dogbot" INFO . Text.unpack $ "Processing link: " <> eLink
    d <- loadDetails eLink
    void $ try @m @SomeException $ do
      sendPics d
      unless (null (detailsVideos d)) $ sendVideos d
    sendLink d
  case r of
    Left e -> liftIO . logM "dogbot" ERROR $ Text.unpack ("Failed to process entry with link: " <> eLink <> "\n") <> show e
    Right () -> liftIO . logM "dogbot" NOTICE . Text.unpack $ "Finished processing entry: " <> eLink

getYesterday :: IO Day
getYesterday = pred . localDay . zonedTimeToLocalTime <$> getZonedTime

extractDate :: Element -> Maybe Text
extractDate e = e ^? to universe . traverse . filteredBy (matchId "Datum_Results") . contents

extractLink :: Element -> Maybe Text
extractLink e = e ^? to universe . traverse . filteredBy (matchId "Titel_Results") . to universe . traverse . named (only "a") . attr "href" . _Just

parseDate :: Text -> Maybe Day
parseDate = parseTimeM False defaultTimeLocale "%-d.%-m.%Y" . Text.unpack

loadEntries :: (MonadIO m, MonadMask m, MonadReader e m, HasMyEnv e) => m [Entry]
loadEntries = do
  s <- view envTiervermittlungSession
  liftIO $ logM "dogbot" DEBUG "Loading entries"
  r <- withRetry (liftIO $ Sess.get s uri)
  let rBody = decodeLatin1 $ r ^. responseBody
  pure (extractEntries rBody)

extractEntries body =
  mapMaybe (\e -> Entry <$> (extractDate e >>= parseDate) <*> extractLink e) $
  body ^.. html . to universe . traverse . element . filtered (\n -> isJust (n ^? matchId "Item_Results"))

loadDetails :: (MonadIO m, MonadMask m, MonadReader e m, HasMyEnv e) => Text -> m Details
loadDetails detailUri = do
  s <- view envTiervermittlungSession
  body <- withRetry (liftIO $ Sess.get s (Text.unpack detailUri) <&> view responseBody <&> decodeLatin1)
  pure $ extractDetails detailUri body

extractDetails :: Text -> Lazy.Text -> Details
extractDetails detailUri body = Details detailUri (extractTitle body) (extractPics body) videos race
  where
    videos = extractYoutubeVideos body ++ extractEmbeddedVideos body
    race = extractRace body

-- ghci, (read via bytestring -> decodeLatin1 -> fromStrict)
-- lt ^.. html . to universe . traverse . Text.Taggy.Lens.element . filteredBy (matchClass "table_tr_daten_item") . Text.Taggy.Lens.children . traverse

extractRace :: Lazy.Text -> Maybe Text
extractRace = const $ Just ""

extractTitle :: Lazy.Text -> Maybe Text
extractTitle = fmap Text.strip . preview (html . to universe . traverse . element . filteredBy (matchClass "Daten_Item_H1") . contents)

extractPics :: Lazy.Text -> [Text]
extractPics = toListOf (html . to universe . traverse . element . filteredBy (attr "class" . _Just . only "img_pic_items") . attr "src" . _Just)

extractYoutubeVideos :: Lazy.Text -> [Video]
extractYoutubeVideos = map YoutubeVideo . toListOf (html . to universe . traverse . element . filteredBy (matchId "video") . allNamed (only "iframe") . attr "src" . _Just . filtered ("youtube.com" `Text.isInfixOf`))

extractEmbeddedVideos :: Lazy.Text -> [Video]
extractEmbeddedVideos = map DirectVideo . toListOf (html . to universe . traverse . element . named (only "video") . attr "src" . _Just)

matchAttr attrName given = attr attrName . _Just . only given

matchId :: Applicative f => Text -> (() -> f ()) -> Element -> f Element
matchId = matchAttr "id"

matchClass :: Applicative f => Text -> (() -> f ()) -> Element -> f Element
matchClass = matchAttr "class"

mediaName :: Text -> Text
mediaName = Text.takeWhileEnd (/= '/')

downloadImage :: forall m e. (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => String -> m BS.ByteString
downloadImage theUri = do
  liftIO $ logM "dogbot.download" DEBUG $ "Downloading: " <> theUri
  s <- view envTiervermittlungSession
  withRetry (liftIO $ Sess.get s theUri <&> view responseBody)

withRetry action =
  recovering (fullJitterBackoff (round @Double 2e6) <> limitRetries 10) [const $ Handler retryStatusException] $ const action
  where
    retryStatusException :: MonadIO m => HttpException -> m Bool
    retryStatusException (HttpExceptionRequest _ (StatusCodeException r _)) = do
      let s = statusCode (responseStatus r)
          shouldRetry = s `elem` codesToRetry || s >= 500
      liftIO $ if shouldRetry
        then logM "dogbot.retry.http" DEBUG $ "Retrying after status " <> show s
        else logM "dogbot.retry.http" DEBUG $ "NOT retrying after status " <> show s
      pure shouldRetry
    retryStatusException _ = pure False
    codesToRetry = [ 429 ]


withToken :: (MonadIO m, MonadReader e m, HasMyEnv e) => m a -> m a
withToken action = do
  bucket <- view envTelegramBucket
  liftIO $ tokenBucketWait bucket burstSize inverseRate
  action
  where burstSize = 1
        inverseRate = round @Double 5e6

-- Tests

runTests :: IO ()
runTests = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Parse search results" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/search.html"
        let entries = extractEntries input
        length entries @?= 150

    , testCase "Parse entry without video" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/no-video.html"
        let theUri = "some-uri"
            details = extractDetails theUri input
            pics = detailsPics details
        assertDetails theUri (Just "Taya") 5 0 details
        detailsRace details @?= Just "foo"
        pics @?= [ "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551.pic"
                 , "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-1.pic"
                 , "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-2.pic"
                 , "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-3.pic"
                 , "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-4.pic"
                 ]

    , testCase "Parse entry with direct video" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/video-direct.html"
        let theUri = "some-uri"
            details = extractDetails theUri input
        assertDetails theUri (Just "Carolina") 3 1 details
        let video = head (detailsVideos details)
        case video of
          YoutubeVideo _ -> assertFailure "Not a direct video"
          DirectVideo u -> u @?= "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492553/video/1492553.mp4"

    , testCase "Parse entry with youtube video" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/video-youtube.html"
        let theUri = "some-uri"
            details = extractDetails theUri input
        assertDetails theUri (Just "Tilike ist sehr lieb und aktiv !") 5 1 details
        let video = head (detailsVideos details)
        case video of
          YoutubeVideo u -> u @?= "https://www.youtube.com/embed/OsJmeXTq-uA?hl=de&fs=1&autoplay=0&rel=0"
          DirectVideo _ -> assertFailure "Not a youtube video"

    , testCase "Extract picture base name" $ do
        mediaName "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-2.pic" @?= "j1492551-2.pic"
    ]

assertDetails theUri title numPics numVideos details = do
        detailsUri details @?= theUri
        detailsTitle details @?= title
        length (detailsPics details) @?= numPics
        length (detailsVideos details) @?= numVideos

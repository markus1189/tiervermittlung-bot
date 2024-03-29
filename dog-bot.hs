#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell
#!nix-shell --pure
#!nix-shell --keep TELEGRAM_BOT_TOKEN
#!nix-shell --keep TELEGRAM_CHAT_ID
#!nix-shell shell.nix

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent.TokenBucket (TokenBucket, newTokenBucket, tokenBucketWait)
import Control.Lens
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
import Control.Lens.Operators ((<&>), (^.), (^..), (^?))
import Control.Lens.TH (makeClassy)
import Control.Logging (LogLevel (..), setLogTimeFormat, withStderrLogging)
import Control.Logging qualified as Logging
import Control.Monad.Catch
  ( Handler (Handler),
    MonadCatch,
    MonadMask,
    SomeException,
    try,
  )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader
  ( MonadReader,
    ReaderT (runReaderT),
  )
import Control.Retry (RetryAction (ConsultPolicy, ConsultPolicyOverrideDelay, DontRetry), fullJitterBackoff, limitRetries, recoveringDynamic)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (for_)
import Data.List (find, partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Encoding (decodeLatin1)
import Data.Time (Day, defaultTimeLocale, getZonedTime, localDay, parseTimeM, zonedTimeToLocalTime)
import Data.Traversable (for)
import Network.HTTP.Client
  ( HttpException (HttpExceptionRequest),
    HttpExceptionContent (StatusCodeException),
    responseHeaders,
    responseStatus,
  )
import Network.HTTP.Types.Status (Status (statusCode))
import Network.Wreq (responseBody)
import Network.Wreq qualified as Wreq
import Network.Wreq.Session qualified as Sess
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Text.Read (readMaybe)
import Text.Taggy.Lens
  ( Element,
    HasContent (contents),
    HasElement (element),
    allNamed,
    attr,
    html,
    named,
  )
import Text.Taggy.Lens qualified as Taggy
import Control.Monad (unless)
import Control.Monad (void)

uri :: String
uri = "https://www.tiervermittlung.de/cgi-bin/haustier/db.cgi?db=hunde5&uid=default&ID=&Tierart=Hund&Rasse=&Groesse=&Geschlecht=weiblich&Alter-gt=3&Alter-lt=15.1&Zeitwert=Monate&Titel=&Name=&Staat=&Land=&PLZ=&PLZ-gt=&PLZ-lt=&Ort=&Grund=&Halter=&Notfall=&Chiffre=&keyword=&Date=&referer=&Nachricht=&E1=&E2=&E3=&E4=&E5=&E6=&E7=&E8=&E9=&E10=&mh=150&sb=0&so=descend&ww=&searchinput=&layout=&session=kNWVQkHlAVH5axV0HJs5&Bild=&video_only=&String_Rasse=&view_records=Suchen"

newtype Token = Token Text deriving (Show, Eq, Ord)

newtype ChatId = ChatId String deriving (Show, Eq, Ord)

data MyEnv = MyEnv
  { _envToken :: Token,
    _envChatId :: ChatId,
    _envTelegramBucket :: TokenBucket,
    _envTelegramSession :: Sess.Session,
    _envTiervermittlungSession :: Sess.Session
  }

makeClassy ''MyEnv

data Entry = Entry {entryDay :: Day, entryLink :: Text} deriving (Show, Eq, Ord)

data Details = Details
  { detailsUri :: Text,
    detailsTitle :: Maybe Text,
    detailsPics :: [Text],
    detailsVideos :: [Video],
    detailsProfile :: Map Text Text,
    detailsAttributes :: Maybe Text
  }
  deriving (Show, Eq, Ord)

detailsRace :: Details -> Maybe Text
detailsRace = Map.lookup "Rasse" . detailsProfile

detailsAge :: Details -> Maybe Text
detailsAge = Map.lookup "Alter" . detailsProfile

data Video = YoutubeVideo Text | DirectVideo Text deriving (Show, Eq, Ord)

telegramSendMessage :: (MonadIO m, MonadReader e m, HasMyEnv e, MonadMask m) => ChatId -> Text -> m ()
telegramSendMessage (ChatId chatId) text = withRetry . withToken $ do
  s <- view envTelegramSession
  theUri <- buildTelegramUri "sendMessage"
  liftIO $
    void $
      Sess.post
        s
        theUri
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
sendPics d =
  unless (null (detailsPics d)) $
    withSystemTempDirectory "tiervermittlung-photos" $ \tmpDir -> do
      ChatId chatId <- view envChatId
      liftIO . Logging.loggingLogger LevelInfo "dogbot.telegram.sendPics" . Text.unpack $ "Sending pictures for " <> detailsUri d
      picParts <- for (detailsPics d) $ \pic -> do
        let name = mediaName pic
            fp = tmpDir </> Text.unpack name
        dl <- downloadImage (Text.unpack pic)
        _ <- liftIO $ BS.writeFile (tmpDir </> Text.unpack name) dl
        pure $ Wreq.partFile name fp
      let mediaJson =
            toJSON $
              map
                ( ( \n ->
                      object
                        ( [ "type" .= ("photo" :: String),
                            "media" .= ("attach://" <> n)
                          ]
                            ++ map ("caption" .=) (maybeToList (detailsTitle d))
                        )
                  )
                    . mediaName
                )
                (detailsPics d)
          parts =
            [ Wreq.partString "chat_id" chatId,
              Wreq.partText "disable_notification" "true",
              Wreq.partLBS "media" (Aeson.encode mediaJson)
            ]
              ++ picParts
      liftIO $ do
        Logging.loggingLogger LevelDebug "dogbot.telegram.pics" (Text.unpack (decodeUtf8 (BS.toStrict (Aeson.encode mediaJson))))
        Logging.loggingLogger LevelDebug "dogbot.telegram.pics" ("Number of parts: " <> show (length parts))
      telegramSendMediaGroup parts

sendVideos :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Details -> m ()
sendVideos d = do
  ChatId chatId <- view envChatId
  let (directVideos, youtubeVideos) =
        partition
          ( \case
              YoutubeVideo _ -> False
              DirectVideo _ -> True
          )
          (detailsVideos d)
  for_ youtubeVideos $ \case
    YoutubeVideo theUri -> do
      telegramSendMessage (ChatId chatId) (maybe "" (<> ": ") (detailsTitle d) <> theUri)
    DirectVideo _ -> pure ()

  unless (null directVideos) $ do
    withSystemTempDirectory "tiervermittlung-videos" $ \tmpDir -> do
      liftIO . Logging.loggingLogger LevelInfo "dogbot.telegram.sendVideos" . Text.unpack $ "Sending videos for " <> detailsUri d <> "\n"
      let videoNames =
            concatMap
              ( \case
                  YoutubeVideo _ -> []
                  DirectVideo u -> [mediaName u]
              )
              (detailsVideos d)
      videoParts <- for directVideos $ \video -> do
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
      let mediaJson =
            toJSON $
              map
                ( ( \n ->
                      object
                        ( [ "type" .= ("video" :: String),
                            "media" .= ("attach://" <> n)
                          ]
                            ++ map ("caption" .=) (maybeToList (detailsTitle d))
                        )
                  )
                    . mediaName
                )
                videoNames
          parts =
            [ Wreq.partString "chat_id" chatId,
              Wreq.partText "disable_notification" "true",
              Wreq.partLBS "media" (Aeson.encode mediaJson)
            ]
              ++ videoParts
      liftIO $ Logging.loggingLogger LevelDebug "dogbot.telegram.videos" (Text.unpack (decodeUtf8 (BS.toStrict (Aeson.encode mediaJson))))
      telegramSendMediaGroup parts

checkDetail :: Details -> Bool
checkDetail d = maybe True (not . (\t -> any (`Text.isInfixOf` t) forbiddenKeywords) . Text.toLower) $ detailsRace d
  where
    forbiddenKeywords = ["englisch setter", "bracke", "brake", "malteser", "dackel"]

sendLink :: (MonadReader e m, MonadIO m, HasMyEnv e, MonadMask m) => Details -> m ()
sendLink d = do
  liftIO . Logging.loggingLogger LevelInfo "dogbot.telegram.sendLink" . Text.unpack $ "Sending link for " <> detailsUri d <> "\n"
  chatId <- view envChatId
  telegramSendMessage chatId (maybe "" (<> ": ") (detailsTitle d) <> detailsUri d)

main :: IO ()
main = runBot

runBot :: IO ()
runBot = runStack loadAndProcessEntries

runStack :: ReaderT MyEnv IO a -> IO a
runStack act = withStderrLogging $ do
  setLogTimeFormat "%c"
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  chatId <- getEnv "TELEGRAM_CHAT_ID"
  bucket <- newTokenBucket
  s1 <- Sess.newAPISession
  s2 <- Sess.newAPISession
  let theEnv = MyEnv (Token (Text.pack token)) (ChatId chatId) bucket s1 s2
  runReaderT act theEnv

loadAndProcessEntries :: (MonadReader e m, MonadIO m, MonadMask m, HasMyEnv e) => m ()
loadAndProcessEntries = do
  yesterday <- liftIO getYesterday
  chatId <- view envChatId
  telegramSendMessage chatId $ "Hunde vom " <> Text.pack (show yesterday)
  es <- filter (\(Entry eDay _) -> eDay == yesterday) <$> loadEntries
  liftIO . Logging.loggingLogger LevelInfo "dogbot" $ "Processing started for day=" <> show yesterday <> " with " <> show (length es) <> " entries"
  for_ ([1 ..] `zip` es) $ uncurry processEntry

processEntry :: forall m e. (MonadIO m, MonadCatch m, MonadMask m, MonadReader e m, HasMyEnv e) => Int -> Entry -> m ()
processEntry i (Entry _ eLink) = do
  r <- try @m @SomeException $ do
    liftIO . Logging.loggingLogger LevelInfo "dogbot" $ "Processing link " <> show i <> ": " <> Text.unpack eLink
    d <- loadDetailsAndExtract eLink
    if checkDetail d
      then do
        void $ try @m @SomeException $ do
          liftIO . Logging.loggingLogger LevelInfo "dogbot" $ "Loaded detail: " <> show d
          sendPics d
          unless (null (detailsVideos d)) $ sendVideos d
        sendLink d
      else liftIO . Logging.loggingLogger LevelDebug "dogbot" $ "Skipping details: " <> show d
  case r of
    Left e -> liftIO . Logging.loggingLogger LevelError "dogbot" $ Text.unpack ("Failed to process entry with link: " <> eLink <> "\n") <> show e
    Right () -> liftIO . Logging.loggingLogger LevelInfo "dogbot" . Text.unpack $ "Finished processing entry: " <> eLink

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
  liftIO $ Logging.loggingLogger @Text LevelDebug "dogbot" "Loading entries"
  r <- withRetry (liftIO $ Sess.get s uri)
  let rBody = decodeLatin1 $ r ^. responseBody
  pure (extractEntries rBody)

extractEntries body =
  mapMaybe (\e -> Entry <$> (extractDate e >>= parseDate) <*> extractLink e) $
    body ^.. html . to universe . traverse . element . filtered (\n -> isJust (n ^? matchId "Item_Results"))

loadDetails detailUri = do
  s <- view envTiervermittlungSession
  withRetry (liftIO $ Sess.get s (Text.unpack detailUri) <&> view responseBody <&> decodeLatin1)

loadDetailsAndExtract :: (MonadIO m, MonadMask m, MonadReader e m, HasMyEnv e) => Text -> m Details
loadDetailsAndExtract detailUri = do
  s <- view envTiervermittlungSession
  body <- withRetry (liftIO $ Sess.get s (Text.unpack detailUri) <&> view responseBody <&> decodeLatin1)
  pure $ extractDetails detailUri body

extractDetails :: Text -> Lazy.Text -> Details
extractDetails detailUri body = Details detailUri (extractTitle body) (extractPics body) videos profile attributes
  where
    videos = extractYoutubeVideos body ++ extractEmbeddedVideos body
    profile = extractProfileAttrs body
    attributes = extractAttributes body

extractProfileAttrs = Map.fromList . mapMaybe (\row -> (,) <$> getRowKey row <*> (getRowValue1 row <|> getRowValue2 row)) . getRows
  where
    getRows = toListOf (html . to universe . traverse . Text.Taggy.Lens.element . filteredBy (matchClass "table_tr_daten_item"))
    getRowKey = fmap (Text.dropWhileEnd (== ':')) . preview (to universe . traverse . filteredBy (matchClass "table_td_daten_item_1") . Taggy.children . traverse . Taggy.contents)
    getRowValue1 = fmap (Text.dropWhileEnd (== ':')) . preview (to universe . traverse . filteredBy (matchClass "table_td_daten_item_2") . Taggy.children . traverse . Taggy.contents)
    getRowValue2 = fmap (Text.dropWhileEnd (== ':')) . preview (to universe . traverse . filteredBy (matchClass "table_td_daten_item_2") . Taggy.contents)

extractAttributes = fmap Text.strip . preview (html . to universe . traverse . element . filteredBy (matchId "Eigenschaften_Item") . contents)

extractTitle :: Lazy.Text -> Maybe Text
extractTitle = fmap Text.strip . preview (html . to universe . traverse . element . filteredBy (matchClass "Daten_Item_H1") . contents)

extractPics :: Lazy.Text -> [Text]
extractPics = toListOf (html . to universe . traverse . element . filteredBy (attr "class" . _Just . only "img_pic_items") . attr "src" . _Just)

extractYoutubeVideos :: Lazy.Text -> [Video]
extractYoutubeVideos = map YoutubeVideo . toListOf (html . to universe . traverse . element . filteredBy (matchId "video") . allNamed (only "iframe") . attr "src" . _Just . filtered ("youtube.com" `Text.isInfixOf`))

extractEmbeddedVideos :: Lazy.Text -> [Video]
extractEmbeddedVideos = map DirectVideo . toListOf (html . to universe . traverse . element . named (only "video") . attr "src" . _Just)

matchAttr :: (Applicative f) => Text -> Text -> (() -> f ()) -> Element -> f Element
matchAttr attrName given = attr attrName . _Just . only given

matchId :: (Applicative f) => Text -> (() -> f ()) -> Element -> f Element
matchId = matchAttr "id"

matchClass :: (Applicative f) => Text -> (() -> f ()) -> Element -> f Element
matchClass = matchAttr "class"

mediaName :: Text -> Text
mediaName = Text.takeWhileEnd (/= '/')

downloadImage :: forall m e. (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => String -> m BS.ByteString
downloadImage theUri = do
  liftIO $ Logging.loggingLogger LevelDebug "dogbot.download" $ "Downloading: " <> theUri
  s <- view envTiervermittlungSession
  withRetry (liftIO $ Sess.get s theUri <&> view responseBody)

withRetry :: (MonadIO m, MonadMask m) => m a -> m a
withRetry action =
  recoveringDynamic (fullJitterBackoff (round @Double 1e6) <> limitRetries 5) [const $ Handler retryStatusException] $ const action

retryStatusException :: (MonadIO m) => HttpException -> m RetryAction
retryStatusException (HttpExceptionRequest _ (StatusCodeException r rBody)) = do
  let s = statusCode (responseStatus r)
      body = decodeUtf8 rBody
      retryAfterFromHeader = (>>= readMaybe @Int) $ fmap (Text.unpack . decodeUtf8 . snd) $ find (\(k, _) -> k == "retry-after") $ responseHeaders r
      consultPolicy = maybe ConsultPolicy (\delay -> ConsultPolicyOverrideDelay (delay * 1000 * 1000)) retryAfterFromHeader
      description = body ^? key "description" . _String
      shouldRetry = s `elem` codesToRetry || s >= 500 || maybe False (`elem` descriptionsToRetry) description
  liftIO $ Logging.loggingLogger LevelDebug "dogbot.retry.http" $ "Response body: " <> body
  liftIO $ Logging.loggingLogger LevelDebug "dogbot.retry.http" $ "Response headers: " <> show (responseHeaders r)
  if shouldRetry
    then do
      liftIO $ Logging.loggingLogger LevelDebug "dogbot.retry.http" $ "Retrying after status " <> show s <> " with policy " <> show consultPolicy
      pure consultPolicy
    else do
      liftIO $ Logging.loggingLogger LevelDebug "dogbot.retry.http" $ "NOT retrying after status " <> show s
      pure DontRetry
  where
    codesToRetry =
      [ 429
      ]
    descriptionsToRetry =
      [ "Bad Request: group send failed"
      ]
retryStatusException e = do
  liftIO $ Logging.loggingLogger LevelDebug "dogbot.retry.http" $ "Not retrying unknown exception: " <> show e
  pure DontRetry

withToken :: (MonadIO m, MonadReader e m, HasMyEnv e) => m a -> m a
withToken action = do
  bucket <- view envTelegramBucket
  liftIO $ tokenBucketWait bucket burstSize inverseRate
  action
  where
    burstSize = 1
    inverseRate = round @Double 5e6

-- Tests

runTests :: IO ()
runTests = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Parse search results" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/search.html"
        let entries = extractEntries input
        length entries @?= 150,
      testCase "Parse entry without video" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/no-video.html"
        let theUri = "some-uri"
            details = extractDetails theUri input
            pics = detailsPics details
        assertDetails theUri (Just "Taya") 5 0 details
        detailsRace details @?= Just "Mischling  (Mischling)"
        pics
          @?= [ "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551.pic",
                "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-1.pic",
                "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-2.pic",
                "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-3.pic",
                "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-4.pic"
              ],
      testCase "Parse entry with direct video" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/video-direct.html"
        let theUri = "some-uri"
            details = extractDetails theUri input
        assertDetails theUri (Just "Carolina") 3 1 details
        detailsRace details @?= Just "Mischling  (Mischling)"
        let video = head (detailsVideos details)
        case video of
          YoutubeVideo _ -> assertFailure "Not a direct video"
          DirectVideo u -> u @?= "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492553/video/1492553.mp4",
      testCase "Parse entry with youtube video" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/video-youtube.html"
        let theUri = "some-uri"
            details = extractDetails theUri input
        assertDetails theUri (Just "Tilike ist sehr lieb und aktiv !") 5 1 details
        detailsRace details @?= Just "Mischling  (Mischling)"
        let video = head (detailsVideos details)
        case video of
          YoutubeVideo u -> u @?= "https://www.youtube.com/embed/OsJmeXTq-uA?hl=de&fs=1&autoplay=0&rel=0"
          DirectVideo _ -> assertFailure "Not a youtube video",
      testCase "Extract picture base name" $ do
        mediaName "https://www.tiervermittlung.de/cgi-bin/haustier/items/1492551/pics/j1492551-2.pic" @?= "j1492551-2.pic",
      testCase "Allow details without race" $ do
        let d = Details "some-uri" Nothing [] [] Map.empty Nothing
        checkDetail d @?= True,
      testCase "Filter out some details based on race" $ do
        checkDetail (Details "some-uri" Nothing [] [] (Map.fromList [("Rasse", "Mischling Bracke")]) Nothing) @?= False
        checkDetail (Details "some-uri" Nothing [] [] (Map.fromList [("Rasse", "Ein dackel-hund")]) Nothing) @?= False
    ]

assertDetails :: Text -> Maybe Text -> Int -> Int -> Details -> IO ()
assertDetails theUri title numPics numVideos details = do
  detailsUri details @?= theUri
  detailsTitle details @?= title
  length (detailsPics details) @?= numPics
  length (detailsVideos details) @?= numVideos
  length (detailsProfile details) @?= 6

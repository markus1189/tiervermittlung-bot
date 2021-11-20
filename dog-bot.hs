#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell
#!nix-shell --pure
#!nix-shell shell.nix

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

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
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe (isJust, mapMaybe, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import           Data.Text.Lazy.Encoding (decodeLatin1)
import           Data.Time (Day, defaultTimeLocale, getZonedTime, localDay, parseTimeM, zonedTimeToLocalTime)
import           Network.Wreq (responseBody)
import qualified Network.Wreq as Wreq
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
import Data.Traversable (for)
import System.FilePath ((</>))
import Data.Foldable (for_)
import System.Environment (getEnv)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Catch

uri :: String
uri = "https://www.tiervermittlung.de/cgi-bin/haustier/db.cgi?db=hunde5&uid=default&ID=&Tierart=Hund&Rasse=&Groesse=&Geschlecht=weiblich&Alter-gt=3&Alter-lt=15.1&Zeitwert=Monate&Titel=&Name=&Staat=&Land=&PLZ=&PLZ-gt=&PLZ-lt=&Ort=&Grund=&Halter=&Notfall=&Chiffre=&keyword=&Date=&referer=&Nachricht=&E1=&E2=&E3=&E4=&E5=&E6=&E7=&E8=&E9=&E10=&mh=150&sb=0&so=descend&ww=&searchinput=&layout=&session=kNWVQkHlAVH5axV0HJs5&Bild=&video_only=&String_Rasse=&view_records=Suchen"

newtype Token = Token Text deriving (Show, Eq, Ord)

data Entry = Entry {entryDay :: Day, entryLink :: Text} deriving (Show, Eq, Ord)

data Details = Details {detailsUri :: Text, detailsTitle :: Maybe Text, detailsPics :: [Text], detailsVideos :: [Video]} deriving (Show, Eq, Ord)

data Video = YoutubeVideo Text | DirectVideo Text deriving (Show, Eq, Ord)

myTelegramChatId = "299952716"

telegramSendMessage :: (MonadIO m, MonadReader Token m) => Text -> Text -> m ()
telegramSendMessage chatId text = do
  Token token <- ask
  liftIO $ void $ Wreq.post ("https://api.telegram.org/bot/" <> Text.unpack token <> "/sendMessage")
            (toJSON (object ["chat_id" .= chatId, "text" .= text]))

telegramSendMediaGroup :: (MonadIO m, MonadReader Token m) => [Wreq.Part] -> m ()
telegramSendMediaGroup parts = do
  Token token <- ask
  liftIO $ void $ Wreq.post ("https://api.telegram.org/bot" <> Text.unpack token <> "/sendMediaGroup") parts

sendPics :: (MonadMask m, MonadIO m, MonadReader Token m) => Details -> m ()
sendPics (Details _ mTitle pics _) = withSystemTempDirectory "tiervermittlung-photos" $ \tmpDir -> do
  picParts <- for pics $ \pic -> do
    let name = mediaName pic
        fp = tmpDir </> Text.unpack name
    dl <- downloadImage (Text.unpack pic)
    _ <- liftIO $ BS.writeFile (tmpDir </> Text.unpack name) dl
    pure $ Wreq.partFile name fp
  let mediaJson = toJSON $ map ((\n -> object ([ "type" .= ("photo" :: String)
                                               , "media" .= ("attach://" <> n)
                                               ] ++ map ("caption" .=) (maybeToList mTitle))) . mediaName) pics
      parts = [ Wreq.partText "chat_id" myTelegramChatId
              , Wreq.partLBS "media" (Aeson.encode mediaJson)
              ] ++ picParts
  telegramSendMediaGroup parts

sendVideos :: (MonadMask m, MonadIO m, MonadReader Token m) => Details -> m ()
sendVideos (Details _ mTitle _ videos) =
  withSystemTempDirectory "tiervermittlung-videos" $ \tmpDir -> do
    let videoNames = concatMap (\case YoutubeVideo _ -> []
                                      DirectVideo u -> [mediaName u]) videos
    let filteredVideos = filter (\case YoutubeVideo _ -> False
                                       DirectVideo _ -> True) videos
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
        parts = [ Wreq.partText "chat_id" myTelegramChatId
                , Wreq.partLBS "media" (Aeson.encode mediaJson)
                ] ++ videoParts
    telegramSendMediaGroup parts

main :: IO ()
main = do
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  yesterday <- getYesterday
  es <- filter (\(Entry eDay _) -> eDay == yesterday) <$> loadEntries
  flip runReaderT (Token (Text.pack token)) $ for_ es $ \(Entry _ eLink) -> do
    liftIO $ print eLink
    d <- loadDetails eLink
    sendPics d
    unless (null (detailsVideos d)) $ sendVideos d
    telegramSendMessage myTelegramChatId (maybe "" (<> ": ") (detailsTitle d) <> detailsUri d)

getYesterday :: IO Day
getYesterday = pred . localDay . zonedTimeToLocalTime <$> getZonedTime

extractDate :: Element -> Maybe Text
extractDate e = e ^? to universe . traverse . filteredBy (matchId "Datum_Results") . contents

extractLink :: Element -> Maybe Text
extractLink e = e ^? to universe . traverse . filteredBy (matchId "Titel_Results") . to universe . traverse . named (only "a") . attr "href" . _Just

parseDate :: Text -> Maybe Day
parseDate = parseTimeM False defaultTimeLocale "%-d.%-m.%Y" . Text.unpack

loadEntries :: IO [Entry]
loadEntries = do
  r <- Wreq.get uri
  let rBody = decodeLatin1 $ r ^. responseBody
  pure (extractEntries rBody)

extractEntries body =
  mapMaybe (\e -> Entry <$> (extractDate e >>= parseDate) <*> extractLink e) $
  body ^.. html . to universe . traverse . element . filtered (\n -> isJust (n ^? matchId "Item_Results"))

loadDetails :: MonadIO m => Text -> m Details
loadDetails detailUri = do
  body <- liftIO $ Wreq.get (Text.unpack detailUri) <&> view responseBody <&> decodeLatin1
  pure $ extractDetails detailUri body

extractDetails :: Text -> Lazy.Text -> Details
extractDetails detailUri body = Details detailUri (extractTitle body) (extractPics body) videos
  where
    videos = extractYoutubeVideos body ++ extractEmbeddedVideos body

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

downloadImage :: MonadIO m => String -> m BS.ByteString
downloadImage theUri = liftIO $ Wreq.get theUri <&> view responseBody

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

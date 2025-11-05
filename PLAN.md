# Dog Bot Improvement Plan

**Generated:** 2025-11-05
**Source:** Multi-agent code review (4 specialized agents)
**Total Improvements:** 20 suggestions across code quality, error handling, performance, and testing

## Executive Summary

This plan outlines improvements to `dog-bot.hs` identified by four specialized review agents:
- **Code Quality Agent**: Structure, maintainability, design patterns
- **Error Handling Agent**: Resilience, exception management, defensive programming
- **Performance Agent**: Optimization, resource usage, concurrency
- **Testing/DevX Agent**: Test coverage, developer experience, configuration

Improvements are organized into 3 phases by effort/gain ratio, with Phase 1 delivering maximum value for minimum time investment.

---

## Phase 1: Quick Wins (1-2 days)

### 1.1 Extract Breed Filter to Top-Level Constant
**Effort:** 15 minutes | **Gain:** High | **Priority:** P0

**Current State:** (line 253)
```haskell
-- Inside checkDetail function's where clause
forbiddenKeywords = ["englisch setter", "bracke", "brake", "malteser", "dackel"]
```

**Implementation:**
```haskell
-- Add at module top-level (after imports, around line 100)
-- | Breed keywords to filter out from notifications.
-- These breeds are excluded based on user preferences.
-- Matching is case-insensitive substring search against the "Rasse" field.
forbiddenBreedKeywords :: [Text]
forbiddenBreedKeywords =
  [ "englisch setter"
  , "bracke"
  , "brake"  -- alternate spelling
  , "malteser"
  , "dackel"
  ]
```

**Changes Required:**
- Move `forbiddenKeywords` from line 253 to top-level constant
- Rename to `forbiddenBreedKeywords` for clarity
- Update reference in `checkDetail` function
- Add Haddock documentation

**Code Reference:** dog-bot.hs:250-253

---

### 1.2 Add CLI Argument Parsing for Test/Bot Mode
**Effort:** 1 hour | **Gain:** Medium-High | **Priority:** P0

**Current State:** (line 262)
Must manually edit `main = runBot` to `main = runTests` to run tests.

**Implementation:**
1. Add `optparse-applicative` to flake.nix dependencies (line 30-48):
```nix
# In haskellPackages.ghcWithPackages section
optparse-applicative
```

2. Replace `main` function (line 261-262):
```haskell
import Options.Applicative

data Command = RunBot | RunTests

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    RunBot -> runBot
    RunTests -> runTests
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "Dog adoption Telegram bot"
     <> header "dog-bot - Tiervermittlung scraper" )

    commandParser = subparser
      ( command "run" (info (pure RunBot) (progDesc "Run the bot"))
     <> command "test" (info (pure RunTests) (progDesc "Run tests")) )
```

**Usage After Implementation:**
```bash
nix run . -- run    # Run the bot
nix run . -- test   # Run tests
nix run            # Defaults to run
```

**Code Reference:** dog-bot.hs:261-262, flake.nix:30-48

---

### 1.3 Add Test Execution to CI Pipeline
**Effort:** 30 minutes | **Gain:** High | **Priority:** P0

**Current State:** GitHub Actions workflows never execute tests.

**Implementation:**

**File 1:** `.github/workflows/run-bot-haskell.yml`
```yaml
# Add new job before or after 'tests' job
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - uses: cachix/install-nix-action@v31
      with:
        nix_path: nixpkgs=channel:nixos-unstable
        extra_nix_config: |
          experimental-features = nix-command flakes
    - name: Run Tests
      run: nix run . -- test
```

**File 2:** `.github/workflows/upgrade.yml`
Modify the "perform flake update" step (line 23):
```yaml
    - name: perform flake update
      run: |
        nix flake update
        nix build
        nix run . -- test  # Add this line
        git config --global user.name 'Markus Hauck'
        # ... rest of commit logic
```

**Code Reference:** .github/workflows/run-bot-haskell.yml:6-21, .github/workflows/upgrade.yml:20-26

**Dependencies:** Requires 1.2 (CLI argument parsing) to be implemented first.

---

### 1.4 Extend Retry Logic to All Network Exceptions
**Effort:** 1 hour | **Gain:** High | **Priority:** P0

**Current State:** (lines 384-414)
Only retries `StatusCodeException`, not connection failures, timeouts, DNS errors, etc.

**Implementation:**
Add import:
```haskell
import qualified Network.HTTP.Client as HTTP
```

Modify `withRetry` function (around line 386):
```haskell
withRetry :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Session -> String -> IO (Response Lazy.ByteString) -> m (Response Lazy.ByteString)
withRetry sess label req = do
  env <- ask
  recoveringDynamic retryPolicy handlers $ \_ -> liftIO req
  where
    retryPolicy = fullJitterBackoff (round @Double 1e6) <> limitRetries 5

    -- Existing handler for status code exceptions
    statusHandler = Handler $ \(e :: HttpException) ->
      liftIO $ retryStatusException env sess e

    -- NEW: Handler for connection failures
    connectionHandler = Handler $ \(e :: HttpException) -> liftIO $ case e of
      HttpExceptionRequest _ (ConnectionFailure _) -> do
        Logging.loggingLogger LevelWarn label $ "Connection failure, will retry: " <> show e
        pure ConsultPolicy
      HttpExceptionRequest _ NoResponseDataReceived -> do
        Logging.loggingLogger LevelWarn label $ "No response data, will retry: " <> show e
        pure ConsultPolicy
      _ -> pure DontRetry

    -- NEW: Handler for timeout exceptions
    timeoutHandler = Handler $ \(e :: HttpException) -> liftIO $ case e of
      HttpExceptionRequest _ ResponseTimeout -> do
        Logging.loggingLogger LevelWarn label $ "Response timeout, will retry: " <> show e
        pure ConsultPolicy
      HttpExceptionRequest _ ConnectionTimeout -> do
        Logging.loggingLogger LevelWarn label $ "Connection timeout, will retry: " <> show e
        pure ConsultPolicy
      _ -> pure DontRetry

    handlers = [statusHandler, connectionHandler, timeoutHandler]
```

**Alternative Simpler Implementation:**
```haskell
-- Replace the handlers section with a single catch-all for common errors
handlers = [Handler $ \(e :: HttpException) -> liftIO $ case e of
  HttpExceptionRequest _ StatusCodeException{} -> retryStatusException env sess e
  HttpExceptionRequest _ (ConnectionFailure _) -> logAndRetry "Connection failure"
  HttpExceptionRequest _ NoResponseDataReceived -> logAndRetry "No response data"
  HttpExceptionRequest _ ResponseTimeout -> logAndRetry "Response timeout"
  HttpExceptionRequest _ ConnectionTimeout -> logAndRetry "Connection timeout"
  _ -> pure DontRetry
  where
    logAndRetry reason = do
      Logging.loggingLogger LevelWarn label $ reason <> ", will retry: " <> show e
      pure ConsultPolicy
]
```

**Code Reference:** dog-bot.hs:384-414

---

### 1.5 Add Error Logging to Nested Exception Handler
**Effort:** 30 minutes | **Gain:** Medium | **Priority:** P1

**Current State:** (lines 294-298)
```haskell
result <- try @m @SomeException $ do
  sendPics d
  sendVideos d
case result of
  Left _ -> pure ()  -- Silent failure!
  Right () -> pure ()
sendLink d
```

**Implementation:**
```haskell
result <- try @m @SomeException $ do
  sendPics d
  sendVideos d
case result of
  Left e -> liftIO $ Logging.loggingLogger LevelError "dogbot.media" $
    "Failed to send media for " <> Text.unpack (detailsUri d) <> ": " <> show e
  Right () -> pure ()
sendLink d
```

**Code Reference:** dog-bot.hs:294-298

---

### 1.6 Extract Magic Numbers to Named Constants
**Effort:** 1 hour | **Gain:** Medium | **Priority:** P1

**Current State:**
- Line 423: `round @Double 5e6` (rate limit)
- Line 422: `burstSize = 1` (token bucket)
- Line 386: `round @Double 1e6` (retry delay)
- Line 386: `limitRetries 5` (max retries)

**Implementation:**
Add constants section after imports (around line 100):

```haskell
-- ============================================================================
-- Configuration Constants
-- ============================================================================

-- | Telegram API rate limiting (microseconds between requests)
-- Conservative limit to avoid hitting Telegram's rate limits.
telegramRateLimitMicros :: Int
telegramRateLimitMicros = 5_000_000  -- 5 seconds

-- | Token bucket burst size (number of requests allowed in burst)
telegramBurstSize :: Int
telegramBurstSize = 1

-- | Maximum number of retry attempts for failed HTTP requests
maxRetryAttempts :: Int
maxRetryAttempts = 5

-- | Initial delay for exponential backoff retries (microseconds)
retryInitialDelayMicros :: Int
retryInitialDelayMicros = 1_000_000  -- 1 second
```

**Changes Required:**
- Line 386: Replace `round @Double 1e6` with `retryInitialDelayMicros`
- Line 386: Replace `limitRetries 5` with `limitRetries maxRetryAttempts`
- Line 422: Replace `burstSize = 1` with `burstSize = telegramBurstSize`
- Line 423: Replace `round @Double 5e6` with `telegramRateLimitMicros`

**Code Reference:** dog-bot.hs:96, 386, 422-423

---

### 1.7 Graceful Environment Variable Error Handling
**Effort:** 1 hour | **Gain:** Medium | **Priority:** P1

**Current State:** (lines 270-271)
```haskell
token <- getEnv "TELEGRAM_BOT_TOKEN"
chatId <- getEnv "TELEGRAM_CHAT_ID"
```

**Implementation:**
Add import:
```haskell
import System.Exit (die)
```

Replace in `runStack` function:
```haskell
validateEnv :: IO (String, String)
validateEnv = do
  tokenM <- lookupEnv "TELEGRAM_BOT_TOKEN"
  chatM <- lookupEnv "TELEGRAM_CHAT_ID"
  case (tokenM, chatM) of
    (Nothing, Nothing) -> die $ unlines
      [ "ERROR: Missing required environment variables:"
      , "  - TELEGRAM_BOT_TOKEN"
      , "  - TELEGRAM_CHAT_ID"
      , ""
      , "Please set these environment variables before running the bot."
      , "See CLAUDE.md for setup instructions."
      ]
    (Nothing, _) -> die "ERROR: Missing required environment variable: TELEGRAM_BOT_TOKEN"
    (_, Nothing) -> die "ERROR: Missing required environment variable: TELEGRAM_CHAT_ID"
    (Just "", _) -> die "ERROR: TELEGRAM_BOT_TOKEN cannot be empty"
    (_, Just "") -> die "ERROR: TELEGRAM_CHAT_ID cannot be empty"
    (Just t, Just c) -> pure (t, c)

-- In runStack function (around line 270):
(token, chatId) <- liftIO validateEnv
```

**Code Reference:** dog-bot.hs:270-271

---

## Phase 2: Configuration & Performance (2-3 days)

### 2.1 Improve Function Naming
**Effort:** 2 hours | **Gain:** Medium-High | **Priority:** P2

**Current State:**
- `checkDetail` (line 250): Doesn't indicate breed filtering
- `processEntry` (line 287): Doesn't indicate Telegram sending
- `runStack` (line 267): Generic name for environment setup

**Implementation:**

1. **Rename `checkDetail` → `isBreedAllowed`** (line 250-258)
```haskell
-- Old:
checkDetail :: Details -> Bool
checkDetail d = ...

-- New:
isBreedAllowed :: Details -> Bool
isBreedAllowed d = ...
  where
    -- Find "Rasse" (breed) in profile attributes
    raceM = d ^? detailsProfile . traverse . filtered (\(k, _) -> k == "Rasse") . _2
    -- Check if breed contains any forbidden keywords (case-insensitive)
    forbiddenKeywords = ["englisch setter", "bracke", "brake", "malteser", "dackel"]
    isForbidden = case raceM of
      Nothing -> False
      Just race -> any (\k -> Text.toLower k `Text.isInfixOf` Text.toLower race) forbiddenKeywords
```

2. **Rename `processEntry` → `processAndSendEntry`** (line 287-302)
```haskell
-- Old:
processEntry :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Int -> Entry -> m ()
processEntry i e = ...

-- New:
processAndSendEntry :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Int -> Entry -> m ()
processAndSendEntry i e = ...
```

3. **Rename `runStack` → `runWithEnvironment`** (line 267-276)
```haskell
-- Old:
runStack :: HasMyEnv e => RIO e a -> IO a
runStack m = ...

-- New:
runWithEnvironment :: HasMyEnv e => RIO e a -> IO a
runWithEnvironment m = ...
```

**Update Call Sites:**
- Line 265: `runStack loadAndProcessEntries` → `runWithEnvironment loadAndProcessEntries`
- Line 285: `uncurry processEntry` → `uncurry processAndSendEntry`
- Line 291: `checkDetail d` → `isBreedAllowed d`

**Code Reference:** dog-bot.hs:250-253, 267-276, 287-302

---

### 2.2 Eliminate Temporary File Storage for Media
**Effort:** 2 hours | **Gain:** Medium | **Priority:** P2

**Current State:** (lines 154-189, 191-248)
Downloads media to ByteString, writes to disk, reads back via `partFile`.

**Implementation:**

**In `sendPics` function** (lines 154-189):
```haskell
-- OLD (lines 160-165):
tmpDir <- liftIO $ createTempDirectory "/tmp" "pics"
pics <- traverse (\url -> do
  let name = mediaName url
  dl <- downloadImage session url
  liftIO $ BS.writeFile (tmpDir </> Text.unpack name) dl
  pure (name, tmpDir </> Text.unpack name)
  ) (take 10 $ detailsPics d)

let picturesParts = fmap (\(name, fp) -> Wreq.partFile (Text.unpack name) fp) pics

-- NEW:
pics <- traverse (\url -> do
  let name = mediaName url
  dl <- downloadImage session url
  pure (name, dl)
  ) (take 10 $ detailsPics d)

let picturesParts = fmap (\(name, bs) -> Wreq.partLBS (Text.unpack name) bs) pics
```

**Remove:**
- Line 159: `withSystemTempDirectory "pics"` wrapper (keep the do-block, remove temp dir)
- Line 164: `BS.writeFile` call

**In `sendVideos` function** (lines 191-248):
Apply same transformation:
```haskell
-- OLD (lines 216-226):
tmpDir <- liftIO $ createTempDirectory "/tmp" "videos"
videos <- catMaybes <$> traverse (\url -> do
  -- ... filtering logic ...
  let name = mediaName url
  dl <- downloadImage session url
  liftIO $ BS.writeFile (tmpDir </> Text.unpack name) dl
  pure $ Just (name, tmpDir </> Text.unpack name)
  ) (detailsVideos d)

let videosParts = fmap (\(name, fp) -> Wreq.partFile (Text.unpack name) fp) videos

-- NEW:
videos <- catMaybes <$> traverse (\url -> do
  -- ... filtering logic ...
  let name = mediaName url
  dl <- downloadImage session url
  pure $ Just (name, dl)
  ) (detailsVideos d)

let videosParts = fmap (\(name, bs) -> Wreq.partLBS (Text.unpack name) bs) videos
```

**Remove:**
- Line 215: `withSystemTempDirectory "videos"` wrapper
- Line 225: `BS.writeFile` call
- Import for `System.IO.Temp` (no longer needed)

**Code Reference:** dog-bot.hs:154-189, 191-248

---

### 2.3 Increase Telegram Rate Limit Efficiency
**Effort:** 30 minutes | **Gain:** Medium | **Priority:** P2

**Current State:** (line 423)
```haskell
inverseRate = round @Double 5e6  -- 5 seconds per request
```

**Implementation:**
```haskell
-- Update constant from Phase 1 (1.6):
telegramRateLimitMicros :: Int
telegramRateLimitMicros = 2_000_000  -- 2 seconds (increased from 5)

-- Line 423 already uses constant, so change is automatic
```

**Rationale:**
- Telegram allows up to 30 messages/second
- Bot has retry logic for 429 errors with Retry-After handling
- Current 5-second limit is overly conservative
- 2 seconds provides 2.5x speedup while remaining safe

**Code Reference:** dog-bot.hs:422-423

---

### 2.4 Add Explicit HTTP Timeouts
**Effort:** 2 hours | **Gain:** High | **Priority:** P2

**Current State:** (lines 273-274)
Sessions created without timeout configuration.

**Implementation:**

Add imports:
```haskell
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wreq.Session as Session
```

Modify `runStack`/`runWithEnvironment` function (lines 273-274):
```haskell
-- Add timeout constants (with Phase 1 constants):
httpRequestTimeoutSeconds :: Int
httpRequestTimeoutSeconds = 30

httpMediaDownloadTimeoutSeconds :: Int
httpMediaDownloadTimeoutSeconds = 120

-- OLD:
telegramSession <- liftIO Wreq.Session.newSession
scrapingSession <- liftIO Wreq.Session.newSession

-- NEW:
let normalTimeout = HTTP.responseTimeoutMicro (httpRequestTimeoutSeconds * 1_000_000)
    mediaTimeout = HTTP.responseTimeoutMicro (httpMediaDownloadTimeoutSeconds * 1_000_000)

telegramSession <- liftIO $ Session.newSessionControl Nothing $
  HTTP.defaultManagerSettings
    { HTTP.managerResponseTimeout = normalTimeout }

scrapingSession <- liftIO $ Session.newSessionControl Nothing $
  HTTP.defaultManagerSettings
    { HTTP.managerResponseTimeout = mediaTimeout }
```

**Code Reference:** dog-bot.hs:273-274

---

### 2.5 Externalized Configuration File
**Effort:** 1 day | **Gain:** Very High | **Priority:** P2

**Current State:**
- Search URL hardcoded (lines 95-96)
- Breed filters hardcoded (line 253)
- Rate limits hardcoded (line 423)

**Implementation:**

**Step 1:** Add dependencies to flake.nix:
```nix
# Already has aeson, add:
yaml
```

**Step 2:** Create `config.yaml` template:
```yaml
# Dog Bot Configuration

# Search parameters for tiervermittlung.de
search:
  base_url: "https://www.tiervermittlung.de/Suche/DetailSearch"
  # Query parameters (will be URL-encoded)
  params:
    Tierart: "Hund"
    Geschlecht: "weiblich"
    Alter-gt: 3
    Alter-lt: 15.1
    Zeitwert: "Monate"
    sb: 0
    so: "descend"

# Breed filtering
filters:
  # Case-insensitive substring matching against "Rasse" field
  forbidden_breeds:
    - "englisch setter"
    - "bracke"
    - "brake"
    - "malteser"
    - "dackel"

# Telegram API settings
telegram:
  # Rate limit in seconds between requests
  rate_limit_seconds: 2
  # Token bucket burst size
  burst_size: 1

# HTTP settings
http:
  # Request timeout in seconds
  request_timeout: 30
  # Media download timeout in seconds
  media_timeout: 120
  # Max retry attempts
  max_retries: 5
  # Initial retry delay in seconds
  retry_initial_delay: 1

# Logging
logging:
  # Log level: debug, info, warn, error
  level: "info"
```

**Step 3:** Add config data types (after imports):
```haskell
import Data.Yaml (decodeFileEither, FromJSON, (.:), (.:?), (.!=))
import qualified Data.Yaml as Yaml

data Config = Config
  { configSearch :: SearchConfig
  , configFilters :: FilterConfig
  , configTelegram :: TelegramConfig
  , configHttp :: HttpConfig
  , configLogging :: LoggingConfig
  } deriving (Show, Generic)

instance FromJSON Config

data SearchConfig = SearchConfig
  { searchBaseUrl :: Text
  , searchParams :: [(Text, Text)]
  } deriving (Show, Generic)

instance FromJSON SearchConfig where
  parseJSON = Yaml.withObject "SearchConfig" $ \v -> SearchConfig
    <$> v .: "base_url"
    <*> (v .: "params" >>= parseParams)
    where
      parseParams = Yaml.withObject "params" $ \obj ->
        pure $ HM.toList $ HM.map toText obj
      toText (Yaml.String t) = t
      toText (Yaml.Number n) = Text.pack (show n)
      toText v = Text.pack (show v)

data FilterConfig = FilterConfig
  { filterForbiddenBreeds :: [Text]
  } deriving (Show, Generic)

instance FromJSON FilterConfig where
  parseJSON = Yaml.withObject "FilterConfig" $ \v ->
    FilterConfig <$> v .: "forbidden_breeds"

data TelegramConfig = TelegramConfig
  { telegramRateLimitSeconds :: Double
  , telegramBurstSize :: Int
  } deriving (Show, Generic)

instance FromJSON TelegramConfig where
  parseJSON = Yaml.withObject "TelegramConfig" $ \v -> TelegramConfig
    <$> v .: "rate_limit_seconds"
    <*> v .: "burst_size"

data HttpConfig = HttpConfig
  { httpRequestTimeout :: Int
  , httpMediaTimeout :: Int
  , httpMaxRetries :: Int
  , httpRetryInitialDelay :: Double
  } deriving (Show, Generic)

instance FromJSON HttpConfig where
  parseJSON = Yaml.withObject "HttpConfig" $ \v -> HttpConfig
    <$> v .: "request_timeout"
    <*> v .: "media_timeout"
    <*> v .: "max_retries"
    <*> v .: "retry_initial_delay"

data LoggingConfig = LoggingConfig
  { loggingLevel :: Text
  } deriving (Show, Generic)

instance FromJSON LoggingConfig where
  parseJSON = Yaml.withObject "LoggingConfig" $ \v ->
    LoggingConfig <$> v .:? "level" .!= "info"
```

**Step 4:** Add Config to MyEnv:
```haskell
data MyEnv = MyEnv
  { chatId :: !String
  , telegramSession :: !Wreq.Session
  , scrapingSession :: !Wreq.Session
  , tokenBucket :: !(TVar (Bucket, UTCTime))
  , config :: !Config  -- NEW
  }
```

**Step 5:** Load config in main:
```haskell
runBot :: IO ()
runBot = do
  -- Load configuration
  configResult <- Yaml.decodeFileEither "config.yaml"
  cfg <- case configResult of
    Left err -> die $ "Failed to parse config.yaml: " <> show err
    Right c -> pure c

  -- Pass config to runWithEnvironment
  runWithEnvironment cfg loadAndProcessEntries
```

**Step 6:** Update `runWithEnvironment` to accept Config:
```haskell
runWithEnvironment :: Config -> RIO MyEnv a -> IO a
runWithEnvironment cfg m = do
  (token, chatId) <- validateEnv
  -- Use config values for timeouts, rate limits, etc.
  -- ... create sessions with config.httpRequestTimeout ...
  let env = MyEnv { ..., config = cfg }
  runRIO env m
```

**Step 7:** Update usage sites:
- Use `cfg ^. configFilters . filterForbiddenBreeds` in `isBreedAllowed`
- Build search URL from `cfg ^. configSearch`
- Use config values for rate limits, timeouts

**Code Reference:** dog-bot.hs:95-96, 102-110, 253, 267-276

---

## Phase 3: Major Quality Improvements (1-2 weeks)

### 3.1 Add Explicit Type Signatures to All Functions
**Effort:** 1-2 days | **Gain:** High | **Priority:** P3

**Current State:** (lines 324-376)
Functions without type signatures rely on `-Wno-missing-signatures` pragma (line 16).

**Implementation:**

Remove pragma:
```haskell
-- Line 16: DELETE THIS
{-# OPTIONS_GHC -Wno-missing-signatures #-}
```

Add type signatures:
```haskell
-- Line 324:
extractEntries :: Lazy.Text -> [Entry]
extractEntries body = ...

-- Line 336:
loadDetails :: (MonadIO m, MonadReader e m, HasMyEnv e, MonadMask m)
            => Entry -> m (Maybe Details)
loadDetails e = ...

-- Line 343:
extractDetails :: Lazy.Text -> Maybe Details
extractDetails body = ...

-- Line 345:
extractProfileAttrs :: Lazy.Text -> [(Text, Text)]
extractProfileAttrs body = ...

-- Line 352:
extractAttributes :: Lazy.Text -> [(Text, Text)]
extractAttributes body = ...

-- Line 355:
extractTitle :: Lazy.Text -> Maybe Text
extractTitle body = ...

-- Line 358:
extractPics :: Lazy.Text -> [Text]
extractPics body = ...

-- Line 361:
extractYoutubeVideos :: Lazy.Text -> [Text]
extractYoutubeVideos body = ...

-- Line 364:
extractEmbeddedVideos :: Lazy.Text -> [Text]
extractEmbeddedVideos body = ...

-- Line 370:
matchAttr :: Text -> Text -> Traversal' Element Element
matchAttr a v = ...

-- Line 372:
matchId :: Text -> Traversal' Element Element
matchId = matchAttr "id"

-- Line 374:
matchClass :: Text -> Traversal' Element Element
matchClass = matchAttr "class"

-- Line 376:
mediaName :: Text -> Text
mediaName url = ...
```

**Process:**
1. Remove pragma
2. Try to compile: `nix build` or `runhaskell dog-bot.hs`
3. Add type signatures for each error
4. Iterate until compilation succeeds

**Code Reference:** dog-bot.hs:16, 324-376

---

### 3.2 Parallel Entry Processing
**Effort:** 2 days | **Gain:** High | **Priority:** P3

**Current State:** (line 285)
```haskell
for_ ([1 ..] `zip` es) $ uncurry processAndSendEntry
```
Processes entries sequentially; each entry takes 3-6 seconds of I/O wait time.

**Implementation:**

**Step 1:** Add dependency to flake.nix:
```nix
async
unliftio  # For exception-safe concurrency with RIO
```

**Step 2:** Modify `loadAndProcessEntries`:
```haskell
import UnliftIO.Async (pooledMapConcurrentlyN)

loadAndProcessEntries :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => m ()
loadAndProcessEntries = do
  es <- loadEntries
  liftIO $ Logging.loggingLogger LevelInfo "dogbot" $
    "Found " <> show (length es) <> " entries, processing up to 3 concurrently..."

  -- Process up to 3 entries in parallel
  pooledMapConcurrentlyN 3 (uncurry processAndSendEntry) ([1 ..] `zip` es)

  pure ()
```

**Considerations:**
- Telegram session: Wreq.Session uses connection pooling, thread-safe
- Token bucket: TVar operations are atomic, thread-safe
- Scraping session: Shared, thread-safe
- Logging: May interleave, but acceptable
- Limit to 3 concurrent to avoid overwhelming servers

**Expected Speedup:** 2-3x for typical workload (10-20 entries)

**Code Reference:** dog-bot.hs:278-286

---

### 3.3 Dry-Run Mode with Telegram API Mocking
**Effort:** 2-3 days | **Gain:** Very High | **Priority:** P3

**Current State:** No way to test locally without sending real Telegram messages.

**Implementation:**

**Step 1:** Extract Telegram operations into type class:
```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- Define Telegram API abstraction
class Monad m => TelegramAPI m where
  sendMediaGroup :: String -> [Wreq.Part] -> m ()
  sendMessage :: String -> Text -> m ()
  getSession :: m Wreq.Session

-- Production implementation
instance (MonadIO m, MonadReader e m, HasMyEnv e, MonadMask m) => TelegramAPI (RIO e) where
  sendMediaGroup cId parts = do
    env <- ask
    sess <- asks telegramSession
    withToken env $ do
      let opts = Wreq.defaults & Wreq.param "chat_id" .~ [Text.pack cId]
      withRetry sess "dogbot.telegram" $
        Wreq.postWith opts sess "https://api.telegram.org/bot..." parts

  sendMessage cId msg = do
    env <- ask
    sess <- asks telegramSession
    withToken env $ do
      let opts = Wreq.defaults
            & Wreq.param "chat_id" .~ [Text.pack cId]
            & Wreq.param "text" .~ [msg]
      withRetry sess "dogbot.telegram.message" $
        Wreq.postWith opts sess "https://api.telegram.org/bot..." []

  getSession = asks telegramSession

-- Dry-run mock implementation
data DryRunEnv = DryRunEnv
  { dryRunConfig :: Config
  , dryRunMessages :: TVar [Text]  -- Accumulate sent messages
  }

newtype DryRunM a = DryRunM { runDryRunM :: ReaderT DryRunEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DryRunEnv)

instance TelegramAPI DryRunM where
  sendMediaGroup cId parts = do
    ref <- asks dryRunMessages
    liftIO $ atomically $ modifyTVar' ref $
      (:) $ "[DRY RUN] SEND_MEDIA to " <> Text.pack cId <> ": "
         <> Text.pack (show (length parts)) <> " parts"

  sendMessage cId msg = do
    ref <- asks dryRunMessages
    liftIO $ atomically $ modifyTVar' ref $
      (:) $ "[DRY RUN] SEND_MESSAGE to " <> Text.pack cId <> ": " <> msg

  getSession = error "No session in dry-run mode"

-- Helper to run in dry-run mode
runDryRun :: Config -> DryRunM a -> IO [Text]
runDryRun cfg action = do
  ref <- newTVarIO []
  let env = DryRunEnv cfg ref
  _ <- runReaderT (runDryRunM action) env
  atomically $ readTVar ref
```

**Step 2:** Refactor send functions to use type class:
```haskell
sendPics :: TelegramAPI m => Details -> m ()
sendPics d = do
  -- ... existing logic ...
  sendMediaGroup chatId picturesParts

sendVideos :: TelegramAPI m => Details -> m ()
sendVideos d = do
  -- ... existing logic ...
  sendMediaGroup chatId videosParts

sendLink :: TelegramAPI m => Details -> m ()
sendLink d = do
  -- ... existing logic ...
  sendMessage chatId message
```

**Step 3:** Add CLI flag (extends 1.2):
```haskell
data Command = RunBot | RunTests | DryRun

commandParser = subparser
  ( command "run" (info (pure RunBot) (progDesc "Run the bot"))
 <> command "test" (info (pure RunTests) (progDesc "Run tests"))
 <> command "dry-run" (info (pure DryRun) (progDesc "Dry run (don't send Telegram messages)"))
  )

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    RunBot -> runBot
    RunTests -> runTests
    DryRun -> runDryRunMode
```

**Step 4:** Implement dry-run execution:
```haskell
runDryRunMode :: IO ()
runDryRunMode = do
  cfg <- loadConfig "config.yaml"
  messages <- runDryRun cfg $ do
    -- Execute bot logic with mocked Telegram
    -- ... (needs refactoring to be polymorphic over TelegramAPI)
    pure ()

  putStrLn "\n=== Dry Run Results ==="
  forM_ (reverse messages) $ \msg ->
    putStrLn $ "  " <> Text.unpack msg
```

**Benefits:**
- Test locally without spamming Telegram
- See what would be sent without side effects
- Validate filtering logic safely
- Debug scraping issues independently

**Code Reference:** dog-bot.hs:132-148, 154-259

---

### 3.4 Single-Pass HTML Extraction
**Effort:** 1 day | **Gain:** Medium-High | **Priority:** P3

**Current State:** (lines 338-365)
Each extractor traverses entire DOM tree via `html . to universe . traverse`.

**Implementation:**

**Step 1:** Compute universe once:
```haskell
extractDetails :: Lazy.Text -> Maybe Details
extractDetails body = do
  -- Compute universe once
  let elements = body ^.. html . to universe . traverse

  -- Pass elements to each extractor
  title <- extractTitle' elements
  let pics = extractPics' elements
      videos = extractYoutubeVideos' elements <> extractEmbeddedVideos' elements
      profile = extractProfileAttrs' elements
      description = extractDescription' elements

  pure $ Details { ... }
```

**Step 2:** Refactor extractors to accept `[Element]`:
```haskell
-- Old:
extractTitle :: Lazy.Text -> Maybe Text
extractTitle body = body ^? html . to universe . traverse . matchId "AnzeigenTitel" ...

-- New:
extractTitle' :: [Element] -> Maybe Text
extractTitle' elements =
  elements ^? traverse . matchId "AnzeigenTitel" ...

-- Similarly for:
extractPics' :: [Element] -> [Text]
extractYoutubeVideos' :: [Element] -> [Text]
extractEmbeddedVideos' :: [Element] -> [Text]
extractProfileAttrs' :: [Element] -> [(Text, Text)]
extractDescription' :: [Element] -> Maybe Text
```

**Expected Improvement:** 3-6x faster detail extraction (eliminates 5 redundant tree traversals).

**Code Reference:** dog-bot.hs:338-365

---

### 3.5 Refactor Duplicate Media Handling Logic
**Effort:** 3-4 days | **Gain:** High | **Priority:** P3

**Current State:** (lines 154-248)
`sendPics` and `sendVideos` are ~90% identical code.

**Implementation:**

**Step 1:** Define media type:
```haskell
data MediaType = PhotoMedia | VideoMedia deriving (Eq, Show)

mediaTypeName :: MediaType -> Text
mediaTypeName PhotoMedia = "photo"
mediaTypeName VideoMedia = "video"
```

**Step 2:** Extract common function:
```haskell
sendMediaGroup :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e)
               => MediaType -> [Text] -> Details -> m ()
sendMediaGroup mediaType urls d = do
  env <- ask
  sess <- asks scrapingSession
  cId <- asks chatId

  when (null urls) $ do
    liftIO $ Logging.loggingLogger LevelInfo ("dogbot." <> mediaTypeName mediaType) $
      "No " <> show mediaType <> " for " <> Text.unpack (detailsUri d)
    pure ()

  unless (null urls) $ do
    liftIO $ Logging.loggingLogger LevelDebug ("dogbot." <> mediaTypeName mediaType) $
      "Downloading " <> show (length urls) <> " " <> show mediaType <> "(s)"

    -- Download media
    media <- traverse (\url -> do
      let name = mediaName url
      dl <- downloadImage sess url
      pure (name, dl)
      ) (take 10 urls)

    -- Build parts
    let mediaParts = fmap (\(name, bs) -> Wreq.partLBS (Text.unpack name) bs) media
        typeField = Wreq.partText "type" (mediaTypeName mediaType)
        chatField = Wreq.partText "chat_id" (Text.pack cId)

        -- Build media JSON
        mediaJson = Aeson.object
          [ "media" .= fmap (\(name, _) ->
              Aeson.object ["type" .= mediaTypeName mediaType, "media" .= ("attach://" <> name)]
            ) media
          ]
        mediaField = Wreq.partText "media" (decodeUtf8 (BS.toStrict (Aeson.encode mediaJson)))

    liftIO $ Logging.loggingLogger LevelDebug ("dogbot." <> mediaTypeName mediaType) $
      "Uploading " <> show (length media) <> " " <> show mediaType <> "(s)"

    -- Send to Telegram
    token <- asks (token . config)  -- Assuming config in env
    let url = "https://api.telegram.org/bot" <> token <> "/sendMediaGroup"

    withToken env $ do
      result <- try @m @SomeException $
        withRetry sess ("dogbot.telegram." <> mediaTypeName mediaType) $
          Wreq.postWith Wreq.defaults sess url (typeField : chatField : mediaField : mediaParts)

      case result of
        Left e -> liftIO $ Logging.loggingLogger LevelError ("dogbot." <> mediaTypeName mediaType) $
          "Failed to send " <> show mediaType <> ": " <> show e
        Right _ -> liftIO $ Logging.loggingLogger LevelInfo ("dogbot." <> mediaTypeName mediaType) $
          "Sent " <> show (length media) <> " " <> show mediaType <> "(s)"
```

**Step 3:** Implement wrappers with filtering:
```haskell
sendPics :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Details -> m ()
sendPics d = sendMediaGroup PhotoMedia (detailsPics d) d

sendVideos :: (MonadMask m, MonadIO m, MonadReader e m, HasMyEnv e) => Details -> m ()
sendVideos d = do
  -- Apply video filtering (YouTube removal)
  let urls = detailsVideos d
  filtered <- catMaybes <$> traverse filterVideo urls
  sendMediaGroup VideoMedia filtered d
  where
    filterVideo url = do
      -- Check if YouTube
      if "youtube" `Text.isInfixOf` Text.toLower url
        then do
          liftIO $ Logging.loggingLogger LevelDebug "dogbot.video" $
            "Skipping YouTube video: " <> Text.unpack url
          pure Nothing
        else pure (Just url)
```

**Benefits:**
- Eliminates ~80 lines of duplication
- Centralizes media handling logic
- Easier to modify media sending behavior
- More maintainable

**Code Reference:** dog-bot.hs:154-248

---

### 3.6 Content-Type Validation for Downloaded Media
**Effort:** 1-2 days | **Gain:** Medium | **Priority:** P3

**Current State:** (lines 378-382)
Downloads any response as media without validation.

**Implementation:**

**Step 1:** Add dependency to flake.nix:
```nix
bytestring  # Already present
network-uri
```

**Step 2:** Add validation function:
```haskell
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString as BS

-- Magic numbers for file type detection
jpegMagic, pngMagic, mp4Magic :: BS.ByteString
jpegMagic = BS.pack [0xFF, 0xD8, 0xFF]
pngMagic = BS.pack [0x89, 0x50, 0x4E, 0x47]
mp4Magic = BS.pack [0x00, 0x00, 0x00, 0x20, 0x66, 0x74, 0x79, 0x70]  -- ftyp

validateMediaContent :: MediaType -> Response Lazy.ByteString -> Either String Lazy.ByteString
validateMediaContent mediaType resp = do
  -- Check Content-Type header
  let contentTypeM = resp ^? Wreq.responseHeader hContentType
  case contentTypeM of
    Nothing -> Left "No Content-Type header"
    Just ct -> do
      let isValidType = case mediaType of
            PhotoMedia -> "image/" `BS.isPrefixOf` ct
            VideoMedia -> "video/" `BS.isPrefixOf` ct
      unless isValidType $
        Left $ "Invalid Content-Type: " <> show ct <> " for " <> show mediaType

  -- Check magic numbers
  let body = resp ^. Wreq.responseBody
      bodyBS = BS.take 8 $ BS.toStrict body
      validMagic = case mediaType of
        PhotoMedia -> jpegMagic `BS.isPrefixOf` bodyBS || pngMagic `BS.isPrefixOf` bodyBS
        VideoMedia -> mp4Magic `BS.isPrefixOf` bodyBS ||
                      BS.pack [0x00, 0x00, 0x00, 0x18] `BS.isPrefixOf` bodyBS  -- alternative mp4

  unless validMagic $
    Left $ "Invalid file signature for " <> show mediaType

  pure body
```

**Step 3:** Modify downloadImage:
```haskell
-- Old:
downloadImage :: Session -> Text -> IO Lazy.ByteString
downloadImage session url = do
  resp <- Wreq.Session.get session (Text.unpack url)
  pure $ resp ^. Wreq.responseBody

-- New:
downloadMediaValidated :: MediaType -> Session -> Text -> IO (Either String Lazy.ByteString)
downloadMediaValidated mediaType session url = do
  resp <- Wreq.Session.get session (Text.unpack url)
  pure $ validateMediaContent mediaType resp
```

**Step 4:** Update call sites in sendMediaGroup:
```haskell
media <- catMaybes <$> traverse (\url -> do
  let name = mediaName url
  result <- liftIO $ downloadMediaValidated mediaType sess url
  case result of
    Left err -> do
      liftIO $ Logging.loggingLogger LevelWarn ("dogbot." <> mediaTypeName mediaType) $
        "Invalid media at " <> Text.unpack url <> ": " <> err
      pure Nothing
    Right dl -> pure $ Just (name, dl)
  ) (take 10 urls)
```

**Benefits:**
- Prevents sending HTML error pages as images
- Catches broken/redirected media URLs early
- Better error messages for debugging
- More robust media handling

**Code Reference:** dog-bot.hs:378-382, usage in sendPics/sendVideos

---

## Implementation Guidelines

### Testing Strategy
After each improvement:
1. **Syntax check:** `runhaskell dog-bot.hs` (will fail on missing env vars, that's OK)
2. **Build check:** `nix build`
3. **Run tests:** `nix run . -- test` (after 1.2)
4. **Dry run:** `nix run . -- dry-run` (after 3.3)

### Git Workflow
Commit after each major improvement or logical group:
```bash
git add dog-bot.hs
git commit -m "Extract breed filter to top-level constant (1.1)"
```

### Rollback Plan
Each improvement is independent and can be skipped or reverted:
- Keep commits small and focused
- Test after each change
- If an improvement causes issues, revert that specific commit

---

## Priority Matrix

| ID | Improvement | Effort | Gain | Phase | Priority |
|----|-------------|--------|------|-------|----------|
| 1.1 | Extract breed filter constant | Very Low | High | 1 | P0 |
| 1.2 | CLI argument parsing | Low | Medium-High | 1 | P0 |
| 1.3 | Tests in CI | Low | High | 1 | P0 |
| 1.4 | Network exception retry | Low | High | 1 | P0 |
| 1.5 | Error logging | Low | Medium | 1 | P1 |
| 1.6 | Extract magic numbers | Low-Med | Medium | 1 | P1 |
| 1.7 | Env var validation | Low | Medium | 1 | P1 |
| 2.1 | Function naming | Low-Med | Medium-High | 2 | P2 |
| 2.2 | Eliminate temp files | Med | Medium | 2 | P2 |
| 2.3 | Increase rate limit | Very Low | Medium | 2 | P2 |
| 2.4 | HTTP timeouts | Med | High | 2 | P2 |
| 2.5 | Config file | High | Very High | 2 | P2 |
| 3.1 | Type signatures | Med-High | High | 3 | P3 |
| 3.2 | Parallel processing | Med | High | 3 | P3 |
| 3.3 | Dry-run mode | Med-High | Very High | 3 | P3 |
| 3.4 | Single-pass extraction | Med | Medium-High | 3 | P3 |
| 3.5 | Refactor media duplication | High | High | 3 | P3 |
| 3.6 | Content-Type validation | Med-High | Medium | 3 | P3 |

---

## Success Metrics

### Phase 1 Complete:
- ✅ Bot survives transient network failures
- ✅ Tests run in CI on every commit
- ✅ Easy to run tests locally
- ✅ Clear error messages for configuration issues
- ✅ Media send failures are logged
- ✅ No magic numbers in code

### Phase 2 Complete:
- ✅ Bot sends notifications 2-5x faster
- ✅ Configuration changes don't require code edits
- ✅ HTTP requests have proper timeouts
- ✅ Function names clearly indicate purpose
- ✅ No unnecessary disk I/O

### Phase 3 Complete:
- ✅ All functions have explicit types
- ✅ Can test locally without Telegram spam
- ✅ Multiple entries process concurrently
- ✅ No code duplication in media handling
- ✅ Invalid media is rejected early
- ✅ HTML extraction is optimized

---

## Questions for Implementation

Before starting, clarify:
1. **Config file location:** Should `config.yaml` be in repo root or in `.config/` subdirectory?
2. **CI test job:** Should tests run on every push or only on PRs?
3. **Parallel processing:** Is 3 concurrent entries appropriate or adjust based on rate limits?
4. **Logging level:** Should dry-run mode have different default log level?
5. **Breaking changes:** OK to change function names (2.1) or maintain compatibility?

---

## Maintenance Notes

After completing improvements:
1. Update CLAUDE.md to reflect new capabilities
2. Update .gitignore if new files added (config.yaml, result symlinks)
3. Document new CLI commands in README (if exists) or CLAUDE.md
4. Consider adding example config.yaml to repository
5. Update GitHub Actions if secrets needed for new features

---

**Document Version:** 1.0
**Last Updated:** 2025-11-05
**Estimated Total Effort:** 2-4 weeks for all phases
**Estimated Quick Wins (Phase 1):** 1-2 days

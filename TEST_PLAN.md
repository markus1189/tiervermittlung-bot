# Testing & Testability Improvement Plan

**Generated:** 2025-11-05
**Focus:** Comprehensive testing strategy for dog-bot.hs
**Current State:** 6 unit tests (parsing only), no integration tests, manual test execution

---

## Executive Summary

The dog-bot currently has minimal test coverage focused only on HTML parsing. This plan addresses:
1. **Test Infrastructure** - Make tests easy to run and integrate into CI
2. **Coverage Gaps** - Add tests for untested functionality (80% of codebase)
3. **Testability** - Refactor code to enable isolated unit testing
4. **Property Testing** - Add generative tests for robust validation
5. **Integration Testing** - Test end-to-end workflows without side effects

---

## Current Test Coverage Analysis

### ✅ What IS Tested (Lines 425-492)
- ✅ `extractEntries` - Parse search results (150 entries)
- ✅ `extractDetails` - Parse detail pages (no video, direct video, YouTube)
- ✅ `mediaName` - Extract filename from URL
- ✅ `checkDetail` - Basic breed filtering (2 cases)

### ❌ What is NOT Tested (Critical Gaps)
- ❌ HTTP operations (`loadEntries`, `loadDetailsAndExtract`, `downloadImage`)
- ❌ Retry logic (`withRetry`, `retryStatusException`)
- ❌ Rate limiting (`withToken`, token bucket behavior)
- ❌ Telegram API integration (`sendPics`, `sendVideos`, `sendLink`)
- ❌ Media processing (download, validation, multipart form building)
- ❌ Error handling (exception paths, nested try-catch blocks)
- ❌ Date filtering (yesterday logic, entry date parsing)
- ❌ Environment setup (`runStack`, config loading)
- ❌ Edge cases (empty results, malformed HTML, network failures)
- ❌ Profile attribute extraction (`extractProfileAttrs`, race finding)

**Estimated Coverage:** ~15% (6 tests covering only parsing layer)
**Target Coverage:** 80%+ (comprehensive unit + integration tests)

---

## Test Infrastructure Improvements

### Phase 1: Quick Wins (2-4 hours)

#### 1.1 Add CLI Test Command (CRITICAL)
**Current:** Must edit `main = runBot` to `main = runTests`
**Impact:** HIGH - Blocks all testing workflows

**Implementation:**
```haskell
-- Add dependency: optparse-applicative
import Options.Applicative

data Command = Run | Test deriving (Show, Eq)

parseCommand :: Parser Command
parseCommand = subparser
  ( command "run" (info (pure Run) (progDesc "Run the bot"))
 <> command "test" (info (pure Test) (progDesc "Run tests"))
  ) <|> pure Run  -- Default to Run for backward compatibility

main :: IO ()
main = do
  cmd <- execParser $ info (parseCommand <**> helper)
    ( fullDesc
   <> progDesc "Dog adoption Telegram bot"
   <> header "dog-bot - Tiervermittlung.de scraper" )
  case cmd of
    Run -> runBot
    Test -> runTests
```

**Usage:** `nix run . -- test` or `runhaskell dog-bot.hs test`

**Files Changed:** dog-bot.hs:261-262, flake.nix (add optparse-applicative)

---

#### 1.2 Add Test Execution to CI (CRITICAL)
**Current:** GitHub Actions never runs tests
**Impact:** HIGH - Tests can break without detection

**Implementation:**

**.github/workflows/test.yml** (new file):
```yaml
name: Tests

on:
  push:
    branches: [ main, master ]
  pull_request:
  workflow_dispatch:

jobs:
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

      - name: Build
        run: nix build
```

**Modify .github/workflows/upgrade.yml:**
```yaml
    - name: perform flake update
      run: |
        nix flake update
        nix build
        nix run . -- test  # ADD THIS LINE
        git config --global user.name 'Markus Hauck'
        # ... rest unchanged
```

**Files Changed:** .github/workflows/test.yml (new), .github/workflows/upgrade.yml:23

---

#### 1.3 Add Test Output Verbosity Control
**Current:** No control over test verbosity
**Impact:** MEDIUM - Harder to debug test failures

**Implementation:**
```haskell
-- In runTests function (line 427)
import Test.Tasty.Options (OptionDescription(..))
import Test.Tasty.Runners (TestPattern(..), parseTestPattern)
import System.Environment (lookupEnv)

runTests :: IO ()
runTests = do
  -- Allow filtering tests via TEST_PATTERN env var
  pattern <- lookupEnv "TEST_PATTERN"
  let ingredients = defaultIngredients
  defaultMainWithIngredients ingredients tests
```

**Usage:**
```bash
nix run . -- test                           # Run all tests
TEST_PATTERN="Parse" nix run . -- test      # Run only parse tests
nix run . -- test --pattern "breed"         # Filter by name
```

**Files Changed:** dog-bot.hs:427-428

---

### Phase 2: Test Organization (1 day)

#### 2.1 Separate Test Module
**Current:** Tests inline with main code (makes file large)
**Impact:** MEDIUM - Better organization

**Create: test/Spec.hs**
```haskell
module Main where

import Test.Tasty
import qualified ParsingTests
import qualified FilteringTests
import qualified HTTPTests

main :: IO ()
main = defaultMain $ testGroup "Dog Bot Tests"
  [ ParsingTests.tests
  , FilteringTests.tests
  , HTTPTests.tests
  ]
```

**Create: test/ParsingTests.hs**
```haskell
module ParsingTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
-- Import functions from main module
import qualified DogBot

tests :: TestTree
tests = testGroup "Parsing Tests"
  [ testSearchResults
  , testDetailExtraction
  -- ... existing tests moved here
  ]
```

**Modify: dog-bot.hs**
- Rename `module Main` to `module DogBot`
- Export all testable functions
- Move tests to test/ directory

**Modify: flake.nix**
- Update to build test suite separately
- Add test dependencies

**Files Changed:** dog-bot.hs (refactor), test/*.hs (new), flake.nix

---

## Coverage Gap Improvements

### Phase 3: Unit Tests for Pure Functions (2-3 days)

#### 3.1 Breed Filter Tests (HIGH PRIORITY)
**Function:** `checkDetail` (line 250)
**Current Coverage:** 2 test cases
**Target:** Comprehensive edge cases

**Add to tests:**
```haskell
testGroup "Breed Filtering"
  [ testCase "Allow details without race field" $
      checkDetail (Details "uri" Nothing [] [] Map.empty Nothing) @?= True

  , testCase "Filter bracke (exact match)" $
      checkDetail (mkDetails "Bracke") @?= False

  , testCase "Filter bracke (case insensitive)" $
      checkDetail (mkDetails "BRACKE") @?= False

  , testCase "Filter bracke (substring)" $
      checkDetail (mkDetails "Mischling Bracke") @?= False

  , testCase "Filter alternate spelling brake" $
      checkDetail (mkDetails "brake") @?= False

  , testCase "Filter englisch setter" $
      checkDetail (mkDetails "Englisch Setter") @?= False

  , testCase "Filter malteser" $
      checkDetail (mkDetails "Malteser") @?= False

  , testCase "Filter dackel" $
      checkDetail (mkDetails "Dackel Mischling") @?= False

  , testCase "Allow similar but different breed (break ≠ brake)" $
      checkDetail (mkDetails "Breakthrough Hound") @?= True

  , testCase "Allow breed with partial match but not forbidden (mal ≠ malteser)" $
      checkDetail (mkDetails "Mallinois") @?= True

  , testCase "Allow empty race" $
      checkDetail (mkDetails "") @?= True

  , testCase "Filter multiple forbidden keywords" $
      checkDetail (mkDetails "Dackel-Bracke Mix") @?= False

  , testCase "Unicode and special chars in race" $
      checkDetail (mkDetails "Röttweiler-Dackel") @?= False
  ]
  where
    mkDetails race = Details "uri" Nothing [] []
      (Map.fromList [("Rasse", race)]) Nothing
```

**Impact:** Comprehensive validation of filtering logic (business-critical)
**Files Changed:** dog-bot.hs (add 10 test cases)

---

#### 3.2 Media Name Extraction Tests
**Function:** `mediaName` (line 376)
**Current Coverage:** 1 test case
**Target:** Edge cases

**Add to tests:**
```haskell
testGroup "Media Name Extraction"
  [ testCase "Extract from standard pic URL" $
      mediaName "https://www.tiervermittlung.de/.../j1492551-2.pic"
        @?= "j1492551-2.pic"

  , testCase "Extract from video URL" $
      mediaName "https://www.tiervermittlung.de/.../1492553.mp4"
        @?= "1492553.mp4"

  , testCase "Handle URL with query params" $
      mediaName "https://example.com/image.jpg?size=large&v=2"
        @?= "image.jpg"

  , testCase "Handle URL with fragment" $
      mediaName "https://example.com/video.mp4#t=10"
        @?= "video.mp4"

  , testCase "Handle URL ending with slash" $
      mediaName "https://example.com/path/file.png/"
        @?= "file.png"

  , testCase "Handle filename with multiple dots" $
      mediaName "https://example.com/my.dog.photo.jpeg"
        @?= "my.dog.photo.jpeg"

  , testCase "Handle URL-encoded filename" $
      mediaName "https://example.com/my%20dog.jpg"
        @?= "my%20dog.jpg"
  ]
```

**Files Changed:** dog-bot.hs (add 6 test cases)

---

#### 3.3 Date Parsing Tests
**Function:** `extractDate` (line 307), `extractEntries` (line 324)
**Current Coverage:** Count only, no date validation
**Target:** Date format validation

**Add to tests:**
```haskell
testGroup "Date Parsing"
  [ testCase "Parse German date format DD.MM.YYYY" $ do
      -- Need fixture with specific date
      let entry = Entry (read "2025-01-15") "http://..."
      entryDate entry @?= read "2025-01-15"

  , testCase "Handle entries without dates" $ do
      input <- decodeLatin1 <$> BS.readFile "fixtures/search.html"
      let entries = extractEntries input
      -- All entries should have valid dates
      all (\(Entry d _) -> d > read "2020-01-01") entries @?= True

  , testCase "Entries are sorted by date descending" $ do
      input <- decodeLatin1 <$> BS.readFile "fixtures/search.html"
      let entries = extractEntries input
          dates = map entryDate entries
      dates @?= reverse (sort dates)
  ]
```

**Files Changed:** dog-bot.hs, may need new fixture

---

#### 3.4 Profile Attribute Extraction Tests
**Function:** `extractProfileAttrs` (line 345), `extractAttributes` (line 352)
**Current Coverage:** Indirect (count check)
**Target:** Validate attribute parsing

**Add to tests:**
```haskell
testGroup "Profile Attributes"
  [ testCase "Extract all profile fields from detail page" $ do
      input <- decodeLatin1 <$> BS.readFile "fixtures/no-video.html"
      let details = extractDetails "uri" input
          profile = detailsProfile details
      Map.lookup "Rasse" profile @?= Just "Mischling  (Mischling)"
      Map.lookup "Geschlecht" profile @?= Just "weiblich"
      isJust (Map.lookup "Alter" profile) @?= True

  , testCase "Handle missing profile fields" $ do
      -- Create minimal fixture or test with existing
      let details = Details "uri" Nothing [] [] Map.empty Nothing
      Map.null (detailsProfile details) @?= True

  , testCase "Handle malformed profile HTML" $ do
      -- Test with corrupted HTML fixture
      let input = "<html><body></body></html>"
      let details = extractDetails "uri" input
      -- Should not crash, return empty or partial data
      True @?= True  -- Adjust based on desired behavior
  ]
```

**Files Changed:** dog-bot.hs (add 3 test cases)

---

### Phase 4: Property-Based Tests (2-3 days)

#### 4.1 Add QuickCheck/Hedgehog
**Current:** Only example-based tests
**Impact:** HIGH - Find edge cases automatically

**Add dependency:** `hedgehog` (already in flake.nix via tasty-hedgehog)

**Implementation:**
```haskell
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "mediaName never returns empty string" $
      property $ do
        -- Generate random URLs
        domain <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
        path <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
        file <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
        ext <- forAll $ Gen.element ["jpg", "png", "mp4", "pic"]
        let url = "https://" <> domain <> "/" <> path <> "/" <> file <> "." <> ext
        mediaName url /== ""

  , testProperty "checkDetail is consistent" $
      property $ do
        race <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
        let details = Details "uri" Nothing [] []
              (Map.fromList [("Rasse", race)]) Nothing
            result1 = checkDetail details
            result2 = checkDetail details
        result1 === result2

  , testProperty "forbidden keywords are case insensitive" $
      property $ do
        keyword <- forAll $ Gen.element ["bracke", "dackel", "malteser"]
        -- Generate random case variations
        caseVariant <- forAll $ Gen.text (Range.singleton (Text.length keyword)) $
          Gen.element (Text.unpack keyword)
        let details = mkDetails caseVariant
        checkDetail details === False

  , testProperty "extractEntries returns valid URIs" $
      property $ do
        -- Would need to generate valid HTML or use real fixture
        input <- forAll genValidSearchHTML
        let entries = extractEntries input
        all (\(Entry _ uri) -> "http" `Text.isPrefixOf` uri) entries === True
  ]
```

**Benefits:**
- Automatically test hundreds of random inputs
- Find edge cases developers miss
- Regression testing with shrinking

**Files Changed:** dog-bot.hs (add property tests section)

---

## Testability Improvements

### Phase 5: Dependency Injection for External Services (3-5 days)

#### 5.1 Abstract HTTP Operations (HIGH IMPACT)
**Problem:** Cannot test `loadEntries`, `loadDetailsAndExtract` without real HTTP
**Solution:** Type class for HTTP operations

**Implementation:**
```haskell
-- Create HTTP abstraction
class Monad m => HTTPClient m where
  httpGet :: String -> m Lazy.ByteString
  httpPost :: String -> [Wreq.Part] -> m Lazy.ByteString

-- Production implementation
instance (MonadIO m, MonadReader e m, HasMyEnv e, MonadMask m)
    => HTTPClient (RIO e) where
  httpGet url = do
    sess <- asks scrapingSession
    resp <- liftIO $ Sess.get sess url
    pure $ resp ^. responseBody

  httpPost url parts = do
    sess <- asks telegramSession
    resp <- liftIO $ Sess.post sess url parts
    pure $ resp ^. responseBody

-- Test implementation
newtype TestHTTP a = TestHTTP { runTestHTTP :: State MockHTTPState a }
  deriving (Functor, Applicative, Monad)

data MockHTTPState = MockHTTPState
  { mockResponses :: Map String Lazy.ByteString
  , mockCalls :: [String]  -- Track what was called
  }

instance HTTPClient TestHTTP where
  httpGet url = TestHTTP $ do
    modify (\s -> s { mockCalls = url : mockCalls s })
    gets (Map.findWithDefault "" url . mockResponses)

  httpPost url _ = TestHTTP $ do
    modify (\s -> s { mockCalls = url : mockCalls s })
    gets (Map.findWithDefault "" url . mockResponses)
```

**Refactor functions to use type class:**
```haskell
loadEntries :: HTTPClient m => m [Entry]
loadEntries = do
  body <- httpGet uri
  pure $ extractEntries (decodeLatin1 body)

loadDetailsAndExtract :: HTTPClient m => Text -> m Details
loadDetailsAndExtract url = do
  body <- httpGet (Text.unpack url)
  case extractDetails url (decodeLatin1 body) of
    Nothing -> error "Failed to extract details"
    Just d -> pure d
```

**Write tests:**
```haskell
testCase "loadEntries with mocked HTTP" $ do
  fixture <- BS.readFile "fixtures/search.html"
  let mockState = MockHTTPState
        { mockResponses = Map.singleton uri fixture
        , mockCalls = []
        }
      (entries, finalState) = runState (runTestHTTP loadEntries) mockState
  length entries @?= 150
  mockCalls finalState @?= [uri]
```

**Impact:** Enable isolated testing of all HTTP-dependent functions
**Files Changed:** dog-bot.hs (major refactor, ~50 lines)

---

#### 5.2 Abstract Telegram API (HIGH IMPACT)
**Problem:** Cannot test `sendPics`, `sendVideos`, `sendLink` without Telegram API
**Solution:** Type class for Telegram operations

**Implementation:**
```haskell
class Monad m => TelegramAPI m where
  sendMediaGroup :: ChatId -> [Wreq.Part] -> m ()
  sendMessage :: ChatId -> Text -> m ()

-- Production implementation
instance (MonadIO m, MonadReader e m, HasMyEnv e, MonadMask m)
    => TelegramAPI (RIO e) where
  sendMediaGroup chatId parts = do
    -- Existing sendMediaGroup logic
    token <- asks (token . envToken)
    -- ... rest of implementation

  sendMessage chatId msg = do
    -- Existing telegramSendMessage logic
    -- ... rest of implementation

-- Test implementation
data TelegramCall
  = MediaGroupSent ChatId Int  -- chatId, number of parts
  | MessageSent ChatId Text
  deriving (Show, Eq)

newtype TestTelegram a = TestTelegram
  { runTestTelegram :: State [TelegramCall] a }
  deriving (Functor, Applicative, Monad)

instance TelegramAPI TestTelegram where
  sendMediaGroup chatId parts = TestTelegram $
    modify (MediaGroupSent chatId (length parts) :)

  sendMessage chatId msg = TestTelegram $
    modify (MessageSent chatId msg :)
```

**Refactor send functions:**
```haskell
sendPics :: (TelegramAPI m, HTTPClient m, MonadIO m)
         => Details -> m ()
sendPics d = do
  -- Download pics (using HTTPClient)
  -- Build parts
  chatId <- getChatId
  sendMediaGroup chatId parts

sendLink :: TelegramAPI m => Details -> m ()
sendLink d = do
  chatId <- getChatId
  sendMessage chatId (detailsUri d)
```

**Write tests:**
```haskell
testCase "sendLink sends message to Telegram" $ do
  let details = Details "http://example.com" Nothing [] [] Map.empty Nothing
      calls = execState (runTestTelegram (sendLink details)) []
  calls @?= [MessageSent (ChatId "test") "http://example.com"]

testCase "sendPics sends media group" $ do
  let details = Details "uri" Nothing ["pic1.jpg", "pic2.jpg"] [] Map.empty Nothing
      -- Need to mock HTTP for pic downloads
      calls = -- ... run sendPics with mocks
  length calls @?= 1
```

**Impact:** Enable isolated testing of Telegram integration
**Files Changed:** dog-bot.hs (major refactor, ~80 lines)

---

#### 5.3 Dry-Run Mode (CRITICAL for Manual Testing)
**Problem:** Must send real Telegram messages to test
**Solution:** Environment flag to mock Telegram calls

**Implementation:**
```haskell
-- Add to CLI options
data Command = Run | Test | DryRun

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Run -> runBot
    Test -> runTests
    DryRun -> runBotDryRun

runBotDryRun :: IO ()
runBotDryRun = do
  putStrLn "=== DRY RUN MODE - No Telegram messages will be sent ==="
  -- Run bot with TestTelegram implementation instead of production
  runWithMockTelegram loadAndProcessEntries

runWithMockTelegram :: TestTelegram a -> IO a
runWithMockTelegram action = do
  let (result, calls) = runState (runTestTelegram action) []
  putStrLn "\n=== Telegram Calls That Would Be Made ==="
  forM_ (reverse calls) $ \call ->
    putStrLn $ "  " <> show call
  pure result
```

**Usage:**
```bash
# Test locally without spamming Telegram
nix run . -- dry-run

# Output:
# === DRY RUN MODE - No Telegram messages will be sent ===
# Processing entries...
# === Telegram Calls That Would Be Made ===
#   MessageSent (ChatId "...") "Hunde vom 2025-11-04"
#   MediaGroupSent (ChatId "...") 5
#   MessageSent (ChatId "...") "Luna: http://..."
```

**Impact:** CRITICAL - Enable safe local testing and development
**Files Changed:** dog-bot.hs (add dry-run mode, ~50 lines)

---

### Phase 6: Integration Tests (3-4 days)

#### 6.1 End-to-End Test with Fixtures
**Scope:** Test complete workflow without external dependencies

**Implementation:**
```haskell
integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testCase "Process single entry end-to-end" $ do
      searchHTML <- BS.readFile "fixtures/search.html"
      detailHTML <- BS.readFile "fixtures/no-video.html"

      let mockHTTP = MockHTTPState
            { mockResponses = Map.fromList
                [ (uri, searchHTML)
                , ("http://detail-url", detailHTML)
                ]
            , mockCalls = []
            }

      (telegramCalls, httpCalls) <- runMockedBot mockHTTP $ do
        entries <- loadEntries
        -- Process first entry
        processEntry 1 (head entries)

      -- Verify HTTP calls were made
      httpCalls `shouldContain` [uri]

      -- Verify Telegram calls
      length (filter isMediaGroup telegramCalls) @?= 1
      length (filter isMessage telegramCalls) @?= 1

  , testCase "Skip filtered breeds end-to-end" $ do
      -- Detail page with forbidden breed
      detailHTML <- BS.readFile "fixtures/no-video.html"
      -- Modify to have "Bracke" in race field
      let modifiedHTML = replaceRace detailHTML "Bracke"
          mockHTTP = MockHTTPState
            { mockResponses = Map.singleton "http://detail-url" modifiedHTML
            , mockCalls = []
            }

      telegramCalls <- runMockedBot mockHTTP $ do
        entry <- pure $ Entry (read "2025-11-04") "http://detail-url"
        processEntry 1 entry

      -- Should only send link, not media
      length (filter isMediaGroup telegramCalls) @?= 0
      length (filter isMessage telegramCalls) @?= 1

  , testCase "Handle network failures gracefully" $ do
      let mockHTTP = MockHTTPState
            { mockResponses = Map.empty  -- No responses = failure
            , mockCalls = []
            }

      -- Should not crash
      result <- try @SomeException $ runMockedBot mockHTTP loadEntries
      case result of
        Left _ -> pure ()  -- Expected failure
        Right _ -> assertFailure "Should have failed"
  ]
```

**Files Changed:** dog-bot.hs (add integration test suite)

---

#### 6.2 Test Retry Logic
**Function:** `withRetry` (line 384)
**Current Coverage:** 0%
**Target:** Validate exponential backoff

**Implementation:**
```haskell
testGroup "Retry Logic"
  [ testCase "Retry on 500 status code" $ do
      let attempts = newIORef 0
      result <- withMockRetry $ do
        count <- readIORef attempts
        modifyIORef' attempts (+1)
        if count < 2
          then throwIO $ StatusCodeException "500 Server Error"
          else pure "success"
      result @?= "success"
      finalAttempts <- readIORef attempts
      finalAttempts @?= 3

  , testCase "Retry on 429 with Retry-After header" $ do
      -- Mock time tracking
      let startTime = getCurrentTime
      result <- withMockRetry $ do
        -- First attempt: 429 with Retry-After: 2
        -- Should wait 2 seconds before retry
        throwIO $ StatusCodeException "429 Too Many Requests"
      -- Verify backoff delay was applied
      endTime <- getCurrentTime
      diffUTCTime endTime startTime >= 2 @?= True

  , testCase "Stop retrying after max attempts" $ do
      let attempts = newIORef 0
      result <- try @SomeException $ withMockRetry $ do
        modifyIORef' attempts (+1)
        throwIO $ StatusCodeException "500 Server Error"

      finalAttempts <- readIORef attempts
      finalAttempts @?= 6  -- 1 initial + 5 retries
      isLeft result @?= True

  , testCase "Don't retry on 404" $ do
      let attempts = newIORef 0
      result <- try @SomeException $ withMockRetry $ do
        modifyIORef' attempts (+1)
        throwIO $ StatusCodeException "404 Not Found"

      finalAttempts <- readIORef attempts
      finalAttempts @?= 1  -- No retries
      isLeft result @?= True
  ]
```

**Files Changed:** dog-bot.hs (add retry logic tests)

---

#### 6.3 Test Rate Limiting
**Function:** `withToken` (line 416)
**Current Coverage:** 0%
**Target:** Validate token bucket behavior

**Implementation:**
```haskell
testGroup "Rate Limiting"
  [ testCase "Token bucket allows burst requests" $ do
      bucket <- newTokenBucket
      start <- getCurrentTime

      -- First request should be immediate
      withTestToken bucket $ pure ()

      mid <- getCurrentTime
      diffUTCTime mid start < 0.1 @?= True  -- < 100ms

  , testCase "Token bucket enforces rate limit" $ do
      bucket <- newTokenBucket
      start <- getCurrentTime

      -- Two requests should take ~5 seconds
      withTestToken bucket $ pure ()
      withTestToken bucket $ pure ()

      end <- getCurrentTime
      let elapsed = diffUTCTime end start
      elapsed >= 4.9 && elapsed <= 5.5 @?= True  -- ~5s ±500ms

  , testCase "Concurrent requests respect rate limit" $ do
      bucket <- newTokenBucket
      start <- getCurrentTime

      -- 3 concurrent requests should serialize
      results <- forConcurrently [1..3] $ \_ ->
        withTestToken bucket $ pure ()

      end <- getCurrentTime
      let elapsed = diffUTCTime end start
      elapsed >= 9.8 && elapsed <= 10.5 @?= True  -- ~10s (2 waits)
  ]
```

**Files Changed:** dog-bot.hs (add rate limit tests)

---

## Test Fixtures Management

### Phase 7: Fixture Improvements (1-2 days)

#### 7.1 Add Missing Fixtures
**Current Fixtures:**
- ✅ fixtures/search.html (150 entries)
- ✅ fixtures/no-video.html
- ✅ fixtures/video-direct.html
- ✅ fixtures/video-youtube.html
- ✅ fixtures/pics.html (unused?)

**Missing Fixtures:**
- ❌ Forbidden breed detail pages (bracke, dackel, malteser, englisch setter)
- ❌ Empty search results
- ❌ Malformed HTML (missing fields)
- ❌ Entry with no pictures
- ❌ Entry with many pictures (10+)
- ❌ Entry with mix of YouTube and direct videos

**Create:**
```bash
fixtures/
  search-empty.html          # No results
  search-single.html         # 1 result only
  detail-bracke.html         # Forbidden breed
  detail-no-pics.html        # No pictures
  detail-many-pics.html      # 15 pictures
  detail-malformed.html      # Missing fields
  detail-unicode.html        # Special chars in fields
```

**Files Created:** 7 new fixture files

---

#### 7.2 Fixture Generation Script
**Problem:** Fixtures can become outdated
**Solution:** Script to fetch fresh samples

**Create: scripts/update-fixtures.sh**
```bash
#!/usr/bin/env bash
set -euo pipefail

# Fetch fresh search results (requires network)
curl -s "$SEARCH_URL" > fixtures/search.html.new

# Optionally fetch detail pages
# (Don't overwrite existing fixtures automatically)

echo "Updated fixtures saved with .new extension"
echo "Review and rename to replace existing fixtures"
```

**Files Created:** scripts/update-fixtures.sh

---

#### 7.3 Fixture Validation Tests
**Ensure fixtures remain valid**

**Implementation:**
```haskell
testGroup "Fixture Validation"
  [ testCase "All fixtures are valid UTF-8/Latin-1" $ do
      files <- glob "fixtures/*.html"
      forM_ files $ \file -> do
        content <- BS.readFile file
        -- Should not throw
        let decoded = decodeLatin1 content
        Lazy.length decoded > 0 @?= True

  , testCase "All fixtures are well-formed HTML" $ do
      files <- glob "fixtures/*.html"
      forM_ files $ \file -> do
        content <- decodeLatin1 <$> BS.readFile file
        -- Should parse without errors
        let parsed = content ^.. html
        not (null parsed) @?= True

  , testCase "Search fixtures contain expected structure" $ do
      searchFiles <- glob "fixtures/search*.html"
      forM_ searchFiles $ \file -> do
        content <- decodeLatin1 <$> BS.readFile file
        let entries = extractEntries content
        -- All entries should have valid structure
        all (\(Entry d u) -> not (Text.null u)) entries @?= True
  ]
```

**Files Changed:** dog-bot.hs (add fixture validation tests)

---

## Golden Testing

### Phase 8: Snapshot Tests (2 days)

#### 8.1 Add Golden Tests for Extraction
**Use tasty-golden to detect unintended changes**

**Implementation:**
```haskell
import Test.Tasty.Golden (goldenVsString)

goldenTests :: TestTree
goldenTests = testGroup "Golden Tests"
  [ goldenVsString "Extract entries from search.html"
      "fixtures/golden/entries.json" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/search.html"
        let entries = extractEntries input
        pure $ Aeson.encode entries

  , goldenVsString "Extract details from no-video.html"
      "fixtures/golden/no-video-details.json" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/no-video.html"
        let details = extractDetails "uri" input
        pure $ Aeson.encode details

  , goldenVsString "Extract profile attributes"
      "fixtures/golden/profile.json" $ do
        input <- decodeLatin1 <$> BS.readFile "fixtures/no-video.html"
        let details = extractDetails "uri" input
        pure $ Aeson.encode (detailsProfile details)
  ]
```

**Benefits:**
- Detect unintended changes in parsing
- Visual diff of changes
- Easy to review and accept changes

**Files Created:** fixtures/golden/*.json (new directory)
**Files Changed:** dog-bot.hs (add golden tests), needs ToJSON instances

---

## Test Quality Improvements

### Phase 9: Test Maintainability (1-2 days)

#### 9.1 Add Test Helpers and Builders
**Problem:** Repetitive test setup code
**Solution:** Helper functions and builders

**Implementation:**
```haskell
-- Test helpers
mkDetails :: Text -> Details
mkDetails race = Details "http://test.com" Nothing [] []
  (Map.fromList [("Rasse", race)]) Nothing

mkDetailsWithPics :: Text -> [Text] -> Details
mkDetailsWithPics race pics = Details "http://test.com" Nothing pics []
  (Map.fromList [("Rasse", race)]) Nothing

mkEntry :: Day -> Text -> Entry
mkEntry = Entry

-- HTTP mock helpers
mockHTTPSuccess :: String -> Lazy.ByteString -> MockHTTPState
mockHTTPSuccess url body = MockHTTPState
  { mockResponses = Map.singleton url body
  , mockCalls = []
  }

mockHTTPFailure :: String -> MockHTTPState
mockHTTPFailure url = MockHTTPState
  { mockResponses = Map.empty
  , mockCalls = []
  }

-- Assertion helpers
(@?~) :: Text -> Text -> Assertion
actual @?~ expected =
  Text.toLower actual @?= Text.toLower expected

shouldContainSubstring :: Text -> Text -> Assertion
haystack `shouldContainSubstring` needle =
  assertBool (Text.unpack needle <> " not found in " <> Text.unpack haystack) $
    needle `Text.isInfixOf` haystack
```

**Files Changed:** dog-bot.hs (add test helper section)

---

#### 9.2 Add Test Documentation
**Document test organization and conventions**

**Create: test/README.md**
```markdown
# Test Organization

## Running Tests
```bash
nix run . -- test                    # All tests
nix run . -- test --pattern="Parse"  # Filter tests
TEST_PATTERN="breed" nix run . -- test
```

## Test Structure
- Unit Tests: Pure function tests
- Property Tests: Generative tests (Hedgehog)
- Integration Tests: Multi-component tests with mocks
- Golden Tests: Snapshot tests for parsing

## Fixtures
Located in `fixtures/`:
- `search*.html` - Search result pages
- `detail-*.html` - Detail pages (various scenarios)
- `golden/*.json` - Expected outputs for golden tests

## Adding New Tests
1. Identify the function to test
2. Choose test type (unit/property/integration)
3. Add to appropriate test group
4. Run tests: `nix run . -- test`
5. Update this README if adding new fixture

## Test Helpers
See `Test Helpers` section in dog-bot.hs:
- `mkDetails` - Create test Details
- `mockHTTPSuccess` - Mock successful HTTP
- Custom assertions: `@?~`, `shouldContainSubstring`
```

**Files Created:** test/README.md

---

## Implementation Priority

### 🔴 Critical (Do First) - 1 week
1. ✅ CLI test command (1.1) - 1 hour
2. ✅ Tests in CI (1.2) - 1 hour
3. ✅ Breed filter tests (3.1) - 2 hours
4. ✅ Dry-run mode (5.3) - 4 hours
5. ✅ Abstract Telegram API (5.2) - 1 day

**Goal:** Make testing easy + enable local testing

### 🟡 High Priority - 2 weeks
6. ✅ Abstract HTTP operations (5.1) - 2 days
7. ✅ Media name tests (3.2) - 1 hour
8. ✅ Profile attribute tests (3.4) - 2 hours
9. ✅ Integration tests (6.1) - 2 days
10. ✅ Test helpers (9.1) - 3 hours

**Goal:** Comprehensive test coverage for core functionality

### 🟢 Medium Priority - 1 month
11. ⏳ Property tests (4.1) - 2 days
12. ⏳ Retry logic tests (6.2) - 1 day
13. ⏳ Rate limit tests (6.3) - 1 day
14. ⏳ Golden tests (8.1) - 2 days
15. ⏳ Date parsing tests (3.3) - 1 day

**Goal:** Robust testing infrastructure

### 🔵 Nice to Have
16. ⏳ Separate test module (2.1) - 1 day
17. ⏳ Missing fixtures (7.1) - 1 day
18. ⏳ Fixture generation (7.2) - 2 hours
19. ⏳ Fixture validation (7.3) - 2 hours
20. ⏳ Test documentation (9.2) - 2 hours

**Goal:** Long-term maintainability

---

## Success Metrics

### After Critical Phase:
- ✅ Tests run with single command
- ✅ CI runs tests on every commit
- ✅ Can test locally without Telegram API
- ✅ Breed filtering has 10+ test cases
- ✅ Telegram operations are mocked

### After High Priority:
- ✅ 60%+ code coverage
- ✅ All pure functions have unit tests
- ✅ HTTP operations are mocked and testable
- ✅ Integration tests cover main workflows
- ✅ Test helpers reduce boilerplate

### After Medium Priority:
- ✅ 80%+ code coverage
- ✅ Property tests find edge cases
- ✅ Retry/rate-limit logic validated
- ✅ Golden tests prevent regressions
- ✅ All fixtures are validated

### After Nice to Have:
- ✅ Tests in separate module
- ✅ Comprehensive fixture library
- ✅ Documented test conventions
- ✅ Easy to add new tests

---

## Next Steps

**Start Here:**
1. Implement CLI test command (1 hour)
2. Add tests to CI (30 minutes)
3. Add comprehensive breed filter tests (2 hours)
4. Implement dry-run mode (4 hours)

**First Day Target:** Items 1-3 complete, can run tests easily

**First Week Target:** Critical phase complete, can test locally

**First Month Target:** High priority complete, 60%+ coverage

Would you like me to start implementing the critical phase improvements?

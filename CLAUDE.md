# Agent Documentation

This file provides guidance to coding agents when working with code in this repository.

## Project Overview

A Telegram bot that scrapes dog adoption listings from tiervermittlung.de, filters based on breed preferences, and sends daily notifications with photos, videos, and links. Single-file Haskell application (~490 lines) with embedded tests.

**Key behavior:**
- Runs daily at 3:30 AM via GitHub Actions
- Fetches yesterday's dog listings from tiervermittlung.de
- Filters out specific breeds (englisch setter, bracke, malteser, dackel)
- Sends media groups (photos/videos) and links to configured Telegram chat
- Rate-limits Telegram API calls and retries failed HTTP requests

## Development Requirements

**Environment:**
- Nix with flakes enabled
- direnv (`.envrc` configured with `use flake`)
- Two environment variables for runtime:
  - `TELEGRAM_BOT_TOKEN` - Telegram bot authentication
  - `TELEGRAM_CHAT_ID` - Target chat for notifications

**Development shell:**
```bash
# Enter development shell (if direnv not active)
nix develop

# Or with direnv
direnv allow
```

The dev shell includes:
- GHC with all required packages (lens, wreq, rio, taggy-lens, etc.)
- haskell-language-server for IDE support
- Testing libraries (tasty, tasty-golden, tasty-hedgehog, tasty-hunit, tasty-hspec)

## Building and Running

### Build
```bash
# Build the bot script
nix build

# Result symlink points to executable wrapper
./result/bin/dog-bot
```

### Run Directly
```bash
# Run without building (requires env vars)
nix run

# Or run the Haskell script directly in dev shell
runhaskell dog-bot.hs
```

### Run Tests
The `dog-bot.hs` file contains both the bot implementation and tests. Tests use the Tasty framework and HTML fixtures in `fixtures/`:

```bash
# In dev shell, load in GHCi to run tests interactively
ghci dog-bot.hs

# Then in GHCi
> runTests

# To run as script (modify main function to call runTests instead of runBot)
```

**Test fixtures:**
- `fixtures/search.html` - Search results page (150 entries)
- `fixtures/no-video.html` - Entry without video
- `fixtures/video-direct.html` - Entry with direct video link
- `fixtures/video-youtube.html` - Entry with YouTube embed
- `fixtures/pics.html` - Entry with multiple pictures

**Test coverage:**
- Entry parsing from search results
- Detail extraction (title, pictures, videos, profile attributes)
- Race filtering logic
- Media name extraction
- Handles both YouTube embeds and direct video URLs

## Architecture

**Single-file structure:**
- Main execution: `main` → `runBot` → `loadAndProcessEntries`
- Configuration: `MyEnv` record with Reader monad for dependency injection
- Data flow: scrape search → extract entries → load details → filter → send to Telegram

**Key components:**

1. **Entry scraping** (`loadEntries`)
   - Fetches search results from hardcoded tiervermittlung.de URL (filtered for female dogs, 3-15 months)
   - Parses HTML with taggy-lens to extract date and link for each listing
   - Returns list of `Entry` records (date + link)

2. **Detail extraction** (`loadDetailsAndExtract`)
   - Fetches individual dog listing page
   - Extracts: title, photos, videos (YouTube/direct), profile attributes, description
   - Returns `Details` record with all parsed information

3. **Filtering** (`checkDetail`)
   - Excludes listings with forbidden breed keywords in race field
   - Forbidden: "englisch setter", "bracke", "brake", "malteser", "dackel"
   - Case-insensitive substring matching

4. **Telegram integration** (`sendPics`, `sendVideos`, `sendLink`)
   - Downloads media to temp directories
   - Sends as media groups using Telegram Bot API
   - Rate-limited via token bucket (1 request per 5 seconds burst)
   - Retries on 429 or 5xx status codes with exponential backoff

5. **HTTP resilience** (`withRetry`, `retryStatusException`)
   - Retry policy: full jitter backoff starting at 1s, max 5 retries
   - Respects `Retry-After` headers from Telegram API
   - Also retries "Bad Request: group send failed" errors
   - Separate HTTP sessions for Telegram and tiervermittlung.de

**Dependencies:**
- `taggy-lens` - HTML parsing with lens; built from official repo's master branch (lens-5 support merged but not released to Hackage)
- `token-bucket` - Rate limiting (requires jailbreak due to outdated time package upper bound)
- Standard: wreq, lens, rio, retry, logging, aeson, temporary

## Common Workflows

### Testing local changes
```bash
# 1. Enter dev shell
nix develop

# 2. Modify dog-bot.hs

# 3. Quick syntax check
runhaskell dog-bot.hs  # Will fail on missing env vars but validates syntax

# 4. Run tests by temporarily changing main
# Edit: main = runTests  (instead of main = runBot)
runhaskell dog-bot.hs

# 5. Test with real bot (requires credentials)
export TELEGRAM_BOT_TOKEN="your-token"
export TELEGRAM_CHAT_ID="your-chat-id"
runhaskell dog-bot.hs
```

### Updating dependencies
```bash
# Update flake inputs (nixpkgs, flake-utils)
nix flake update

# Test build still works
nix build

# Commit flake.lock changes
git add flake.lock && git commit -m "Flake Update"
```

### Updating search criteria
The search URL is hardcoded in `uri` constant (line 95-96). Current filters:
- `Tierart=Hund` - Dog type
- `Geschlecht=weiblich` - Female
- `Alter-gt=3&Alter-lt=15.1&Zeitwert=Monate` - Age 3-15 months
- `sb=0&so=descend` - Sort by date descending

To modify search parameters, edit the URL query string and test with fixtures.

### Adding/removing breed filters
Edit `forbiddenKeywords` list in `checkDetail` function (line 253). Matching is case-insensitive substring search against the "Rasse" profile field.

## Code Conventions

**Lens usage:**
- Extensive use of lens operators (`^.`, `^..`, `^?`) for data extraction
- `taggy-lens` provides HTML traversal primitives
- Custom helpers: `matchId`, `matchClass`, `matchAttr` for element selection

**Error handling:**
- `try @m @SomeException` catches all exceptions per entry
- Individual entry failures don't stop batch processing
- Logging at multiple levels (Debug, Info, Error) with `Control.Logging`

**Testing:**
- Tests inline with main code (bottom of file, lines 425-492)
- Golden test fixtures in `fixtures/` directory
- `runTests` function for manual test execution

**Nix integration:**
- Shebang at top supports standalone script execution via nix-shell (currently references old shell.nix)
- Flake provides both development shell and production wrapper script
- `writeScriptBin` creates executable wrapper that runs via `runhaskell`

## GitHub Actions

**run-bot-haskell.yml:**
- Schedule: Daily at 3:30 AM UTC
- Manual trigger: `workflow_dispatch`
- Runs `nix run` with secrets from GitHub

**upgrade.yml:**
- Schedule: Daily at 3:30 AM UTC
- Auto-updates flake.lock and commits changes
- Ensures dependencies stay current

## Known Quirks

1. **taggy-lens from git**: Uses official `alpmestan/taggy-lens` repo's master branch instead of Hackage because lens-5 compatibility fix (PR #7, merged April 2022) hasn't been released to Hackage yet. Pinned to commit `87235bfb9c3ee8b3d487c1cf48a22f247e59286d` for reproducibility. The package works fine; maintainer just hasn't published a new version.

2. **token-bucket jailbreak**: Requires `doJailbreak` + `unmarkBroken` because its cabal file has outdated upper bound `time < 1.13` while current nixpkgs has `time-1.15`. The package actually works fine with newer time versions; only the version constraint is outdated. Package is marked broken in nixpkgs due to this constraint, hence the workaround. See: https://github.com/haskell-hvr/token-bucket

3. **Latin-1 encoding**: tiervermittlung.de uses Latin-1 encoding, decoded via `decodeLatin1` before parsing.

4. **Hardcoded rate limiting**: Telegram API rate limit hardcoded to 1 request per 5 seconds (conservative to avoid hitting limits).

5. **Undo-tree backups**: Repository contains `.~undo-tree~` files from editor backups (not in .gitignore).

6. **Tests require manual execution**: Tests are embedded but must be manually invoked by changing `main` function. Consider adding a CLI flag or separate test entrypoint.

7. **Result symlink**: `nix build` creates `result` symlink in repo root (gitignored).

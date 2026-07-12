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
- Environment variables for runtime:
  - `TELEGRAM_BOT_TOKEN` - Telegram bot authentication
  - `TELEGRAM_CHAT_ID` - Target chat for notifications
  - `DRY_RUN` (optional) - Set to any non-empty value to log what would be sent instead of calling Telegram; makes the two variables above optional

**Development shell:**
```bash
# Enter development shell (if direnv not active)
nix develop

# Or with direnv
direnv allow
```

The dev shell includes:
- GHC with all required packages (lens, wreq, taggy-lens, etc.)
- haskell-language-server for IDE support (dev shell only, not in the build closure)
- Testing libraries (tasty, tasty-hunit)

## Building and Running

### Build
```bash
# Compile the bot with GHC (also serves as a typecheck gate)
nix build

# Result symlink points to the compiled binary
./result/bin/dog-bot
```

### Run Directly
```bash
# Run without building (requires env vars, or DRY_RUN=1)
nix run

# Dry run: scrape and log, but send nothing to Telegram (no credentials needed)
DRY_RUN=1 nix run

# Or run the Haskell script directly in dev shell
runhaskell dog-bot.hs
```

### Run Tests
The `dog-bot.hs` file contains both the bot implementation and tests. Tests use the Tasty framework and HTML fixtures in `fixtures/`. The `test` argument dispatches to the test suite (extra args are passed through to Tasty):

```bash
# After nix build
./result/bin/dog-bot test

# Or in the dev shell
runhaskell dog-bot.hs test

# Interactively in GHCi
ghci dog-bot.hs
> runTests
```

Tests also run in CI (`ci.yml`) on every push.

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
   - Link message (`linkMessage`) includes title plus Rasse/Alter/Aufenthalt/Land profile fields
   - Rate-limited via token bucket (1 request per 5 seconds burst)
   - Retries on 429 or 5xx status codes with exponential backoff
   - `DRY_RUN` env var short-circuits both send functions to log-only

5. **HTTP resilience** (`withRetry`, `retryStatusException`)
   - Retry policy: full jitter backoff starting at 1s, max 5 retries
   - Respects `Retry-After` headers from Telegram API
   - Also retries "Bad Request: group send failed" errors
   - Retries transient network errors (connection/response timeouts, connection failures/closes)
   - Separate HTTP sessions for Telegram and tiervermittlung.de
   - Logged exceptions are passed through `redactToken` because Telegram HTTP errors embed the bot token in the request URL

**Dependencies:**
- `taggy-lens` - HTML parsing with lens; Hackage version jailbroken to drop a stale `lens < 5` upper bound (see Known Quirks)
- `token-bucket` - Rate limiting (requires jailbreak due to outdated time package upper bound)
- Standard: wreq, lens, retry, logging, aeson, temporary

## Common Workflows

### Testing local changes
```bash
# 1. Enter dev shell
nix develop

# 2. Modify dog-bot.hs

# 3. Typecheck (compiles with -Wall)
nix build

# 4. Run tests
runhaskell dog-bot.hs test

# 5. Dry run against the live site without sending anything to Telegram
DRY_RUN=1 runhaskell dog-bot.hs

# 6. Test with real bot (requires credentials)
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
The search URL is hardcoded in the `uri` constant. Current filters:
- `Tierart=Hund` - Dog type
- `Geschlecht=weiblich` - Female
- `Alter-gt=3&Alter-lt=15.1&Zeitwert=Monate` - Age 3-15 months
- `sb=0&so=descend` - Sort by date descending

To modify search parameters, edit the URL query string and test with fixtures.

### Adding/removing breed filters
Edit `forbiddenKeywords` list in `checkDetail` function. Matching is case-insensitive substring search against the "Rasse" profile field.

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
- Tests inline with main code (bottom of file)
- HTML test fixtures in `fixtures/` directory
- Run via `dog-bot test` argument (or `runTests` in GHCi)

**Nix integration:**
- Flake provides a development shell (with haskell-language-server) and a compiled binary
- `nix build` compiles `dog-bot.hs` with `ghc -Wall`, so it doubles as a typecheck gate

## GitHub Actions

**run-bot-haskell.yml:**
- Schedule: Daily at 3:30 AM UTC
- Manual trigger: `workflow_dispatch`
- Builds (typechecks), then runs `nix run` with secrets from GitHub

**upgrade.yml:**
- Schedule: Daily at 2:30 AM UTC (staggered an hour before the bot run)
- Auto-updates flake.lock, builds, and commits changes
- Ensures dependencies stay current

**ci.yml:**
- Runs on every push and via `workflow_dispatch`
- `nix build` (compile + typecheck) followed by the test suite

## Known Quirks

1. **taggy-lens jailbreak**: Uses Hackage's `taggy-lens` (in nixpkgs) with `doJailbreak` to drop its stale `lens < 5` upper bound. The code compiles cleanly against lens-5; only the cabal constraint is outdated (the fix, PR #7, was merged April 2022 but never released to Hackage). Previously this was pinned to a git commit of the maintainer's master branch — the jailbreak is equivalent and needs no external checkout.

2. **token-bucket jailbreak**: Requires `doJailbreak` + `unmarkBroken` because its cabal file has outdated upper bound `time < 1.13` while current nixpkgs has `time-1.15`. The package actually works fine with newer time versions; only the version constraint is outdated. Package is marked broken in nixpkgs due to this constraint, hence the workaround. See: https://github.com/haskell-hvr/token-bucket

3. **Latin-1 encoding**: tiervermittlung.de uses Latin-1 encoding, decoded via `decodeLatin1` before parsing.

4. **Hardcoded rate limiting**: Telegram API rate limit hardcoded to 1 request per 5 seconds (conservative to avoid hitting limits).

5. **Result symlink**: `nix build` creates `result` symlink in repo root (gitignored).

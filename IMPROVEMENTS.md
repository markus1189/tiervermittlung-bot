# Improvement Ideas — Brainstorm

Rated by **Value** (impact on reliability, usefulness, maintainability) and
**Complexity** (effort to implement). Scale: ⭐ low → ⭐⭐⭐ high value;
🔧 small → 🔧🔧🔧 large effort. Grouped by theme, roughly ordered by
value-per-effort within each group.

Quick-win shortlist (high value, low effort): **1, 2, 5, 8, 14, 19, 24, 30**.

---

## Correctness & reliability (bugs and near-bugs)

### 1. Bot token can leak into GitHub Actions logs — ⭐⭐⭐ / 🔧
`processEntry` logs `show e` for any `SomeException`. For Telegram calls,
`HttpExceptionRequest` includes the full request, whose path contains
`bot<TELEGRAM_BOT_TOKEN>`. A failed Telegram request after retries therefore
prints the bot token into public Actions logs (GitHub masks exact secret
matches, but only in some renderings, and not if the token is embedded in a
longer `Show` output with escaping). Fix: sanitize exceptions before logging
(strip/redact the token substring), or log only status code + response body
for HTTP errors.

### 2. `nix build` never typechecks the bot — ⭐⭐⭐ / 🔧🔧
`packages.default` is just a `writeScriptBin` wrapper around `runhaskell`,
so `nix build` succeeds even if `dog-bot.hs` doesn't compile. Consequence:
the nightly `upgrade.yml` bumps nixpkgs, runs `nix build` "successfully",
commits — and the bot can then fail at 3:30 AM with a type error against the
new library versions, silently, every night thereafter. Fix: compile the
script in the build (e.g. `pkgs.runCommand` with `ghc -Wall -Werror -o` or
convert to a small cabal project via `callCabal2nix`). Bonus: compiled
startup is much faster than `runhaskell` re-interpreting ~490 lines daily.

### 3. No dedup / persisted state → missed or duplicated notifications — ⭐⭐⭐ / 🔧🔧
The bot's only memory is "yesterday's date". Two failure modes:
- If a run fails (site down, Actions outage, broken upgrade), that day's
  dogs are never sent — the next run only looks at its own yesterday.
- A manual `workflow_dispatch` re-run re-sends everything for the day.
Fix: persist the set of already-sent listing IDs (or last successful date)
— options: a state file committed to a `state` branch, GitHub Actions
cache/artifact, or a gist. Then process "everything since last success,
minus already-sent".

### 4. Telegram media groups are capped at 10 items — ⭐⭐ / 🔧
`sendPics`/`sendVideos` send *all* pics in one `sendMediaGroup`. Telegram
rejects groups with more than 10 media (and requires ≥2 for a true album).
A listing with 11+ photos fails the whole media send (retry loop even
recognizes "group send failed" and retries futilely 5 times). Fix:
`chunksOf 10` and send multiple groups.

### 5. Transient network errors are never retried — ⭐⭐ / 🔧
`retryStatusException` only retries `StatusCodeException`. Connection
timeouts, resets, DNS blips (`ConnectionTimeout`, `ConnectionFailure`,
`ResponseTimeout`) hit the catch-all clause → `DontRetry`. For a bot that
scrapes a small hobbyist site nightly, these are the *most likely* transient
failures. Fix: add these `HttpExceptionContent`s to the retry set.

### 6. Timezone is implicitly the runner's (UTC), site is German — ⭐⭐ / 🔧
`getYesterday` uses `getZonedTime` — UTC on GitHub runners. At the 3:30 UTC
schedule this happens to agree with Berlin's "yesterday", but it breaks if
someone changes the cron (a run at 23:30 UTC would compute the wrong day)
and local dev runs give different results than CI. Fix: pin
`TZ=Europe/Berlin` in the workflow, or better, compute the day explicitly in
`Europe/Berlin` in code.

### 7. Search capped at 150 results, no pagination — ⭐ / 🔧🔧
`mh=150` + descending date sort means a day with >150 matching listings
silently drops the tail. Unlikely at current filter breadth, but becomes
real if filters are widened. Fix: paginate until entries older than
yesterday appear, or at least log a warning when exactly 150 entries parse.

### 8. Hardcoded stale `session=` parameter in search URL — ⭐ / 🔧
The URL carries `session=kNWVQkHlAVH5axV0HJs5`, someone's captured session
value. It appears the site ignores stale values today, but it's a booby
trap. Fix: drop the parameter (and the empty `E1..E10=` noise) and verify
against fixtures.

### 9. Alert when the run itself fails — ⭐⭐ / 🔧
Nobody is told when the nightly run crashes; dogs silently stop arriving.
Fix: an `if: failure()` step in the workflow that sends a plain
`curl` Telegram message ("dog-bot run failed, see $RUN_URL"). Pairs
with #3 for full recovery.

---

## Testing & CI

### 10. Tests are never run automatically — ⭐⭐⭐ / 🔧
Tests exist but require manually editing `main`. Fix in two small steps:
1. Argument dispatch: `main = getArgs >>= \case ["test"] -> runTests; _ -> runBot`.
2. A CI workflow on push/PR that runs `nix develop -c runhaskell dog-bot.hs test`
   (or the compiled test binary once #2 lands).

### 11. Run tests in `upgrade.yml` before committing — ⭐⭐ / 🔧
The auto-upgrade should gate the commit on build **and** tests passing, so a
nixpkgs bump that breaks parsing/compilation never lands on `main`.
Depends on #2/#10 to be meaningful.

### 12. Golden tests for extraction — ⭐ / 🔧
`tasty-golden` is already a dependency (currently unused). Snapshot the full
`Details` records extracted from each fixture. Cheap regression net when
touching lens selectors, better failure output than positional asserts like
`length (detailsProfile details) @?= 6`.

### 13. Fixture freshness check — ⭐ / 🔧🔧
A scheduled job that re-downloads one live search page and one detail page,
runs extraction, and alerts if zero entries / no title parse. Detects the
real killer risk for a scraper: silent site-layout changes. (The bot already
half-detects this — a layout change makes "0 entries" look like a quiet
day.) Could be as simple as: warn in-chat when 0 entries parse from a
non-empty response.

---

## Features

### 14. Richer notification content — ⭐⭐ / 🔧
The link message is just `Title: URL`. The profile map already contains
Rasse, Alter, Ort etc. — include them (`parse_mode=HTML`, a few bold
labels). Value for the human scanning 20 dogs per morning is high; the code
change is one function.

### 15. Run summary message — ⭐⭐ / 🔧
End each run with "X gefunden, Y gesendet, Z gefiltert (Rasse), N Fehler".
Today the header "Hunde vom ..." arrives even when nothing follows, and
errors are invisible unless you read Actions logs. Also nicer: German date
format instead of `show yesterday`'s ISO.

### 16. Configurable filters without code edits — ⭐⭐ / 🔧🔧
Forbidden breeds, age range, gender, and sort are hardcoded (URL string +
`forbiddenKeywords`). Move to env vars or a small TOML/JSON config, and
build the query URL from parts instead of one 500-char literal. Also
enables #17.

### 17. Multiple search profiles / multiple chats — ⭐ / 🔧🔧
Run N searches (e.g. different breeds/regions) each to its own chat. Natural
follow-up of #16; the Reader-env structure makes this a loop over configs.

### 18. Positive filters & highlights — ⭐ / 🔧
E.g. flag `Notfall` listings, or a keyword allowlist that pins ❤️ to
matches. Substring machinery already exists in `checkDetail`.

### 19. Dry-run mode — ⭐⭐ / 🔧
`DRY_RUN=1` (or `main` arg, see #10) that logs what *would* be sent instead
of calling Telegram. Makes local iteration possible without spamming the
real chat or needing a second bot token.

### 20. Interactive Telegram commands — ⭐ / 🔧🔧🔧
`/filter add dackel`, `/pause`, `/search now` via long-polling or webhook.
Big shift: the bot becomes a long-running service (or needs
getUpdates-draining per run), needs state (#3). Fun, but the cron model is
genuinely a good fit for the problem — only worth it if filter churn is
frequent.

### 21. Skip videos that exceed Telegram's upload limit — ⭐ / 🔧
Bot API rejects uploads >50 MB. Check `Content-Length` before downloading;
fall back to sending the video URL as text. Also avoids downloading huge
files into runner memory (`downloadImage` is fully strict).

---

## Code quality

### 22. Remove dead code and duplicate imports — ⭐ / 🔧
`loadDetails` (l.328) is never called; `import Control.Monad (unless)` and
`(void)` are two separate import lines (l.92–93) that belong in one; in
`sendVideos`, `videoNames` are already the result of `mediaName` yet
`mediaName` is applied again when building `mediaJson`, and the
`YoutubeVideo` branch inside the `directVideos` loop is unreachable.

### 23. Drop `-Wno-missing-signatures` / `-Wno-incomplete-patterns` — ⭐ / 🔧
Add the handful of missing type signatures (`extractEntries`,
`extractProfileAttrs`, …) and restructure the video partition so the
unreachable patterns go away (e.g. `mapMaybe` into two typed lists instead
of `partition` + partial lambdas). Then compile `-Wall -Werror` (via #2's
build) so warnings can't rot.

### 24. Prune unused dependencies — ⭐ / 🔧
`rio`, `either`, `unordered-containers`, `tasty-golden` (until #12),
`tasty-hedgehog`, `tasty-hspec` are in `flake.nix` but unused in the code.
Every one costs build time in the nightly upgrade job.

### 25. Fix or remove the stale shebang block — ⭐ / 🔧
Lines 1–6 reference `shell.nix`, which was renamed `old_shell.nix`, so
direct `./dog-bot.hs` execution is broken. Either point it at the flake
(`nix shell .#`-style) or delete the block and `old_shell.nix` together.

### 26. Split the file into modules (only alongside #2) — ⭐ / 🔧🔧
~490 lines is still fine for one file, but if it becomes a cabal project
anyway: `Scrape.hs` (pure extraction), `Telegram.hs`, `Main.hs`, tests in
`test/`. Pure-function separation would also let tests run without the
network deps in scope. Not worth doing while it stays a `runhaskell` script.

### 27. Consider retiring the taggy-lens git pin — ⭐ / 🔧🔧
The dependency is unmaintained and pinned to a 2022 commit; each nixpkgs
bump risks it finally breaking. Alternatives with the same "select by
id/class" ergonomics: `scalpel` or `html-conduit` + `xml-lens`. The fixture
tests (#12 helps) make this a mechanical, verifiable swap. Low urgency —
do it when it breaks, but keep it on the radar.

---

## Repo hygiene & infrastructure

### 28. Add a `.gitignore`; untrack `result` — ⭐⭐ / 🔧
There is no `.gitignore`, and the `result` symlink **is committed**
(CLAUDE.md claims otherwise). Ignore `result`, `.direnv/`, `*.~undo-tree~`,
`dist-newstyle/`; `git rm --cached result`.

### 29. Declare the `nixpkgs` input explicitly — ⭐ / 🔧
`flake.nix` uses `nixpkgs` in `outputs` without declaring it in `inputs`
(it resolves via the implicit registry). Declare
`inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"` so the pin in
`flake.lock` is explicit and `upgrade.yml` updates what you think it
updates. Also: the flake `description` is still "A very basic flake", and
`devShell` is the deprecated attr name (`devShells.default` is current).

### 30. De-conflict the two 3:30 AM cron jobs — ⭐ / 🔧
`run-bot-haskell.yml` and `upgrade.yml` fire at the same minute: the bot
run races the flake update (and pays an uncached eval of the *new* nixpkgs
right when it should be doing its job). Stagger them (upgrade at 02:30,
bot at 03:30) and add a `concurrency:` group. Also fix the cosmetics: the
bot job is named `tests`, and `nix shell -c echo Initialized` is a no-op
that doesn't build the run closure (`nix build` would actually warm it).

### 31. Speed up / cache the Actions builds — ⭐ / 🔧
Add `DeterminateSystems/magic-nix-cache-action` (or Cachix) so the nightly
run doesn't rebuild the GHC closure after every flake bump. Cuts run time
and flake-update build time substantially.

### 32. Pin action versions by SHA & set workflow permissions — ⭐ / 🔧
`actions/checkout@v4` / `cachix/install-nix-action@v31` are tag-pinned;
supply-chain best practice is SHA-pinning. Also set top-level
`permissions: contents: read` (write only for the upgrade job) instead of
the default token scope.

### 33. Update CLAUDE.md as things change — ⭐ / 🔧
It already drifts from reality (gitignore claim, "line 95-96 / 253"
references, undo-tree files that no longer exist). Worth refreshing
whenever items above land, since agents act on it literally.

---

## Wild ideas (explicitly invited)

### 34. Archive every sent listing — ⭐⭐ / 🔧🔧
Listings on tiervermittlung.de disappear once the dog is adopted. Persist
each sent listing (JSON + pics) to a `data/` branch or S3; optionally
render a static "dogs we saw" gallery with GitHub Pages. Sentimental value:
high. Also gives you a dataset for #35/#36.

### 35. Score/rank instead of binary filter — ⭐ / 🔧🔧🔧
Use a small LLM call (or even keyword weights) on the description +
attributes to rank listings by fit ("gut mit Katzen", "für Anfänger"), send
the top ones first with a score. The description text is already extracted
(`detailsAttributes`) but currently unused in messages.

### 36. Duplicate-dog detection — ⭐ / 🔧🔧🔧
Shelters re-post the same dog every few weeks. Fuzzy-match new listings
against the archive (#34) by name + breed + shelter, tag repeats with
"schon mal gesehen am …".

### 37. Trend stats — ⭐ / 🔧🔧
Monthly message: how many puppies posted, breed distribution, average time
to adoption (via re-scraping archived listings for 404s). Zero product
value, high nerd value. Rated "go wild" as requested.

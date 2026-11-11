# =============================================================================
# 06_bundle_app.R — make app/ self-contained for deployment
# -----------------------------------------------------------------------------
# PURPOSE
#   Copy everything the app reads into app/bundle/, so that the app directory
#   can be published on its own.
#
#   Inside the repository the app reads data/processed/ and policies/ from the
#   project root. No hosting option publishes the project root:
#     - shinyapps.io / Posit Connect: rsconnect::deployApp("app") uploads app/
#     - shinylive: shinylive::export("app", "<site>") converts app/ only
#     - Shiny Server: the app directory is what gets copied to the server
#   Published alone, the app used to stop at start-up with "Could not locate
#   the project root". With a bundle, global.R finds no project root and reads
#   app/bundle/ instead (see the DATA_ROOT block there).
#
# WHAT GOES IN, AND WHY ALL OF IT
#   The whole of data/processed/ and policies/, not a hand-kept list of the
#   files global.R happens to read today. A list here would be a second copy of
#   global.R's paths and would drift: the first new data layer added to the app
#   would deploy without its file. data/processed/ is under 2 MB.
#
#   The attribution files travel with the data. facilities.rds incorporates a
#   substantial part of two CC BY 4.0 databases (Climate TRACE, GEM), which
#   makes it Adapted Material under §4(2): anyone redistributing it must carry
#   the notice forward. A deployed bundle IS a redistribution, so SOURCES*.md
#   and LICENSE go in with it (KARBON_ATLASI.md §10).
#
# MANIFEST
#   app/bundle/MANIFEST.json records when the bundle was cut, from which commit,
#   whether the working tree was clean, and the SHA-256 of every file. A
#   deployed copy can then be traced to exactly the data it serves, and
#   tests/testthat/test-bundle.R uses the digests to detect a stale bundle.
#
# app/bundle/ IS NOT TRACKED (.gitignore). It is a build product, like a
# compiled binary; the reproducible inputs are the pipeline and this script.
#
# USAGE (from the project root)
#   Rscript scripts/06_bundle_app.R
#   then publish app/ — e.g. rsconnect::deployApp("app")
#                       or  shinylive::export("app", "site")
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

stopifnot("Run from the project root" = file.exists("KARBON_ATLASI.md"))

# Target directory. Defaults to app/bundle/; an argument overrides it, which is
# how tests/testthat/test-bundle.R builds a throwaway deployment outside the
# repository without touching the real bundle.
args   <- commandArgs(trailingOnly = TRUE)
BUNDLE <- if (length(args) >= 1) args[1] else file.path("app", "bundle")

# Start from empty, so a file deleted from data/processed/ cannot survive in
# the bundle and keep being served.
#
# But delete only what is recognisably an earlier bundle. The target can be
# given on the command line, and a slip — `06_bundle_app.R app` for
# `app/bundle` — would otherwise erase the whole app directory without a
# prompt. Anything that exists, is not empty and carries no MANIFEST.json is
# refused.
if (dir.exists(BUNDLE)) {
  is_bundle <- file.exists(file.path(BUNDLE, "MANIFEST.json"))
  is_empty  <- length(list.files(BUNDLE, all.files = TRUE, no.. = TRUE)) == 0
  if (!is_bundle && !is_empty) {
    stop("Refusing to replace '", BUNDLE, "': it exists, is not empty and ",
         "is not a bundle (no MANIFEST.json). Pass an empty or new directory.",
         call. = FALSE)
  }
  unlink(BUNDLE, recursive = TRUE)
}
dir.create(file.path(BUNDLE, "data", "processed"), recursive = TRUE)
dir.create(file.path(BUNDLE, "policies"), recursive = TRUE)

# --- data/processed/ ----------------------------------------------------------
# Generated validation reports are left out: they are build diagnostics, not
# data the app reads, and they are untracked for the same reason.
processed <- list.files(file.path("data", "processed"), full.names = TRUE)
processed <- processed[!grepl("_validation\\.html$", processed) &
                         !dir.exists(processed)]
ok <- file.copy(processed, file.path(BUNDLE, "data", "processed"))
stopifnot("copying data/processed failed" = all(ok))

# --- policies/ ----------------------------------------------------------------
# Including policies/_schema/: small, and it documents what each file means.
ok <- file.copy(file.path("policies", list.files("policies")),
                file.path(BUNDLE, "policies"), recursive = TRUE)
stopifnot("copying policies failed" = all(ok))

# --- licence and attribution ----------------------------------------------------
stopifnot("copying LICENSE failed" =
            file.copy("LICENSE", file.path(BUNDLE, "LICENSE")))

# --- manifest -------------------------------------------------------------------
# git output, or NULL when git cannot be run (no git, or not a repository).
# An empty character vector is a real answer — a clean `status --porcelain`.
git <- function(...) {
  tryCatch(suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE)),
           error = function(e) NULL)
}
status <- git("status", "--porcelain")
commit <- git("rev-parse", "HEAD")

files <- list.files(BUNDLE, recursive = TRUE, all.files = FALSE)
files <- setdiff(files, "MANIFEST.json")

manifest <- list(
  bundled_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  commit        = if (length(commit) == 1) commit else NA_character_,
  # A bundle cut from uncommitted changes cannot be reproduced from history.
  # It is allowed (testing a deploy before committing is normal) but recorded;
  # NA means git could not be asked.
  working_tree_clean = if (is.null(status)) NA else length(status) == 0,
  source_release = unique(readRDS(file.path(BUNDLE, "data", "processed",
                                            "facilities.rds"))$source_release),
  files = lapply(setNames(files, files), function(f) list(
    bytes  = file.size(file.path(BUNDLE, f)),
    sha256 = digest(file.path(BUNDLE, f), algo = "sha256", file = TRUE)
  ))
)

write_json(manifest, file.path(BUNDLE, "MANIFEST.json"),
           auto_unbox = TRUE, pretty = TRUE)

total_kb <- round(sum(vapply(manifest$files, `[[`, numeric(1), "bytes")) / 1024)
message("Bundle written to ", BUNDLE, ": ", length(files), " files, ",
        total_kb, " KB. Commit ", substr(manifest$commit, 1, 7),
        if (!isTRUE(manifest$working_tree_clean)) " (UNCOMMITTED CHANGES)" else "")
message("Publish the app directory, e.g. rsconnect::deployApp(\"app\") or ",
        "shinylive::export(\"app\", \"site\").")

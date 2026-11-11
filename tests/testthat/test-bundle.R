# =============================================================================
# test-bundle.R — the deployment bundle works, and is not stale
# -----------------------------------------------------------------------------
# Hosting publishes app/ on its own (shinyapps.io, shinylive, Shiny Server).
# scripts/06_bundle_app.R copies the data into app/bundle/ so that works. Two
# things can go wrong, and neither raises an error until a visitor arrives:
#
#   1. The bundled app does not start, because something it reads was not
#      bundled or a path still assumes the repository. Tested by building a
#      throwaway deployment OUTSIDE the repository — app code plus a fresh
#      bundle, nothing above it — and starting the server there.
#   2. A bundle cut before the last pipeline run is published, serving old
#      numbers under a new commit. Tested by comparing the existing bundle's
#      manifest digests with the repository's files.
# =============================================================================

test_that("app/ published alone, with its bundle, starts and reads the bundle", {
  skip_if_not_installed("shiny")
  skip_if_not(file.exists(file.path(PROCESSED, "facilities.rds")),
              "facilities.rds not built")
  withr::local_options(encoding = getOption("encoding"))   # see test-app-server.R

  # A deployment directory with no KARBON_ATLASI.md anywhere above it.
  deploy <- withr::local_tempdir("atlas-deploy-")
  app    <- file.path(deploy, "app")
  dir.create(app)
  file.copy(list.files(file.path(ROOT, "app"), pattern = "\\.R$", full.names = TRUE), app)
  file.copy(file.path(ROOT, "app", "www"), app, recursive = TRUE)

  # Bundle through the real script, into the throwaway directory.
  out <- withr::with_dir(ROOT, system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path("scripts", "06_bundle_app.R"), shQuote(file.path(app, "bundle"))),
    stdout = TRUE, stderr = TRUE))
  expect_true(file.exists(file.path(app, "bundle", "MANIFEST.json")),
              info = paste(out, collapse = "\n"))

  shiny::testServer(app, {
    expect_identical(DATA_SOURCE, "bundle")
    expect_equal(nrow(facilities), nrow(readRDS(file.path(PROCESSED, "facilities.rds"))))
    expect_false(is.null(panel))
    expect_false(is.null(policy_events))
    expect_true(is.numeric(TR_ETS_THRESHOLD_T))
  })
})


test_that("an existing app/bundle matches the data it was cut from", {
  manifest_path <- file.path(ROOT, "app", "bundle", "MANIFEST.json")
  skip_if_not(file.exists(manifest_path), "no bundle present — nothing to be stale")

  m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  data_files <- grep("^(data/processed|policies)/", names(m$files), value = TRUE)

  stale <- Filter(function(f) {
    src <- file.path(ROOT, f)
    !file.exists(src) ||
      digest::digest(src, algo = "sha256", file = TRUE) != m$files[[f]]$sha256
  }, data_files)

  # Files built since the bundle was cut, which it therefore lacks.
  repo_files <- c(
    file.path("data/processed", list.files(PROCESSED)),
    file.path("policies", list.files(POLICIES, recursive = TRUE)))
  repo_files <- repo_files[!grepl("_validation\\.html$", repo_files) &
                             !dir.exists(file.path(ROOT, repo_files))]
  missing <- setdiff(repo_files, names(m$files))

  expect_true(length(stale) == 0 && length(missing) == 0,
              info = paste0("app/bundle is stale — re-run ",
                            "`Rscript scripts/06_bundle_app.R` before deploying.\n",
                            "changed: ", paste(stale, collapse = ", "), "\n",
                            "not bundled: ", paste(missing, collapse = ", ")))
})

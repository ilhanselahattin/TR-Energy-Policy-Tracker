# =============================================================================
# test-app-server.R — the app's filter logic, driven through the real server
# -----------------------------------------------------------------------------
# Every other file here tests artefacts. This one starts app/server.R under
# shiny::testServer() and checks what the filters actually return, because the
# app's failures look like the pipeline's: not an error, a plausible count.
#
# The assertions are INVARIANTS rather than remembered numbers, so a new
# upstream release changes the counts without breaking the tests:
#   - each ownership filter returns exactly the facilities whose flag is TRUE,
#     with NA ("no record") never counted as TRUE;
#   - the "certain" variant of a claim never exceeds the plain one;
#   - the route options partition the register and none of them is empty
#     (a hand-written list once offered a route no facility has);
#   - headline figures use Turkish number marks.
# =============================================================================

APP_DIR <- file.path(ROOT, "app")

# Every filter input at its interface default. setInputs() needs them all,
# because the server reads them unconditionally. The sector list is read from
# the data here: this function lives in the test environment, which cannot see
# the app's own ALL_SECTORS.
default_inputs <- function(session, ...) {
  args <- modifyList(
    list(asset_classes    = c("industrial", "energy"),
         sectors          = unique(readRDS(file.path(PROCESSED,
                                                     "facilities.rds"))$sector),
         provinces        = NULL,
         only_captive     = FALSE,
         ownership_filter = "all",
         route_filter     = "all"),
    list(...))
  do.call(session$setInputs, args)
}

# Skips when the app cannot be built, and isolates the test from the app.
#
# Starting the app runs app/global.R, which sets options(encoding = "UTF-8")
# for the whole R process. Left in place, that leaked into every later test
# file and broke jsonvalidate, which then misread its own bundle.js ("invalid
# input found on input connection") in test-validate-gates.R. The option is
# restored when the calling test ends.
skip_if_app_unbuilt <- function(env = parent.frame()) {
  skip_if_not_installed("shiny")
  skip_if_not(file.exists(file.path(PROCESSED, "facilities.rds")),
              "facilities.rds not built — run the pipeline first")
  withr::local_options(encoding = getOption("encoding"), .local_envir = env)
}


test_that("ownership filters return exactly the facilities whose flag is TRUE", {
  skip_if_app_unbuilt()
  skip_if_not(file.exists(file.path(PROCESSED, "ownership_summary.rds")),
              "ownership_summary.rds not built")

  s <- readRDS(file.path(PROCESSED, "ownership_summary.rds"))
  # Independent tally: NA is "no record", never TRUE.
  expected <- c(
    eu              = sum(s$has_eu_parent %in% TRUE),
    eu_certain      = sum(s$has_eu_parent_certain %in% TRUE),
    foreign         = sum(s$has_foreign_parent %in% TRUE),
    foreign_certain = sum(s$has_foreign_parent_certain %in% TRUE),
    unknown         = sum(!s$ownership_known))

  shiny::testServer(APP_DIR, {
    default_inputs(session)
    expect_equal(nrow(selected_facilities()), N_FACILITIES)

    got <- vapply(names(expected), function(f) {
      default_inputs(session, ownership_filter = f)
      nrow(selected_facilities())
    }, integer(1))

    expect_equal(got, expected)
    expect_lte(got[["eu_certain"]], got[["eu"]])
    expect_lte(got[["foreign_certain"]], got[["foreign"]])
  })
})


test_that("route options partition the register and none of them is empty", {
  skip_if_app_unbuilt()

  shiny::testServer(APP_DIR, {
    opts <- setdiff(unname(ROUTE_CHOICES), "all")
    counts <- vapply(opts, function(r) {
      default_inputs(session, route_filter = r)
      nrow(selected_facilities())
    }, integer(1))

    expect_true(all(counts > 0), info = paste(opts[counts == 0], collapse = ", "))
    expect_equal(sum(counts), N_FACILITIES)
  })
})


test_that("population and sector filters compose rather than override", {
  skip_if_app_unbuilt()

  shiny::testServer(APP_DIR, {
    default_inputs(session, asset_classes = "industrial")
    ind <- selected_facilities()
    expect_true(all(ind$asset_class == "industrial"))
    expect_equal(nrow(ind), sum(facilities$asset_class == "industrial"))

    # Industrial population, but only an energy sector selected: nothing.
    default_inputs(session, asset_classes = "industrial",
                   sectors = "electricity-generation")
    expect_equal(nrow(selected_facilities()), 0L)
  })
})


test_that("headline figures use Turkish number marks", {
  skip_if_app_unbuilt()
  skip_if_not(file.exists(file.path(PROCESSED, "facility_panel.rds")),
              "facility_panel.rds not built")

  shiny::testServer(APP_DIR, {
    default_inputs(session, year = DEFAULT_YEAR)
    session$elapse(300)          # past the year debounce
    html <- as.character(output$status_strip$html)

    # "74,9 Mt" is right; "74.9 Mt" reads as seventy-four thousand.
    expect_match(html, "[0-9],[0-9] Mt")
    expect_no_match(html, "[0-9]\\.[0-9] Mt")
  })
})


test_that("the TR-ETS threshold comes from the policy file, in app and pipeline", {
  skip_if_app_unbuilt()

  json_t <- jsonlite::fromJSON(file.path(POLICIES, "tr_ets.json"))$scope$threshold_tco2e_per_year
  expect_true(is.numeric(json_t) && length(json_t) == 1)

  shiny::testServer(APP_DIR, {
    expect_equal(TR_ETS_THRESHOLD_T, json_t)
  })

  # The classification the pipeline wrote must have used the same number.
  cov_path <- file.path(PROCESSED, "tr_ets_coverage.csv")
  skip_if_not(file.exists(cov_path), "tr_ets_coverage.csv not built")
  cov <- utils::read.csv(cov_path, fileEncoding = "UTF-8")
  expect_true(all(cov$threshold_t == json_t))
  expect_equal(cov$above_threshold, cov$emissions_t >= json_t)
})

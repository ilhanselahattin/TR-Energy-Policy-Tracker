# =============================================================================
# test-policies.R — properties of policies/*.json
# -----------------------------------------------------------------------------
# KARBON_ATLASI.md §7: every parameter file must carry a source_url and a
# retrieval date, because "a regulatory number without a citation is not usable
# in this project". That rule was already violated once — the €75 / €150 price
# scenarios arrived with no source — so it is now enforced rather than trusted.
# =============================================================================

# jsonlite is called with `::` rather than attached. `library()` emits a
# "package was built under R version x.y.z" WARNING on this machine, and
# suppressPackageStartupMessages() silences messages, not warnings — so
# attaching it here would show up as a warning in every test run and mask a real
# one later.

policy_files <- function() {
  list.files(POLICIES, pattern = "\\.json$", full.names = TRUE)
}

test_that("every policy file is valid JSON", {
  files <- policy_files()
  skip_if(length(files) == 0, "no policy files yet")

  # NOTE: expect_no_error()'s `message` argument is a regexp matched against the
  # condition, NOT a human-readable label. Passing a description there silently
  # changes the assertion's meaning. The file name goes in `info` instead.
  for (f in files) {
    parsed <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
                       error = function(e) conditionMessage(e))
    expect_false(is.character(parsed) && length(parsed) == 1,
                 info = paste("invalid JSON:", basename(f)))
  }
})


test_that("every policy file declares its provenance", {
  files <- policy_files()
  skip_if(length(files) == 0, "no policy files yet")

  for (f in files) {
    j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    nm <- basename(f)

    expect_true(!is.null(j$meta),
                info = paste(nm, "has no meta block"))
    expect_true(!is.null(j$meta$retrieved_date),
                info = paste(nm, "meta has no retrieved_date"))
    expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}$", j$meta$retrieved_date %||% ""),
                info = paste(nm, "retrieved_date is not ISO yyyy-mm-dd"))
  }
})


test_that("uncited numbers are explicitly flagged, not silently used", {
  path <- file.path(POLICIES, "carbon_price_scenarios.json")
  skip_if_not(file.exists(path), "carbon_price_scenarios.json not present")

  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  # A scenario either carries a source_url, or admits it needs one. What must
  # never happen is a bare number with neither.
  for (nm in names(j$scenarios)) {
    s <- j$scenarios[[nm]]
    has_source   <- !is.null(s$source_url) && nzchar(s$source_url)
    admits_gap   <- isTRUE(s$citation_required)
    user_defined <- identical(nm, "custom")

    expect_true(has_source || admits_gap || user_defined,
                info = paste0("scenario '", nm,
                              "' has neither a source_url nor citation_required"))
  }
})


test_that("CBAM phase-in factors are internally consistent and legislated", {
  path <- file.path(POLICIES, "cbam_phase_in.json")
  skip_if_not(file.exists(path), "cbam_phase_in.json not present")

  j <- jsonlite::fromJSON(path)
  p <- j$phase_in

  expect_true(all(p$cbam_factor >= 0 & p$cbam_factor <= 1))
  # The factor and the free-allocation share are complements by construction.
  expect_true(all(abs(p$cbam_factor + p$free_allocation_share - 1) < 1e-9))
  # Monotonic: the obligation phases in, it never reverses.
  expect_true(all(diff(p$cbam_factor) > 0))
  # Full application in 2034 is the legislated endpoint.
  expect_equal(p$cbam_factor[p$year == 2034], 1)
  expect_false(is.null(j$meta$source_url))
})


test_that("provisional data is marked provisional", {
  path <- file.path(POLICIES, "cbam_goods_cn_codes.json")
  skip_if_not(file.exists(path), "cbam_goods_cn_codes.json not present")

  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  # The customs codes are HS2/HS4 aggregates standing in for the Annex I CN8
  # list. That is acceptable during development and unacceptable at publication,
  # so the status must always be one of two known values — never absent, never
  # quietly edited to something meaningless.
  expect_true(j$meta$scope_status %in%
                c("PROVISIONAL_AGGREGATE", "annex_i_verified"))
})


# =============================================================================
# The policy timeline
# =============================================================================
# This file is the one place in the project where an interface element makes a
# claim about POLICY rather than about emissions, so the assertions guard the
# claim as much as the structure. The register is knowingly partial — only EU
# CBAM and Turkish ETS, nothing of Turkish energy policy — and the danger is
# that it stops saying so.

test_that("every timeline event is traceable to a source and a file", {
  path <- file.path(POLICIES, "policy_timeline.json")
  skip_if_not(file.exists(path), "policy_timeline.json not present")

  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_gt(length(j$events), 0)

  for (e in j$events) {
    # §7: a regulatory fact without a citation is not usable here. The same rule
    # the price scenarios are held to.
    has_url <- !is.null(e$source_url) && nzchar(e$source_url)
    expect_true(has_url || isTRUE(e$citation_required),
                info = paste(e$id, "has neither source_url nor citation_required"))

    # Every event in this file was extracted from a parameter file already in
    # the repository, and `derived_from` names it so a reader can check the
    # provenance without leaving the project.
    expect_true(nzchar(e$derived_from), info = paste(e$id, "has no derived_from"))
  }
})


test_that("event dates match their declared granularity", {
  path <- file.path(POLICIES, "policy_timeline.json")
  skip_if_not(file.exists(path), "policy_timeline.json not present")

  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  for (e in j$events) {
    # Several source facts give only a year. Drawing those as 1 January would
    # invent a precision the source does not have, so the pair must agree.
    is_day <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", e$date)
    expect_equal(e$granularity, if (is_day) "day" else "year",
                 info = paste(e$id, "date", e$date))
    if (is_day) {
      expect_false(is.na(suppressWarnings(as.Date(e$date))),
                   info = paste(e$id, "unparseable date"))
    }
  }
})


test_that("event ids are unique and every category is defined", {
  path <- file.path(POLICIES, "policy_timeline.json")
  skip_if_not(file.exists(path), "policy_timeline.json not present")

  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  ids <- vapply(j$events, function(e) e$id, character(1))
  expect_equal(anyDuplicated(ids), 0)

  # An undefined category renders with no colour and no label, which looks like
  # a missing event rather than a broken reference.
  cats <- vapply(j$events, function(e) e$category, character(1))
  expect_true(all(cats %in% names(j$categories)))
})


test_that("the timeline does not claim to be complete while it is not", {
  path <- file.path(POLICIES, "policy_timeline.json")
  skip_if_not(file.exists(path), "policy_timeline.json not present")

  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  expect_true(j$meta$status %in% c("partial", "complete"))

  # `status` is a claim about coverage, and the interface leans on it. It may
  # only say "complete" once at least one event bears on the fleet — Turkish
  # energy policy is what would make that true. Flipping the string alone must
  # not be enough.
  if (identical(j$meta$status, "complete")) {
    expect_true(any(vapply(j$events,
                           function(e) isTRUE(e$affects_fleet), logical(1))))
  } else {
    # While partial, the file must keep saying what it is missing, because the
    # interface quotes that field back to the reader.
    expect_true(nzchar(j$meta$what_is_missing))
  }
})

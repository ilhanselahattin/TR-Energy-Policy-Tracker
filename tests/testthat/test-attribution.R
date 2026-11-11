# =============================================================================
# test-attribution.R — licence conditions are conditions, not courtesies
# -----------------------------------------------------------------------------
# Climate TRACE and GEM both publish under CC BY 4.0. Attribution is a term of
# that licence, so redistributing derived data without it is a breach, not an
# oversight. These tests exist because "remember to credit GEM" is exactly the
# kind of obligation that survives a conversation and not a codebase.
#
# Each test skips when the artefact it guards has not been produced yet, so a
# fresh clone stays green before the pipeline has run.
# =============================================================================

test_that("Climate TRACE attribution travels with the processed data", {
  path <- file.path(PROCESSED, "SOURCES.md")
  skip_if_not(file.exists(path), "SOURCES.md not generated yet")

  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, "Climate TRACE", fixed = TRUE)
  expect_match(txt, "CC BY 4.0", fixed = TRUE)
  # The release tag is what makes a figure checkable years later.
  expect_match(txt, "v\\d+_\\d+_\\d+")
  expect_match(txt, "SHA-256", fixed = TRUE)
})


test_that("every CC BY notice states that the material was modified", {
  # CC BY 4.0 §3(1)(a)(ii). Naming the source is not enough. The Climate TRACE
  # notice — the primary source — lacked this section until 2026-11-11 while
  # GEM's and Ember's had it, so the check covers all three.
  for (f in c("SOURCES.md", "SOURCES_GEM.md", "SOURCES_EMBER.md")) {
    path <- file.path(PROCESSED, f)
    if (!file.exists(path)) next
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_match(txt, "Modifications", fixed = TRUE, info = f)
    expect_match(txt, "without warranties", fixed = TRUE, info = f)
  }
})


test_that("GEM attribution exists whenever GEM data has been ingested", {
  gem_data <- file.path(PROCESSED, "gem_commissioning.csv")
  skip_if_not(file.exists(gem_data), "GEM not ingested yet")

  path <- file.path(PROCESSED, "SOURCES_GEM.md")

  # If GEM data is in data/processed/ then its attribution must be too. This is
  # the assertion that turns a licence obligation into a build failure.
  expect_true(file.exists(path),
              info = paste("gem_commissioning.csv exists but SOURCES_GEM.md",
                           "does not — re-run scripts/01c_ingest_gem.R"))

  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # CC BY 4.0 §3(1)(a): creator, licence notice, URI, warranty disclaimer.
  expect_match(txt, "Global Energy Monitor", fixed = TRUE)
  expect_match(txt, "CC BY 4.0", fixed = TRUE)
  expect_match(txt, "creativecommons.org/licenses/by/4.0", fixed = TRUE)
  expect_match(txt, "without warranties", fixed = TRUE)

  # §3(1)(a)(ii): the clause requiring a statement THAT the material was
  # modified. This project filters, renames, joins and derives, so the statement
  # is not optional.
  expect_match(txt, "Modifications", ignore.case = TRUE)

  # §2(5)(c): no implied endorsement.
  expect_match(txt, "does not endorse", fixed = TRUE)

  # §4: the tracker is a database, so Sui Generis Database Rights attach to the
  # derived table.
  expect_match(txt, "Database rights", ignore.case = TRUE)
})


test_that("Ember attribution exists whenever Ember data has been fetched", {
  ember_data <- file.path(PROCESSED, "ember_generation.csv")
  skip_if_not(file.exists(ember_data), "Ember not fetched yet")

  path <- file.path(PROCESSED, "SOURCES_EMBER.md")
  expect_true(file.exists(path),
              info = paste("ember_generation.csv exists but SOURCES_EMBER.md",
                           "does not — re-run scripts/01d_fetch_ember.R"))

  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, "Ember", fixed = TRUE)
  expect_match(txt, "CC BY 4.0", fixed = TRUE)
  expect_match(txt, "creativecommons.org/licenses/by/4.0", fixed = TRUE)
  expect_match(txt, "without warranties", fixed = TRUE)
  expect_match(txt, "Modifications", ignore.case = TRUE)
  expect_match(txt, "does not endorse", fixed = TRUE)
})


test_that("the three grid intensity estimates stay within a defensible spread", {
  path <- file.path(PROCESSED, "grid_intensity.csv")
  skip_if_not(file.exists(path), "grid_intensity.csv not built yet")

  g <- read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(ember_g_per_kwh), !is.na(ct_reported_g_per_kwh))
  skip_if(nrow(g) == 0, "no overlapping years")

  # Ember and Climate TRACE reach their figures by unrelated methods. Close
  # agreement is the evidence that either can be used; a sudden divergence would
  # mean one of them changed methodology and the comparison needs revisiting.
  expect_lt(max(abs(g$ember_vs_ct_pct)), 10,
            label = "Ember vs Climate TRACE reported intensity spread (%)")

  # The naive fleet estimate must stay clearly above both, because it divides
  # real emissions by a denominator missing ~45% of generation. If it ever
  # matched them, the register would have gained renewables and E1 would need
  # reopening.
  naive <- g |> filter(!is.na(ct_naive_g_per_kwh))
  if (nrow(naive) > 0) {
    expect_true(all(naive$ct_naive_g_per_kwh > naive$ember_g_per_kwh))
    expect_true(all(naive$ct_coverage_share < 0.8),
                info = "Climate TRACE power coverage rose above 80% — recheck E1")
  }
})


test_that("derived-data licence is compatible with every upstream licence", {
  path <- file.path(ROOT, "README.md")
  skip_if_not(file.exists(path), "README.md missing")

  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # The chain is CC BY 4.0 (Climate TRACE, GEM, Ember) + public domain (Natural
  # Earth) -> CC BY 4.0 derived. No ShareAlike anywhere.
  expect_match(txt, "CC BY 4.0", fixed = TRUE)

  # The check is that no source IN USE is ShareAlike — not that the string never
  # appears. The README names CC BY-SA precisely to explain why geoBoundaries
  # was rejected for province boundaries, and an assertion that forbids the
  # string outright punishes the project for documenting the decision. Only the
  # rows of the sources table are examined.
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  table_rows <- lines[grepl("^\\|", lines) & grepl("CC BY|domain|Public", lines)]

  offending <- table_rows[grepl("CC BY-SA", table_rows, fixed = TRUE)]
  expect_equal(length(offending), 0L,
               info = paste("ShareAlike source listed in use:",
                            paste(offending, collapse = " / ")))
})

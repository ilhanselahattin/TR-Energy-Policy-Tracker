# =============================================================================
# test-ownership.R — properties of the ownership layer
# -----------------------------------------------------------------------------
# Ownership data fails differently from emissions data. It is not wrong in ways
# that look like bad numbers; it is wrong in ways that look like FINDINGS. The
# first build of this layer put British American Tobacco, Life Insurance
# Corporation of India and the Government of India at the top of a table about
# Turkish industry, because nine Ankara landfill-gas plants are operated by a
# company recorded as "ITC Ltd" and Climate TRACE resolved that to India's ITC
# Limited. Every link above the collision was correct and sourced.
#
# So these assertions guard the CLAIMS, not just the schema.
# =============================================================================

NON_ENTITY <- c("natural person(s)", "small shareholder(s)", "unknown")

test_that("every ownership row belongs to a facility in the register", {
  own <- read_processed("ownership.rds")
  facilities <- read_processed("facilities.rds")

  expect_gt(nrow(own), 0)
  expect_true(all(own$facility_id %in% facilities$facility_id))
})


test_that("the summary covers every facility, including those with no owner", {
  s <- read_processed("ownership_summary.rds")
  facilities <- read_processed("facilities.rds")

  # A facility with no ownership record must appear with ownership_known FALSE
  # rather than being dropped — otherwise "39 facilities have no owner" silently
  # becomes "39 facilities are Turkish-owned".
  expect_setequal(s$facility_id, facilities$facility_id)
  expect_true(any(!s$ownership_known))
  expect_true(all(is.na(s$has_eu_parent[!s$ownership_known])))
})


test_that("foreign and EU flags cannot contradict each other", {
  own <- read_processed("ownership.rds")

  # EU implies foreign. A Turkish headquarters is neither. An inversion here
  # would flip the conclusion rather than break the build.
  expect_equal(sum(own$is_eu & !own$is_foreign, na.rm = TRUE), 0)
  expect_equal(sum(own$parent_hq_country == "TUR" &
                     (own$is_foreign | own$is_eu), na.rm = TRUE), 0)
})


test_that("placeholder owners are never counted as named companies", {
  own <- read_processed("ownership.rds")

  # "natural person(s)" and "small shareholder(s)" are Climate TRACE's literals
  # for an owner that is not a company. Counting them would put them at the top
  # of every ranking.
  leak <- own |> dplyr::filter(is_named_entity, parent_name %in% NON_ENTITY)
  expect_equal(nrow(leak), 0)
})


test_that("no ownership figure is expressed as a proportion", {
  s <- read_processed("ownership_summary.rds")

  # `overall_share_percent` is a path product, not a partition: per-facility
  # sums reach 342%. Any field promising a share of ownership would be
  # arithmetic on a quantity that cannot bear it. `parent_primary_share` is the
  # single permitted exception — it reports ONE published path value verbatim
  # and aggregates nothing.
  suspect <- grep("(share|pct|percent|weighted)", names(s),
                  value = TRUE, ignore.case = TRUE)
  expect_setequal(suspect, "parent_primary_share")
})


test_that("the certainty split exists and is a subset", {
  s <- read_processed("ownership_summary.rds")

  # Every ownership claim comes in two strengths, and `_certain` — chains whose
  # final link to the facility is quantified — must be a subset of `any`.
  # Measured, foreign falls 53 -> 32 while EU falls only 28 -> 26, which is what
  # makes the EU claim usable and the general foreign claim not.
  expect_true(all(c("has_foreign_parent", "has_foreign_parent_certain",
                    "has_eu_parent", "has_eu_parent_certain") %in% names(s)))

  expect_equal(sum(s$has_foreign_parent_certain & !s$has_foreign_parent,
                   na.rm = TRUE), 0)
  expect_equal(sum(s$has_eu_parent_certain & !s$has_eu_parent, na.rm = TRUE), 0)

  expect_lte(sum(s$has_eu_parent_certain, na.rm = TRUE),
             sum(s$has_eu_parent, na.rm = TRUE))
})


test_that("the ITC name collision stays excluded from certain claims", {
  own <- read_processed("ownership.rds")

  # A regression guard on a specific, understood defect. India's tobacco
  # conglomerate and its shareholders reach nine Turkish landfill-gas plants
  # through a name match on "ITC Ltd"; every one of those paths ends in an
  # unquantified link, so none may survive into a certain claim. If this ever
  # fails, either Climate TRACE fixed the resolution or our filter stopped
  # working — and the two need different responses.
  indian <- c("British American Tobacco PLC", "Government of India",
              "Life Insurance Corporation of India Ltd",
              "Tobacco Manufacturers (India) Ltd",
              "Specified Undertaking of the Unit Trust of India")

  rows <- own |> dplyr::filter(parent_name %in% indian)
  skip_if(nrow(rows) == 0, "the collision is gone from the upstream data")

  expect_true(all(rows$path_terminal_unknown))
})


test_that("the by-parent roll-up is ranked by the defensible count", {
  path <- file.path(PROCESSED, "ownership_by_parent.csv")
  skip_if_not(file.exists(path), "ownership_by_parent.csv not built")

  bp <- read_csv(path, locale = locale(encoding = "UTF-8"),
                 show_col_types = FALSE, progress = FALSE)

  expect_true(all(c("facilities", "facilities_certain") %in% names(bp)))
  expect_true(all(bp$facilities_certain <= bp$facilities))

  # Sorted by the certain count. Sorting by the raw count would put the tobacco
  # chain near the top of a table about Turkish industry.
  expect_false(is.unsorted(rev(bp$facilities_certain)))
})

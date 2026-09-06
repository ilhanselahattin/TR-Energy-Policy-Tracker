# =============================================================================
# 01b_fetch_eu_trade.R — EU imports of CBAM goods from Türkiye
# -----------------------------------------------------------------------------
# PURPOSE
#   Fetch the trade quantities that `eu_export_share` is built from: how much of
#   each CBAM good the EU imported from Türkiye, per year, in tonnes.
#
#   CBAM liability arises only on goods actually exported to the EU. A facility
#   selling entirely into the domestic market has no CBAM exposure. Without this
#   step the tool would be answering "what if every plant exported everything to
#   the EU", which is a different and much weaker question.
#
# NUMBERING
#   `01b` rather than `04` because this is an ACQUISITION step and belongs
#   beside 01_fetch_climate_trace.R, not after 03_build_panel.R which consumes
#   it. Flagged for the author: rename if the convention should be strict.
#
# WHAT THIS SCRIPT DOES NOT DO
#   It does not decide what `eu_export_share` actually is. Section 5 computes a
#   naive ratio purely as a diagnostic, wrapped in warnings, because the
#   numerator and denominator are NOT measured in the same thing:
#
#     numerator   = EU imports of finished CBAM goods, tonnes of product
#     denominator = Climate TRACE `activity`, tonnes of crude steel / cement /
#                   aluminium produced
#
#   Crude steel is not finished steel: there is yield loss between them, and
#   Annex I does not cover every steel product. Dividing one by the other
#   without a stated conversion is a modelling choice, and it belongs to the
#   author (KARBON_ATLASI.md §9).
#
# OUTPUTS
#   data/raw/eurostat/comext_<sector>_<year>.json   cached API responses
#   data/processed/eu_imports_from_tr.csv           tidy trade quantities
#   data/processed/eu_export_share_diagnostic.csv   naive ratio, FOR REVIEW ONLY
#
# RUN
#   Rscript scripts/01b_fetch_eu_trade.R
#   Rscript scripts/01b_fetch_eu_trade.R --force
# =============================================================================

options(encoding = "UTF-8")
options(timeout = 600)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(readr)
  library(stringr)
})

source(file.path("scripts", "_sources.R"))

FORCE <- "--force" %in% commandArgs(trailingOnly = TRUE)


# =============================================================================
# 1. CONFIGURATION
# =============================================================================

POLICY_FILE <- file.path("policies", "cbam_goods_cn_codes.json")
policy      <- jsonlite::fromJSON(POLICY_FILE, simplifyVector = FALSE)

# Refuse to run silently on a provisional code list. The script still works —
# development needs it — but nobody should be able to use its output without
# having seen this.
if (!identical(policy$meta$scope_status, "annex_i_verified")) {
  warning("\n",
          strrep("!", 70), "\n",
          "cbam_goods_cn_codes.json scope_status = '", policy$meta$scope_status, "'\n",
          "The customs codes are HS2/HS4 AGGREGATES, not the CN8 list in Annex I.\n",
          "They OVERSTATE CBAM scope, so every share derived here is an UPPER BOUND.\n",
          "Do not publish any figure from this run.\n",
          strrep("!", 70), "\n", immediate. = TRUE)

} else if (!isTRUE(policy$meta$exclusions_applied_in_query)) {
  # Verifying the list against Annex I and applying it are different jobs, and
  # the first must not silence the warning that belongs to the second. Chapter
  # 72 is still requested whole here while Annex I carves out the ferro-alloys
  # and 7204 scrap; chapter 76 likewise keeps 7602 and 7615. The overstatement
  # is unchanged — the only difference is that it is now written down.
  warning("\n",
          strrep("!", 70), "\n",
          "cbam_goods_cn_codes.json: code list matches Annex I, but this script\n",
          "does NOT subtract meta exclusions. Steel and aluminium shares are\n",
          "still UPPER BOUNDS. Cement is exact — Annex I lists all five 2523\n",
          "subheadings, so nothing is carved out of it.\n",
          "To fix: query at CN8 and sum only the included codes.\n",
          strrep("!", 70), "\n", immediate. = TRUE)
}

COMEXT_URL <- policy$meta$trade_source$endpoint
REPORTER   <- "EU27_2020"     # the EU as a single reporter, not 27 separate rows
PARTNER    <- "TR"
FLOW       <- "1"             # IMPORT: EU imports from TR == TR exports to the EU

# Trade years to request. The facility panel starts at t0 = 2021.
YEARS <- 2021:2025

# Climate TRACE sectors, in this project's vocabulary.
SECTORS <- names(policy$sectors)

DIR_RAW       <- file.path("data", "raw", "eurostat")
DIR_CT        <- file.path("data", "raw", "climate_trace")
DIR_PROCESSED <- file.path("data", "processed")

dir.create(DIR_RAW,       recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)

# QUANTITY_IN_100KG is in hundreds of kilograms: 1 unit = 100 kg = 0.1 t.
HUNDRED_KG_TO_TONNES <- 0.1


# =============================================================================
# 2. FETCH
# =============================================================================
# Eurostat rejects unfiltered queries on this dataset with HTTP 413, so every
# request is filtered down to one product code and one year.

message("[1/4] Fetching Eurostat Comext (", REPORTER, " imports from ", PARTNER, ")")

fetch_one <- function(product, year) {
  cache <- file.path(DIR_RAW, sprintf("comext_%s_%s.json", product, year))

  if (file.exists(cache) && !FORCE) {
    payload <- fromJSON(cache, simplifyVector = FALSE)
  } else {
    resp <- request(COMEXT_URL) |>
      req_url_query(format = "JSON", freq = "A", reporter = REPORTER,
                    partner = PARTNER, product = product, flow = FLOW,
                    time = as.character(year)) |>
      req_retry(max_tries = 3, backoff = function(i) 2^i) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()

    if (resp_status(resp) != 200L) {
      warning("HTTP ", resp_status(resp), " for product ", product, " ", year,
              " — recorded as unavailable, not substituted with zero.")
      return(tibble())
    }
    writeLines(resp_body_string(resp), cache, useBytes = TRUE)
    Sys.sleep(0.3)
    payload <- fromJSON(cache, simplifyVector = FALSE)
  }

  # JSON-stat: `value` is a sparse map from flat index to number, and the index
  # runs over the dimensions in `id` order. Only `indicators` varies here, so
  # the flat index IS the indicator position.
  ind_index <- payload$dimension$indicators$category$index
  if (length(ind_index) == 0 || length(payload$value) == 0) return(tibble())

  vals <- payload$value
  # Absent combinations are simply missing from the map — that is genuinely
  # "not reported", and must stay NA rather than becoming 0.
  pick <- function(name) {
    pos <- ind_index[[name]]
    if (is.null(pos)) return(NA_real_)
    v <- vals[[as.character(pos)]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }

  tibble(
    product        = product,
    year           = year,
    value_eur      = pick("VALUE_IN_EUROS"),
    quantity_100kg = pick("QUANTITY_IN_100KG")
  )
}

requests <- SECTORS |>
  map(function(s) {
    codes <- unlist(policy$sectors[[s]]$codes)
    expand_grid(sector = s, product = codes, year = YEARS)
  }) |>
  list_rbind()

message("      ", nrow(requests), " requests (",
        length(SECTORS), " sectors x codes x ", length(YEARS), " years)")

trade_raw <- requests |>
  pmap(function(sector, product, year) {
    out <- fetch_one(product, year)
    if (nrow(out) == 0) return(tibble())
    out |> mutate(sector = sector)
  }) |>
  list_rbind()

if (nrow(trade_raw) == 0) stop("No trade data returned — check the API and codes.")


# =============================================================================
# 3. TIDY
# =============================================================================

message("[2/4] Aggregating to sector-year")

trade <- trade_raw |>
  mutate(quantity_t = quantity_100kg * HUNDRED_KG_TO_TONNES)

# Chapter-level and heading-level codes OVERLAP: "72" already contains every
# 72xx heading. Summing them would double count, so headings nested inside a
# chapter that is itself requested are dropped before aggregation.
chapters_present <- trade |> filter(nchar(product) == 2) |> distinct(sector, product)

trade_dedup <- trade |>
  rowwise() |>
  mutate(
    nested_in_chapter = any(
      chapters_present$sector == sector &
      nchar(product) > 2 &
      str_starts(product, chapters_present$product)
    )
  ) |>
  ungroup() |>
  filter(!nested_in_chapter)

n_dropped <- nrow(trade) - nrow(trade_dedup)
if (n_dropped > 0) {
  message("      dropped ", n_dropped,
          " heading rows nested inside a requested chapter (double-count guard)")
}

trade_sector_year <- trade_dedup |>
  group_by(sector, year) |>
  summarise(
    eu_import_t   = sum(quantity_t, na.rm = TRUE),
    eu_import_eur = sum(value_eur,  na.rm = TRUE),
    n_codes       = n(),
    n_missing     = sum(is.na(quantity_t)),
    .groups = "drop"
  ) |>
  arrange(sector, year)

write_csv(trade_sector_year, file.path(DIR_PROCESSED, "eu_imports_from_tr.csv"))


# =============================================================================
# 4. TURKISH PRODUCTION, FOR CONTEXT
# =============================================================================

message("[3/4] Reading Turkish production from Climate TRACE")

pkg          <- ct_download_package("co2", "TUR", DIR_CT)
members      <- ct_sector_files(pkg$path, SECTORS)
sector_files <- ct_extract(pkg$path, members, file.path(DIR_CT, "extracted"))

# NAME SHADOWING TRAP: the Climate TRACE CSV already has a `sector` column whose
# value is "manufacturing" for all three of our sectors — the useful split lives
# in `subsector`. Writing `mutate(sector = sector)` inside imap() would resolve
# `sector` to that existing column rather than to the function argument, quietly
# collapsing all three sectors into one and producing a denominator three times
# too large. The argument is therefore named `sector_key` and assigned via
# .env$ so it cannot be captured by a column of the same name.
production <- sector_files |>
  imap(function(path, sector_key) {
    read_csv(path, locale = locale(encoding = "UTF-8"),
             col_types = cols(.default = col_character(), activity = col_double()),
             progress = FALSE) |>
      mutate(sector_key = .env$sector_key,
             year       = as.integer(str_sub(start_time, 1, 4)))
  }) |>
  list_rbind() |>
  filter(year %in% YEARS) |>
  group_by(sector = sector_key, year) |>
  summarise(production_t = sum(activity, na.rm = TRUE), .groups = "drop")

# The join below is the only place these two sources meet. If the keys ever stop
# lining up the result is silently NA rather than an error, so assert instead.
stopifnot(
  "Production sectors must match the trade sectors" =
    setequal(production$sector, trade_sector_year$sector),
  "Production must cover every requested year" =
    setequal(production$year, YEARS)
)


# =============================================================================
# 5. NAIVE RATIO — DIAGNOSTIC ONLY, NOT A RESULT
# =============================================================================
# Read the header of this file before using this. The numerator counts finished
# goods; the denominator counts crude production. They are different physical
# quantities. This ratio is printed so the author can see the magnitudes and
# decide what eu_export_share should actually be — it is NOT that value.

message("[4/4] Writing diagnostic ratio (NOT eu_export_share)")

diagnostic <- trade_sector_year |>
  left_join(production, by = c("sector", "year")) |>
  mutate(
    naive_ratio = eu_import_t / production_t,
    value_type  = "diagnostic_not_for_use",
    # The caveat states the CURRENT state of the codes rather than a fixed
    # string, so it cannot go stale the way the previous one did: it said
    # "PROVISIONAL aggregate" for a run whose codes had already been verified
    # against Annex I.
    caveat      = paste0(
      "Numerator is finished-goods trade; denominator is crude production. ",
      "These are different physical quantities and the ratio is NOT a valid ",
      "export share. Customs codes: ",
      if (identical(policy$meta$scope_status, "annex_i_verified")) {
        if (isTRUE(policy$meta$exclusions_applied_in_query)) {
          "Annex I verified, carve-outs applied."
        } else {
          paste0("Annex I verified but carve-outs NOT subtracted in the query, ",
                 "so steel and aluminium remain upper bounds; cement is exact.")
        }
      } else {
        "PROVISIONAL aggregates, overstating scope in every sector."
      }
    )
  ) |>
  arrange(sector, year)

write_csv(diagnostic, file.path(DIR_PROCESSED, "eu_export_share_diagnostic.csv"))


cat("\n", strrep("=", 72), "\n", sep = "")
cat("EU TRADE FETCH COMPLETE\n")
cat(strrep("=", 72), "\n\n", sep = "")

cat("EU27 imports from Türkiye, tonnes:\n")
print(trade_sector_year |> select(sector, year, eu_import_t, n_missing), n = Inf)

cat("\nDiagnostic ratio vs crude production — NOT eu_export_share:\n")
print(diagnostic |>
        transmute(sector, year,
                  eu_import_kt   = round(eu_import_t / 1000),
                  production_kt  = round(production_t / 1000),
                  naive_ratio    = round(naive_ratio, 3)), n = Inf)

cat("\n", strrep("!", 72), "\n", sep = "")
cat("The ratio above is a DIAGNOSTIC. Defining eu_export_share — reconciling\n")
cat("finished-goods trade against crude production, and narrowing the customs\n")
cat("codes to Annex I — is analytical core work reserved for the author (§9).\n")
cat(strrep("!", 72), "\n", sep = "")

cat("\nWritten:\n",
    "  ", file.path(DIR_PROCESSED, "eu_imports_from_tr.csv"), "\n",
    "  ", file.path(DIR_PROCESSED, "eu_export_share_diagnostic.csv"), "\n", sep = "")

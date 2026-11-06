# =============================================================================
# 03_build_panel.R — build the facility x year emissions panel
# -----------------------------------------------------------------------------
# PURPOSE
#   Produce `data/processed/facility_panel.rds`: one row per facility per year,
#   carrying everything that varies over time (KARBON_ATLASI.md §6). The
#   facilities table holds what does not change; this holds what does.
#
#   The substantive work is collapsing MONTHLY source records into annual rows
#   without introducing an error that looks like a number. Climate TRACE
#   publishes one row per facility-month: Turkish cement is 58 facilities x 65
#   months = 3,770 rows. How each variable collapses depends on what kind of
#   quantity it is, and getting that wrong produces a plausible chart rather
#   than an exception.
#
# PREREQUISITES
#   Rscript scripts/01_fetch_climate_trace.R
#   Rscript scripts/02_build_facilities.R
#
# OUTPUTS
#   data/processed/facility_panel.rds
#   data/processed/panel_coverage.csv          rows, months and years per sector
#   data/processed/panel_b1_inputs.csv         DIAGNOSTIC, see section 7
#
# RUN
#   Rscript scripts/03_build_panel.R
# =============================================================================

options(encoding = "UTF-8")
options(timeout = 600)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(readr)
  library(stringr)
})

source(file.path("scripts", "_sources.R"))
source(file.path("scripts", "_validate.R"))


# =============================================================================
# 1. CONFIGURATION
# =============================================================================

CT_COUNTRY <- "TUR"

# Mirrors 00 and 02. The two populations read from different gas packages and
# that is deliberate: CBAM is a CO2 instrument, while only 18% of coal mining's
# footprint is CO2. Keeping them in separate reads means there is no single
# table in which both appear as "emissions" to be summed by accident.
CT_ASSET_CLASSES <- list(
  industrial = list(
    gas        = "co2",
    subsectors = c("iron-and-steel", "cement", "aluminum")
  ),
  energy = list(
    gas        = "co2e_100yr",
    subsectors = c("electricity-generation", "coal-mining",
                   "oil-and-gas-production", "oil-and-gas-refining",
                   "oil-and-gas-transport")
  )
)

DIR_RAW       <- file.path("data", "raw", "climate_trace")
DIR_EXTRACT   <- file.path(DIR_RAW, "extracted")
DIR_PROCESSED <- file.path("data", "processed")

FACILITIES <- file.path(DIR_PROCESSED, "facilities.rds")

if (!file.exists(FACILITIES)) {
  stop("facilities.rds not found. Run scripts/02_build_facilities.R first.",
       call. = FALSE)
}


# --- The observed / projected boundary ---------------------------------------
# KARBON_ATLASI.md §5 instructs that Climate TRACE's estimates beyond the last
# observed year enter the panel as `value_type = projected`, drawn dashed and
# badged, never rendered as an observation. It names 2025-2026 as those years.
#
# B5 IS NOW ANSWERED, AND THE ANSWER IS THAT THE BOUNDARY DOES NOT EXIST.
# Three sources were checked (2026-09-01 and 2026-09-06):
#
#   about_the_data.pdf     Describes every column and the licence, and says the
#                          models "provide our current best estimates". It draws
#                          no line between measured and nowcast years.
#   the confidence files   Published per period, but the ratings do not vary by
#                          period. Turkish cement emissions_quantity is "low"
#                          for 9 facilities and "medium" for 49 in 2021 and in
#                          2026 alike — a per-facility property with no temporal
#                          signal.
#   the v5.10.0 changelog  Extends the data period month by month and says
#                          nothing about nowcasting for our four sectors.
#
# Climate TRACE is a modelled inventory throughout. Not "2021-2024 measured,
# 2025-2026 estimated" — every figure is a model estimate, and the provider
# never claims otherwise.
#
# The changelog PROVES this rather than merely failing to contradict it: where a
# year really is predicted, it says so. For agriculture, "the 2026 production
# data were predicted using a regression-based approach". No such sentence
# exists for electricity generation, iron and steel, cement or coal mining;
# those read "full refresh of asset-level emissions from 2021 to Jun 2026" and
# "added new month, June 2026". The provider distinguishes predicted years where
# it makes one, and does not make one here.
#
# SO THIS CONSTANT IS AN INSTRUCTION WITH NO EVIDENCE BEHIND IT. §5 names
# 2025-2026 as projected; nothing in the source supports singling out 2025.
# It is left unchanged deliberately: choosing between keeping the conservative
# setting, moving it, and replacing the observed/projected vocabulary with one
# this source can bear is a methodological decision reserved to the author (§9).
# Stated once, here, so that decision is a one-line edit. See FINDINGS A7.
LAST_OBSERVED_YEAR <- 2024

# t0, established empirically by 00_coverage_audit.R rather than assumed.
T0 <- 2021


# =============================================================================
# 2. READ THE MONTHLY SOURCE RECORDS
# =============================================================================

message("[1/6] Reading monthly emissions records")

facilities <- readRDS(FACILITIES)

read_sector <- function(path) {
  read_csv(path,
           locale    = locale(encoding = "UTF-8"),
           col_types = cols(.default = col_character()),
           progress  = FALSE)
}

#' Read every monthly record for one asset class, from its own gas package.
#'
#' Returns the long monthly table plus the release tag, which must be carried
#' into the panel as `vintage`. The two packages ship at DIFFERENT releases —
#' co2 at v5_9_0 and co2e_100yr at v5_10_0 on the day these were fetched — so a
#' single project-wide version constant would be a lie for one of them.
read_monthly <- function(cfg, class_name) {
  pkg     <- ct_download_package(cfg$gas, CT_COUNTRY, DIR_RAW)
  members <- ct_sector_files(pkg$path, cfg$subsectors)
  files   <- ct_extract(pkg$path, members, file.path(DIR_EXTRACT, cfg$gas))

  raw <- map(files, read_sector) |>
    list_rbind() |>
    filter(iso3_country == CT_COUNTRY)

  release <- ct_release_tag(members)

  message("      ", class_name, ": ", nrow(raw), " facility-months (",
          cfg$gas, ", ", release, ")")

  list(raw = raw, release = release, pkg = pkg)
}

industrial <- read_monthly(CT_ASSET_CLASSES$industrial, "industrial")
energy     <- read_monthly(CT_ASSET_CLASSES$energy,     "energy")


# =============================================================================
# 3. CONFIDENCE RATINGS
# =============================================================================
# Retained because KARBON_ATLASI.md §11 requires it and because the archive for
# a given release stops being downloadable once that release ages out — this
# information cannot be bought back later.
#
# Read as a per-facility attribute, NOT a per-year one. Section 1 records the
# measurement showing the ratings do not vary over time despite being published
# per period. Collapsing them here rather than joining per year makes that
# explicit instead of implying a temporal resolution the data does not have.

message("[2/6] Reading confidence ratings")

read_confidence <- function(cfg, pkg_obj, class_name) {
  members <- ct_confidence_files(pkg_obj$path, cfg$subsectors)
  members <- members[!is.na(members)]
  if (length(members) == 0) {
    warning("No confidence files for ", class_name,
            "; confidence columns will be NA.")
    return(NULL)
  }

  ct_extract(pkg_obj$path, members,
             file.path(DIR_EXTRACT, cfg$gas, "confidence")) |>
    map(read_sector) |>
    list_rbind() |>
    filter(iso3_country == CT_COUNTRY) |>
    group_by(source_id) |>
    summarise(
      confidence_emissions = first(na.omit(emissions_quantity)),
      confidence_activity  = first(na.omit(activity)),
      confidence_capacity  = first(na.omit(capacity)),
      .groups = "drop"
    )
}

confidence <- bind_rows(
  read_confidence(CT_ASSET_CLASSES$industrial, industrial$pkg, "industrial"),
  read_confidence(CT_ASSET_CLASSES$energy,     energy$pkg,     "energy")
) |>
  distinct(source_id, .keep_all = TRUE)

message("      confidence resolved for ", nrow(confidence), " facilities")


# =============================================================================
# 4. COLLAPSE MONTHS TO YEARS
# =============================================================================
# THE CENTRAL DECISION OF THIS SCRIPT. Each variable collapses according to what
# kind of quantity it is, and the three kinds behave differently:
#
#   FLOWS      emissions_quantity, activity. Accumulated over the period, so
#              they SUM. Twelve months of production is the year's production.
#
#   STOCKS     capacity. A plant does not have 12 x 500 MW of capacity in a
#              year; it has 500 MW for twelve months. Summing gives a figure
#              exactly twelve times too large that still renders as a plausible
#              megawatt number. The annual figure is the MEAN of the monthly
#              values — the author's decision, 2026-09-01, on the grounds that
#              it answers "how much capacity existed during that year" and so
#              shows a plant commissioned mid-year at part capacity, which is
#              the correct behaviour for a fleet-turnover animation.
#              `capacity_month_max` is retained so the gate can prove the mean
#              was taken; see gate_panel().
#
#   RATIOS     emission_intensity. RECOMPUTED from the annual totals, as
#              sum(emissions) / sum(activity). It is NOT the mean of the twelve
#              monthly ratios: averaging ratios weights a low-output month
#              equally with a high-output one, which for a seasonal industry
#              like cement biases the result systematically rather than
#              randomly. Same reason a batting average is not the average of
#              monthly averages.
#
# `months_covered` is counted, never assumed. It is a property of the upstream
# release, not of the calendar: the co2 package stops after 5 months of 2026 and
# co2e_100yr after 6, and a version bump changes that without changing any code.

message("[3/6] Collapsing months to years")

collapse_to_years <- function(monthly, gas_basis, release) {
  monthly |>
    mutate(
      year               = as.integer(format(as.Date(start_time), "%Y")),
      emissions_quantity = suppressWarnings(as.numeric(emissions_quantity)),
      activity           = suppressWarnings(as.numeric(activity)),
      capacity           = suppressWarnings(as.numeric(capacity))
    ) |>
    group_by(source_id, year) |>
    summarise(
      # Flows: sum. na.rm = FALSE deliberately — a missing month must surface as
      # NA rather than be quietly treated as a zero-emission month.
      emissions_reported_t = sum(emissions_quantity),
      production_activity  = sum(activity),

      # Stock: mean over the months actually present, plus the max so the gate
      # can verify that a mean and not a sum was taken.
      capacity_mw_or_capacity_t = mean(capacity, na.rm = TRUE),
      capacity_month_max        = suppressWarnings(max(capacity, na.rm = TRUE)),

      # Counted from the data, never assumed to be 12.
      months_covered = n_distinct(start_time),

      # Units differ by sector and a number without its unit is not a number.
      activity_units = first(na.omit(activity_units)),
      capacity_units = first(na.omit(capacity_units)),

      .groups = "drop"
    ) |>
    mutate(
      # -Inf is what max() returns over an all-NA vector. Left as NA rather than
      # propagated into a comparison in the gate.
      capacity_month_max = if_else(is.finite(capacity_month_max),
                                   capacity_month_max, NA_real_),
      capacity_mw_or_capacity_t = if_else(is.finite(capacity_mw_or_capacity_t),
                                          capacity_mw_or_capacity_t, NA_real_),

      # Ratio, recomputed from the annual totals rather than averaged.
      emission_intensity = if_else(
        !is.na(production_activity) & production_activity > 0,
        emissions_reported_t / production_activity,
        NA_real_
      ),

      gas_basis = gas_basis,
      vintage   = release,
      source    = "climate_trace"
    )
}

panel_raw <- bind_rows(
  collapse_to_years(industrial$raw, CT_ASSET_CLASSES$industrial$gas,
                    industrial$release),
  collapse_to_years(energy$raw, CT_ASSET_CLASSES$energy$gas, energy$release)
)

message("      ", nrow(panel_raw), " facility-years before joining facilities")


# =============================================================================
# 5. JOIN THE FACILITY REGISTER
# =============================================================================
# An inner join, deliberately. facilities.rds is the definition of this
# project's population — 300 records, filtered, geocoded, gated and counted in
# every documented figure. A source_id present in the archive but absent from
# that register is out of scope, and letting it into the panel would break every
# "300 facilities" statement silently.
#
# The count dropped here is reported rather than swallowed.

message("[4/6] Joining the facility register")

panel_joined <- panel_raw |>
  mutate(facility_id = paste0("CT", source_id)) |>
  inner_join(
    facilities |> select(facility_id, asset_class, sector, commissioning_year),
    by = "facility_id"
  ) |>
  left_join(confidence, by = "source_id")

dropped <- n_distinct(panel_raw$source_id) - n_distinct(panel_joined$source_id)
message("      ", n_distinct(panel_joined$facility_id), " facilities matched; ",
        dropped, " source_ids in the archive are outside the register")

if (n_distinct(panel_joined$facility_id) != nrow(facilities)) {
  warning(nrow(facilities) - n_distinct(panel_joined$facility_id),
          " facilities in facilities.rds have no rows in the panel. ",
          "Investigate before trusting any total.")
}


# =============================================================================
# 6. ASSEMBLE THE PANEL
# =============================================================================

message("[5/6] Assembling")

panel <- panel_joined |>
  mutate(

    # `status` follows GEM's vocabulary (§6). Only two of its five values are
    # derivable here, and only where a commissioning year exists: Climate TRACE
    # lists every facility in every year regardless of whether it was operating,
    # so presence in the archive proves nothing. Facilities without a
    # commissioning year carry NA rather than an assumption that they always
    # existed — §6 requires them to be visibly excluded from the pre-2021 fleet
    # view, which an invented "operating" would prevent.
    status = case_when(
      is.na(commissioning_year)      ~ NA_character_,
      commissioning_year >  year     ~ "pre_commissioning",
      commissioning_year <= year     ~ "operating"
    ),

    # See LAST_OBSERVED_YEAR in section 1 for why this is an instruction rather
    # than a measurement, and what was checked before accepting it.
    value_type = if_else(year > LAST_OBSERVED_YEAR, "projected", "observed"),

    # ---- Reserved for author decisions, NA until they are taken -------------
    # These are not oversights and must not be filled by inference.
    #
    # co2_direct_t / co2_indirect_t  await B1, the direct/indirect
    #   decomposition. It cannot be done generically: the other1..other10 slots
    #   mean different things in different sectors — iron & steel `other2` is a
    #   direct-plus-indirect QUANTITY, cement `other2` is a calcination FACTOR,
    #   aluminium `other2` is a total QUANTITY. `emissions_reported_t` carries
    #   the published figure meanwhile, so the panel is usable without them.
    #
    # eu_export_share awaits B7. It is a sector-level assumption from HS-code
    #   trade statistics, and for aluminium the ratio it is derived from exceeds
    #   1, which is structurally impossible — see FINDINGS C2. Facility-level
    #   export shares are not public and must never be fabricated (§8.1).
    co2_direct_t    = NA_real_,
    co2_indirect_t  = NA_real_,
    eu_export_share = NA_real_
  ) |>
  select(
    facility_id,
    year,
    status,

    # The gas basis travels on every row. The two populations are never summed,
    # and a total without a stated basis is meaningless (§6).
    gas_basis,

    # The published figure. Not in the §6 schema, added because without it the
    # panel would carry no emissions at all until B1 is decided, which would
    # make it useless for the time slider it exists to feed.
    emissions_reported_t,

    capacity_mw_or_capacity_t,
    capacity_month_max,
    capacity_units,
    production_activity,
    activity_units,

    co2_direct_t,
    co2_indirect_t,
    emission_intensity,
    eu_export_share,

    # Added to the §6 schema. Without it the difference between a 5-month and a
    # 12-month year is invisible in every chart that draws them side by side —
    # and the two gas packages do not even agree on how much of 2026 they carry.
    months_covered,

    # Retained per §11; a per-facility property, not a per-year one.
    confidence_emissions,
    confidence_activity,
    confidence_capacity,

    value_type,
    vintage,
    source,

    # Carried for convenience so the app can filter the panel without joining
    # back to facilities for every interaction.
    asset_class,
    sector
  ) |>
  arrange(facility_id, year)


# =============================================================================
# 7. DIAGNOSTIC: the inputs B1 will need
# =============================================================================
# NOT part of the panel and never read by the app. B1 — the direct/indirect
# decomposition — is the author's decision under §9, and it is currently
# expensive to make because the evidence is spread across ten ambiguously named
# columns in three sectors.
#
# This writes that evidence out once: for each sector, what each other* slot is
# DEFINED as by its own _def label, and the annual totals. It makes the decision
# a reading task rather than a research task. It decides nothing.

message("[6/6] Writing the B1 diagnostic")

# The bare value columns are renamed to `otherN_val` BEFORE pivoting. This is
# not cosmetic: with `.value` in `names_to`, the pair `other1` / `other1_def`
# makes tidyr derive an EMPTY name for the value half, and it fails with
# "Names can't be empty" pointing at column positions rather than at the cause.
# Giving both halves an explicit suffix removes the ambiguity.
b1_inputs <- industrial$raw |>
  mutate(year = as.integer(format(as.Date(start_time), "%Y"))) |>
  select(subsector, year, source_id,
         matches("^other([1-9]|10)(_def)?$")) |>
  rename_with(~ paste0(.x, "_val"), matches("^other([1-9]|10)$")) |>
  pivot_longer(
    cols          = matches("^other([1-9]|10)_(val|def)$"),
    names_to      = c("slot", ".value"),
    names_pattern = "^(other(?:[1-9]|10))_(val|def)$"
  ) |>
  rename(value = "val", definition = "def") |>
  filter(!is.na(definition), definition != "") |>
  mutate(value = suppressWarnings(as.numeric(value))) |>
  group_by(subsector, slot, definition, year) |>
  summarise(
    facilities   = n_distinct(source_id),
    annual_total = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(subsector, slot, year)

write_csv(b1_inputs, file.path(DIR_PROCESSED, "panel_b1_inputs.csv"))
message("      panel_b1_inputs.csv: ", nrow(b1_inputs), " rows")


# =============================================================================
# 8. VALIDATE, THEN WRITE
# =============================================================================
# The gate runs BEFORE saveRDS, so a panel that violates a structural rule
# produces no artefact rather than a plausible-looking wrong one on disk for the
# app to read.

panel <- gate_panel(panel, facilities = facilities)

saveRDS(panel, file.path(DIR_PROCESSED, "facility_panel.rds"))

# =============================================================================
# 9. TR-ETS COVERAGE
# =============================================================================
# Closes the TODO that has been sitting in policies/tr_ets.json as
# `coverage_check_against_this_project: {computed: false}`.
#
# TR-ETS applies above 50,000 tCO2e per year. The threshold matters for v2 and
# not for v1: a facility BELOW it pays no domestic carbon price, so it has
# nothing to deduct from its CBAM liability, and its net position differs from
# an identical facility above the line.
#
# Two honest limits, both stated in the output:
#   - The threshold is on "annual emission capacity of the installation", which
#     is a permitting concept. Realised emissions are what this project has, so
#     this is an approximation of coverage, not a determination of it.
#   - TR-ETS also covers fertiliser production, which this project does not
#     cover at all (no open facility-level source). So the two scopes do not
#     coincide even where the threshold agrees.

message("[7/7] Checking TR-ETS threshold coverage")

# Read from the policy file (§7), the same source the app reads, so the
# classification written here and the threshold the app displays beside it can
# never disagree after a regulatory change.
ETS_THRESHOLD_T <- jsonlite::fromJSON(
  file.path("policies", "tr_ets.json")
)$scope$threshold_tco2e_per_year
stopifnot(is.numeric(ETS_THRESHOLD_T), length(ETS_THRESHOLD_T) == 1)

ets_year <- max(panel$year[panel$value_type == "observed"])

ets_coverage <- panel |>
  filter(asset_class == "industrial", year == ets_year) |>
  left_join(facilities |> select(facility_id, facility_name_tr, province_name_tr),
            by = "facility_id") |>
  transmute(
    facility_id, facility_name_tr, sector, province_name_tr,
    year = ets_year,
    emissions_t = emissions_reported_t,
    threshold_t = ETS_THRESHOLD_T,
    above_threshold = emissions_reported_t >= ETS_THRESHOLD_T,
    basis = "realised CO2, not permitted annual emission capacity",
    note = paste0("Approximation. TR-ETS applies the threshold to the ",
                  "installation's permitted annual emission capacity; this ",
                  "compares realised emissions in ", ets_year, ".")
  ) |>
  arrange(desc(emissions_t))

write_csv(ets_coverage, file.path(DIR_PROCESSED, "tr_ets_coverage.csv"))

n_above <- sum(ets_coverage$above_threshold, na.rm = TRUE)
message("      ", n_above, " of ", nrow(ets_coverage),
        " industrial facilities are above 50,000 tCO2 in ", ets_year)
print(ets_coverage |> count(sector, above_threshold) |> as.data.frame())


coverage <- panel |>
  group_by(asset_class, sector, gas_basis, year) |>
  summarise(
    facilities     = n_distinct(facility_id),
    months_covered = first(months_covered),
    value_type     = first(value_type),
    emissions_t    = sum(emissions_reported_t, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(asset_class, sector, year)

write_csv(coverage, file.path(DIR_PROCESSED, "panel_coverage.csv"))

message("\nWrote facility_panel.rds: ", nrow(panel), " rows, ",
        n_distinct(panel$facility_id), " facilities, ",
        min(panel$year), "-", max(panel$year))
message("Wrote panel_coverage.csv and panel_b1_inputs.csv")

# =============================================================================
# _validate.R — data quality gates for the pipeline
# -----------------------------------------------------------------------------
# NOT a numbered pipeline step. Approved as the second exception to the
# numbering convention in KARBON_ATLASI.md §3, alongside `_sources.R`.
# Acquisition and validation are different jobs and belong in different files.
#
# WHY GATES AND NOT ONLY TESTS
#   `tests/` asserts properties of artefacts AFTER they are written. That is
#   useful but late: a bad `facilities.rds` still lands on disk, still gets read
#   by the app, and is only caught if someone remembers to run the suite. These
#   gates run INSIDE the pipeline, before `saveRDS()`, so a build that violates
#   a hard rule produces no artefact at all.
#
# TIERING — the important design decision
#   Not every problem should stop a build.
#
#   STOP   structural impossibilities. A duplicate key, a coordinate outside
#          Türkiye, a missing schema column. These mean the code is wrong, and
#          writing the file would propagate a silent error downstream.
#
#   WARN   data-quality observations. A facility 129 m from a province border,
#          an operator name that could not be resolved. These are properties of
#          the world, not bugs. They must be visible and recorded — never
#          silently swallowed, never fatal.
#
#   The distinction matters because a gate that stops on everything gets
#   disabled within a week, and a gate that warns on everything is ignored.
#
# Usage:
#   source(file.path("scripts", "_validate.R"))
#   gate_facilities(facilities)      # stops on structural failure
# =============================================================================

suppressPackageStartupMessages({
  library(pointblank)
  library(dplyr)
  library(stringr)
})


# --- Constants ---------------------------------------------------------------

# Bounding box for Türkiye INCLUDING its maritime jurisdiction. The northern
# bound is 43.5 rather than the 42.5 of the land border because Turkish offshore
# gas production sits in the Black Sea at about 42.94°N — the Sakarya field,
# which the first version of this gate rejected as a coordinate error.
#
# Widening it does not weaken the guard. The job here is catching a lon/lat
# swap, and a swap of those very coordinates gives 31.3°N / 42.9°E, in Saudi
# Arabia, still far outside the box. The box has to admit real assets or it
# trains people to disable it.
TR_LON <- c(25.5, 45.5)
TR_LAT <- c(35.5, 43.5)

VALID_ASSET_CLASS     <- c("industrial", "energy")
VALID_LIABILITY_CLASS <- c("direct", "indirect_driver", "neutral")
VALID_GEOCODE_QUALITY <- c("within_province", "boundary_proximate",
                           "snapped_to_nearest", "offshore")

# Boundary distance below which a province assignment is reported as uncertain.
# A judgement call, not a standard — recorded here so it is visible and tunable
# in one place rather than buried in a script.
BOUNDARY_WARN_M <- 2000


# --- Reporting ---------------------------------------------------------------

.gate_header <- function(label) {
  message("\n", strrep("-", 68))
  message("GATE: ", label)
  message(strrep("-", 68))
}

#' Stop the build, printing every structural failure rather than only the first.
#'
#' Reporting all of them at once matters: fixing one, re-running a five-minute
#' pipeline, and discovering the next is how a validation layer becomes hated.
.gate_stop <- function(failures, label) {
  if (length(failures) == 0) return(invisible(TRUE))
  stop("\n", strrep("!", 68), "\n",
       "GATE FAILED: ", label, "\n",
       paste0("  - ", failures, collapse = "\n"), "\n",
       strrep("!", 68),
       "\nNo artefact was written. Fix the pipeline, do not relax the gate.",
       call. = FALSE)
}

.gate_warn <- function(observations, label) {
  if (length(observations) == 0) {
    message("  (no quality observations)")
    return(invisible(TRUE))
  }
  for (o in observations) message("  WARN  ", o)
  invisible(TRUE)
}


# --- Generic checks ----------------------------------------------------------

#' Assert that a data frame carries every required column.
require_columns <- function(df, required, what = "table") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    sprintf("%s is missing required columns: %s",
            what, paste(missing, collapse = ", "))
  } else character(0)
}

#' Assert a column has no NA.
require_complete <- function(df, cols) {
  cols |>
    purrr::keep(~ .x %in% names(df) && any(is.na(df[[.x]]))) |>
    purrr::map_chr(~ sprintf("column `%s` contains %d NA values",
                             .x, sum(is.na(df[[.x]]))))
}

#' Assert a column is unique.
require_unique <- function(df, cols) {
  cols |>
    purrr::keep(~ .x %in% names(df) && any(duplicated(df[[.x]]))) |>
    purrr::map_chr(~ sprintf("column `%s` has %d duplicate values",
                             .x, sum(duplicated(df[[.x]]))))
}

#' Assert a column's values are drawn from a closed set.
require_in_set <- function(df, col, allowed) {
  if (!col %in% names(df)) return(character(0))
  bad <- setdiff(unique(df[[col]]), c(allowed, NA))
  if (length(bad) > 0) {
    sprintf("column `%s` contains values outside the allowed set: %s",
            col, paste(bad, collapse = ", "))
  } else character(0)
}


# --- The facilities gate -----------------------------------------------------

#' Validate a facilities table before it is written.
#'
#' Returns the table invisibly so it can be used inline:
#'   facilities |> gate_facilities() |> saveRDS(path)
#'
#' @param df       the assembled facilities table
#' @param report   optional path; writes a pointblank agent report as HTML
gate_facilities <- function(df, report = NULL) {

  .gate_header(sprintf("facilities (%d rows)", nrow(df)))

  # ---- STOP tier: structural impossibilities -------------------------------
  required <- c("facility_id", "facility_name_tr", "sector", "technology",
                "liability_class", "lat", "lon", "province_code",
                "nuts2_code", "country_iso3", "regime_id",
                "source", "source_id", "geocode_quality")

  failures <- c(
    require_columns(df, required, "facilities"),
    require_unique(df, c("facility_id", "source_id")),
    require_complete(df, c("facility_id", "sector", "lat", "lon",
                           "province_code", "nuts2_code")),
    require_in_set(df, "liability_class", VALID_LIABILITY_CLASS),
    require_in_set(df, "geocode_quality", VALID_GEOCODE_QUALITY),
    require_in_set(df, "country_iso3", "TUR")
  )

  if ("asset_class" %in% names(df)) {
    failures <- c(failures, require_in_set(df, "asset_class", VALID_ASSET_CLASS))
  }

  # Geography. A lon/lat inversion is the single most consequential silent error
  # available here, so it is checked explicitly rather than trusted.
  if (all(c("lat", "lon") %in% names(df))) {
    out_lat <- sum(df$lat < TR_LAT[1] | df$lat > TR_LAT[2], na.rm = TRUE)
    out_lon <- sum(df$lon < TR_LON[1] | df$lon > TR_LON[2], na.rm = TRUE)
    if (out_lat > 0 || out_lon > 0) {
      failures <- c(failures, sprintf(
        paste("%d facilities fall outside Türkiye's bounding box",
              "(%d by latitude, %d by longitude) — check for a lon/lat swap"),
        max(out_lat, out_lon), out_lat, out_lon))
    }
  }

  if ("province_code" %in% names(df)) {
    bad <- setdiff(unique(df$province_code), 1:81)
    if (length(bad) > 0) {
      failures <- c(failures, sprintf("province_code outside 1..81: %s",
                                      paste(bad, collapse = ", ")))
    }
  }

  # A province mapping to two different İBBS-2 regions means the reference table
  # is corrupted, and every regional aggregate downstream would be wrong.
  if (all(c("province_code", "nuts2_code") %in% names(df))) {
    dup <- df |>
      distinct(province_code, nuts2_code) |>
      count(province_code) |>
      filter(n > 1)
    if (nrow(dup) > 0) {
      failures <- c(failures, sprintf(
        "province(s) mapped to more than one NUTS-2 region: %s",
        paste(dup$province_code, collapse = ", ")))
    }
  }

  # Encoding damage is silent on Windows and surfaces as mojibake in the browser.
  if ("province_name_tr" %in% names(df)) {
    mojibake <- df |> filter(str_detect(province_name_tr, "Ã|Â|�"))
    if (nrow(mojibake) > 0) {
      failures <- c(failures, sprintf(
        "%d province names show mojibake — the file was read or written with the wrong encoding",
        nrow(mojibake)))
    }
  }

  .gate_stop(failures, "facilities")
  message("  STOP tier: passed (", length(required), " columns, ",
          nrow(df), " rows)")

  # ---- WARN tier: observations about the world, not the code ---------------
  observations <- character(0)

  if ("geocode_quality" %in% names(df)) {
    q <- table(factor(df$geocode_quality, levels = VALID_GEOCODE_QUALITY))
    if (q[["boundary_proximate"]] > 0) {
      observations <- c(observations, sprintf(
        "%d facilities lie within %d m of a province border — assignment uncertain",
        q[["boundary_proximate"]], BOUNDARY_WARN_M))
    }
    if (q[["snapped_to_nearest"]] > 0) {
      observations <- c(observations, sprintf(
        "%d facilities fell outside every polygon and were snapped to the nearest province",
        q[["snapped_to_nearest"]]))
    }
    if (q[["offshore"]] > 0) {
      observations <- c(observations, sprintf(
        paste("%d facilities are offshore — assigned to the nearest coastal",
              "province, which is an administrative convenience rather than a",
              "location"),
        q[["offshore"]]))
    }
  }

  if ("operator_name" %in% names(df)) {
    n_na <- sum(is.na(df$operator_name))
    if (n_na > 0) {
      observations <- c(observations,
                        sprintf("%d facilities have no resolved operator", n_na))
    }
  }

  # The fleet timeline rests entirely on this field, so its absence is reported
  # as a share rather than a count — a bare number hides whether the timeline is
  # viable.
  if ("commissioning_year" %in% names(df)) {
    n_na <- sum(is.na(df$commissioning_year))
    if (n_na > 0) {
      observations <- c(observations, sprintf(
        "%d of %d facilities (%.0f%%) have no commissioning year — they must be excluded from the pre-2021 fleet animation, not assumed to have always existed",
        n_na, nrow(df), 100 * n_na / nrow(df)))
    }
  }

  # Near-coincident facilities. The naive version of this check listed every
  # pair within 500 m and, once the energy layer arrived, produced thirty
  # warnings — mine-mouth power stations beside their mine, captive plants
  # beside their industrial host, a refinery beside its own generator. All are
  # genuinely distinct facilities that happen to share a site, and burying the
  # real signal in them is how a WARN tier gets ignored.
  #
  # Two cases are now separated:
  #   SAME subsector  — plausibly one site recorded twice. Listed individually,
  #                     because this is the Kars cement question.
  #   CROSS subsector — expected co-location. Counted, not enumerated, and
  #                     reported because it is what the grid factor has to net
  #                     out (ROADMAP E5).
  if (all(c("lat", "lon") %in% names(df)) && nrow(df) > 1 &&
      requireNamespace("sf", quietly = TRUE)) {
    pts <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
    d <- sf::st_distance(pts); units(d) <- NULL; diag(d) <- Inf
    near <- which(d < 500, arr.ind = TRUE)
    near <- near[near[, 1] < near[, 2], , drop = FALSE]

    if (nrow(near) > 0) {
      grp <- if ("sector" %in% names(df)) df$sector else rep("", nrow(df))
      same <- grp[near[, 1]] == grp[near[, 2]]

      for (i in which(same)) {
        observations <- c(observations, sprintf(
          "possible duplicate: %s and %s, same subsector, %.0f m apart",
          df$facility_name_tr[near[i, 1]], df$facility_name_tr[near[i, 2]],
          d[near[i, 1], near[i, 2]]))
      }

      if (any(!same)) {
        observations <- c(observations, sprintf(
          paste("%d co-located pairs across different subsectors (mine-mouth",
                "plants, captive generation, refinery own-use) — expected, but",
                "they must be netted out of any site-level total"),
          sum(!same)))
      }
    }
  }

  .gate_warn(observations, "facilities")

  # ---- Optional pointblank report ------------------------------------------
  # The gates above are the enforcement. pointblank adds a shareable HTML
  # artefact for the record, which is useful evidence for METHODOLOGY but is not
  # what stops a bad build.
  if (!is.null(report)) {
    agent <- create_agent(tbl = df, tbl_name = "facilities",
                          label = "Facility table validation") |>
      col_exists(all_of(required)) |>
      rows_distinct(vars(facility_id)) |>
      col_vals_between(vars(lat), TR_LAT[1], TR_LAT[2]) |>
      col_vals_between(vars(lon), TR_LON[1], TR_LON[2]) |>
      col_vals_between(vars(province_code), 1, 81) |>
      col_vals_in_set(vars(liability_class), VALID_LIABILITY_CLASS) |>
      col_vals_in_set(vars(geocode_quality), VALID_GEOCODE_QUALITY) |>
      col_vals_not_null(vars(facility_id, sector, lat, lon)) |>
      interrogate()

    export_report(agent, filename = report, quiet = TRUE)
    message("  pointblank report: ", report)
  }

  invisible(df)
}


# =============================================================================
# The panel gate
# =============================================================================
# The panel is where this project's characteristic failure mode lives. A
# facilities table is wrong in ways you can see — a dot in the sea, a duplicate
# name. A panel is wrong in ways that look like a number: a capacity summed over
# twelve months instead of averaged is exactly twelve times too large and still
# renders as a plausible megawatt figure on a plausible chart.
#
# These checks therefore target arithmetic that produces believable wrong
# answers, not malformed data.

VALID_VALUE_TYPE <- c("observed", "legislated", "scenario", "projected",
                      "assumption", "diagnostic_not_for_use")

VALID_GAS_BASIS <- c("co2", "co2e_100yr")

# Twelve for a complete year. Anything else is a partial year and must be
# visible as one rather than being compared with a full one.
MONTHS_COMPLETE <- 12

#' Validate the facility-year panel before it is written.
#'
#' @param df       the assembled panel
#' @param facilities the facilities table, to check referential integrity
gate_panel <- function(df, facilities = NULL) {

  .gate_header(sprintf("facility_panel (%d rows)", nrow(df)))

  # ---- STOP tier: structural impossibilities -------------------------------
  required <- c("facility_id", "year", "gas_basis", "emissions_reported_t",
                "production_activity", "months_covered", "value_type",
                "vintage", "source")

  failures <- c(
    require_columns(df, required, "facility_panel"),
    require_complete(df, c("facility_id", "year", "gas_basis", "value_type")),
    require_in_set(df, "value_type", VALID_VALUE_TYPE),
    require_in_set(df, "gas_basis", VALID_GAS_BASIS)
  )

  # The panel key is the PAIR. A duplicate facility-year silently doubles that
  # facility everywhere it is aggregated, and nothing about the resulting number
  # looks wrong.
  if (all(c("facility_id", "year") %in% names(df))) {
    dup <- sum(duplicated(df[c("facility_id", "year")]))
    if (dup > 0) {
      failures <- c(failures, sprintf(
        "facility_id x year is not unique: %d duplicate pairs", dup))
    }
  }

  # A facility-year with no facility is a join that lost rows. Reported as
  # structural because every downstream layer joins on this key.
  if (!is.null(facilities) && "facility_id" %in% names(df)) {
    orphan <- setdiff(unique(df$facility_id), facilities$facility_id)
    if (length(orphan) > 0) {
      failures <- c(failures, sprintf(
        "%d facility_id values in the panel are absent from facilities.rds: %s",
        length(orphan), paste(utils::head(orphan, 5), collapse = ", ")))
    }
  }

  # Negative emissions or activity are not a data-quality nuance here: none of
  # these subsectors can remove carbon or produce negative tonnes.
  for (col in c("emissions_reported_t", "production_activity",
                "capacity_mw_or_capacity_t")) {
    if (col %in% names(df) && any(df[[col]] < 0, na.rm = TRUE)) {
      failures <- c(failures, sprintf(
        "column `%s` contains %d negative values", col,
        sum(df[[col]] < 0, na.rm = TRUE)))
    }
  }

  # months_covered must be a real month count. If this is ever 0 the aggregation
  # produced a row from nothing; above 12 it double-counted.
  if ("months_covered" %in% names(df)) {
    bad <- df$months_covered < 1 | df$months_covered > MONTHS_COMPLETE
    if (any(bad, na.rm = TRUE)) {
      failures <- c(failures, sprintf(
        "months_covered outside 1-12 for %d rows", sum(bad, na.rm = TRUE)))
    }
  }

  # THE TWELVE-TIMES CHECK. Capacity is a stock: the annual figure is the mean
  # of the monthly values, so it must sit inside the monthly range. If someone
  # changes the aggregation to sum(), every complete year jumps by roughly 12x
  # and this is what catches it.
  if (all(c("capacity_mw_or_capacity_t", "capacity_month_max") %in% names(df))) {
    over <- df$capacity_mw_or_capacity_t > df$capacity_month_max * 1.001
    if (any(over, na.rm = TRUE)) {
      failures <- c(failures, sprintf(
        paste0("annual capacity exceeds the largest monthly value for %d rows. ",
               "Capacity is a stock and must be averaged, not summed."),
        sum(over, na.rm = TRUE)))
    }
  }

  .gate_stop(failures, "facility_panel")

  # ---- WARN tier: quality observations -------------------------------------
  obs <- character(0)

  # Partial years are legitimate and must not stop a build, but a partial year
  # compared against a complete one is the mistake this project is most likely
  # to make in a chart. Report the shape so it is visible at build time.
  if (all(c("year", "months_covered", "gas_basis") %in% names(df))) {
    partial <- df |>
      dplyr::filter(months_covered < MONTHS_COMPLETE) |>
      dplyr::distinct(gas_basis, year, months_covered)

    for (i in seq_len(nrow(partial))) {
      obs <- c(obs, sprintf(
        "%s year %s is partial: %d of 12 months. Never annualise by scaling.",
        partial$gas_basis[i], partial$year[i], partial$months_covered[i]))
    }

    # The two populations having DIFFERENT partial depths is worse than either
    # being partial, because a chart will place them side by side as one year.
    depths <- partial |>
      dplyr::group_by(year) |>
      dplyr::summarise(n = dplyr::n_distinct(months_covered), .groups = "drop") |>
      dplyr::filter(n > 1)

    if (nrow(depths) > 0) {
      obs <- c(obs, sprintf(
        paste0("year %s has DIFFERENT month depths across gas bases. The two ",
               "populations' figures for it are not commensurable and any ",
               "chart drawing them together must say so."),
        paste(depths$year, collapse = ", ")))
    }
  }

  # A facility reporting emissions in a year its commissioning register says it
  # did not yet exist. Not a build failure — the two sources disagree and this
  # project's rule is to report the disagreement rather than reconcile it
  # silently — but it directly threatens the fleet timeline, which is the entire
  # reason commissioning years were fetched.
  if (all(c("status", "emissions_reported_t") %in% names(df))) {
    contra <- df |>
      dplyr::filter(status == "pre_commissioning",
                    !is.na(emissions_reported_t),
                    emissions_reported_t > 0)
    if (nrow(contra) > 0) {
      obs <- c(obs, sprintf(
        paste0("%d facility-years report emissions before their GEM ",
               "commissioning year (%d facilities). GEM and Climate TRACE ",
               "disagree; neither has been overridden."),
        nrow(contra), dplyr::n_distinct(contra$facility_id)))
    }
  }

  # More than one upstream release inside one artefact. Legitimate here — the
  # two gas packages ship at different versions — but it must never be silent,
  # because release-to-release revisions in this source are large: Turkish
  # generation for the identical fleet moved 12% between v5_9_0 and v5_10_0.
  if ("vintage" %in% names(df)) {
    vints <- sort(unique(stats::na.omit(df$vintage)))
    if (length(vints) > 1) {
      obs <- c(obs, sprintf(
        paste0("the panel mixes %d upstream releases (%s). Any figure ",
               "comparing the two populations must cite both."),
        length(vints), paste(vints, collapse = ", ")))
    }
  }

  # An intensity with no activity behind it is not an error, but it is a hole in
  # the audit trail and the user must be told which facilities it affects.
  if ("production_activity" %in% names(df)) {
    no_act <- sum(is.na(df$production_activity) | df$production_activity == 0)
    if (no_act > 0) {
      obs <- c(obs, sprintf(
        "%d of %d rows have no activity, so no intensity can be computed",
        no_act, nrow(df)))
    }
  }

  # Placeholder columns reserved for author decisions. Reported every build so
  # they cannot quietly become permanent.
  for (col in c("co2_direct_t", "co2_indirect_t", "eu_export_share")) {
    if (col %in% names(df) && all(is.na(df[[col]]))) {
      obs <- c(obs, sprintf(
        "`%s` is entirely NA — awaiting an author decision, not yet computed",
        col))
    }
  }

  .gate_warn(obs, "facility_panel")

  invisible(df)
}


# =============================================================================
# The fleet timeline gate
# =============================================================================
# A cumulative series has one failure mode that looks entirely normal: it goes
# down. Every other defect here is of the same family — a number that is the
# right shape, the right type and quietly impossible.

#' Validate the 2000-2026 fleet development series before it is written.
#'
#' @param df       the assembled timeline
#' @param coverage the per-group coverage table, for cross-checking totals
gate_fleet_timeline <- function(df, coverage = NULL) {

  .gate_header(sprintf("fleet_timeline (%d rows)", nrow(df)))

  required <- c("group", "year", "added_mw", "added_n", "cum_mw", "cum_n",
                "undated_n")

  failures <- c(
    require_columns(df, required, "fleet_timeline"),
    require_complete(df, c("group", "year", "cum_n"))
  )

  if (all(c("group", "year") %in% names(df))) {
    dup <- sum(duplicated(df[c("group", "year")]))
    if (dup > 0) {
      failures <- c(failures, sprintf(
        "group x year is not unique: %d duplicate pairs", dup))
    }

    # Every year in the window must be present for every group. A missing year
    # breaks a line chart into segments, which reads as missing data rather than
    # as a year in which nothing was built.
    per_group <- df |> dplyr::count(group, name = "years")
    if (dplyr::n_distinct(per_group$years) > 1) {
      failures <- c(failures, sprintf(
        "groups have different year counts (%s) — the series is not rectangular",
        paste(sprintf("%s:%d", per_group$group, per_group$years),
              collapse = ", ")))
    }
  }

  # THE CUMULATIVE CHECK. A cumulative series cannot decrease. If it does, the
  # sort order was lost or a group was mixed, and the resulting chart shows a
  # fleet shrinking — which this data cannot express at all, because retirement
  # is not modelled. It would be read as plants closing.
  for (col in c("cum_mw", "cum_n")) {
    if (!col %in% names(df)) next
    # No `default =` on lag(): cum_n is integer and cum_mw is double, and a
    # shared numeric default fails the type check on one of them. The first row
    # of each group has no predecessor, which is not a decrease.
    bad <- df |>
      dplyr::filter(!is.na(.data[[col]])) |>
      dplyr::arrange(group, year) |>
      dplyr::group_by(group) |>
      dplyr::mutate(.prev = dplyr::lag(.data[[col]])) |>
      dplyr::filter(!is.na(.prev), .data[[col]] < .prev) |>
      dplyr::ungroup()
    if (nrow(bad) > 0) {
      failures <- c(failures, sprintf(
        paste0("`%s` decreases at %d points (first: %s %d). A cumulative ",
               "series cannot fall, and retirement is not modelled here, so ",
               "this would render as plants closing."),
        col, nrow(bad), bad$group[1], bad$year[1]))
    }
  }

  # Megawatts belong only to the groups measured in megawatts. A zero in the
  # counted-only groups would read as "no capacity" rather than "not measured".
  MW_GROUPS <- c("ct_combustion", "renewable")
  if (all(c("group", "cum_mw") %in% names(df))) {
    leaked <- df |> dplyr::filter(!group %in% MW_GROUPS, !is.na(cum_mw))
    if (nrow(leaked) > 0) {
      failures <- c(failures, sprintf(
        paste0("%d rows carry megawatts for a group measured in tonnes or ",
               "barrels (%s). Those capacities are not commensurable and must ",
               "stay NA."),
        nrow(leaked), paste(unique(leaked$group), collapse = ", ")))
    }
  }

  .gate_stop(failures, "fleet_timeline")

  # ---- WARN tier -----------------------------------------------------------
  obs <- character(0)

  # Coverage asymmetry. Not a defect — a property of the sources — but drawing
  # two curves of very different completeness together is the mistake this whole
  # view is most likely to make, so the numbers are printed on every build.
  if (!is.null(coverage) && "share" %in% names(coverage)) {
    mw_cov <- coverage[coverage$group %in% MW_GROUPS, ]
    if (nrow(mw_cov) > 1) {
      spread <- diff(range(mw_cov$share))
      obs <- c(obs, sprintf(
        "coverage across the megawatt groups differs by %.0f points (%s)",
        100 * spread,
        paste(sprintf("%s %.0f%%", mw_cov$group, 100 * mw_cov$share),
              collapse = ", ")))
      if (spread > 0.15) {
        obs <- c(obs, paste0(
          "that spread exceeds 15 points: the curves are NOT directly ",
          "comparable and the view must show the coverage band, not a footnote."))
      }
    }
  }

  # Our cumulative against the national reference. Reported as a share rather
  # than a difference, because the gap is coverage MINUS retirement and the two
  # work in opposite directions — it cannot be attributed to either alone.
  if (all(c("ember_total_gw", "cum_mw", "year") %in% names(df))) {
    last_yr <- df |>
      dplyr::filter(!is.na(ember_total_gw)) |>
      dplyr::pull(year) |> max(na.rm = TRUE)

    if (is.finite(last_yr)) {
      ours <- sum(df$cum_mw[df$year == last_yr], na.rm = TRUE) / 1000
      theirs <- df$ember_total_gw[df$year == last_yr][1]
      if (!is.na(theirs) && theirs > 0) {
        obs <- c(obs, sprintf(
          paste0("%d: this atlas accounts for %.1f GW of Ember's %.1f GW ",
                 "installed (%.0f%%). The shortfall is coverage minus ",
                 "unmodelled retirement and cannot be attributed to either."),
          last_yr, ours, theirs, 100 * ours / theirs))
      }
    }
  }

  if ("undated_n" %in% names(df)) {
    und <- df |> dplyr::distinct(group, undated_n) |>
      dplyr::filter(undated_n > 0)
    for (i in seq_len(nrow(und))) {
      obs <- c(obs, sprintf(
        paste0("%s: %d facilities carry no commissioning year — they must be ",
               "shown as their own state, never folded into a year"),
        und$group[i], und$undated_n[i]))
    }
  }

  .gate_warn(obs, "fleet_timeline")

  invisible(df)
}


# =============================================================================
# The policy parameter gate
# =============================================================================
# Policy files are hand-edited, and hand-edited JSON drifts. A renamed field, a
# date typed as 19-08-2026, a scope_status quietly changed to something the
# fetch script does not recognise — none of these raise an error at read time.
# They produce NULL, and NULL propagates into a figure.
#
# Two layers, because they catch different things:
#   1. JSON Schema, in policies/_schema/, validates STRUCTURE.
#   2. R checks below validate RELATIONSHIPS between elements, which JSON Schema
#      cannot express — monotonicity of the phase-in, the factor and free
#      allocation summing to one, a scenario having either a source or an
#      admission that it needs one.

POLICY_SCHEMA_DIR <- file.path("policies", "_schema")

#' Read a file as a single UTF-8 string.
#'
#' Explicit about encoding because these files carry Turkish characters and the
#' Windows default is not UTF-8 (§2). Returning one string rather than a path
#' also keeps jsonvalidate from having to guess what it was handed.
.read_utf8 <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  s <- rawToChar(raw)
  Encoding(s) <- "UTF-8"
  s
}

#' Escape every non-ASCII character as a \uXXXX JSON escape.
#'
#' WHY THIS EXISTS. jsonvalidate runs ajv inside V8, and handing V8 a string
#' containing raw UTF-8 Turkish characters fails intermittently on Windows with
#' `SyntaxError: Invalid or unexpected token` — a message that names neither the
#' file nor the field. It reproduced under `testthat` and not outside it, which
#' makes it an encoding-handoff problem somewhere in the R → V8 boundary rather
#' than a defect in the documents.
#'
#' `\uXXXX` escaping is part of the JSON specification, so the escaped text is
#' the same document by any conforming parser. Escaping removes the ambiguity
#' instead of depending on every layer agreeing about encoding.
#'
#' Handles the Basic Multilingual Plane, which covers all Turkish characters.
#' Anything above U+FFFF would need surrogate pairs; if such a character ever
#' appears in a policy file this must be extended rather than silently mangling.
.ascii_escape <- function(s) {
  cp <- utf8ToInt(s)
  if (all(cp < 128L)) return(s)

  if (any(cp > 0xFFFFL)) {
    stop("Policy JSON contains a character above U+FFFF; .ascii_escape() ",
         "handles the BMP only and must be extended before this can be validated.",
         call. = FALSE)
  }

  out <- ifelse(cp < 128L, intToUtf8(cp, multiple = TRUE),
                sprintf("\\u%04x", cp))
  paste(out, collapse = "")
}

#' Validate every policy file against its schema and its internal relationships.
#'
#' @param dir       directory holding the policy JSON files
#' @param schema_dir directory holding `<name>.schema.json`
gate_policies <- function(dir = "policies", schema_dir = POLICY_SCHEMA_DIR) {

  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)

  .gate_header(sprintf("policies (%d files)", length(files)))

  if (length(files) == 0) {
    stop("No policy files found in ", dir, call. = FALSE)
  }

  failures <- character(0)

  for (f in files) {
    nm     <- tools::file_path_sans_ext(basename(f))
    schema <- file.path(schema_dir, paste0(nm, ".schema.json"))

    # An unschema'd policy file is itself a failure. Otherwise the way to bypass
    # validation is simply to add a new file, which defeats the gate entirely.
    if (!file.exists(schema)) {
      failures <- c(failures, sprintf(
        "%s has no schema at %s — every policy file must be schema-backed",
        basename(f), schema))
      next
    }

    # Read both sides as explicit UTF-8 strings rather than handing jsonvalidate
    # a path. jsonvalidate decides whether an argument is a filename or a JSON
    # literal, and on Windows that guess interacts badly with backslash paths
    # and with the Turkish characters inside these files — the symptom is a V8
    # "SyntaxError: Invalid or unexpected token" that says nothing about which
    # file or which field. Passing strings removes the guess.
    ok <- jsonvalidate::json_validate(
      json    = .ascii_escape(.read_utf8(f)),
      schema  = .ascii_escape(.read_utf8(schema)),
      engine  = "ajv",
      verbose = TRUE
    )
    if (!ok) {
      errs <- attr(ok, "errors")
      detail <- if (is.data.frame(errs) && nrow(errs) > 0) {
        paste0(errs$instancePath, " ", errs$message, collapse = "; ")
      } else "schema validation failed"
      failures <- c(failures, sprintf("%s: %s", basename(f), detail))
    }
  }

  # --- Relationship checks JSON Schema cannot express ------------------------

  timeline_path <- file.path(dir, "policy_timeline.json")
  if (file.exists(timeline_path)) {
    tl <- jsonlite::fromJSON(timeline_path, simplifyDataFrame = FALSE)
    ev <- tl$events

    ids <- vapply(ev, function(e) e$id, character(1))
    if (anyDuplicated(ids) > 0) {
      failures <- c(failures, sprintf(
        "policy_timeline.json: duplicate event id (%s). Ids are referenced from the interface and must be stable and unique.",
        paste(unique(ids[duplicated(ids)]), collapse = ", ")))
    }

    for (e in ev) {
      # A bare year with granularity "day", or a full date with granularity
      # "year", means the two disagree — and the interface trusts granularity to
      # decide whether to draw a point or a band. Schema can check the pattern
      # but not the pairing.
      is_day <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", e$date)
      if (is_day && e$granularity != "day") {
        failures <- c(failures, sprintf(
          "policy_timeline.json: %s carries a full date but granularity '%s'",
          e$id, e$granularity))
      }
      if (!is_day && e$granularity != "year") {
        failures <- c(failures, sprintf(
          "policy_timeline.json: %s carries a bare year but granularity '%s'",
          e$id, e$granularity))
      }
      if (is_day && is.na(suppressWarnings(as.Date(e$date)))) {
        failures <- c(failures, sprintf(
          "policy_timeline.json: %s has an unparseable date '%s'", e$id, e$date))
      }

      # Section 7: a regulatory fact without a citation is not usable. Same rule
      # the price scenarios are held to.
      has_url <- !is.null(e$source_url) && nzchar(e$source_url)
      if (!has_url && !isTRUE(e$citation_required)) {
        failures <- c(failures, sprintf(
          "policy_timeline.json: %s has no source_url and does not admit citation_required",
          e$id))
      }

      # A category the file does not define would render with no colour and no
      # label, which reads as a missing event rather than a broken reference.
      if (!e$category %in% names(tl$categories)) {
        failures <- c(failures, sprintf(
          "policy_timeline.json: %s uses category '%s', which is not defined in `categories`",
          e$id, e$category))
      }
    }

    # `status` is a claim about coverage. It may only say "complete" when at
    # least one event actually claims to bear on the fleet — otherwise the file
    # is asserting it covers the policy history while containing nothing that
    # touches the period the fleet chart draws.
    if (identical(tl$meta$status, "complete") &&
        !any(vapply(ev, function(e) isTRUE(e$affects_fleet), logical(1)))) {
      failures <- c(failures, paste0(
        "policy_timeline.json: status is 'complete' but no event is marked ",
        "affects_fleet. Turkish energy policy is what would make it complete; ",
        "see meta$what_is_missing."))
    }
  }

  phase_in_path <- file.path(dir, "cbam_phase_in.json")
  if (file.exists(phase_in_path)) {
    p <- jsonlite::fromJSON(phase_in_path)$phase_in

    if (!all(abs(p$cbam_factor + p$free_allocation_share - 1) < 1e-9)) {
      failures <- c(failures,
        "cbam_phase_in.json: cbam_factor and free_allocation_share must sum to 1")
    }
    if (!all(diff(p$cbam_factor) > 0)) {
      failures <- c(failures,
        "cbam_phase_in.json: cbam_factor must increase monotonically — the obligation phases in, it never reverses")
    }
    if (!isTRUE(p$cbam_factor[p$year == 2034] == 1)) {
      failures <- c(failures,
        "cbam_phase_in.json: full application in 2034 is the legislated endpoint and must equal 1")
    }
  }

  prices_path <- file.path(dir, "carbon_price_scenarios.json")
  if (file.exists(prices_path)) {
    j <- jsonlite::fromJSON(prices_path, simplifyVector = FALSE)
    for (nm in names(j$scenarios)) {
      s <- j$scenarios[[nm]]
      has_source <- !is.null(s$source_url) && nzchar(s$source_url)
      admits_gap <- isTRUE(s$citation_required)
      user_set   <- identical(nm, "custom")
      if (!(has_source || admits_gap || user_set)) {
        failures <- c(failures, sprintf(
          "carbon_price_scenarios.json: scenario '%s' has neither a source_url nor citation_required — section 7 forbids an uncited regulatory number",
          nm))
      }
    }
  }

  .gate_stop(failures, "policies")
  message("  STOP tier: passed (", length(files), " files, all schema-backed)")

  # --- WARN tier -------------------------------------------------------------
  observations <- character(0)

  if (file.exists(prices_path)) {
    j <- jsonlite::fromJSON(prices_path, simplifyVector = FALSE)
    needy <- names(j$scenarios)[vapply(j$scenarios,
                                       function(s) isTRUE(s$citation_required),
                                       logical(1))]
    if (length(needy) > 0) {
      observations <- c(observations, sprintf(
        "scenario(s) still awaiting a citation: %s", paste(needy, collapse = ", ")))
    }
  }

  codes_path <- file.path(dir, "cbam_goods_cn_codes.json")
  if (file.exists(codes_path)) {
    st <- jsonlite::fromJSON(codes_path, simplifyVector = FALSE)$meta$scope_status
    if (!identical(st, "annex_i_verified")) {
      observations <- c(observations,
        "cbam_goods_cn_codes.json is still PROVISIONAL_AGGREGATE — derived shares are upper bounds")
    }
  }

  .gate_warn(observations, "policies")
  invisible(TRUE)
}

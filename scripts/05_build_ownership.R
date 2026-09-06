# =============================================================================
# 05_build_ownership.R — who owns the carbon
# -----------------------------------------------------------------------------
# PURPOSE
#   Produce `data/processed/ownership.rds`: one row per facility per parent
#   entity, plus `ownership_summary.rds`, one row per facility.
#
#   Climate TRACE ships an ownership file alongside every sector file and this
#   project has been reading exactly one column out of it — `immediate_source_
#   owner`, for the operator name. The rest carries the ultimate parents, their
#   entity type, their headquarter country, and the share reaching them through
#   the ownership path.
#
#   That answers a question the atlas could not previously ask: not what emits,
#   but WHO OWNS what emits — and, for a CBAM project, how much of Türkiye's
#   industrial carbon sits under EU-headquartered groups. A German cement group
#   operating in Türkiye pays CBAM at the EU border on its own product.
#
# THE TRAP THAT SHAPES THIS WHOLE SCRIPT
#   `overall_share_percent` is documented as "share percent from multiplying all
#   the share percentages in the ownership_path together". It is NOT a partition
#   of the facility. Measured across our 300 facilities, the per-facility sums
#   are:
#
#       <= 50%     108 facilities
#       ~100%       84
#       100-200%    46
#       > 200%      21
#
#   Multiple paths can reach the same ultimate parent, and each carries its own
#   product. Summing them double-counts; weighting emissions by them produces a
#   number that looks like a percentage and is not one.
#
#   SO NO EMISSIONS-WEIGHTED OWNERSHIP PERCENTAGE IS COMPUTED HERE, and none
#   should be computed downstream. Attribution is by PRESENCE — "this facility
#   has at least one EU-headquartered parent" — which the data does support.
#   The share is carried through for reference and flagged, never aggregated.
#
# PREREQUISITE
#   Rscript scripts/02_build_facilities.R
#
# OUTPUTS
#   data/processed/ownership.rds           facility x parent, as published
#   data/processed/ownership_summary.rds   one row per facility
#
# RUN
#   Rscript scripts/05_build_ownership.R
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

DIR_RAW       <- file.path("data", "raw", "climate_trace")
DIR_EXTRACT   <- file.path(DIR_RAW, "extracted")
DIR_PROCESSED <- file.path("data", "processed")

FACILITIES <- file.path(DIR_PROCESSED, "facilities.rds")
if (!file.exists(FACILITIES)) {
  stop("facilities.rds not found. Run scripts/02_build_facilities.R first.",
       call. = FALSE)
}

CT_COUNTRY <- "TUR"

CT_ASSET_CLASSES <- list(
  industrial = list(gas = "co2",
                    subsectors = c("iron-and-steel", "cement", "aluminum")),
  energy     = list(gas = "co2e_100yr",
                    subsectors = c("electricity-generation", "coal-mining",
                                   "oil-and-gas-production",
                                   "oil-and-gas-refining",
                                   "oil-and-gas-transport"))
)

# EU27 as ISO3, for the CBAM-relevant question. The United Kingdom is
# deliberately NOT here: it left the Union and has its own CBAM starting later,
# so a UK parent is foreign but not EU. Norway, Iceland, Liechtenstein and
# Switzerland are outside CBAM's scope entirely (Regulation 2023/956 Annex III)
# and are likewise not EU here.
EU27_ISO3 <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
               "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
               "POL","PRT","ROU","SVK","SVN","ESP","SWE")

# Climate TRACE uses these literal strings where the owner is not a named
# entity. They are not companies and must never be counted as one — a top-10
# parents table led by "natural person(s)" would be a bug, not a finding.
NON_ENTITY_PARENTS <- c("natural person(s)", "small shareholder(s)", "unknown")


# =============================================================================
# 1. READ
# =============================================================================

message("[1/4] Reading ownership files")

facilities <- readRDS(FACILITIES)
ct_ids     <- sub("^CT", "", facilities$facility_id)

read_ownership <- function(cfg, class_name) {
  pkg     <- ct_download_package(cfg$gas, CT_COUNTRY, DIR_RAW)
  members <- ct_ownership_files(pkg$path, cfg$subsectors)
  members <- members[!is.na(members)]
  if (length(members) == 0) {
    warning("No ownership files for ", class_name)
    return(NULL)
  }

  ct_extract(pkg$path, members,
             file.path(DIR_EXTRACT, cfg$gas, "ownership")) |>
    map(~ read_csv(.x, locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE, progress = FALSE,
                   col_types = cols(.default = col_character()))) |>
    list_rbind() |>
    mutate(asset_class = class_name)
}

own_raw <- imap(CT_ASSET_CLASSES,
                ~ read_ownership(.x, .y)) |>
  compact() |>
  list_rbind() |>
  filter(iso3_country == CT_COUNTRY, source_id %in% ct_ids)

message("      ", nrow(own_raw), " ownership rows for ",
        n_distinct(own_raw$source_id), " of ", nrow(facilities), " facilities")


# =============================================================================
# 2. THE LONG TABLE
# =============================================================================

message("[2/4] Building the facility x parent table")

ownership <- own_raw |>
  transmute(
    facility_id = paste0("CT", source_id),
    source_id,
    asset_class,

    parent_name,
    parent_entity_type,

    # Kept apart on purpose. A group can be registered in one jurisdiction for
    # tax and headquartered in another; for the CBAM question the operating
    # headquarters is the more meaningful of the two, but hiding the difference
    # would be a choice made silently.
    parent_hq_country       = parent_headquarter_country,
    parent_registration_country,

    parent_lei,

    # Carried for reference and NEVER aggregated. See the header: this is a
    # path product, not a partition, and per-facility sums reach 200%+.
    share_pct_unreliable = suppressWarnings(as.numeric(overall_share_percent)),

    ownership_path,
    immediate_source_owner,
    source_operator,

    is_named_entity = !parent_name %in% NON_ENTITY_PARENTS &
                      !is.na(parent_name),
    is_foreign      = !is.na(parent_headquarter_country) &
                      parent_headquarter_country != "TUR",
    is_eu           = !is.na(parent_headquarter_country) &
                      parent_headquarter_country %in% EU27_ISO3,

    # HOW MANY LINKS, AND IS THE LAST ONE QUANTIFIED.
    #
    # The path reads "A -> B [x%] -> C [y%] -> facility [z%]". When the final
    # link is "[unknown %]", nobody has quantified the connection between the
    # immediate owner and the plant, so every entity above it is asserted on an
    # unmeasured foundation. 274 of 702 rows are in that state, and dropping
    # them cuts the foreign-owned facility count from 53 to 32.
    #
    # This is also where a name collision does its damage. Nine Turkish
    # landfill-gas stations are operated by a company recorded as "ITC Ltd";
    # Climate TRACE has resolved that to India's ITC Limited and walked its
    # shareholder register upward, so British American Tobacco, Life Insurance
    # Corporation of India and the Government of India all appear as owners of
    # Ankara's municipal waste plants. Every one of those rows terminates in
    # "[unknown %]" — the flag catches the failure without needing to know that
    # this particular ITC is the wrong one.
    path_terminal_unknown = str_detect(coalesce(ownership_path, ""),
                                       fixed("[unknown %]")),
    path_links = str_count(coalesce(ownership_path, ""), fixed("->"))
  ) |>
  arrange(facility_id, desc(share_pct_unreliable))


# =============================================================================
# 3. ONE ROW PER FACILITY
# =============================================================================
# Every field here is either a count, a flag, or a single named value. None is a
# weighted percentage, because the weight is not trustworthy.

message("[3/4] Summarising per facility")

# The largest KNOWN share among named entities, computed separately rather than
# inside summarise(). A facility with no published share gets NA rather than
# falling back to whichever row happened to sort first, which would dress an
# arbitrary ordering up as a finding.
primary_parent <- ownership |>
  filter(is_named_entity, !is.na(share_pct_unreliable)) |>
  group_by(facility_id) |>
  slice_max(share_pct_unreliable, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(facility_id,
         parent_primary       = parent_name,
         parent_primary_share = share_pct_unreliable)

summary_tbl <- ownership |>
  group_by(facility_id) |>
  summarise(
    n_parents        = n(),
    n_named_parents  = sum(is_named_entity),

    hq_countries = paste(sort(unique(na.omit(parent_hq_country))),
                         collapse = ", "),

    # Presence, not proportion. "Has at least one parent headquartered in the
    # EU27" is a claim this data can carry; "is 34% EU-owned" is not.
    #
    # Each flag comes in two strengths, and the pair is the point. `_certain`
    # counts only chains whose final link to the facility is quantified. The gap
    # between them is the uncertainty, exposed rather than averaged away:
    # measured, foreign goes 53 -> 32 while EU goes 28 -> 26, so the EU claim
    # survives the filter and the general foreign claim largely does not.
    has_foreign_parent         = any(is_foreign),
    has_foreign_parent_certain = any(is_foreign & !path_terminal_unknown),
    has_eu_parent              = any(is_eu),
    has_eu_parent_certain      = any(is_eu & !path_terminal_unknown),

    # How much of this facility's ownership rests on an unquantified last link.
    n_paths_unquantified = sum(path_terminal_unknown),
    longest_chain        = max(path_links),

    # How much of the ownership is not a named company. High opacity means the
    # facility's parents are natural persons, small shareholders or unknown —
    # a real property of Turkish industrial ownership, not a data defect.
    opacity = 1 - (sum(is_named_entity) / n()),

    .groups = "drop"
  ) |>
  left_join(primary_parent, by = "facility_id")

full_summary <- facilities |>
  select(facility_id, facility_name_tr, asset_class, sector, province_name_tr,
         nuts2_code, operator_name) |>
  left_join(summary_tbl, by = "facility_id") |>
  mutate(
    ownership_known = !is.na(n_parents),
    across(c(has_foreign_parent, has_foreign_parent_certain,
             has_eu_parent, has_eu_parent_certain),
           ~ if_else(is.na(.x), NA, .x)),
    n_parents       = coalesce(n_parents, 0L),
    n_named_parents = coalesce(n_named_parents, 0L)
  )

n_known <- sum(full_summary$ownership_known)
message("      ownership known for ", n_known, " of ", nrow(facilities),
        " facilities (", round(100 * n_known / nrow(facilities)), "%)")
message("      foreign parent: ",
        sum(full_summary$has_foreign_parent, na.rm = TRUE), " any / ",
        sum(full_summary$has_foreign_parent_certain, na.rm = TRUE),
        " with a quantified final link")
message("      EU27 parent:    ",
        sum(full_summary$has_eu_parent, na.rm = TRUE), " any / ",
        sum(full_summary$has_eu_parent_certain, na.rm = TRUE),
        " with a quantified final link")


# =============================================================================
# 4. VALIDATE, THEN WRITE
# =============================================================================

ownership   <- gate_ownership(ownership, full_summary, facilities)

saveRDS(ownership,   file.path(DIR_PROCESSED, "ownership.rds"))
saveRDS(full_summary, file.path(DIR_PROCESSED, "ownership_summary.rds"))

# A readable roll-up for inspection. Counts of facilities, never shares.
#
# It carries `facilities_certain` beside `facilities`, and it is SORTED BY THE
# CERTAIN COUNT. Sorting by the raw count puts British American Tobacco and the
# Government of India near the top of a table about Turkish industry, which is
# an artefact of one name collision rather than a fact about ownership. The two
# columns together let a reader see exactly that.
by_parent <- ownership |>
  filter(is_named_entity) |>
  distinct(parent_name, parent_hq_country, facility_id, path_terminal_unknown) |>
  group_by(parent_name, parent_hq_country) |>
  summarise(
    facilities         = n(),
    facilities_certain = sum(!path_terminal_unknown),
    .groups = "drop"
  ) |>
  arrange(desc(facilities_certain), desc(facilities))

write_csv(by_parent, file.path(DIR_PROCESSED, "ownership_by_parent.csv"))

message("\nWrote ownership.rds (", nrow(ownership), " rows), ",
        "ownership_summary.rds (", nrow(full_summary), "), ",
        "ownership_by_parent.csv (", nrow(by_parent), ")")

message("\nTop parents by QUANTIFIED facility links:")
print(as.data.frame(head(by_parent, 12)))

message("\nParents that lose the most when unquantified chains are excluded:")
print(as.data.frame(
  by_parent |>
    mutate(lost = facilities - facilities_certain) |>
    filter(lost > 0) |>
    arrange(desc(lost)) |>
    head(8)))

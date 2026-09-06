# =============================================================================
# global.R — libraries, data and constants shared by ui.R and server.R
# -----------------------------------------------------------------------------
# Sourced once at app startup, before ui.R and server.R. Anything expensive
# belongs here so it runs once per process rather than once per session.
#
# SCOPE OF THIS VERSION
#   Both populations on one map: 88 industrial installations and 212 energy
#   assets, now over time — `facility_panel.rds` supplies 1,800 facility-years
#   and the time slider drives them.
#
#   Still absent: the CBAM liability figure. The panel's `co2_direct_t` and
#   `co2_indirect_t` columns are deliberately NA pending the direct/indirect
#   decomposition, which is author work (KARBON_ATLASI.md §9). Nothing here
#   fabricates a value to fill that gap, and the interface says so rather than
#   leaving an empty box to be read as zero.
# =============================================================================

# Windows defaults to a non-UTF-8 encoding. Turkish UI labels break silently
# without this, and the failure shows up as mojibake in the browser rather than
# as an error. See KARBON_ATLASI.md §2.
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(dplyr)
  library(jsonlite)
})


# =============================================================================
# 1. DATA
# =============================================================================
# The app reads only from data/processed/. It never fetches, never computes a
# pipeline step, and never writes. If the file is missing, fail with an
# instruction rather than an obscure error 40 lines later.

# shiny::runApp() sets the working directory to the app folder, so a path
# relative to the project root resolves to app/data/... and fails. Locate the
# root by its marker file instead, so the app runs identically whether launched
# from the project root, from app/, or by RStudio's Run App button.
find_project_root <- function(marker = "KARBON_ATLASI.md", max_up = 3) {
  path <- normalizePath(".", winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up + 1)) {
    if (file.exists(file.path(path, marker))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) break        # reached the filesystem root
    path <- parent
  }
  stop("Could not locate the project root (no ", marker, " found above ",
       normalizePath(".", winslash = "/", mustWork = FALSE), ").", call. = FALSE)
}

PROJECT_ROOT    <- find_project_root()
FACILITIES_PATH <- file.path(PROJECT_ROOT, "data", "processed", "facilities.rds")

if (!file.exists(FACILITIES_PATH)) {
  stop(
    "facilities.rds not found.\n",
    "Run the pipeline first:\n",
    "  Rscript scripts/01_fetch_climate_trace.R\n",
    "  Rscript scripts/01c_ingest_gem.R      # needs a manual GEM download\n",
    "  Rscript scripts/02_build_facilities.R",
    call. = FALSE
  )
}

facilities <- readRDS(FACILITIES_PATH)

# The panel. Required for the time slider; the app still runs without it, with
# the slider hidden and a notice in its place, so a half-built clone is honest
# rather than broken.
PANEL_PATH <- file.path(PROJECT_ROOT, "data", "processed", "facility_panel.rds")
panel <- if (file.exists(PANEL_PATH)) readRDS(PANEL_PATH) else NULL

# Ownership, from 05. Optional like every other enrichment: the app runs without
# it and says so rather than failing.
OWN_SUMMARY_PATH <- file.path(PROJECT_ROOT, "data", "processed",
                              "ownership_summary.rds")
OWN_PATH         <- file.path(PROJECT_ROOT, "data", "processed",
                              "ownership.rds")
OWN_BY_PARENT_PATH <- file.path(PROJECT_ROOT, "data", "processed",
                                "ownership_by_parent.csv")

ownership_summary <- if (file.exists(OWN_SUMMARY_PATH)) {
  readRDS(OWN_SUMMARY_PATH)
} else NULL

ownership <- if (file.exists(OWN_PATH)) readRDS(OWN_PATH) else NULL

ownership_by_parent <- if (file.exists(OWN_BY_PARENT_PATH)) {
  utils::read.csv(OWN_BY_PARENT_PATH, fileEncoding = "UTF-8")
} else NULL

# TR-ETS threshold check, from 03.
TR_ETS_PATH <- file.path(PROJECT_ROOT, "data", "processed",
                         "tr_ets_coverage.csv")
tr_ets_coverage <- if (file.exists(TR_ETS_PATH)) {
  utils::read.csv(TR_ETS_PATH, fileEncoding = "UTF-8")
} else NULL

# Optional: the grid intensity comparison, if 01d has run. The app degrades
# rather than failing when it is absent.
GRID_PATH <- file.path(PROJECT_ROOT, "data", "processed", "grid_intensity.csv")
grid_intensity <- if (file.exists(GRID_PATH)) {
  utils::read.csv(GRID_PATH, fileEncoding = "UTF-8")
} else NULL


# =============================================================================
# 2. DISPLAY VOCABULARY
# =============================================================================
# Code and data stay in English; everything the user reads is Turkish (§2).
# Keeping the translation in one named vector means a label is never hardcoded
# at a call site, where it would drift out of sync with its twin.

ASSET_CLASS_LABELS <- c(
  "industrial" = "Sanayi tesisleri",
  "energy"     = "Enerji varlıkları"
)

SECTOR_LABELS <- c(
  # Industrial — the CBAM population
  "iron-and-steel"         = "Demir-Çelik",
  "cement"                 = "Çimento",
  "aluminum"               = "Alüminyum",
  # Energy
  "electricity-generation" = "Elektrik Üretimi",
  "coal-mining"            = "Kömür Madenciliği",
  "oil-and-gas-production" = "Petrol-Gaz Üretimi",
  "oil-and-gas-refining"   = "Rafineri",
  "oil-and-gas-transport"  = "Petrol-Gaz Taşıma"
)

# Warm hues for the CBAM subjects, cool for the energy fleet, so the two
# populations separate at a glance before any legend is read.
#
# CALIBRATED FOR CartoDB.DarkMatter, and that is not a cosmetic difference. The
# previous palette was built for the light Positron basemap and several of its
# colours fail outright on black: coal mining was #4D4D4D, a dark grey that is
# very nearly the basemap itself, and refining was #01665E, dark enough to read
# as a hole rather than a facility. Every hue here sits in the upper half of the
# luminance range so it separates from the ground it is drawn on.
#
# The warm/cool split survives the move: industrial reds and ambers, energy
# cyans, greens and violets, with coal mining as a light neutral because it is
# the one energy asset that is neither combustion nor extraction of a flowing
# fuel.
# Iron-and-steel and cement were measured too close together at a first pass
# (55 units of RGB distance, both red-orange) and pushed apart: steel toward
# crimson, cement toward amber. They are the two largest industrial sectors and
# sit side by side across the İskenderun and Marmara clusters, so confusing them
# would misread the map's densest area.
SECTOR_COLOURS <- c(
  "iron-and-steel"         = "#FF4D6D",
  "cement"                 = "#FF9F1C",
  "aluminum"               = "#FFD166",
  "electricity-generation" = "#4CC9F0",
  "coal-mining"            = "#ADB5BD",
  "oil-and-gas-production" = "#57D9A3",
  "oil-and-gas-refining"   = "#6C8AE4",
  "oil-and-gas-transport"  = "#A78BFA"
)

# liability_class is the field the whole merge turns on, so it is shown to the
# user rather than kept as an internal code.
LIABILITY_LABELS <- c(
  "direct"          = "Doğrudan — SKDM yükümlülüğü doğuran gömülü emisyon",
  "indirect_driver" = "Dolaylı sürücü — şebeke yoğunluğunu belirler, kendisi SKDM yükümlüsü değil",
  "neutral"         = "Nötr — haritalanır ve sayılır, SKDM hesabının ve şebeke faktörünün dışında"
)

LIABILITY_SHORT <- c(
  "direct"          = "Doğrudan",
  "indirect_driver" = "Dolaylı sürücü",
  "neutral"         = "Nötr"
)

# Geocode quality is shown to the user rather than hidden: a facility whose
# province was inferred by snapping from offshore is a weaker claim than one
# sitting well inside a polygon, and the interface should say so (§8.4).
GEOCODE_LABELS <- c(
  "within_province"    = "İl sınırları içinde",
  "boundary_proximate" = "İl sınırına yakın — atama belirsiz",
  "snapped_to_nearest" = "Poligon dışında — en yakın ile atandı",
  "offshore"           = "Açık deniz — en yakın kıyı iline atandı"
)

GEOCODE_BADGE <- c(
  "within_province"    = "#4C9A2A",
  "boundary_proximate" = "#E8A33D",
  "snapped_to_nearest" = "#C0392B",
  "offshore"           = "#2166AC"
)

# The two populations are denominated in different gases and must never be
# summed. The label travels with every figure that carries a gas basis.
GAS_LABELS <- c(
  "co2"        = "CO₂",
  "co2e_100yr" = "CO₂e (100 yıl)"
)

# value_type is epistemic, not decorative. An observation and a projection must
# never be rendered identically (§5), so each one carries its own label, colour
# and marker treatment, defined here once.
VALUE_TYPE_LABELS <- c(
  "observed"  = "Gözlem",
  "projected" = "Kestirim"
)

VALUE_TYPE_LONG <- c(
  "observed"  = paste0("Gözlem yılı — Climate TRACE'in modellenmiş tahmini. ",
                       "Doğrulanmış tesis raporu değildir."),
  "projected" = paste0("KESTİRİM YILI — Climate TRACE bu yıl için gözlem değil ",
                       "kestirim yayımlıyor. Gerçekleşmiş rakam olarak ",
                       "okunmamalıdır.")
)


# =============================================================================
# 3. THE TIME AXIS
# =============================================================================
# Three decisions live here, all of them made once and recorded rather than
# scattered through the interface.

if (!is.null(panel)) {

  PANEL_YEARS <- sort(unique(panel$year))

  # Months of data behind each year, per gas basis. Counted from the panel,
  # never assumed to be 12: the two populations do not agree on how much of
  # 2026 exists — the co2 package stops after 5 months and co2e_100yr after 6 —
  # and that is a property of the upstream release which a version bump changes
  # without changing any code.
  PANEL_DEPTH <- panel |>
    distinct(gas_basis, year, months_covered) |>
    arrange(year, gas_basis)

  # Years where both populations carry a full twelve months. Only these are
  # safely comparable across the two halves of the atlas.
  COMPLETE_YEARS <- PANEL_DEPTH |>
    group_by(year) |>
    summarise(complete = all(months_covered == 12), .groups = "drop") |>
    filter(complete) |>
    pull(year)

  OBSERVED_YEARS <- panel |>
    filter(value_type == "observed") |>
    pull(year) |>
    unique()

  # DEFAULT YEAR. The slider opens on the most recent year that is BOTH observed
  # and complete — currently 2024.
  #
  # Not 2026, even though it is the year CBAM's definitive regime applies and
  # the obvious "latest" choice: it holds five months for one population and six
  # for the other, and opening there would put a partial year in front of every
  # first-time reader as though it were a year.
  #
  # Not 2025 either. It is complete but Climate TRACE publishes it as an
  # estimate rather than an observation, and §5 is explicit that a projection
  # must never be the thing a reader meets first.
  DEFAULT_YEAR <- max(intersect(OBSERVED_YEARS, COMPLETE_YEARS))

  # How much of a given year each population actually carries, as a sentence.
  # Used wherever a year is labelled, so the 5-versus-6-month asymmetry cannot
  # hide behind a number that looks like a year.
  year_depth_note <- function(yr) {
    d <- PANEL_DEPTH[PANEL_DEPTH$year == yr, ]
    if (nrow(d) == 0) return(NULL)
    if (all(d$months_covered == 12)) return(NULL)
    paste0(
      vapply(seq_len(nrow(d)), function(i) {
        pop <- if (d$gas_basis[i] == "co2") "sanayi" else "enerji"
        paste0(pop, " ", d$months_covered[i], "/12 ay")
      }, character(1)),
      collapse = " · ")
  }

} else {
  PANEL_YEARS    <- integer(0)
  PANEL_DEPTH    <- NULL
  COMPLETE_YEARS <- integer(0)
  OBSERVED_YEARS <- integer(0)
  DEFAULT_YEAR   <- NA_integer_
  year_depth_note <- function(yr) NULL
}


# =============================================================================
# 4. NUMBER FORMATTING
# =============================================================================
# Turkish convention inverts the English one: "." groups thousands and ","
# marks the decimal, so 1.234,5 is a thousand two hundred and thirty four point
# five. Both marks must be passed together — supplying only `big.mark = "."`
# leaves the decimal mark at its locale default, which on this machine is also
# ".", and R warns that the result "could be confusing". It is right: 1.234
# would be unreadable as either a thousand or as one point two three four.
#
# Defined once here rather than at each call site, because the two marks have to
# agree and a call that sets one of them is always a bug.
fmt_tr <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, trim = TRUE, scientific = FALSE)
}

#' TRUE where a logical vector is TRUE, FALSE where it is NA.
#'
#' Ownership flags are deliberately NA for the 39 facilities with no parent
#' record, because "no record" is not "Turkish-owned". Subsetting with a vector
#' containing NA silently yields NA rows, which then propagate into a filter as
#' phantom facilities. This makes the intent explicit at every call site that
#' asks "is this flag true".
isTRUE_vec <- function(x) !is.na(x) & x


# =============================================================================
# 5. DERIVED CONSTANTS
# =============================================================================
# Computed once here rather than inside a reactive, because they never change.

sector_counts <- table(facilities$sector)

# Choices are grouped by asset class so the sector list reads as two
# populations rather than eight unrelated categories.
SECTOR_CHOICES_BY_CLASS <- lapply(names(ASSET_CLASS_LABELS), function(cls) {
  secs <- facilities |> filter(asset_class == cls) |> pull(sector) |> unique()
  secs <- secs[order(match(secs, names(SECTOR_LABELS)))]
  setNames(secs,
           paste0(unname(SECTOR_LABELS[secs]), " (",
                  as.integer(sector_counts[secs]), ")"))
})
names(SECTOR_CHOICES_BY_CLASS) <- unname(ASSET_CLASS_LABELS)

ALL_SECTORS <- unlist(unname(SECTOR_CHOICES_BY_CLASS), use.names = FALSE)

PROVINCE_CHOICES <- facilities |>
  count(province_name_tr, name = "n") |>
  arrange(province_name_tr) |>
  with(setNames(province_name_tr, paste0(province_name_tr, " (", n, ")")))

# Data vintage, surfaced in the UI. A figure without a vintage is not auditable.
SOURCE_RELEASE <- unique(facilities$source_release)[1]
N_FACILITIES   <- nrow(facilities)

# Records and places are different numbers: Climate TRACE lists each oil and gas
# field twice, under production and under transport, at one location. Both are
# shown so neither is quoted by accident.
N_SITES <- nrow(dplyr::distinct(facilities, lat, lon))

N_WITH_YEAR   <- sum(!is.na(facilities$commissioning_year))
COMMISSION_PCT <- round(100 * N_WITH_YEAR / N_FACILITIES)

YEAR_RANGE <- range(facilities$commissioning_year, na.rm = TRUE)

# --- Basemap -----------------------------------------------------------------
# NOT CartoDB, and the reason is a defect found by finally looking at the app in
# a browser on 2026-09-01. CARTO now requires an API key for its basemaps and
# serves WATERMARKED tiles without one: every tile carried "API KEY REQUIRED"
# and "carto.com/basemaps/apikey" diagonally across it.
#
# This predates the move to a dark basemap — Positron is the same service and
# the same watermark, so the app had been rendering that way for as long as it
# has had a map. Nobody saw it because nobody had opened it in a browser and
# looked. That is the whole argument for driving the app rather than trusting
# that a reactive returned without error.
#
# Esri's Dark Gray Canvas is free, needs no key, and splits base from labels so
# the labels can sit ABOVE the facility markers — which is better than CARTO
# managed anyway, since a city name under a marker is unreadable. Note the
# {z}/{y}/{x} order: Esri puts row before column, and getting it wrong yields a
# map that loads tiles happily and shows the wrong part of the world.
ESRI_BASE <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/",
                    "Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}")
ESRI_LABELS <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/",
                      "Canvas/World_Dark_Gray_Reference/MapServer/tile/{z}/{y}/{x}")

# Attribution is a condition of use, not a courtesy — the same rule this project
# applies to Climate TRACE, GEM and Ember.
ESRI_ATTRIB <- paste0(
  "Tiles &copy; Esri &mdash; Esri, HERE, Garmin, ",
  "&copy; OpenStreetMap contributors, and the GIS user community")

# Map extent. Türkiye's land border reaches 42.1°N; the northern bound is 43.2
# because Black Sea offshore gas production sits at about 42.94°N and would
# otherwise fall outside the initial view. Defined once here so the initial view
# and the pan limit cannot drift apart.
MAP_BOUNDS <- list(lng1 = 25.6, lat1 = 35.8, lng2 = 44.8, lat2 = 43.2)


# =============================================================================
# 6. FLEET CONTEXT — renewables, if built
# =============================================================================
# A SEPARATE register with a different epistemic status, deliberately not merged
# into `facilities`.
#
# The 300 facilities are what this project models: they emit, they come from
# Climate TRACE, and they carry a gas basis. Renewable plants come from GEM,
# emit essentially nothing, and exist here to answer one question — where the
# rest of Türkiye's generation is. Folding them into the same table would put
# 3,000 zero-emission dots into a register whose whole purpose is emissions, and
# would make every "300 facilities" statement in the documentation wrong for no
# analytical gain.
#
# They are the visible answer to ROADMAP E1: Climate TRACE's power register sees
# 52% of national generation, and this is the half it cannot see.

FLEET_PATH <- file.path(PROJECT_ROOT, "data", "processed", "fleet_renewables.rds")

fleet_renewables <- if (file.exists(FLEET_PATH)) readRDS(FLEET_PATH) else NULL

# The 2000–2026 development series, built by 04_build_fleet_timeline.R. Computed
# in the pipeline rather than here because it is analysis — cumulative capacity
# with coverage statistics — and the app computes no pipeline step.
TIMELINE_PATH <- file.path(PROJECT_ROOT, "data", "processed",
                           "fleet_timeline.csv")
COVERAGE_PATH <- file.path(PROJECT_ROOT, "data", "processed",
                           "fleet_timeline_coverage.csv")

fleet_timeline <- if (file.exists(TIMELINE_PATH)) {
  utils::read.csv(TIMELINE_PATH, fileEncoding = "UTF-8")
} else NULL

fleet_coverage <- if (file.exists(COVERAGE_PATH)) {
  utils::read.csv(COVERAGE_PATH, fileEncoding = "UTF-8")
} else NULL

# The four groups the timeline splits into. Two are measured in megawatts and
# two are counted only, and the distinction is carried in the vocabulary rather
# than remembered at each call site: a cement plant's capacity is in tonnes of
# cement and cannot join a megawatt axis.
TL_LABELS <- c(
  "ct_combustion" = "Yanma santralleri",
  "renewable"     = "Yenilenebilir filo",
  "industrial"    = "Sanayi tesisleri",
  "energy_other"  = "Kömür ocağı, petrol-gaz"
)

TL_COLOURS <- c(
  "ct_combustion" = "#FF8F6B",
  "renewable"     = "#57D9A3",
  "industrial"    = "#FF4D6D",
  "energy_other"  = "#ADB5BD"
)

TL_HAS_MW <- c("ct_combustion", "renewable")

TIMELINE_YEARS <- if (!is.null(fleet_timeline)) {
  sort(unique(fleet_timeline$year))
} else integer(0)

# The timeline opens at its own last year rather than at the emissions panel's
# default. This axis answers "how did the fleet get here", so the end of the
# journey is the right place to start reading, and there is no projection
# problem: a commissioning year is a record, not an estimate.
TIMELINE_DEFAULT <- if (length(TIMELINE_YEARS) > 0) max(TIMELINE_YEARS) else NA


# =============================================================================
# 7. POLICY TIMELINE
# =============================================================================
# Dated regulatory events, read from policies/policy_timeline.json at runtime
# like every other policy parameter (§7).
#
# READ meta$what_is_missing BEFORE TRUSTING THIS VIEW. Every event currently in
# the file is EU CBAM or Turkish ETS, and every one is dated 2023 or later. The
# Turkish energy policy that would explain the 2000-2026 fleet curve — YEKDEM,
# the capacity mechanism, coal support, the licensing regime — is not here,
# because no open machine-readable register of it exists and each entry has to
# be read out of a primary source and cited. That is author work under §9.
#
# The consequence for the chart is stated rather than left to be inferred: these
# markers sit at the right-hand edge and do not explain the curve behind them.
# The renewable build-out of 2015-2024 had already happened before any of this
# regulation existed.

POLICY_TIMELINE_PATH <- file.path(PROJECT_ROOT, "policies",
                                  "policy_timeline.json")

policy_timeline <- if (file.exists(POLICY_TIMELINE_PATH)) {
  jsonlite::fromJSON(POLICY_TIMELINE_PATH, simplifyDataFrame = FALSE)
} else NULL

# Flattened to a data frame for plotting, with a numeric year for the x axis.
# A year-only event is placed at mid-year rather than at 1 January, so it is not
# drawn with a precision its source does not have.
policy_events <- if (!is.null(policy_timeline)) {
  do.call(rbind, lapply(policy_timeline$events, function(e) {
    yr <- as.integer(substr(e$date, 1, 4))
    x  <- if (identical(e$granularity, "day")) {
      d <- as.Date(e$date)
      yr + (as.integer(format(d, "%j")) - 1) / 365
    } else yr + 0.5

    data.frame(
      id         = e$id,
      date       = e$date,
      granularity = e$granularity,
      year       = yr,
      x          = x,
      category   = e$category,
      value_type = e$value_type,
      title_tr   = e$title_tr,
      summary_tr = e$summary_tr,
      affects_fleet = isTRUE(e$affects_fleet),
      source_url = if (is.null(e$source_url)) NA_character_ else e$source_url,
      derived_from = e$derived_from,
      stringsAsFactors = FALSE
    )
  }))
} else NULL

if (!is.null(policy_events)) {
  policy_events <- policy_events[order(policy_events$x), ]
}

POLICY_CATEGORY_LABELS <- if (!is.null(policy_timeline)) {
  vapply(policy_timeline$categories, function(c) c$label_tr, character(1))
} else character(0)

POLICY_CATEGORY_COLOURS <- if (!is.null(policy_timeline)) {
  vapply(policy_timeline$categories, function(c) c$colour, character(1))
} else character(0)

POLICY_VALUE_TYPE_LABELS <- c(
  "legislated" = "Mevzuat",
  "observed"   = "Gerçekleşen",
  "scheduled"  = "Planlanan",
  "assumption" = "Varsayım"
)


# =============================================================================
# 8. PROCESS ROUTE AND OWNERSHIP VOCABULARY
# =============================================================================
# The route is the field the indirect-emissions argument turns on, so it gets a
# colour of its own rather than being buried in a popup line. Electric routes
# are drawn cool because their footprint moves with the grid; coke-based routes
# warm because it does not.
ROUTE_LABELS <- c(
  "EAF"          = "Elektrik ark ocağı (EAF)",
  "BF-BOF"       = "Yüksek fırın + konvertör",
  "BF-BOF + EAF" = "Entegre — hem yüksek fırın hem EAF",
  "dry kiln"     = "Kuru yöntem döner fırın"
)

ROUTE_COLOURS <- c(
  "EAF"          = "#4CC9F0",
  "BF-BOF"       = "#FF6B35",
  "BF-BOF + EAF" = "#FFD166",
  "dry kiln"     = "#B084F5"
)

ROUTE_NOTES <- c(
  "EAF" = paste0(
    "Hurdayı elektrikle eritir. Emisyonunun büyük kısmı DOLAYLIDIR ve ",
    "tükettiği elektriğin karbon yoğunluğuyla birlikte hareket eder — yani ",
    "şebeke temizlendikçe bu tesisin SKDM maruziyeti kendiliğinden düşer."),
  "BF-BOF" = paste0(
    "Cevheri kok kömürüyle indirger. Emisyonu DOĞRUDANDIR ve şebekeden ",
    "bağımsızdır; şebekenin temizlenmesi bu tesisi kurtarmaz."),
  "BF-BOF + EAF" = paste0(
    "Entegre tesis: iki rotayı birlikte işletir, dolayısıyla maruziyeti ",
    "kısmen şebekeye bağlı kısmen bağımsızdır."),
  "dry kiln" = paste0(
    "Çimentonun emisyonunun büyük kısmı kalsinasyondan gelir — kimyasal ",
    "reaksiyonun kendisinden, yakıttan değil. Şebeke yoğunluğu bu tesiste ",
    "çeliktekinden çok daha az belirleyicidir.")
)

# TR-ETS threshold, read from the policy file rather than repeated here.
TR_ETS_THRESHOLD_T <- 50000


FLEET_LABELS <- c(
  "hydropower"          = "Hidroelektrik",
  "utility-scale solar" = "Güneş",
  "wind"                = "Rüzgâr",
  "geothermal"          = "Jeotermal",
  "nuclear"             = "Nükleer"
)

# Also raised for the dark basemap, and raised further than the sector palette.
# These are drawn at 55% opacity and behind the emitting facilities, so their
# effective contrast is what counts, not their nominal colour — measured against
# the blended result rather than the swatch. Hydropower and geothermal both
# failed that test at a first pass and were brightened.
FLEET_COLOURS <- c(
  "hydropower"          = "#7FC4F5",
  "utility-scale solar" = "#FEC44F",
  "wind"                = "#8FD98A",
  "geothermal"          = "#FF8AC4",
  "nuclear"             = "#B197E8"
)

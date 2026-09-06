# =============================================================================
# test-palette.R — can the map's colours actually be seen on the map?
# -----------------------------------------------------------------------------
# The app draws on CartoDB.DarkMatter. A colour chosen against a white editor
# background can be invisible on it, and nothing in the pipeline would notice:
# the marker renders, the legend renders, the facility is simply not there to
# the eye. This is the only failure mode in the project that no data check can
# catch, because the data is correct.
#
# Measured rather than judged. The first pass of the dark palette failed three
# of these — coal mining had been #4D4D4D, near-black on near-black, and two
# renewable fuels fell below threshold once their 55% opacity was accounted for.
#
# Thresholds:
#   3.0   WCAG 2.1 SC 1.4.11, the minimum contrast for a non-text graphical
#         object against its background.
#   60    Euclidean RGB distance between two categorical colours. Not a
#         standard; a floor established by measurement here, below which
#         iron-and-steel and cement were confusable in the İskenderun cluster
#         where they sit side by side.
# =============================================================================

# DarkMatter's land polygons sit around this value. Water is darker, so this is
# the harder of the two grounds and the right one to test against.
DARK_GROUND <- "#12181C"

#' Relative luminance, WCAG 2.1 definition.
.rel_luminance <- function(hex) {
  v <- grDevices::col2rgb(hex)[, 1] / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055) ^ 2.4)
  sum(v * c(0.2126, 0.7152, 0.0722))
}

.contrast_ratio <- function(a, b) {
  la <- .rel_luminance(a); lb <- .rel_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

#' Load app/global.R into an isolated environment.
#'
#' Two pieces of global state have to be restored afterwards, and the second one
#' cost a debugging round:
#'
#'   working directory  global.R resolves paths from it, so it is changed rather
#'                      than assumed, then put back.
#'
#'   options(encoding)  global.R sets it to "UTF-8" for the app's own sake (§2).
#'                      That is a PROCESS-WIDE setting, and leaving it changed
#'                      broke jsonvalidate in a later test file: V8 reads its own
#'                      bundle.js through readLines() and threw "SyntaxError:
#'                      Invalid or unexpected token" from inside the package,
#'                      naming neither this file nor the option. The same V8
#'                      encoding trap the project already documents, reached by
#'                      a new route. testthat runs files alphabetically, so
#'                      `palette` leaks into `validate-gates`.
.app_env <- function() {
  app_dir <- file.path(ROOT, "app")
  skip_if_not(dir.exists(app_dir), "app/ not present")

  old_wd  <- setwd(app_dir)
  old_enc <- getOption("encoding")
  on.exit({ setwd(old_wd); options(encoding = old_enc) }, add = TRUE)

  e <- new.env()
  suppressWarnings(suppressMessages(source("global.R", local = e)))
  e
}


test_that("every sector colour is visible on the dark basemap", {
  e <- .app_env()

  for (nm in names(e$SECTOR_COLOURS)) {
    r <- .contrast_ratio(e$SECTOR_COLOURS[[nm]], DARK_GROUND)
    expect_gte(r, 3.0,
               label = paste0(nm, " (", e$SECTOR_COLOURS[[nm]],
                              ") contrast ", round(r, 2)))
  }
})


test_that("fleet colours survive being drawn at 55% opacity", {
  e <- .app_env()
  skip_if(is.null(e$FLEET_COLOURS), "no fleet palette")

  # The nominal swatch is not what the viewer sees. These are drawn
  # semi-transparent and behind the emitting facilities, so the blended result
  # is what has to clear the threshold.
  ALPHA <- 0.55

  for (nm in names(e$FLEET_COLOURS)) {
    col <- e$FLEET_COLOURS[[nm]]
    mixed <- grDevices::rgb(
      t(round(ALPHA * grDevices::col2rgb(col) +
                (1 - ALPHA) * grDevices::col2rgb(DARK_GROUND))),
      maxColorValue = 255)
    r <- .contrast_ratio(mixed, DARK_GROUND)
    expect_gte(r, 3.0,
               label = paste0(nm, " (", col, " -> ", mixed, ") contrast ",
                              round(r, 2)))
  }
})


test_that("no two sector colours are confusable", {
  e <- .app_env()

  cols <- e$SECTOR_COLOURS
  d <- as.matrix(stats::dist(t(grDevices::col2rgb(cols))))
  diag(d) <- Inf

  worst_i <- which(d == min(d), arr.ind = TRUE)[1, ]
  expect_gte(min(d), 60,
             label = paste0("closest pair: ", names(cols)[worst_i[1]], " / ",
                            names(cols)[worst_i[2]], " at dRGB ",
                            round(min(d), 1)))
})


test_that("the interface theme is legible on its own ground", {
  e <- .app_env()

  # The theme moved the whole application onto a dark ground, not just the map,
  # and the same failure mode follows it: a colour picked against a white editor
  # is invisible here and nothing errors. These are the values that are NOT
  # governed by atlas.css because base graphics and leaflet cannot read CSS
  # custom properties, so they are the ones that can drift out of the palette
  # without anyone noticing.

  # Text and axis furniture against the plot surface. 3.0 is the graphical-object
  # threshold; ink is body text and is held to a stricter bar.
  expect_gte(.contrast_ratio(e$PLOT_INK,  e$PLOT_BG), 7.0)
  expect_gte(.contrast_ratio(e$PLOT_SOFT, e$PLOT_BG), 4.5)
  expect_gte(.contrast_ratio(e$PLOT_FAINT, e$PLOT_BG), 3.0)

  # The two gas bases are drawn on one plot and must be separable from the
  # surface AND from each other, because reading one curve as the other is
  # exactly the confusion §6 forbids.
  for (nm in names(e$GAS_COLOURS)) {
    r <- .contrast_ratio(e$GAS_COLOURS[[nm]], e$PLOT_BG)
    expect_gte(r, 3.0, label = paste0("gas ", nm, " contrast ", round(r, 2)))
  }
  d_gas <- stats::dist(t(grDevices::col2rgb(e$GAS_COLOURS)))
  expect_gte(as.numeric(d_gas), 60)

  # Geocode badges. These carry the strength of a province assignment, so a
  # badge the user cannot distinguish defeats the point of showing it (§8.4).
  for (nm in names(e$GEOCODE_BADGE)) {
    r <- .contrast_ratio(e$GEOCODE_BADGE[[nm]], e$PLOT_BG)
    expect_gte(r, 3.0,
               label = paste0("geocode badge ", nm, " (", e$GEOCODE_BADGE[[nm]],
                              ") contrast ", round(r, 2)))
  }
})


test_that("the plot constants still match the tokens in atlas.css", {
  # PLOT_* duplicates values that atlas.css declares as custom properties,
  # because base graphics cannot read CSS. Duplication that nothing checks
  # drifts, and the symptom would be a plot panel a few shades off the box it
  # sits in — visible, annoying, and easy to blame on the screenshot.
  css_path <- file.path(ROOT, "app", "www", "atlas.css")
  skip_if_not(file.exists(css_path), "atlas.css not present")

  e   <- .app_env()
  css <- readLines(css_path, encoding = "UTF-8", warn = FALSE)

  token <- function(name) {
    line <- grep(paste0("^\\s*--", name, ":"), css, value = TRUE)[1]
    if (is.na(line)) return(NA_character_)
    toupper(sub(".*:\\s*(#[0-9A-Fa-f]{6}).*", "\\1", line))
  }

  expect_equal(token("surface"),     toupper(e$PLOT_BG))
  expect_equal(token("ink"),         toupper(e$PLOT_INK))
  expect_equal(token("ink-soft"),    toupper(e$PLOT_SOFT))
  expect_equal(token("ink-faint"),   toupper(e$PLOT_FAINT))
  expect_equal(token("rule"),        toupper(e$PLOT_RULE))
  expect_equal(token("rule-strong"), toupper(e$PLOT_AXIS))
})


test_that("every sector and fleet fuel has a colour and a label", {
  e <- .app_env()

  # A missing entry does not error — it yields NA, leaflet draws a transparent
  # marker, and the facility silently disappears from the map while still being
  # counted in every total on the page.
  expect_setequal(names(e$SECTOR_COLOURS), names(e$SECTOR_LABELS))

  facilities <- read_processed("facilities.rds")
  expect_true(all(unique(facilities$sector) %in% names(e$SECTOR_COLOURS)))

  if (!is.null(e$fleet_renewables)) {
    expect_true(all(unique(e$fleet_renewables$fuel_type) %in%
                      names(e$FLEET_COLOURS)))
    expect_setequal(names(e$FLEET_COLOURS), names(e$FLEET_LABELS))
  }
})

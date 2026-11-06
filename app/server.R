# =============================================================================
# server.R — reactive logic
# -----------------------------------------------------------------------------
# Pattern: filter ONCE in a reactive, let every observer and output consume that
# single result (§4). Recomputing the filter inside each output would run it
# six times per interaction and drift out of sync.
# =============================================================================

function(input, output, session) {

  # ===========================================================================
  # 1. THE SINGLE FILTERED DATASET
  # ===========================================================================
  # Everything downstream reads this. Two reactives, layered: the register
  # filter, then the year join. Splitting them means the map's geometry does not
  # recompute when only the year changes, and the year join does not recompute
  # when only a province is deselected.

  selected_facilities <- reactive({
    out <- facilities |>
      filter(asset_class %in% input$asset_classes,
             sector %in% input$sectors)

    if (length(input$provinces) > 0) {
      out <- out |> filter(province_name_tr %in% input$provinces)
    }

    if (isTRUE(input$only_captive)) {
      out <- out |> filter(!is.na(is_captive), is_captive)
    }

    # Ownership. The `_certain` variants restrict to chains whose final link to
    # the facility is quantified — the same filter that drops the ITC Ltd name
    # collision, and the difference between "53 foreign-owned" and "32".
    if (!is.null(input$ownership_filter) && input$ownership_filter != "all" &&
        !is.null(ownership_summary)) {
      s <- ownership_summary
      keep <- switch(
        input$ownership_filter,
        "eu"              = s$facility_id[isTRUE_vec(s$has_eu_parent)],
        "eu_certain"      = s$facility_id[isTRUE_vec(s$has_eu_parent_certain)],
        "foreign"         = s$facility_id[isTRUE_vec(s$has_foreign_parent)],
        "foreign_certain" = s$facility_id[isTRUE_vec(s$has_foreign_parent_certain)],
        "unknown"         = s$facility_id[!s$ownership_known],
        s$facility_id
      )
      out <- out |> filter(facility_id %in% keep)
    }

    if (!is.null(input$route_filter) && input$route_filter != "all") {
      out <- if (input$route_filter == "none") {
        out |> filter(is.na(process_route))
      } else {
        out |> filter(!is.na(process_route), process_route == input$route_filter)
      }
    }

    out
  })

  # Debounced: dragging the slider fires an event per pixel, and each one would
  # otherwise redraw 300 markers and a plot. 250 ms is short enough to feel
  # immediate and long enough to collapse a drag into one redraw (§4).
  current_year <- reactive({
    if (is.null(input$year)) DEFAULT_YEAR else input$year
  }) |> debounce(250)

  # The selected facilities joined to the selected year. A left join, not an
  # inner one: a facility with no panel row for that year must still appear on
  # the map with its emissions unknown, rather than silently vanishing and
  # making the fleet look smaller than it is.
  selected_year_data <- reactive({
    df <- selected_facilities()
    if (is.null(panel) || nrow(df) == 0) {
      return(df |> mutate(emissions_reported_t = NA_real_,
                          production_activity  = NA_real_,
                          emission_intensity   = NA_real_,
                          months_covered       = NA_integer_,
                          value_type           = NA_character_,
                          status               = NA_character_,
                          confidence_emissions = NA_character_))
    }

    yr <- current_year()

    df |>
      left_join(
        panel |>
          filter(year == yr) |>
          select(facility_id, emissions_reported_t, production_activity,
                 emission_intensity, months_covered, value_type, status,
                 confidence_emissions),
        by = "facility_id"
      )
  })

  # Is the selected year a projection? Asked once, used by several outputs.
  year_is_projected <- reactive({
    !is.null(panel) && current_year() %in%
      setdiff(PANEL_YEARS, OBSERVED_YEARS)
  })


  # ===========================================================================
  # 2. THE YEAR'S STATUS
  # ===========================================================================
  # Two outputs, both driven by the same question: can this year be read the way
  # a year is normally read? They stay silent when it can, so that when they
  # speak they are still noticed.

  output$year_status <- renderUI({
    yr <- current_year()
    vt <- if (year_is_projected()) "projected" else "observed"

    depth <- year_depth_note(yr)

    tags$div(
      style = "margin-top:-6px; font-size:12px; color:var(--ink-2);",
      tags$span(
        class = paste("yr-badge", if (vt == "projected") "is-projected" else "is-live"),
        unname(VALUE_TYPE_LABELS[vt])
      ),
      if (!is.null(depth)) {
        tags$span(style = "margin-left:8px; color:var(--ink-3);", depth)
      }
    )
  })

  output$year_warning <- renderUI({
    yr    <- current_year()
    depth <- year_depth_note(yr)
    out   <- tagList()

    # A projection rendered like an observation is the single most misleading
    # thing this interface could do (§5), so it is stated in words, not colour.
    if (year_is_projected()) {
      out <- tagList(out, fold_note(
        tagList(tags$b(yr, " bir KESTİRİM yılıdır."),
                " Gerçekleşmiş rakam olarak okunmamalıdır."),
        "Climate TRACE bu yıl için gözlem değil kestirim yayımlıyor. ",
        "Haritadaki noktalar kesik çizgili ve soluk çizilir."
      ))
    }

    # The 5-versus-6 month asymmetry. Not a footnote: a bar labelled 2026 beside
    # another labelled 2026 asserts they are comparable, and here they are not.
    if (!is.null(depth)) {
      out <- tagList(out, fold_note(
        tagList(tags$b(yr, " kısmi bir yıldır — ", depth, "."),
                " İki popülasyon aynı uzunlukta değildir."),
        "Doğrudan karşılaştırılamaz. Kısmi yıl, tam yıla ",
        tags$b("ölçeklenerek tamamlanmaz"), ": çimento üretimi mevsimseldir, ",
        "12/5 çarpanı rastgele değil sistematik sapma üretir."
      ))
    }

    out
  })


  # ===========================================================================
  # 3. HEADLINE FIGURES
  # ===========================================================================
  # The two populations get their own box each, because their figures are in
  # different gases and a combined total would be meaningless (§6).

  # Every figure goes through fmt_tr() (global.R): a Turkish reader takes
  # "74.9" for seventy-four thousand nine hundred, so English marks are a wrong
  # number, not a style difference.
  fmt_mt <- function(t) {
    if (all(is.na(t))) return("—")
    paste0(fmt_tr(sum(t, na.rm = TRUE) / 1e6, 1), " Mt")
  }

  # The four value boxes, now one line. Same four quantities; what changed is
  # that they no longer occupy 110px each in saturated blocks that outshouted
  # the map. The keys stay uppercase and letter-spaced so the row reads as a
  # scan status line rather than as four headlines.
  #
  # The last cell is deliberately an em dash and is toned "null", which renders
  # it dim: a CBAM figure does not exist yet (§6, the decomposition is author
  # work) and a zero here would be read as "no exposure".
  output$status_strip <- renderUI({
    df   <- selected_facilities()
    ind  <- selected_year_data() |> filter(asset_class == "industrial")
    ene  <- selected_year_data() |> filter(asset_class == "energy")
    yr   <- current_year()

    readout_strip(
      readout("Görüntülenen kayıt",
              paste0(fmt_tr(nrow(df)), " / ", fmt_tr(N_FACILITIES))),
      readout(paste0("Sanayi CO₂ · ", nrow(ind), " tesis · ", yr),
              fmt_mt(ind$emissions_reported_t)),
      readout(paste0("Enerji CO₂e · ", nrow(ene), " varlık · ", yr),
              fmt_mt(ene$emissions_reported_t)),
      readout("SKDM maruziyeti", "hesap yok", tone = "null")
    )
  })

  # The map frame's corner tags. The poster device carries real values rather
  # than ornament: the year at top right, the record count at bottom left.
  output$map_tag_year <- renderUI({
    tagList(current_year(), if (year_is_projected()) " · kestirim" else "")
  })
  output$map_tag_n <- renderUI({
    paste0(nrow(selected_facilities()), " kayıt")
  })

  output$map_title <- renderText({
    df    <- selected_facilities()
    sites <- nrow(dplyr::distinct(df, lat, lon))
    paste0("Türkiye Karbon Atlası — ", current_year(), " · ",
           nrow(df), " kayıt, ", sites, " ayrı konum")
  })


  # ===========================================================================
  # 3. MAP
  # ===========================================================================

  # Place names sit ABOVE the facility markers, so a city name is never buried
  # under a dot. They live in their own pane (z 450, over the overlay pane's
  # 400) and are added ONCE when the map is built. They used to be removed and
  # re-added inside every marker observer, which re-requested the label tiles
  # on each filter change and made the map flicker through every step of the
  # slider animation. The pane is made click-through in ui.R.
  add_place_labels <- function(map, maxZoom) {
    map |>
      addMapPane("place_labels", zIndex = 450) |>
      addTiles(urlTemplate = ESRI_LABELS,
               options = tileOptions(pane = "place_labels", minZoom = 5,
                                     maxZoom = maxZoom, opacity = 0.9))
  }
  # Rendered ONCE with a fixed view; markers are updated through leafletProxy
  # (§4). Re-rendering the whole map on every filter change would reset the
  # user's pan and zoom, which is both slow and infuriating.

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      preferCanvas = TRUE,
      # The atlas is about Türkiye. Letting the view drift into the Atlantic
      # makes it look unfinished and invites the reader to look for data that
      # was never claimed. maxBounds pins panning; minZoom stops the country
      # shrinking to a dot. A world view belongs to a future trade layer with
      # its own map, not to this one.
      minZoom = 5, maxZoom = 14
    )) |>
      # Dark ground, so the basemap recedes almost completely and the facility
      # points are the only saturated thing on screen. Base and labels are two
      # layers: the labels are added AFTER the markers in the observer below, so
      # a city name is never buried under a dot. See ESRI_BASE in global.R for
      # why this is not CartoDB.
      addTiles(urlTemplate = ESRI_BASE, attribution = ESRI_ATTRIB,
               options = tileOptions(minZoom = 5, maxZoom = 14)) |>
      add_place_labels(maxZoom = 14) |>
      fitBounds(lng1 = MAP_BOUNDS$lng1, lat1 = MAP_BOUNDS$lat1,
                lng2 = MAP_BOUNDS$lng2, lat2 = MAP_BOUNDS$lat2) |>
      # A little slack beyond the data so coastal facilities are not pinned to
      # the very edge of the viewport.
      setMaxBounds(lng1 = MAP_BOUNDS$lng1 - 1.5, lat1 = MAP_BOUNDS$lat1 - 1.0,
                   lng2 = MAP_BOUNDS$lng2 + 1.5, lat2 = MAP_BOUNDS$lat2 + 1.0) |>
      addLegend(
        position = "bottomright",
        colors   = unname(SECTOR_COLOURS),
        labels   = unname(SECTOR_LABELS),
        title    = "Sektör",
        opacity  = 0.9
      )
  })

  observe({
    df <- selected_year_data()
    yr <- current_year()

    proxy <- leafletProxy("map", data = df) |> clearGroup("facilities")

    if (nrow(df) == 0) return(invisible(NULL))

    projected <- year_is_projected()

    # Outlines are inverted for the dark basemap. A thin near-black ring
    # separates overlapping dots without adding brightness; the highlight for an
    # uncertain geocode is a thick WHITE halo, because on this ground white is
    # what draws the eye. The previous dark highlight was designed for a light
    # basemap and would now be invisible against it.
    stroke_colour <- if (isTRUE(input$flag_uncertain)) {
      ifelse(df$geocode_quality == "within_province", "#11161A", "#FFFFFF")
    } else "#11161A"

    stroke_weight <- if (isTRUE(input$flag_uncertain)) {
      ifelse(df$geocode_quality == "within_province", 1, 3)
    } else 1

    # AREA scales with emissions, so radius takes the square root. Scaling the
    # radius directly would make a plant twice as dirty look four times as big,
    # which is the standard way a bubble map lies.
    #
    # The two populations are scaled SEPARATELY, because their figures are in
    # different gases and a shared scale would invite exactly the comparison
    # §6 forbids. Within each half, area is comparable; across them, only
    # position is.
    scaled_radius <- function(v, cls) {
      out <- rep(NA_real_, length(v))
      for (k in unique(cls)) {
        i <- cls == k
        top <- suppressWarnings(max(v[i], na.rm = TRUE))
        if (!is.finite(top) || top <= 0) { out[i] <- 4; next }
        out[i] <- 4 + 11 * sqrt(pmax(v[i], 0) / top)
      }
      # A facility with no emissions figure for this year still has to be
      # visible and must not be mistaken for a zero-emission one.
      out[is.na(out)] <- 3
      out
    }

    radius <- scaled_radius(df$emissions_reported_t, df$asset_class)

    # A projection is drawn dashed and desaturated. Never identical to an
    # observation (§5) — colour alone is not enough, so the outline changes too.
    dash <- if (projected) "3,4" else NULL
    fill_opacity <- if (projected) 0.45 else 0.85

    proxy |>
      addCircleMarkers(
        lng          = ~lon,
        lat          = ~lat,
        layerId      = ~facility_id,
        group        = "facilities",
        radius       = radius,
        fillColor    = ~unname(SECTOR_COLOURS[sector]),
        fillOpacity  = fill_opacity,
        color        = stroke_colour,
        weight       = stroke_weight,
        dashArray    = dash,
        label        = ~paste0(facility_name_tr, " — ",
                               ifelse(is.na(emissions_reported_t), "veri yok",
                                      paste0(fmt_tr(emissions_reported_t / 1000, 1),
                                             " kt"))),
        popup        = ~paste0(
          "<strong>", facility_name_tr, "</strong><br/>",
          "<em>", unname(SECTOR_LABELS[sector]), " · ",
          unname(ASSET_CLASS_LABELS[asset_class]), "</em><br/><br/>",

          "<b>", yr, " emisyonu:</b> ",
          ifelse(is.na(emissions_reported_t), "veri yok",
                 paste0(fmt_tr(emissions_reported_t / 1000, 1),
                        " kt ", unname(GAS_LABELS[gas_basis]))),
          ifelse(is.na(value_type), "",
                 paste0(" <span style='color:",
                        ifelse(value_type == "projected", "var(--ink-3)", "var(--live)"),
                        ";'>(", unname(VALUE_TYPE_LABELS[value_type]), ")</span>")),
          "<br/>",

          "<b>Üretim:</b> ",
          ifelse(is.na(production_activity), "yayımlanmıyor",
                 fmt_tr(production_activity)),
          "<br/>",
          "<b>Yoğunluk:</b> ",
          ifelse(is.na(emission_intensity), "hesaplanamıyor",
                 fmt_tr(emission_intensity, 3)),
          "<br/>",
          "<b>Güven (emisyon):</b> ",
          ifelse(is.na(confidence_emissions), "—", confidence_emissions),
          "<br/><br/>",

          "<b>İşletmeci:</b> ", ifelse(is.na(operator_name), "—", operator_name), "<br/>",
          "<b>Teknoloji:</b> ", ifelse(is.na(technology), "—", technology), "<br/>",
          "<b>Devreye giriş:</b> ",
          ifelse(is.na(commissioning_year), "bilinmiyor", commissioning_year), "<br/>",
          "<b>İl:</b> ", province_name_tr, " (", province_code, ")<br/>",
          "<b>Yükümlülük:</b> ", unname(LIABILITY_SHORT[liability_class])
        )
      )
  })


  # ---------------------------------------------------------------------------
  # Renewable fleet — a separate register, drawn as context
  # ---------------------------------------------------------------------------
  # Off by default and in its own layer group, because these plants are not part
  # of the modelled population. They emit essentially nothing, they come from
  # GEM rather than Climate TRACE, and they are here to answer one question:
  # where the rest of Türkiye's generation is. Drawn smaller and semi-
  # transparent so they read as background against the emitting facilities.

  observe({
    proxy <- leafletProxy("map") |> clearGroup("fleet")

    if (is.null(fleet_renewables) || !isTRUE(input$show_fleet)) {
      return(invisible(NULL))
    }

    df <- fleet_renewables
    if (length(input$provinces) > 0) {
      df <- df |> filter(province_name_tr %in% input$provinces)
    }
    if (nrow(df) == 0) return(invisible(NULL))

    proxy |>
      addCircleMarkers(
        data        = df,
        lng         = ~lon,
        lat         = ~lat,
        group       = "fleet",
        # Radius scales with capacity so a 600 MW dam does not read like a 1 MW
        # rooftop array, but stays small enough to sit behind the facilities.
        radius      = ~pmax(2, pmin(9, sqrt(pmax(capacity_mw, 0)) / 3)),
        fillColor   = ~unname(FLEET_COLOURS[fuel_type]),
        fillOpacity = 0.55,
        color       = "#11161A",
        weight      = 0.5,
        label       = ~plant_name,
        popup       = ~paste0(
          "<strong>", plant_name, "</strong><br/>",
          "<em>", unname(FLEET_LABELS[fuel_type]), " — yenilenebilir filo</em>",
          "<br/><br/>",
          "<b>Kapasite:</b> ", fmt_tr(capacity_mw, 1), " MW",
          ifelse(units > 1, paste0(" (", units, " ünite)"), ""), "<br/>",
          "<b>Devreye giriş:</b> ",
          ifelse(is.na(commissioning_year), "bilinmiyor", commissioning_year), "<br/>",
          "<b>İl:</b> ", province_name_tr, "<br/>",
          "<b>Kaynak:</b> Global Energy Monitor<br/><br/>",
          "<span style='color:var(--ink-2);'>Bu tesis modellenen 300 kaydın parçası ",
          "değildir. Emisyon üretmediği için emisyon tablolarında yer almaz; ",
          "haritada, Climate TRACE'in elektrik kütüğünün göremediği üretimi ",
          "göstermek için bulunur.</span>"
        )
      )
  })


  # ===========================================================================
  # 3b. THE SERIES
  # ===========================================================================
  # What a single year cannot show. Base graphics deliberately: neither ggplot2
  # nor plotly is in renv.lock, and adding a dependency to draw six points would
  # be a poor trade.
  #
  # The two populations are drawn on SEPARATE axes — left for industrial CO2,
  # right for energy CO2e — because they are different gases. Sharing one axis
  # would put them in the same visual space and invite the sum that §6 forbids.

  trend_series <- reactive({
    req(!is.null(panel))
    ids <- selected_facilities()$facility_id
    if (length(ids) == 0) return(NULL)

    panel |>
      filter(facility_id %in% ids) |>
      group_by(gas_basis, year) |>
      summarise(mt         = sum(emissions_reported_t, na.rm = TRUE) / 1e6,
                months     = first(months_covered),
                value_type = first(value_type),
                .groups = "drop")
  })

  output$trend_plot <- renderPlot({
    s <- trend_series()
    if (is.null(s) || nrow(s) == 0) {
      op <- plot_theme(mar = c(0, 0, 0, 0))
      on.exit(par(op), add = TRUE)
      plot.new()
      text(0.5, 0.5, "Seçili tesis yok", col = PLOT_FAINT, cex = 1.1)
      return(invisible(NULL))
    }

    op <- plot_theme(mar = c(3.2, 4.0, 0.6, 4.0), cex.axis = 0.8, cex.lab = 0.85,
                     mgp = c(2.3, 0.6, 0), tcl = -0.25, las = 1)
    on.exit(par(op), add = TRUE)

    yrs <- sort(unique(s$year))

    draw_one <- function(basis, colour, side, first) {
      d <- s[s$gas_basis == basis, ]
      if (nrow(d) == 0) return(invisible(NULL))
      d <- d[order(d$year), ]

      if (first) {
        plot(d$year, d$mt, type = "n", xlab = "", ylab = "",
             xlim = range(yrs), ylim = c(0, max(d$mt) * 1.15),
             xaxt = "n", yaxt = "n", bty = "n")
        axis(1, at = yrs, labels = yrs, col = PLOT_AXIS, col.axis = PLOT_SOFT)
        grid(nx = NA, ny = NULL, col = PLOT_RULE, lty = 1)
      } else {
        par(new = TRUE)
        plot(d$year, d$mt, type = "n", xlab = "", ylab = "",
             xlim = range(yrs), ylim = c(0, max(d$mt) * 1.15),
             axes = FALSE, bty = "n")
      }
      # Tick labels in Turkish marks too; a single small facility puts
      # decimals on this axis ("0,05", not "0.05").
      tk <- axTicks(side)
      axis(side, at = tk, labels = fmt_tr(tk, if (all(tk == round(tk))) 0 else 2),
           col = colour, col.axis = colour)
      mtext(unname(GAS_LABELS[basis]), side = side, line = 2.4,
            col = colour, cex = 0.8)

      # Observed and projected are drawn as two segments of one line: solid up
      # to the last observed year, dashed beyond it. The join point is repeated
      # in both so the line does not break.
      obs  <- d[d$value_type == "observed", ]
      proj <- d[d$value_type == "projected", ]

      if (nrow(obs) > 0) {
        lines(obs$year, obs$mt, col = colour, lwd = 2.4)
        points(obs$year, obs$mt, col = colour, pch = 19, cex = 1.0)
      }
      if (nrow(proj) > 0) {
        bridge <- rbind(obs[nrow(obs), , drop = FALSE], proj)
        lines(bridge$year, bridge$mt, col = colour, lwd = 2.0, lty = 2)
        # Hollow points for projections: a different shape as well as a
        # different line, because line style alone is easy to miss.
        points(proj$year, proj$mt, col = colour, pch = 21, bg = PLOT_BG,
               cex = 1.0, lwd = 1.8)
      }

      # A partial year is marked on the plot itself, not only in the caption.
      part <- d[!is.na(d$months) & d$months < 12, ]
      if (nrow(part) > 0) {
        text(part$year, part$mt, labels = paste0(part$months, "a"),
             pos = 3, col = "#FF9F1C", cex = 0.75, font = 2)
      }
    }

    first <- TRUE
    for (b in c("co2", "co2e_100yr")) {
      if (any(s$gas_basis == b)) {
        draw_one(b, unname(GAS_COLOURS[b]), if (first) 2 else 4, first)
        first <- FALSE
      }
    }

    abline(v = current_year(), col = paste0(PLOT_SOFT, "66"), lwd = 1.5, lty = 3)
  # The device itself is opened dark. par(bg=) alone is not enough: the margins
  # outside the plot region keep the device background, so a white device shows
  # a white frame around a dark panel.
  }, bg = PLOT_BG)

  output$trend_note <- renderUI({
    s <- trend_series()
    if (is.null(s) || nrow(s) == 0) return(NULL)

    tags$div(
      style = "margin-top:10px;",
      fold_note(
        tagList(tags$b("İki ayrı eksen, iki ayrı gaz."),
                " Eğriler aynı ölçekte değildir ve toplanmaz."),
        "Sol eksen sanayi CO₂, sağ eksen enerji CO₂e. ",
        "Dolu nokta ve düz çizgi gözlem, içi boş nokta ve kesik çizgi ",
        "kestirimdir. Nokta üstündeki etiket, o yılın kaç aylık veriyle ",
        "hesaplandığını gösterir. Dikey noktalı çizgi slider'ın bulunduğu yıldır."
      )
    )
  })


  # ===========================================================================
  # 4. SELECTED FACILITY DETAIL
  # ===========================================================================

  selected_id <- reactiveVal(NULL)

  observeEvent(input$map_marker_click, {
    selected_id(input$map_marker_click$id)
  })

  # A facility filtered out of view must not linger in the detail panel.
  observeEvent(selected_facilities(), {
    if (!is.null(selected_id()) &&
        !selected_id() %in% selected_facilities()$facility_id) {
      selected_id(NULL)
    }
  })

  output$facility_detail <- renderUI({
    if (is.null(selected_id())) {
      return(tags$p(style = "color:var(--ink-3);",
                    "Ayrıntı için haritadan bir tesis seçin."))
    }

    f <- facilities |> filter(facility_id == selected_id())

    row <- function(label, value) {
      tags$tr(tags$td(style = "color:var(--ink-2); white-space:nowrap;", label),
              tags$td(style = "font-weight:600;", value))
    }

    tagList(
      tags$h4(f$facility_name_tr, style = "margin-top:0;"),
      tags$span(
        class = "sector-chip",
        style = paste0("background:", SECTOR_COLOURS[[f$sector]], ";"),
        SECTOR_LABELS[[f$sector]]
      ),

      tags$table(
        class = "kv", style = "margin-top:14px; width:100%;",
        row("Varlık sınıfı", ASSET_CLASS_LABELS[[f$asset_class]]),
        row("İşletmeci",     ifelse(is.na(f$operator_name), "—", f$operator_name)),
        row("Teknoloji",     ifelse(is.na(f$technology), "—", f$technology)),
        row("Devreye giriş",
            if (is.na(f$commissioning_year)) "bilinmiyor"
            else paste0(f$commissioning_year, " (", f$commissioning_source, ")")),
        row("İl",       paste0(f$province_name_tr, " (", f$province_code, ")")),
        row("İBBS-2",   paste0(f$nuts2_name_tr, " (", f$nuts2_code, ")")),
        row("Koordinat", sprintf("%.4f, %.4f", f$lat, f$lon)),
        row("Gaz tabanı", GAS_LABELS[[f$gas_basis]]),
        row("Kaynak",   paste0("Climate TRACE ", f$source_release)),
        row("Kaynak ID", f$source_id)
      ),

      # The selected year's figures for this facility, with their vintage. Kept
      # in its own block so the time-varying numbers are visibly separate from
      # the attributes that do not change — the same split the data model makes
      # between facilities.rds and facility_panel.rds (§6).
      if (!is.null(panel)) {
        p <- panel |> filter(facility_id == selected_id(), year == current_year())
        if (nrow(p) == 0) {
          tags$p(style = "font-size:12px; color:var(--ink-3); margin-top:10px;",
                 current_year(), " için panel kaydı yok.")
        } else {
          tags$div(
            style = paste0("margin-top:12px; padding:10px; background:var(--panel);",
                           " border:1px solid var(--line);"),
            tags$div(
              style = "font-size:12px; color:var(--ink-2); margin-bottom:6px;",
              tags$b(current_year()),
              tags$span(
                class = paste("yr-badge",
                              if (p$value_type == "projected") "is-projected"
                              else "is-live"),
                style = "margin-left:8px;",
                unname(VALUE_TYPE_LABELS[p$value_type])),
              if (!is.na(p$months_covered) && p$months_covered < 12)
                tags$span(style = "margin-left:8px; color:var(--ink-3);",
                          paste0(p$months_covered, "/12 ay"))
            ),
            tags$table(
              class = "kv", style = "width:100%;",
              row("Emisyon",
                  if (is.na(p$emissions_reported_t)) "veri yok"
                  else paste0(fmt_tr(p$emissions_reported_t / 1000, 1),
                              " kt ", GAS_LABELS[[p$gas_basis]])),
              row("Üretim",
                  if (is.na(p$production_activity)) "yayımlanmıyor"
                  else paste0(fmt_tr(p$production_activity), " ",
                              ifelse(is.na(p$activity_units), "",
                                     p$activity_units))),
              row("Yoğunluk",
                  if (is.na(p$emission_intensity)) "hesaplanamıyor"
                  else fmt_tr(p$emission_intensity, 3)),
              row("Güven", ifelse(is.na(p$confidence_emissions), "—",
                                  p$confidence_emissions)),
              row("Sürüm", p$vintage)
            ),
            tags$p(
              style = "font-size:11px; color:var(--ink-3); margin:8px 0 0 0;",
              tags$b("SKDM maruziyeti hesaplanmıyor."), " Doğrudan/dolaylı ",
              "ayrıştırma yapılmadan bu tesise bir maliyet baskısı rakamı ",
              "atanamaz; boş bırakmak, uydurmaktan iyidir."
            )
          )
        }
      },

      # --- Process route ------------------------------------------------------
      # Given its own block rather than a table row, because for a steel plant
      # this single field decides whether the grid tab is relevant to it at all.
      if (!is.null(f$process_route) && !is.na(f$process_route)) {
        col <- ROUTE_COLOURS[[f$process_route]]
        tags$div(
          style = paste0("margin-top:12px; padding:10px 12px;",
                         " border-left:4px solid ", col, "; background:var(--panel);"),
          tags$div(style = paste0("font-weight:600; font-size:13px; color:", col, ";"),
                   ROUTE_LABELS[[f$process_route]]),
          tags$div(style = "font-size:12px; color:var(--ink-2); margin-top:4px;",
                   ROUTE_NOTES[[f$process_route]]),

          if (f$sector == "iron-and-steel" && !is.na(f$cap_crude_kt)) {
            tags$div(
              style = "font-size:12px; margin-top:6px; color:var(--ink-2);",
              tags$b("Kapasite: "), fmt_tr(f$cap_crude_kt), " kt ham çelik",
              if (!is.na(f$cap_eaf_kt))
                paste0(" · EAF ", fmt_tr(f$cap_eaf_kt), " kt"),
              if (!is.na(f$cap_bof_kt))
                paste0(" · BOF ", fmt_tr(f$cap_bof_kt), " kt")
            )
          },

          if (f$sector == "cement" && !is.na(f$cap_clinker_mtpa)) {
            tags$div(
              style = "font-size:12px; margin-top:6px; color:var(--ink-2);",
              tags$b("Klinker kapasitesi: "),
              fmt_tr(f$cap_clinker_mtpa, 1), " Mt/yıl",
              if (!is.na(f$clinker_substitution))
                tagList(tags$br(), tags$b("Klinker ikamesi: "),
                        f$clinker_substitution,
                        tags$span(style = "color:var(--ink-3);",
                                  " (GEM bunu bant/derece olarak yayımlar, oran olarak değil)"))
            )
          },

          tags$div(style = "font-size:11px; color:var(--ink-3); margin-top:6px;",
                   "Kaynak: Global Energy Monitor, koordinat eşleştirmesiyle")
        )
      },

      # --- TR-ETS threshold ---------------------------------------------------
      if (!is.null(tr_ets_coverage) && f$asset_class == "industrial") {
        e <- tr_ets_coverage[tr_ets_coverage$facility_id == selected_id(), ]
        if (nrow(e) == 1) {
          above <- isTRUE(e$above_threshold)
          tags$div(
            style = paste0("margin-top:12px; padding:8px 10px; font-size:12px;",
                           " border-left:4px solid ",
                           if (above) "var(--live)" else "var(--ink-2)",
                           "; background:var(--panel);"),
            tags$b(if (above) "TR-ETS eşiğinin üstünde" else
                     "TR-ETS eşiğinin altında"),
            " — ", e$year, " emisyonu ", fmt_tr(e$emissions_t / 1000, 1),
            " kt, eşik ", fmt_tr(TR_ETS_THRESHOLD_T / 1000), " kt.",
            tags$div(
              style = "color:var(--ink-3); margin-top:4px;",
              "Yaklaşımdır: mevzuat eşiği ", tags$i("izinli yıllık emisyon ",
              "kapasitesine"), " uygular, burada gerçekleşen emisyon ",
              "karşılaştırılıyor."
            )
          )
        }
      },

      # --- Ownership ----------------------------------------------------------
      if (!is.null(ownership)) {
        rows <- ownership[ownership$facility_id == selected_id(), ]
        if (nrow(rows) == 0) {
          tags$div(
            style = paste0("margin-top:12px; padding:8px 10px; font-size:12px;",
                           " background:var(--panel); color:var(--ink-3);"),
            tags$b("Sahiplik kaydı yok."), " Bu tesis için ana şirket ",
            "yayımlanmamış — Türk sahipli sayılmadı, bilinmiyor olarak duruyor."
          )
        } else {
          named <- rows[rows$is_named_entity, ]
          # Quantified chains first, then by share, largest first.
          named <- named[order(named$path_terminal_unknown,
                               -coalesce(named$share_pct_unreliable, -1)), ]
          tags$div(
            style = "margin-top:12px; padding:10px 12px; background:var(--panel); border:1px solid var(--line);",
            tags$div(style = "font-size:12px; color:var(--ink-2); margin-bottom:6px;",
                     tags$b("Sahiplik zinciri"), " — ", nrow(rows), " kayıt, ",
                     sum(rows$is_named_entity), " isimli şirket"),

            if (nrow(named) > 0) {
              tagList(lapply(seq_len(min(6, nrow(named))), function(i) {
                r <- named[i, ]
                unk <- isTRUE(r$path_terminal_unknown)
                tags$div(
                  style = paste0("font-size:12.5px; padding:3px 0;",
                                 # Same recalibration as the policy calendar:
                                 # .55 was legible on white, not on this ground.
                                 if (unk) " opacity:.7;" else ""),
                  tags$span(style = "font-weight:600;", r$parent_name),
                  if (!is.na(r$parent_hq_country))
                    tags$span(style = "color:var(--ink-3);", " · ", r$parent_hq_country),
                  if (!is.na(r$share_pct_unreliable))
                    tags$span(style = "color:var(--ink-3);",
                              " · %", fmt_tr(r$share_pct_unreliable, 1)),
                  if (unk)
                    tags$span(style = "color:var(--ink); font-size:11px;",
                              " · son halka nicelendirilmemiş")
                )
              }))
            },

            if (any(rows$is_eu)) {
              tags$div(
                style = paste0("margin-top:8px; padding:6px 8px; font-size:12px;",
                               " background:transparent; border-left:3px solid var(--ink-2);"),
                tags$b("AB27 merkezli bir sahibi var."),
                if (any(rows$is_eu & !rows$path_terminal_unknown))
                  " Zincirin son halkası nicelendirilmiş."
                else
                  tagList(" ", tags$span(style = "color:var(--ink);",
                          "Ancak son halka nicelendirilmemiş — bu iddia zayıf."))
              )
            },

            tags$div(
              style = "font-size:11px; color:var(--ink-3); margin-top:8px;",
              "Paylar bir ", tags$b("yol çarpımıdır"), ", tesisin bölüşümü ",
              "değil; tesis başına toplamları %342'ye kadar çıkar. ",
              "Bu yüzden burada hiçbir sahiplik yüzdesi hesaplanmaz."
            )
          )
        }
      },

      # The field the whole merge turns on, spelled out rather than coded.
      tags$div(
        style = "margin-top:12px; padding:8px 10px; background:var(--panel); font-size:12px;",
        tags$b("Yükümlülük sınıfı: "),
        LIABILITY_LABELS[[f$liability_class]]
      ),

      if (!is.na(f$is_captive) && f$is_captive) {
        tags$div(
          style = paste0("margin-top:8px; padding:8px 10px; background:transparent;",
                         " border-left:4px solid var(--ink-2); font-size:12px;"),
          tags$b("Kendi tesisini besleyen santral"),
          if (!is.na(f$captive_industry))
            paste0(" — ", f$captive_industry), ". ",
          "Şebekeyi beslemediği için şebeke emisyon faktörünün hem payından ",
          "hem paydasından çıkarılır."
        )
      },

      tags$div(
        style = paste0("margin-top:12px; padding:8px 10px; border-left:4px solid ",
                       GEOCODE_BADGE[[f$geocode_quality]], "; background:var(--panel);",
                       " font-size:12px;"),
        tags$b("Konum ataması: "), GEOCODE_LABELS[[f$geocode_quality]]
      )
    )
  })


  # ===========================================================================
  # 5. QUALITY SUMMARIES
  # ===========================================================================
  # Surfacing these is a deliberate choice. A tool that shows 300 confident dots
  # and hides that 80 of them sit on or beyond a border is overstating what it
  # knows (§8.4).

  # --- ownership overview -----------------------------------------------------
  # Counts of facilities, never shares. Each claim is shown at both strengths so
  # the reader sees the uncertainty rather than a single confident number.

  output$ownership_panel <- renderUI({
    if (is.null(ownership_summary)) {
      return(tags$p(style = "font-size:13px; color:var(--ink-3);",
                    "Sahiplik katmanı üretilmedi. Çalıştırın: ",
                    tags$code("Rscript scripts/05_build_ownership.R")))
    }

    ids <- selected_facilities()$facility_id
    s   <- ownership_summary[ownership_summary$facility_id %in% ids, ]
    if (nrow(s) == 0) return(tags$p("Seçili tesis yok."))

    known <- sum(s$ownership_known)

    stat <- function(label, any_n, certain_n, colour) {
      tags$div(
        style = "margin-bottom:10px;",
        tags$div(style = "display:flex; justify-content:space-between; font-size:13px;",
                 tags$span(label),
                 tags$span(style = "font-weight:600;",
                           any_n, " / ", tags$span(style = paste0("color:", colour, ";"),
                                                   certain_n))),
        tags$div(style = "background:var(--panel); height:6px; border-radius:2px;",
                 tags$div(style = paste0("background:", colour, "; height:6px;",
                                         " border-radius:2px; width:",
                                         if (known > 0) round(100*certain_n/known) else 0,
                                         "%;")))
      )
    }

    tagList(
      tags$p(style = "font-size:13px; margin:0 0 12px;",
             tags$b(known), " / ", nrow(s), " tesiste ana şirket kaydı var. ",
             tags$span(style = "color:var(--ink-3);",
                       "Kaydı olmayan ", nrow(s) - known,
                       " tesis Türk sahipli sayılmadı — bilinmiyor.")),

      stat("AB27 merkezli sahibi olan",
           sum(isTRUE_vec(s$has_eu_parent)),
           sum(isTRUE_vec(s$has_eu_parent_certain)), "var(--ink-2)"),
      stat("Yabancı sahibi olan",
           sum(isTRUE_vec(s$has_foreign_parent)),
           sum(isTRUE_vec(s$has_foreign_parent_certain)), "var(--ink)"),

      tags$p(
        style = paste0("font-size:12px; color:var(--ink-2); margin-top:4px;",
                       " border-top:1px solid var(--line); padding-top:10px;"),
        tags$b("İki sayı, iki güç. "), "Soldaki herhangi bir sahiplik zinciri, ",
        "sağdaki ", tags$b("son halkası nicelendirilmiş"), " olanlar. Aradaki ",
        "fark, tesise giden bağın payı bilinmeyen zincirlerdir — ve isim ",
        "çakışmalarının saklandığı yer orası."
      ),

      if (!is.null(ownership_by_parent)) {
        top <- head(ownership_by_parent[ownership_by_parent$facilities_certain > 0, ], 8)
        tagList(
          tags$div(style = "font-weight:600; font-size:13px; margin-top:14px;",
                   "En çok tesise sahip şirketler"),
          tags$div(style = "font-size:11px; color:var(--ink-3); margin-bottom:6px;",
                   "Zinciri kesin olan tesis sayısına göre sıralı"),
          # The list mixes two different things and the data cannot separate
          # them: a cement group that runs its plants, and an index fund that
          # holds shares in a listed company that runs them. `parent_entity_type`
          # says "legal entity" for both. Naming the ambiguity is honest;
          # inventing a classifier for it would not be.
          tags$div(
            style = paste0("font-size:11px; color:var(--ink); margin-bottom:8px;",
                           " padding:5px 7px; background:transparent;"),
            tags$b("Bu liste iki farklı şeyi karıştırıyor"), " ve veri onları ",
            "ayıramıyor: tesisi ", tags$i("işleten"), " sanayi grubu ile o ",
            "gruba ", tags$i("hisse tutan"), " fon aynı alanda ",
            "\"legal entity\" olarak geçiyor. BlackRock ve Vanguard portföy ",
            "yatırımcısıdır; Heidelberg ve Çimsa işletmecidir. Ayrımı biz ",
            "uydurmadık, çünkü kaynak yapmıyor."),
          tags$table(
            style = "width:100%; font-size:12.5px;",
            lapply(seq_len(nrow(top)), function(i) {
              tags$tr(
                tags$td(style = "padding:3px 0;", top$parent_name[i]),
                tags$td(style = "text-align:right; color:var(--ink-3); width:44px;",
                        top$parent_hq_country[i]),
                tags$td(style = "text-align:right; font-weight:600; width:34px;",
                        top$facilities_certain[i])
              )
            })
          )
        )
      },

      tags$p(
        style = "font-size:11.5px; color:var(--ink-3); margin-top:12px;",
        tags$b("Burada hiçbir yüzde yok, ve bu kasıtlı. "),
        "Climate TRACE'in pay alanı bir yol çarpımıdır, tesisin bölüşümü ",
        "değil — tesis başına toplamları %342'ye çıkıyor. Emisyonu bu paya ",
        "göre ağırlıklandırmak, pay gibi davranmayan bir büyüklükle aritmetik ",
        "yapmak olurdu."
      )
    )
  })


  output$geocode_summary <- renderUI({
    df <- selected_facilities()
    if (nrow(df) == 0) return(tags$p("Seçili tesis yok."))

    tally <- df |> count(geocode_quality, name = "n")

    tagList(
      tags$table(
        style = "width:100%; font-size:13px;",
        lapply(seq_len(nrow(tally)), function(i) {
          q <- tally$geocode_quality[i]
          tags$tr(
            tags$td(style = paste0("padding:5px 0; border-left:4px solid ",
                                   GEOCODE_BADGE[[q]], "; padding-left:8px;"),
                    GEOCODE_LABELS[[q]]),
            tags$td(style = "text-align:right; font-weight:600;", tally$n[i])
          )
        })
      ),
      tags$p(
        style = "margin-top:10px; font-size:12px; color:var(--ink-2);",
        "İl sınırları Natural Earth 10m katmanından türetilmiştir. Bu ",
        "çözünürlük, il sınırına çok yakın veya kıyı dolgusu üzerindeki ",
        "tesisleri kesin olarak ayırt edemez. Tesis bazında mesafeler ",
        tags$code("data/processed/facilities_geocode_report.csv"), " dosyasındadır."
      )
    )
  })

  output$commissioning_summary <- renderUI({
    df <- selected_facilities()
    if (nrow(df) == 0) return(tags$p("Seçili tesis yok."))

    by_class <- df |>
      group_by(asset_class) |>
      summarise(n = n(), known = sum(!is.na(commissioning_year)),
                .groups = "drop")

    tagList(
      tags$table(
        style = "width:100%; font-size:13px;",
        lapply(seq_len(nrow(by_class)), function(i) {
          tags$tr(
            tags$td(ASSET_CLASS_LABELS[[by_class$asset_class[i]]]),
            tags$td(style = "text-align:right; font-weight:600;",
                    paste0(by_class$known[i], " / ", by_class$n[i]))
          )
        })
      ),
      tags$p(
        style = "margin-top:10px; font-size:12px; color:var(--ink-2);",
        "Devreye giriş yılı Global Energy Monitor'den, koordinat yakınlığıyla ",
        "eşleştirilerek alınmıştır — Climate TRACE 2021'de başlar ve bu alanı ",
        "taşımaz. Yılı bilinmeyen tesisler, 2021 öncesi filo animasyonundan ",
        tags$b("görünür şekilde dışlanmalıdır"), "; her zaman var oldukları ",
        "varsayılmamalıdır."
      )
    )
  })


  output$fleet_summary <- renderUI({
    if (is.null(fleet_renewables)) {
      return(tags$p("Yenilenebilir filo verisi üretilmedi. Çalıştırın: ",
                    tags$code("Rscript scripts/02_build_facilities.R")))
    }

    by_fuel <- fleet_renewables |>
      group_by(fuel_type) |>
      summarise(plants = n(),
                gw = sum(capacity_mw, na.rm = TRUE) / 1000,
                .groups = "drop") |>
      arrange(desc(gw))

    tagList(
      tags$table(
        style = "width:100%; font-size:13px;",
        lapply(seq_len(nrow(by_fuel)), function(i) {
          ft <- by_fuel$fuel_type[i]
          tags$tr(
            tags$td(style = paste0("padding:4px 0; border-left:4px solid ",
                                   FLEET_COLOURS[[ft]], "; padding-left:8px;"),
                    FLEET_LABELS[[ft]]),
            tags$td(style = "text-align:right;", by_fuel$plants[i]),
            tags$td(style = "text-align:right; font-weight:600;",
                    paste0(fmt_tr(by_fuel$gw[i], 1), " GW"))
          )
        }),
        tags$tr(
          tags$td(style = "padding-top:8px; border-top:1px solid var(--line);",
                  tags$b("Toplam")),
          tags$td(style = "text-align:right; padding-top:8px; border-top:1px solid var(--line);",
                  tags$b(nrow(fleet_renewables))),
          tags$td(style = "text-align:right; padding-top:8px; border-top:1px solid var(--line);",
                  tags$b(paste0(fmt_tr(sum(by_fuel$gw), 1), " GW")))
        )
      ),
      tags$p(
        style = "margin-top:10px; font-size:12px; color:var(--ink-2);",
        tags$b("Bu tesisler modellenen 300 kaydın parçası değildir. "),
        "Global Energy Monitor'den gelirler, emisyon üretmezler ve emisyon ",
        "tablolarında yer almazlar. Haritada bulunma sebepleri tek bir soruyu ",
        "cevaplamaktır: Climate TRACE'in elektrik kütüğünün göremediği üretim ",
        "nerede? Kütük ulusal üretimin yaklaşık yarısını görüyor ve görmediği ",
        "yarı, büyük ölçüde burada duruyor."
      )
    )
  })


  # ===========================================================================
  # 5b. FLEET DEVELOPMENT, 2000–2026
  # ===========================================================================
  # A second time axis with its own slider. It answers a different question from
  # the map tab — how the fleet arrived, rather than who emits how much — and it
  # reaches back twenty years further, into a period where no emissions data
  # exists at all.

  fleet_year <- reactive({
    if (is.null(input$fleet_year)) TIMELINE_DEFAULT else input$fleet_year
  }) |> debounce(250)

  # Both registers as one table, built once per session: none of it depends on
  # an input. Grouped exactly as 04_build_fleet_timeline.R groups them, so the
  # map and the chart can never disagree about which population a facility is
  # in.
  fleet_points <- bind_rows(
    facilities |>
      transmute(
        name = facility_name_tr, lat, lon, commissioning_year,
        detail = unname(SECTOR_LABELS[sector]),
        tl_group = case_when(
          asset_class == "industrial"        ~ "industrial",
          sector == "electricity-generation" ~ "ct_combustion",
          TRUE                               ~ "energy_other"),
        mw = NA_real_),
    if (!is.null(fleet_renewables)) {
      fleet_renewables |>
        transmute(name = plant_name, lat, lon, commissioning_year,
                  detail = unname(FLEET_LABELS[fuel_type]),
                  tl_group = "renewable",
                  mw = capacity_mw)
    }
  )
  fleet_dated   <- fleet_points[!is.na(fleet_points$commissioning_year), ]
  fleet_undated <- fleet_points[is.na(fleet_points$commissioning_year), ]
  FLEET_MARKER_YEARS <- sort(unique(fleet_dated$commissioning_year))

  # Everything commissioned up to and including the selected year, plus the
  # undated facilities as their own state. Counts only — the map no longer
  # reads this; it shows and hides year groups instead (below).
  fleet_visible <- reactive({
    req(!is.null(fleet_timeline))
    yr <- fleet_year()
    list(
      # Arrived: commissioned on or before the selected year.
      arrived = fleet_dated[fleet_dated$commissioning_year <= yr, ],
      # Not yet: commissioned later. Drawn nowhere, but counted so the reader
      # can see the fleet is still growing beyond the cursor.
      later   = fleet_dated[fleet_dated$commissioning_year > yr, ],
      # Undated: shown at every year in their own neutral state. §6 requires
      # them to be visibly excluded rather than assumed to have always existed,
      # and a marker that never changes is what "we do not know when" looks like.
      undated = fleet_undated
    )
  })

  output$fleet_year_status <- renderUI({
    req(!is.null(fleet_timeline))
    v <- fleet_visible()
    tags$p(
      style = "margin:-6px 0 0; font-size:12.5px; color:var(--ink-2);",
      tags$b(fleet_year()), " itibarıyla devreye alınmış: ",
      tags$b(fmt_tr(nrow(v$arrived))), " tesis. ",
      tags$span(style = "color:var(--ink-2);",
                fmt_tr(nrow(v$undated)),
                " tesis tarihsiz ve hiçbir yıla ait değil."),
      if (nrow(v$later) > 0) {
        tags$span(style = "color:#8B99A3;", " · ",
                  fmt_tr(nrow(v$later)), " tesis daha sonra.")
      }
    )
  })

  output$fleet_map_title <- renderText({
    paste0("Filo — ", fleet_year(), "'e kadar devreye alınanlar")
  })

  output$fleet_map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE,
                                     minZoom = 5, maxZoom = 12)) |>
      addTiles(urlTemplate = ESRI_BASE, attribution = ESRI_ATTRIB,
               options = tileOptions(minZoom = 5, maxZoom = 12)) |>
      add_place_labels(maxZoom = 12) |>
      fitBounds(lng1 = MAP_BOUNDS$lng1, lat1 = MAP_BOUNDS$lat1,
                lng2 = MAP_BOUNDS$lng2, lat2 = MAP_BOUNDS$lat2) |>
      setMaxBounds(lng1 = MAP_BOUNDS$lng1 - 1.5, lat1 = MAP_BOUNDS$lat1 - 1.0,
                   lng2 = MAP_BOUNDS$lng2 + 1.5, lat2 = MAP_BOUNDS$lat2 + 1.0) |>
      addLegend(position = "bottomright",
                colors = c(unname(TL_COLOURS), "#5A6670"),
                labels = c(unname(TL_LABELS), "Tarihi bilinmiyor"),
                title = "Filo", opacity = 0.9)
  })

  # Shiny suspends outputs in hidden tabs, so this map does not exist until the
  # fleet tab is first opened — by which time the observer below has already
  # fired and sent its markers to a leafletProxy with nothing behind it. The
  # commands are dropped silently, the map opens empty, and it then fills in the
  # moment anything re-triggers the observer, which makes it look intermittent
  # rather than broken. Rendering it even while hidden is the fix.
  #
  # This call must come AFTER the assignment above: outputOptions() looks the
  # output up by name and raises "not in list of output objects" if it does not
  # exist yet.
  outputOptions(output, "fleet_map", suspendWhenHidden = FALSE)

  # The markers are sent to the browser ONCE, the first time the fleet tab is
  # opened, each in a layer group named after its commissioning year ("y2014").
  # The slider then only shows and hides groups.
  #
  # The previous version cleared and resent every arrived marker on each slider
  # step. Measured: about 600 KB per step for 3,136 renewable plants, sent to
  # every visitor at page load whether or not they ever opened this tab, and
  # roughly 15 MB across one play of the animation. A step now costs a list of
  # group names, and a visitor who never opens the tab downloads none of it.
  fleet_drawn <- reactiveVal(FALSE)

  observeEvent(input$tab, {
    req(identical(input$tab, "fleet"), !fleet_drawn(), !is.null(fleet_timeline))

    proxy <- leafletProxy("fleet_map")

    if (nrow(fleet_dated) > 0) {
      proxy <- proxy |>
        addCircleMarkers(
          data = fleet_dated, lng = ~lon, lat = ~lat,
          group = ~paste0("y", commissioning_year),
          # Renewables carry a capacity, so their radius can scale; the modelled
          # facilities are drawn at a fixed size because their capacities are in
          # four incompatible units and any shared scale would be a fiction.
          radius = ~if_else(is.na(mw), 5,
                            pmax(2.5, pmin(10, sqrt(pmax(mw, 0)) / 2.6))),
          fillColor = ~unname(TL_COLOURS[tl_group]),
          fillOpacity = 0.8, color = "#11161A", weight = 0.6,
          label = ~paste0(name, " · ", commissioning_year),
          popup = ~paste0(
            "<strong>", name, "</strong><br/>",
            "<em>", detail, "</em><br/><br/>",
            "<b>Devreye giriş:</b> ", commissioning_year, "<br/>",
            "<b>Kapasite:</b> ",
            ifelse(is.na(mw), "bu görünümde ölçülmüyor",
                   paste0(fmt_tr(mw, 1), " MW")))
        )
    }

    if (nrow(fleet_undated) > 0) {
      proxy |>
        addCircleMarkers(
          data = fleet_undated, lng = ~lon, lat = ~lat, group = "undated",
          # Hollow, neutral, and identical at every year. A third visual state
          # for a third epistemic state, exactly as projected years get their
          # own treatment on the emissions map.
          radius = 4.5, fillOpacity = 0,
          color = "#7C8A94", weight = 1.6, opacity = 0.9,
          label = ~paste0(name, " — devreye giriş yılı bilinmiyor"),
          popup = ~paste0(
            "<strong>", name, "</strong><br/>",
            "<em>", detail, "</em><br/><br/>",
            "<b>Devreye giriş yılı bilinmiyor.</b><br/>",
            "<span style='color:#93A1AB;'>Bu tesis hiçbir yıla atanmadı. ",
            "Zaman çizelgesinin dışındadır ve her yılda aynı şekilde ",
            "görünür — var olduğu varsayılmadığı için gizlenmedi, ",
            "bilinmediği için de bir yıla yazılmadı.</span>")
        )
    }

    fleet_drawn(TRUE)
  })

  # The slider: a year group is visible when its year is on or before the
  # cursor. Also runs once right after the first draw, because fleet_drawn()
  # changes then — every group arrives visible and the later ones are hidden
  # in the same flush, after the markers, so they never flash on screen.
  observe({
    req(fleet_drawn())
    yr    <- fleet_year()
    shown <- FLEET_MARKER_YEARS <= yr

    proxy <- leafletProxy("fleet_map")
    if (any(shown))  proxy <- proxy |> showGroup(paste0("y", FLEET_MARKER_YEARS[shown]))
    if (any(!shown)) proxy |> hideGroup(paste0("y", FLEET_MARKER_YEARS[!shown]))
  })


  # --- headline figures for the fleet tab -------------------------------------

  tl_at <- reactive({
    req(!is.null(fleet_timeline))
    fleet_timeline[fleet_timeline$year == fleet_year(), ]
  })

  tl_pick <- function(d, grp, col) {
    v <- d[[col]][d$group == grp]
    if (length(v) == 0) NA else v[1]
  }

  # The fleet tab's four boxes, collapsed the same way.
  output$fleet_strip <- renderUI({
    d    <- tl_at()
    comb <- tl_pick(d, "ct_combustion", "cum_mw")
    ren  <- tl_pick(d, "renewable", "cum_mw")
    pct  <- if (is.na(comb) || is.na(ren) || (comb + ren) == 0) NA
            else round(100 * ren / (comb + ren))
    gw   <- function(mw) if (is.na(mw)) "—" else paste0(fmt_tr(mw / 1000, 1), " GW")
    undated <- sum(unique(fleet_timeline[, c("group", "undated_n")])$undated_n)

    readout_strip(
      readout(paste0("Yanma · ", tl_pick(d, "ct_combustion", "cum_n"), " tesis"),
              gw(comb)),
      readout(paste0("Yenilenebilir · ",
                     fmt_tr(tl_pick(d, "renewable", "cum_n")), " tesis"),
              gw(ren)),
      readout("Yenilenebilirin güçteki payı",
              if (is.na(pct)) "—" else paste0("%", pct)),
      # Not a defect to be hidden: these are the records GEM gives no
      # commissioning year for, and they are absent from every curve on this
      # tab. The strip names the size of that absence.
      readout("Tarihsiz — hiçbir yılda yok", fmt_tr(undated), tone = "null")
    )
  })


  # --- the chart -------------------------------------------------------------
  # Dual axis by design (decision 2c): gigawatts on the left, plant count on the
  # right. The two diverging is itself the finding — renewables arrive in very
  # large numbers of very small plants, and a single axis hides whichever half it
  # is not scaled to.

  output$fleet_plot <- renderPlot({
    req(!is.null(fleet_timeline))
    tl <- fleet_timeline
    yrs <- sort(unique(tl$year))
    mode <- if (is.null(input$fleet_chart_mode)) "cum" else input$fleet_chart_mode

    mw_col <- if (mode == "cum") "cum_mw" else "added_mw"
    n_col  <- if (mode == "cum") "cum_n"  else "added_n"

    op <- plot_theme(mar = c(3.0, 4.2, 0.6, 4.2), cex.axis = .8, cex.lab = .85,
                     mgp = c(2.4, .6, 0), tcl = -.25, las = 1)
    on.exit(par(op), add = TRUE)

    mw_groups <- tl[tl$group %in% TL_HAS_MW, ]
    ymax <- max(mw_groups[[mw_col]], tl$ember_total_gw * 1000, na.rm = TRUE)

    plot(NA, xlim = range(yrs), ylim = c(0, ymax * 1.08),
         xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    axis(1, at = seq(min(yrs), max(yrs), by = 4), col = PLOT_AXIS, col.axis = PLOT_SOFT)
    axis(2, at = pretty(c(0, ymax)), labels = pretty(c(0, ymax)) / 1000,
         col = PLOT_AXIS, col.axis = PLOT_SOFT)
    mtext(if (mode == "cum") "Kümülatif GW" else "Yıllık eklenen GW",
          side = 2, line = 2.7, cex = .8, col = PLOT_SOFT)
    grid(nx = NA, ny = NULL, col = PLOT_RULE, lty = 1)

    # The national reference. Dotted, neutral, and only on the cumulative view:
    # Ember publishes a stock, so comparing it with annual additions would be a
    # category error.
    if (mode == "cum" && any(!is.na(tl$ember_total_gw))) {
      e <- unique(tl[, c("year", "ember_total_gw")])
      e <- e[!is.na(e$ember_total_gw), ]
      lines(e$year, e$ember_total_gw * 1000, col = "#8B99A3",
            lwd = 2, lty = 3)
      text(max(e$year), max(e$ember_total_gw) * 1000, "Ember kurulu güç",
           pos = 2, offset = .4, col = "#8B99A3", cex = .72, font = 3)
    }

    for (g in TL_HAS_MW) {
      d <- tl[tl$group == g, ]
      d <- d[order(d$year), ]
      lines(d$year, d[[mw_col]], col = TL_COLOURS[[g]], lwd = 2.6)
    }

    # Right axis: plant counts, on their own scale.
    par(new = TRUE)
    nmax <- max(tl[[n_col]], na.rm = TRUE)
    plot(NA, xlim = range(yrs), ylim = c(0, nmax * 1.08),
         axes = FALSE, xlab = "", ylab = "", bty = "n")
    axis(4, at = axTicks(4), labels = fmt_tr(axTicks(4)),   # 3.000, not 3000
         col = PLOT_AXIS, col.axis = PLOT_SOFT)
    mtext(if (mode == "cum") "Kümülatif tesis sayısı" else "Yıllık eklenen tesis",
          side = 4, line = 2.9, cex = .8, col = PLOT_SOFT)

    for (g in names(TL_LABELS)) {
      d <- tl[tl$group == g, ]
      d <- d[order(d$year), ]
      lines(d$year, d[[n_col]], col = TL_COLOURS[[g]], lwd = 1.4, lty = 2)
    }

    # Policy events as vertical marks. Drawn last so they sit above the curves,
    # and only where they fall inside the plotted window — several run past 2026
    # and clipping them silently would misrepresent the calendar as ending.
    if (isTRUE(input$show_policy) && !is.null(policy_events)) {
      pe <- policy_events[policy_events$x >= min(yrs) &
                            policy_events$x <= max(yrs), ]
      if (nrow(pe) > 0) {
        cols <- POLICY_CATEGORY_COLOURS[pe$category]
        # A scheduled publication has not happened. Dotted, like every other
        # not-yet-real quantity in this project.
        ltys <- ifelse(pe$value_type == "scheduled", 3, 1)
        for (i in seq_len(nrow(pe))) {
          abline(v = pe$x[i], col = paste0(cols[i], "AA"),
                 lwd = 1.6, lty = ltys[i])
        }
        # Ticks along the top edge rather than labels: thirteen labels on a
        # 26-year axis would be unreadable, and the panel below carries the text.
        usr <- par("usr")
        points(pe$x, rep(usr[4] * 0.985, nrow(pe)), pch = 25,
               col = cols, bg = cols, cex = 0.7)
      }
    }

    abline(v = fleet_year(), col = paste0(PLOT_SOFT, "66"), lwd = 1.5, lty = 3)
  }, bg = PLOT_BG)

  output$fleet_plot_note <- renderUI({
    tags$div(
      style = "margin-top:10px;",
      fold_note(
        tagList(tags$b("Kalın düz çizgi GW,"), " ince kesik çizgi tesis sayısı."),
        "Sol eksen GW, sağ eksen adet. Yenilenebilirin iki eğrisinin açılması ",
        "bir bulgudur: çok sayıda, çok küçük santral. Sanayi ve kömür/petrol-gaz ",
        "yalnızca sayılır — kapasiteleri ton ve varil cinsinden olduğu için ",
        "megawatt eksenine girmez. Noktalı gri çizgi Ember'in gerçek ulusal ",
        "kurulu gücüdür."
      ),
      if (isTRUE(input$show_policy) && !is.null(policy_events)) {
        inside <- sum(policy_events$x >= min(fleet_timeline$year) &
                        policy_events$x <= max(fleet_timeline$year))
        fold_note(
          tagList(tags$b("Dikey çizgiler politika olaylarıdır."),
                  " Hiçbiri eğrideki kırılmaları açıklamıyor."),
          "Takvimdeki ", nrow(policy_events), " olaydan yalnızca ",
          tags$b(inside), " tanesi bu pencereye düşüyor; kalanı ",
          max(fleet_timeline$year), " sonrası. Onları açıklayacak Türkiye ",
          "enerji politikası takvimi henüz derlenmedi. Noktalı olanlar henüz ",
          "gerçekleşmemiş planlanan tarihler."
        )
      }
    )
  })

  # --- the policy calendar ----------------------------------------------------
  # Every event here is derived from a parameter file already in the repository,
  # and each row says which one. What the panel says most loudly is what it does
  # NOT contain, because a policy tracker that quietly covers only EU regulation
  # would be read as covering Turkish policy too.

  output$policy_panel <- renderUI({
    if (is.null(policy_events)) {
      return(tags$p(style = "font-size:13px; color:var(--ink-3);",
                    "Politika takvimi dosyası bulunamadı: ",
                    tags$code("policies/policy_timeline.json")))
    }

    yr <- fleet_year()
    ev <- policy_events

    row_ui <- function(i) {
      e   <- ev[i, ]
      col <- POLICY_CATEGORY_COLOURS[[e$category]]
      # Events the slider has not reached yet are dimmed rather than hidden, so
      # the calendar reads as a calendar and not as a filtered list.
      future <- e$year > yr

      # One line per event, the rest behind a disclosure. Fully expanded, the
      # thirteen events ran to roughly 1,300px — the largest single stack on the
      # page, and the reason the calendar could not be read as a calendar. The
      # date, the category and the title are what a calendar is; the summary and
      # the provenance are what you open an entry to find.
      tags$details(
        class = "ev",
        style = paste0("border-left:2px solid ", col, ";",
                       # Events past the slider year are dimmed rather than
                       # hidden: a calendar that ends at the chosen year
                       # misrepresents itself. 0.5 was set against a white page
                       # and is below readable on this ground.
                       if (future) " opacity:.68;" else ""),
        tags$summary(
          tags$span(class = "ev-date", e$date),
          tags$span(class = "ev-cat",
                    style = paste0("color:", col, "; border-color:", col, ";"),
                    POLICY_CATEGORY_LABELS[[e$category]]),
          tags$span(class = "ev-title", e$title_tr),
          if (e$value_type == "scheduled") {
            tags$span(class = "ev-flag", "planlanan")
          },
          if (e$granularity == "year") {
            tags$span(class = "ev-flag", "yıl hassasiyeti")
          }
        ),
        tags$div(
          class = "ev-body",
          e$summary_tr,
          tags$div(
            style = "font-size:11px; color:var(--ink-3); margin-top:6px;",
            "Kaynak: ",
            if (!is.na(e$source_url)) {
              tags$a(href = e$source_url, target = "_blank", rel = "noopener",
                     "bağlantı")
            } else "—",
            " · türetildiği dosya: ", tags$code(e$derived_from))
        )
      )
    }

    tagList(
      # The honest header. This is the single most important thing on the panel,
      # so its claim stays on screen and only the argument folds away.
      fold_note(
        tagList(tags$b("Bu takvim eksik ve eksikliği rastgele değil."),
                " ", nrow(ev), " olayın tamamı 2023 ve sonrası."),
        "Buradaki olayların tamamı AB SKDM ve Türkiye ETS mevzuatı. Filo eğrisi ",
        "2000'de başlıyor, yani bu işaretler grafiğin sağ ucunda duruyor ve ",
        tags$b("arkalarındaki eğriyi açıklamıyorlar"), " — 2015-2024 yenilenebilir ",
        "atılımı bu mevzuatın hiçbiri yokken çoktan olmuştu.",
        tags$br(), tags$br(),
        "Eğriyi açıklayacak olan Türkiye enerji politikası — YEKDEM, kapasite ",
        "mekanizması, kömür teşvikleri, lisans rejimi — burada ",
        tags$b("yok"), ", çünkü açık ve makine okunur bir kütüğü yok; her madde ",
        "birincil kaynaktan okunup kaynaklandırılmalı."
      ),

      lapply(seq_len(nrow(ev)), row_ui),

      tags$p(
        style = paste0("font-size:11.5px; color:var(--ink-3); margin-top:12px;",
                       " border-top:1px solid var(--line); padding-top:10px;"),
        "Hiçbir olay bu proje için yeniden araştırılmadı; hepsi ",
        tags$code("policies/"), " altındaki mevcut parametre dosyalarından ",
        "türetildi ve kaynağı taşındı. Solmuş satırlar slider'ın henüz ",
        "ulaşmadığı yıllardır."
      )
    )
  })


  output$fleet_coverage_panel <- renderUI({
    req(!is.null(fleet_coverage))
    cv <- fleet_coverage[order(-fleet_coverage$share), ]

    # The sentences quote the data rather than numbers typed in once and left
    # to go stale at the next GEM release.
    pct      <- function(x) round(100 * x)
    spread   <- pct(max(cv$share)) - pct(min(cv$share))
    comb_pct <- pct(cv$share[cv$group == "ct_combustion"])
    ren_pct  <- pct(cv$share[cv$group == "renewable"])

    bar <- function(share, colour) {
      tags$div(
        style = "background:var(--panel); height:7px; border-radius:2px; width:100%;",
        tags$div(style = paste0("background:", colour, "; height:7px;",
                                " border-radius:2px; width:",
                                round(100 * share), "%;")))
    }

    tagList(
      tags$p(style = "font-size:13px; margin:0 0 12px;",
             "Bir tesis ancak devreye giriş yılı biliniyorsa bir yıla ",
             "atanabilir. Bu oranlar gruplar arasında ",
             tags$b(spread, " puan"), " fark ediyor, ve bu fark eğrilerin ",
             "karşılaştırılabilirliğini doğrudan sınırlıyor."),
      lapply(seq_len(nrow(cv)), function(i) {
        g <- cv$group[i]
        tags$div(
          style = "margin-bottom:11px;",
          tags$div(
            style = "display:flex; justify-content:space-between; font-size:13px;",
            tags$span(TL_LABELS[[g]]),
            tags$span(style = "font-weight:600;",
                      paste0(cv$n_dated[i], " / ", cv$n_total[i], " · %",
                             round(100 * cv$share[i])))),
          bar(cv$share[i], TL_COLOURS[[g]]))
      }),
      tags$p(
        style = paste0("font-size:12px; color:var(--ink-2); margin-top:14px;",
                       " border-top:1px solid var(--line); padding-top:10px;"),
        tags$b("Yanma %", comb_pct, ", yenilenebilir %", ren_pct, ". "),
        # The direction of the bias is a claim about the data, so it is only
        # made while the data still supports it.
        if (length(comb_pct) == 1 && length(ren_pct) == 1 && comb_pct < ren_pct)
          paste0("İki eğriyi ham hâlde karşılaştırmak, yanmayı olduğundan ",
                 "küçük ve geçişi olduğundan keskin gösterir. "),
        "Kapsama oranı burada gösteriliyor ",
        "ki okuyucu kendi indirimini yapabilsin — dipnot yerine bant, çünkü ",
        "dipnot okunmuyor."
      )
    )
  })


  # ===========================================================================
  # 6. GRID INTENSITY
  # ===========================================================================
  # Three estimates, one of them deliberately wrong. Publishing only the chosen
  # figure would hide why it was chosen (§7).

  output$grid_panel <- renderUI({
    if (is.null(grid_intensity)) {
      return(tags$p(
        "Şebeke yoğunluğu verisi henüz üretilmedi. Çalıştırın: ",
        tags$code("Rscript scripts/01d_fetch_ember.R")))
    }

    g <- grid_intensity[grid_intensity$year >= 2021, ]

    tagList(
      tags$p(
        "Sanayi tesislerinin ", tags$b("dolaylı"), " emisyonu, tükettikleri ",
        "elektriğin karbon yoğunluğuyla çarpılarak bulunur. O yoğunluk için üç ",
        "bağımsız tahmin var ve aradaki fark başlı başına bir sonuçtur."
      ),

      tags$table(
        style = "width:100%; margin-top:12px; font-size:13px; border-collapse:collapse;",
        tags$thead(tags$tr(
          lapply(c("Yıl", "Ember", "Climate TRACE", "CT filosu (naif)",
                   "CT kapsaması", "Yenilenebilir"),
                 function(h) tags$th(style = "text-align:left; padding:6px; border-bottom:2px solid var(--line);", h))
        )),
        tags$tbody(lapply(seq_len(nrow(g)), function(i) {
          tags$tr(
            tags$td(style = "padding:6px; border-bottom:1px solid var(--line);", g$year[i]),
            tags$td(style = "padding:6px; border-bottom:1px solid var(--line); font-weight:600;",
                    round(g$ember_g_per_kwh[i])),
            tags$td(style = "padding:6px; border-bottom:1px solid var(--line); font-weight:600;",
                    round(g$ct_reported_g_per_kwh[i])),
            # Struck through and dimmed rather than coloured. This column is
            # arithmetically correct and substantively wrong — Climate TRACE's
            # Turkish electricity register is combustion-only, so the
            # denominator misses roughly half the fleet. It is kept visible so
            # that anyone who computes the same ratio later recognises it, and
            # the strike says "not for use" without spending a ninth hue.
            tags$td(style = paste0("padding:6px; border-bottom:1px solid var(--line);",
                                   " color:var(--ink-3); text-decoration:line-through;",
                                   " text-decoration-thickness:1px;"),
                    round(g$ct_naive_g_per_kwh[i])),
            tags$td(style = "padding:6px; border-bottom:1px solid var(--line);",
                    paste0("%", round(100 * g$ct_coverage_share[i]))),
            tags$td(style = "padding:6px; border-bottom:1px solid var(--line);",
                    paste0("%", round(100 * g$renewable_share[i])))
          )
        }))
      ),
      tags$p(style = "font-size:12px; color:var(--ink-2); margin-top:6px;",
             "gCO₂/kWh"),

      tags$div(
        style = paste0("margin-top:16px; padding:10px 14px; background:transparent;",
                       " border-left:4px solid var(--ink); font-size:13px;"),
        tags$b("Üçüncü sütun neden yanlış? "),
        "Climate TRACE'in Türkiye elektrik kütüğü yalnızca yanma tesislerini ",
        "içeriyor — hidro, rüzgâr, güneş, jeotermal ve nükleer yok. Ulusal ",
        "üretimin yaklaşık yarısını görüyor ve görmediği yarı neredeyse tam ",
        "olarak yenilenebilir olan yarı. Aritmetik doğru, payda eksik. Sütun ",
        "silinmedi çünkü birinin ileride aynı hesabı yapıp doğru sanmasını ",
        "engellemek, hatayı göstermekten geçiyor."
      ),

      tags$p(style = "margin-top:14px; font-size:13px;",
             tags$b("İlk iki sütun bağımsız yöntemlerden geliyor ve uyuşuyor. "),
             "Hangisinin hesaba gireceği ve kendi tesisini besleyen santrallerin ",
             "nasıl netleştirileceği metodolojik bir karardır; ",
             tags$code("policies/grid_emission_factor.json"), " içinde ",
             tags$code("selection.chosen"), " bilerek boş bırakılmıştır.")
    )
  })


  # ===========================================================================
  # 7. SOURCES TAB
  # ===========================================================================

  # CC BY 4.0 §3(1)(a) asks for more than a name: the creator, a URI to the
  # material, a licence notice WITH a link, a statement that the material was
  # MODIFIED, and the warranty disclaimer. Climate TRACE and Ember carried only
  # the name until 2026-11-06; on a public page that is a licence breach, not a
  # missing courtesy. Every source is now written through one constructor so
  # none of the five parts can be left off again. The modification lists mirror
  # data/processed/SOURCES_*.md.
  cc_by_link <- tags$a(href = "https://creativecommons.org/licenses/by/4.0/deed.tr",
                       target = "_blank", rel = "noopener", "CC BY 4.0")

  attribution <- function(title, creator, url, changes, extra = NULL) {
    tagList(
      tags$h4(title),
      tags$p(creator, " — ",
             tags$a(href = url, target = "_blank", rel = "noopener", url),
             ". Lisans: ", cc_by_link, ".", extra),
      tags$p(style = "font-size:12px; color:var(--ink-2);",
             tags$b("Değiştirilmiştir: "), changes)
    )
  }

  output$sources_panel <- renderUI({
    tagList(
      attribution(
        "Climate TRACE — tesis düzeyinde emisyon tahminleri",
        "Climate TRACE", "https://climatetrace.org",
        paste0("Türkiye kayıtlarına filtrelendi; aylık değerler yıllığa ",
               "toplandı; sütunlar bu projenin şemasına göre yeniden adlandırıldı; ",
               "Global Energy Monitor kayıtlarıyla birleştirildi; il ve İBBS-2 ",
               "bölgesi koordinatlardan türetildi."),
        extra = tagList(" Sürüm: ", tags$code(SOURCE_RELEASE), ".")
      ),

      attribution(
        "Global Energy Monitor — devreye giriş yılları ve filo",
        "Global Energy Monitor (Global Integrated Power Tracker, Global Coal Mine Tracker)",
        "https://globalenergymonitor.org",
        paste0("Türkiye kayıtlarına filtrelendi; sütunlar yeniden adlandırıldı ",
               "ve bir kısmı alındı; Climate TRACE kayıtlarıyla ad ve koordinat ",
               "yakınlığıyla eşleştirildi; il ve İBBS-2 bölgesi koordinatlardan ",
               "türetildi.")
      ),

      attribution(
        "Ember — ulusal üretim ve şebeke yoğunluğu",
        "Ember (Ember Energy Research CIC)",
        "https://ember-energy.org/data/yearly-electricity-data/",
        paste0("Türkiye'ye ve 2000 sonrasına filtrelendi; yakıt bazlı üretim ve ",
               "enerji sektörü CO₂ yoğunluğu serileriyle sınırlandı; toplamlar ",
               "yakıt satırlarından yeniden hesaplandı; Climate TRACE değerleriyle ",
               "karşılaştırmak için birleştirildi.")
      ),

      tags$h4("Natural Earth — idari sınırlar"),
      tags$p("Made with Natural Earth — ",
             tags$a(href = "https://www.naturalearthdata.com", target = "_blank",
                    rel = "noopener", "https://www.naturalearthdata.com"),
             ". ", tags$b("Kamu malı"), "."),

      # §5 warranty disclaimer and §2(5)(c) no-endorsement, stated once for all
      # three CC BY sources.
      tags$p(style = "font-size:12px; color:var(--ink-2);",
             tags$b("Garanti reddi ve onay yokluğu: "),
             "Kaynak veriler lisanslarının 5. maddesi uyarınca \"olduğu gibi\" ",
             "ve hiçbir garanti olmaksızın sunulur. Climate TRACE, Global Energy ",
             "Monitor ve Ember bu projeyi onaylamamaktadır; böyle bir onay ",
             "ima edilmemelidir."),

      tags$h4("Esri — altlık harita"),
      tags$p("Dark Gray Canvas. ", HTML(ESRI_ATTRIB), "."),
      tags$p(
        style = "font-size:12px; color:var(--ink-2);",
        tags$b("Neden CartoDB değil: "),
        "CARTO altlık haritaları için artık API anahtarı istiyor ve anahtarsız ",
        "kullanımda karoların üstüne çapraz olarak ",
        tags$code("API KEY REQUIRED"),
        " filigranı basıyor. Bu, hem koyu (DarkMatter) hem açık (Positron) ",
        "varyantı için geçerli. Kusur 2026-09-01'de, uygulama ilk kez tarayıcıda ",
        "açılıp bakıldığında görüldü."
      ),

      tags$p(style = "font-size:12px; color:var(--ink-2);",
             "Atıf, CC BY 4.0'ın ", tags$b("hukuki koşuludur"),
             "; nezaket değildir. Hiçbir kaynak bu projeyi onaylamamaktadır."),

      tags$hr(),

      tags$h4("Kapsam dışı kalanlar"),
      tags$ul(
        tags$li(tags$b("Gübre: "),
                "Climate TRACE'te gübre üretimi alt sektörü yoktur. Benzer adlı ",
                tags$code("synthetic-fertilizer-application"),
                " tarımsal topraklardan salınan N₂O'yu kapsar — farklı bir ",
                "emisyon kaynağıdır ve SKDM'nin düzenlediği şey değildir."),
        tags$li(tags$b("Alüminyum PFC: "),
                "SKDM alüminyumda CO₂ ile birlikte PFC'leri de kapsar. PFC ülke ",
                "paketi yayımlanmadığı için alüminyum maruziyeti eksik tahmin ",
                "edilmektedir. Boşluk doldurulmamış, işaretlenmiştir."),
        tags$li(tags$b("Yenilenebilir santraller: "),
                "Climate TRACE'in elektrik kütüğünde hidro, rüzgâr, güneş, ",
                "jeotermal ve nükleer yoktur. Bu haritadaki enerji varlıkları ",
                "yalnızca yanma tesisleri, kömür ocakları ve petrol-gaz ",
                "tesisleridir."),
        tags$li(tags$b("Elektrik ve hidrojen: "),
                "SKDM'nin altı mal grubundan ikisi kapsam dışıdır. Elektrik ",
                tags$i("üretim varlıkları"), " kapsamdadır; elektrik ",
                tags$i("ithal bir SKDM malı"), " olarak kapsam dışıdır.")
      ),

      tags$hr(),

      tags$h4("Lisanslar"),
      tags$ul(
        tags$li("Kaynak kod: MIT"),
        tags$li("Belgeler ve türetilmiş veri: CC BY 4.0"),
        tags$li("Kaynak veriler kendi lisanslarını korur")
      ),

      tags$p(style = "margin-top:16px; font-size:12px; color:var(--ink-2);",
             "Tam atıf zinciri, sürüm etiketleri ve SHA-256 özetleri için: ",
             tags$code("data/processed/SOURCES.md"), ", ",
             tags$code("SOURCES_GEM.md"), ", ",
             tags$code("SOURCES_EMBER.md"))
    )
  })
}

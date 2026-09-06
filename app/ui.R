# =============================================================================
# ui.R — interface definition
# -----------------------------------------------------------------------------
# Turkish labels throughout; English code and identifiers (KARBON_ATLASI.md §2).
#
# SCOPE: both populations on one map — 88 industrial installations and 212
# energy assets — over 2021–2026, driven by the time slider.
#
# The slider sits ABOVE every other control and outside the collapsible block,
# because KARBON_ATLASI.md §5 names it the most important control element. It is
# the only input that changes what the numbers mean rather than which subset is
# shown.
# =============================================================================

dashboardPage(
  skin = "black",

  # --- HEADER ---------------------------------------------------------------
  dashboardHeader(
    title = "Türkiye Karbon Atlası",
    titleWidth = 300
  ),

  # --- SIDEBAR --------------------------------------------------------------
  dashboardSidebar(
    width = 300,

    sidebarMenu(
      id = "tab",
      # Two time axes, and the labels say which is which. The map tab answers
      # "who emits how much" over 2021–2026; the fleet tab answers "how did the
      # fleet get here" over 2000–2026. Each carries its own slider so neither
      # ever shows an empty range (decision 6b).
      menuItem("Tesis Haritası · 2021–26", tabName = "map",
               icon = icon("map-location-dot")),
      menuItem("Filo Gelişimi · 2000–26",  tabName = "fleet",
               icon = icon("chart-area")),
      menuItem("Şebeke Yoğunluğu", tabName = "grid",    icon = icon("bolt")),
      menuItem("Veri Kaynakları",  tabName = "sources", icon = icon("book"))
    ),

    tags$hr(style = "border-color: #4b5c66; margin: 10px 15px;"),

    # --- THE TIME SLIDER ------------------------------------------------------
    # First control, deliberately. Everything below it selects a subset; this
    # selects a year, and the year changes what every figure on the page means.
    tags$div(
      style = "padding: 0 15px 4px 15px;",

      if (length(PANEL_YEARS) > 0) {
        tagList(
          sliderInput(
            inputId = "year",
            label   = tags$span(style = "font-size:14px;", "Yıl"),
            min     = min(PANEL_YEARS),
            max     = max(PANEL_YEARS),
            value   = DEFAULT_YEAR,
            step    = 1,
            sep     = "",            # 2024, not 2,024
            ticks   = FALSE,
            width   = "100%",
            animate = animationOptions(interval = 1400, loop = FALSE)
          ),
          # Rendered server-side because what needs saying depends on the year:
          # whether it is observed or a projection, and how many months it holds
          # for each population.
          uiOutput("year_status")
        )
      } else {
        tags$div(
          style = paste0("padding:10px; background:#3c4b52; font-size:12px;",
                         " border-left:3px solid #E8A33D; color:#c8d0d4;"),
          tags$b("Zaman serisi yok."), " Emisyon paneli üretilmemiş. ",
          "Çalıştırın: ", tags$code("Rscript scripts/03_build_panel.R")
        )
      }
    ),

    tags$hr(style = "border-color: #4b5c66; margin: 6px 15px 10px 15px;"),

    tags$div(
      style = "padding: 0 15px;",

      # asset_class is the first thing every filter, colour and aggregate keys
      # on (§6), so it is the first control the user meets.
      checkboxGroupInput(
        inputId  = "asset_classes",
        label    = "Varlık sınıfı",
        choices  = setNames(names(ASSET_CLASS_LABELS), unname(ASSET_CLASS_LABELS)),
        selected = names(ASSET_CLASS_LABELS)
      ),

      selectizeInput(
        inputId  = "sectors",
        label    = "Sektör",
        choices  = SECTOR_CHOICES_BY_CLASS,
        selected = ALL_SECTORS,
        multiple = TRUE,
        options  = list(placeholder = "Tüm sektörler")
      ),

      selectizeInput(
        inputId  = "provinces",
        label    = "İl",
        choices  = PROVINCE_CHOICES,
        selected = NULL,
        multiple = TRUE,
        options  = list(placeholder = "Tüm iller")
      ),

      tags$hr(style = "border-color: #4b5c66;"),

      # The renewable fleet is context, not part of the modelled population, so
      # it is off by default and labelled as a separate register.
      checkboxInput(
        inputId = "show_fleet",
        label   = "Yenilenebilir filoyu göster (GEM, emisyon dışı)",
        value   = FALSE
      ),

      checkboxInput(
        inputId = "flag_uncertain",
        label   = "Konum ataması belirsiz olanları vurgula",
        value   = FALSE
      ),

      checkboxInput(
        inputId = "only_captive",
        label   = "Yalnızca kendi tesisini besleyen santraller",
        value   = FALSE
      )
    )
  ),

  # --- BODY -----------------------------------------------------------------
  dashboardBody(

    # charset must be declared explicitly or Turkish characters can render as
    # mojibake regardless of the file's own encoding (§2).
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .small-box h3 { font-size: 26px; }
        .estimate-note {
          background: #FFF8E1; border-left: 4px solid #E8A33D;
          padding: 10px 14px; margin-bottom: 12px; font-size: 13px;
        }
        .basis-note {
          background: #EAF2FA; border-left: 4px solid #2166AC;
          padding: 10px 14px; margin-bottom: 12px; font-size: 13px;
        }
        /* A projection must never look like an observation (§5). The badge is
           deliberately loud: it is easier to ignore a colour than a word. */
        .projection-note {
          background: #F3E5F5; border-left: 4px solid #7B1FA2;
          padding: 10px 14px; margin-bottom: 12px; font-size: 13px;
        }
        .partial-note {
          background: #FFF3E0; border-left: 4px solid #E8A33D;
          padding: 10px 14px; margin-bottom: 12px; font-size: 13px;
        }
        .yr-badge {
          display: inline-block; padding: 1px 7px; border-radius: 3px;
          font-size: 11px; font-weight: 600; letter-spacing: .3px;
        }
        /* Dark basemap. The container colour matters while tiles load and
           wherever they fail: a light gap on a dark map reads as a rendering
           fault, and on a slow connection it is the first thing seen. */
        .leaflet-container { background: #0E1418; }
        /* Leaflet's legend and popups default to white panels, which punch
           bright holes in a dark map. Restyled to sit on it instead. */
        .leaflet-control .legend,
        .leaflet-bottom .info.legend {
          background: rgba(14,20,24,0.88) !important;
          color: #DDE3E7 !important;
          border: 1px solid #2A3339 !important;
          border-radius: 3px;
        }
        .leaflet-popup-content-wrapper, .leaflet-popup-tip {
          background: #161D22; color: #DDE3E7;
          box-shadow: 0 2px 12px rgba(0,0,0,.5);
        }
        .leaflet-popup-content-wrapper a { color: #4CC9F0; }
        .leaflet-tooltip {
          background: rgba(14,20,24,0.92); color: #DDE3E7;
          border: 1px solid #2A3339;
        }
        .leaflet-tooltip-left:before { border-left-color: #2A3339; }
        .leaflet-tooltip-right:before { border-right-color: #2A3339; }
        table.kv td { padding: 4px 10px 4px 0; font-size: 13px; }
      "))
    ),

    tabItems(

      # ---- TAB: MAP --------------------------------------------------------
      tabItem(
        tabName = "map",

        # Every emissions figure in this project is a modelled estimate, and the
        # interface has to say so where the user can see it, not in a footnote
        # (§8.3).
        tags$div(
          class = "estimate-note",
          tags$strong("Bu veriler modellenmiş tahmindir."),
          " Tesis düzeyinde doğrulanmış sera gazı raporları Türkiye'de kamuya",
          " açık değildir. Konumlar ve tesis bilgileri Climate TRACE",
          " kestirimlerine dayanır ve ölçüm değildir."
        ),

        # The single most misreadable thing on this page is a total spanning
        # both populations, so the interface refuses the idea before it forms.
        tags$div(
          class = "basis-note",
          tags$strong("İki popülasyon, iki gaz tabanı."),
          " Sanayi tesisleri CO₂ cinsinden (SKDM bir CO₂ aracıdır); enerji",
          " varlıkları CO₂e cinsinden, çünkü kömür ocaklarının ayak izinin",
          " yalnızca %18'i CO₂'dir. ",
          tags$b("Bu iki taban toplanmaz.")
        ),

        # Fires only when the selected year is a projection or a partial year.
        # Silent otherwise — a warning shown on every screen stops being read.
        uiOutput("year_warning"),

        fluidRow(
          valueBoxOutput("box_total",      width = 3),
          valueBoxOutput("box_industrial", width = 3),
          valueBoxOutput("box_energy",     width = 3),
          valueBoxOutput("box_year",       width = 3)
        ),

        fluidRow(
          column(
            width = 8,
            box(
              width = NULL, status = "primary", solidHeader = TRUE,
              title = textOutput("map_title", inline = TRUE),
              leafletOutput("map", height = 620)
            )
          ),
          column(
            width = 4,
            box(
              width = NULL, status = "warning", solidHeader = TRUE,
              title = "Seçili Tesis",
              uiOutput("facility_detail")
            ),
            # The series a single year cannot show. Solid where observed,
            # dashed where projected — the same distinction the markers make.
            box(
              width = NULL, status = "primary", solidHeader = TRUE,
              title = "Emisyon Serisi — Seçili Kapsam",
              plotOutput("trend_plot", height = 220),
              uiOutput("trend_note")
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              title = "Konum Ataması Güvenilirliği",
              collapsible = TRUE, collapsed = TRUE,
              uiOutput("geocode_summary")
            ),
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              title = "Devreye Giriş Yılı Kapsaması",
              collapsible = TRUE, collapsed = TRUE,
              uiOutput("commissioning_summary")
            ),
            box(
              width = NULL, status = "primary", solidHeader = TRUE,
              title = "Yenilenebilir Filo — Bağlam Katmanı",
              collapsible = TRUE, collapsed = TRUE,
              uiOutput("fleet_summary")
            )
          )
        )
      ),

      # ---- TAB: FLEET DEVELOPMENT ------------------------------------------
      # The 2000–2026 axis. Its own slider, because the emissions panel starts
      # in 2021 and a shared control would leave two decades of empty map.
      tabItem(
        tabName = "fleet",

        tags$div(
          class = "basis-note",
          tags$strong("Bu eğri kümülatif devreye alıştır, çalışan filo değildir."),
          " Global Energy Monitor 4.174 Türkiye kaydının yalnızca ikisinde ",
          "kapanış yılı veriyor, dolayısıyla 2015'te kapanmış bir santral bu ",
          "eğride sonsuza kadar durur. Ember'in gerçek kurulu gücü referans ",
          "çizgisi olarak yanında duruyor: ",
          tags$b("aradaki fark, eksik kapsama eksi modellenmemiş emekliliktir"),
          " ve ikisi ters yönde çalıştığı için birine atfedilemez."
        ),

        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Yıl",
            if (length(TIMELINE_YEARS) > 0) {
              tagList(
                sliderInput(
                  inputId = "fleet_year",
                  label   = NULL,
                  min     = min(TIMELINE_YEARS),
                  max     = max(TIMELINE_YEARS),
                  value   = TIMELINE_DEFAULT,
                  step    = 1, sep = "", ticks = FALSE, width = "100%",
                  animate = animationOptions(interval = 900, loop = FALSE)
                ),
                uiOutput("fleet_year_status")
              )
            } else {
              tags$div(
                class = "estimate-note",
                tags$b("Filo serisi üretilmemiş."), " Çalıştırın: ",
                tags$code("Rscript scripts/04_build_fleet_timeline.R")
              )
            }
          )
        ),

        fluidRow(
          valueBoxOutput("fbox_combustion", width = 3),
          valueBoxOutput("fbox_renewable",  width = 3),
          valueBoxOutput("fbox_share",      width = 3),
          valueBoxOutput("fbox_undated",    width = 3)
        ),

        fluidRow(
          column(
            width = 7,
            box(
              width = NULL, status = "primary", solidHeader = TRUE,
              title = textOutput("fleet_map_title", inline = TRUE),
              leafletOutput("fleet_map", height = 470)
            )
          ),
          column(
            width = 5,
            box(
              width = NULL, status = "primary", solidHeader = TRUE,
              title = "Kümülatif Kurulu Güç ve Tesis Sayısı",
              # Two views of the same series. Cumulative answers "how big is the
              # fleet"; annual additions answer "when did policy bite", and the
              # second is far more legible for the 2020–24 collapse in
              # combustion. Neither replaces the other.
              radioButtons(
                inputId = "fleet_chart_mode", label = NULL, inline = TRUE,
                choices = c("Kümülatif" = "cum", "Yıllık ekleme" = "add"),
                selected = "cum"
              ),
              plotOutput("fleet_plot", height = 300),
              checkboxInput("show_policy", "Politika olaylarını göster",
                            value = TRUE),
              uiOutput("fleet_plot_note")
            ),
            box(
              width = NULL, status = "danger", solidHeader = TRUE,
              title = "Politika Takvimi",
              collapsible = TRUE,
              uiOutput("policy_panel")
            ),
            box(
              width = NULL, status = "warning", solidHeader = TRUE,
              title = "Kapsama — bu eğriler ne kadarını görüyor",
              collapsible = TRUE,
              uiOutput("fleet_coverage_panel")
            )
          )
        )
      ),

      # ---- TAB: GRID INTENSITY ---------------------------------------------
      # The disagreement between estimates is itself a result (§7), so it gets a
      # tab rather than a footnote.
      tabItem(
        tabName = "grid",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Şebeke Karbon Yoğunluğu — Üç Bağımsız Tahmin",
            uiOutput("grid_panel")
          )
        )
      ),

      # ---- TAB: SOURCES ----------------------------------------------------
      # Climate TRACE, GEM and Ember are all CC BY 4.0 — attribution is a
      # licence condition, so this tab is required, not decorative (§10).
      tabItem(
        tabName = "sources",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Veri Kaynakları ve Atıf",
            uiOutput("sources_panel")
          )
        )
      )
    )
  )
)

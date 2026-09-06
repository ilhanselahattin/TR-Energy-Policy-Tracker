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

    tags$hr(style = "border-color: var(--rule); margin: 10px 15px;"),

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
          style = paste0("padding:10px; background:var(--surface-2); font-size:12px;",
                         " border-left:3px solid var(--warn); color:var(--ink-soft);"),
          tags$b("Zaman serisi yok."), " Emisyon paneli üretilmemiş. ",
          "Çalıştırın: ", tags$code("Rscript scripts/03_build_panel.R")
        )
      }
    ),

    tags$hr(style = "border-color: var(--rule); margin: 6px 15px 10px 15px;"),

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

      tags$hr(style = "border-color: var(--rule);"),

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
      ),

      # Ownership filter. "Kesin" restricts to chains whose final link to the
      # plant is quantified — the filter that drops the ITC name collision.
      selectInput(
        inputId  = "ownership_filter",
        label    = "Sahiplik",
        choices  = c("Tümü"                          = "all",
                     "AB27 merkezli sahibi olanlar"  = "eu",
                     "AB27 — zinciri kesin olanlar"  = "eu_certain",
                     "Yabancı sahibi olanlar"        = "foreign",
                     "Yabancı — zinciri kesin"       = "foreign_certain",
                     "Sahiplik kaydı olmayanlar"     = "unknown"),
        selected = "all"
      ),

      selectInput(
        inputId  = "route_filter",
        label    = "Proses rotası",
        choices  = c("Tümü" = "all",
                     setNames(names(ROUTE_LABELS), unname(ROUTE_LABELS)),
                     "Rotası bilinmeyen" = "none"),
        selected = "all"
      )
    )
  ),

  # --- BODY -----------------------------------------------------------------
  dashboardBody(

    # charset must be declared explicitly or Turkish characters can render as
    # mojibake regardless of the file's own encoding (§2).
    tags$head(
      tags$meta(charset = "UTF-8"),

      # The document language must be Turkish, and this is not metadata hygiene
      # — it changes what the page renders. atlas.css sets `text-transform:
      # uppercase` on every label, and CSS casing is language-dependent: under
      # lang="en" the browser maps i → I, so "İşletmeci" comes out "IŞLETMECI"
      # and "İl" comes out "IL". Under lang="tr" it maps i → İ and the labels
      # read TESİS HARİTASI, FİLO GELİŞİMİ, VERİ KAYNAKLARI as they should.
      # Verified in Chrome by rendering the sidebar under both languages.
      #
      # It is set from a script because shinydashboard::dashboardPage() takes no
      # `lang` argument and emits the <html> tag itself. The script sits in
      # <head>, so it runs before the body is parsed and there is no flash of
      # wrongly-cased text.
      tags$script(HTML("document.documentElement.lang = 'tr';")),

      # IBM Plex Sans / Mono. One superfamily drawn for technical documentation,
      # carrying prose and instrumentation respectively. Loaded from Google
      # Fonts; every rule in atlas.css declares a system fallback stack, so the
      # app degrades to Segoe UI / Consolas offline rather than breaking.
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(
        rel  = "stylesheet",
        href = paste0(
          "https://fonts.googleapis.com/css2",
          "?family=IBM+Plex+Mono:wght@400;500;600",
          "&family=IBM+Plex+Sans:wght@400;500;600;700",
          "&display=swap"
        )
      ),

      # Everything visual lives in one stylesheet rather than inline here, so
      # the palette can be read, reviewed and changed in one place.
      tags$link(rel = "stylesheet", type = "text/css", href = "atlas.css")
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

        # Written as a div rather than fluidRow() only so the row can carry a
        # class. atlas.css turns it into a flex row above 1200px, which is what
        # makes the sticky map below possible: Bootstrap 3 columns are floated,
        # so the left column would otherwise be exactly as tall as the map and a
        # sticky element inside it would have no distance to travel.
        tags$div(
          class = "row sticky-row",
          column(
            width = 8,
            # The map holds its position while the right-hand column scrolls.
            # Selecting a facility fills that column with the detail panel, the
            # series, ownership and the geocode note, which is far taller than
            # the map — so reading any of it used to scroll the map off the
            # screen, and the user lost the point they had just clicked.
            # Disabled below 1200px, where the columns stack and there is
            # nothing beside the map to scroll against.
            tags$div(
              class = "sticky-map",
              box(
                width = NULL, status = "primary", solidHeader = TRUE,
                title = textOutput("map_title", inline = TRUE),
                leafletOutput("map", height = 620)
              )
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
              width = NULL, status = "danger", solidHeader = TRUE,
              title = "Sahiplik — Karbon Kimin Elinde",
              collapsible = TRUE, collapsed = TRUE,
              uiOutput("ownership_panel")
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

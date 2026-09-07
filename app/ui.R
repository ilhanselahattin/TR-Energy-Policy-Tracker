# =============================================================================
# ui.R — interface definition
# -----------------------------------------------------------------------------
# Turkish labels throughout; English code and identifiers (KARBON_ATLASI.md §2).
#
# SCOPE: both populations on one map — 88 industrial installations and 212
# energy assets — over 2021–2026, driven by the time slider.
#
# LAYOUT DECISIONS, AND WHAT THEY COST TO REVERSE
#
# The previous version put 13 separate surfaces on the map tab and spent 359px
# of a 900px viewport before the first pixel of the map: two standing notices,
# four value boxes, then the map. Measured, not estimated. Three things changed.
#
#   The four value boxes became one readout strip. Same four numbers, one 44px
#   line instead of four 110px blocks, and the numbers stopped competing with
#   the map for saturation.
#
#   The standing notices became folded notes. The sentence that must be read
#   stays on screen — an emissions figure not marked as modelled is a false
#   claim (§8.3) — and its reasoning moves one click away rather than being
#   deleted. The fleet tab carried 835 words this way; that is an essay, not a
#   caption, and none of it could honestly be cut.
#
#   The filters became conditional. Fourteen inputs were rendered on all four
#   tabs; the grid and sources tabs consume none of them.
#
# The slider stays above every other control and outside the folded block,
# because §5 names it the most important control element: it is the only input
# that changes what the numbers mean rather than which subset is shown.
# =============================================================================

dashboardPage(
  skin = "black",

  # --- HEADER ---------------------------------------------------------------
  # Lowercased and closed with a full stop by CSS — the poster's device, and the
  # only large type in the interface.
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

    # --- THE TIME SLIDER ------------------------------------------------------
    # Map tab only. The fleet tab runs 2000–2026 and carries its own control;
    # showing both at once invited the reader to think one drove the other.
    conditionalPanel(
      condition = "input.tab == 'map'",
      tags$hr(),
      tags$div(
        style = "padding: 0 18px 4px;",
        if (length(PANEL_YEARS) > 0) {
          tagList(
            sliderInput(
              inputId = "year",
              label   = "Yıl",
              min     = min(PANEL_YEARS),
              max     = max(PANEL_YEARS),
              value   = DEFAULT_YEAR,
              step    = 1,
              sep     = "",            # 2024, not 2,024
              ticks   = FALSE,
              width   = "100%",
              animate = animationOptions(interval = 1400, loop = FALSE)
            ),
            # Rendered server-side because what needs saying depends on the
            # year: whether it is observed or a projection, and how many months
            # it holds for each population.
            uiOutput("year_status")
          )
        } else {
          tags$div(
            class = "fold-body",
            tags$b("Zaman serisi yok."), " Emisyon paneli üretilmemiş. ",
            "Çalıştırın: ", tags$code("Rscript scripts/03_build_panel.R")
          )
        }
      )
    ),

    # --- FILTERS --------------------------------------------------------------
    # Only the two tabs that read them. The grid and sources tabs consume no
    # filter, and rendering fourteen inert controls beside them said otherwise.
    conditionalPanel(
      condition = "input.tab == 'map' || input.tab == 'fleet'",
      tags$hr(),
      tags$div(
        style = "padding: 0 18px;",

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
        ),

        tags$hr(),

        # The renewable fleet is context, not part of the modelled population,
        # so it is off by default and labelled as a separate register.
        checkboxInput("show_fleet",
                      "Yenilenebilir filoyu göster (GEM, emisyon dışı)", FALSE),
        checkboxInput("flag_uncertain",
                      "Konum ataması belirsiz olanları vurgula", FALSE),
        checkboxInput("only_captive",
                      "Yalnızca kendi tesisini besleyen santraller", FALSE)
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
      # — it changes what the page renders. atlas.css sets `text-transform` on
      # every label, and CSS casing is language-dependent: under lang="en" the
      # browser maps i to I, so "İşletmeci" comes out "ISLETMECI" and the
      # header's lowercased title mangles in the other direction. Under
      # lang="tr" both are correct. Verified in Chrome under both languages.
      #
      # Set from a script because shinydashboard::dashboardPage() takes no
      # `lang` argument and emits the <html> tag itself. It sits in <head>, so
      # it runs before the body is parsed and there is no flash.
      tags$script(HTML("document.documentElement.lang = 'tr';")),

      # Two families, one job each. Inter Tight is the poster layer — the
      # header, and nothing else. IBM Plex Mono carries every label, figure and
      # sentence, which is what makes the page read as instrumentation rather
      # than as a document about instrumentation.
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(
        rel  = "stylesheet",
        href = paste0(
          "https://fonts.googleapis.com/css2",
          "?family=IBM+Plex+Mono:wght@400;500;600",
          "&family=Inter+Tight:wght@600;700",
          "&display=swap"
        )
      ),

      tags$link(rel = "stylesheet", type = "text/css", href = "atlas.css")
    ),

    tabItems(

      # ---- TAB: MAP --------------------------------------------------------
      tabItem(
        tabName = "map",

        # Both standing warnings survive, folded. The head of each is the claim
        # that cannot be left off the screen; the body is the reasoning.
        fold_note(
          tagList(tags$b("Bu veriler modellenmiş tahmindir."),
                  " Ölçüm değildir."),
          "Tesis düzeyinde doğrulanmış sera gazı raporları Türkiye'de kamuya ",
          "açık değildir. Konumlar ve tesis bilgileri Climate TRACE ",
          "kestirimlerine dayanır."
        ),
        fold_note(
          tagList(tags$b("İki popülasyon, iki gaz tabanı."),
                  " Bu iki taban toplanmaz."),
          "Sanayi tesisleri CO₂ cinsinden (SKDM bir CO₂ aracıdır); enerji ",
          "varlıkları CO₂e cinsinden, çünkü kömür ocaklarının ayak izinin ",
          "yalnızca %18'i CO₂'dir."
        ),

        # Fires only when the selected year is a projection or a partial year.
        # Silent otherwise — a warning shown on every screen stops being read.
        uiOutput("year_warning"),

        # Four value boxes, now one line.
        uiOutput("status_strip"),

        tags$div(
          class = "row sticky-row",
          column(
            width = 8,
            tags$div(
              class = "sticky-map",
              atlas_panel(
                label = textOutput("map_title", inline = TRUE), num = "01",
                tags$div(
                  class = "map-wrap",
                  leafletOutput("map", height = 640),
                  # The poster's corner tags, carrying real values.
                  tags$div(class = "map-tag tr", uiOutput("map_tag_year", inline = TRUE)),
                  tags$div(class = "map-tag bl", uiOutput("map_tag_n", inline = TRUE))
                )
              )
            )
          ),
          column(
            width = 4,
            atlas_panel(label = "Seçili Tesis", num = "02", tone = "live",
                        uiOutput("facility_detail")),
            atlas_panel(
              label = "Emisyon Serisi", num = "03",
              plotOutput("trend_plot", height = 220),
              uiOutput("trend_note")
            ),
            atlas_panel(label = "Sahiplik — Karbon Kimin Elinde", num = "04",
                        fold = TRUE, uiOutput("ownership_panel")),
            atlas_panel(label = "Konum Ataması Güvenilirliği", num = "05",
                        fold = TRUE, uiOutput("geocode_summary")),
            atlas_panel(label = "Devreye Giriş Yılı Kapsaması", num = "06",
                        fold = TRUE, uiOutput("commissioning_summary")),
            atlas_panel(label = "Yenilenebilir Filo — Bağlam", num = "07",
                        fold = TRUE, uiOutput("fleet_summary"))
          )
        )
      ),

      # ---- TAB: FLEET DEVELOPMENT ------------------------------------------
      # The 2000–2026 axis. Its own slider, because the emissions panel starts
      # in 2021 and a shared control would leave two decades of empty map.
      tabItem(
        tabName = "fleet",

        fold_note(
          tagList(tags$b("Bu eğri kümülatif devreye alıştır,"),
                  " çalışan filo değildir."),
          "Global Energy Monitor 4.174 Türkiye kaydının yalnızca ikisinde ",
          "kapanış yılı veriyor, dolayısıyla 2015'te kapanmış bir santral bu ",
          "eğride sonsuza kadar durur. Ember'in gerçek kurulu gücü referans ",
          "çizgisi olarak yanında duruyor: ",
          tags$b("aradaki fark, eksik kapsama eksi modellenmemiş emekliliktir"),
          " ve ikisi ters yönde çalıştığı için birine atfedilemez."
        ),

        if (length(TIMELINE_YEARS) > 0) {
          atlas_panel(
            label = "Yıl", num = "01",
            sliderInput(
              inputId = "fleet_year", label = NULL,
              min = min(TIMELINE_YEARS), max = max(TIMELINE_YEARS),
              value = TIMELINE_DEFAULT,
              step = 1, sep = "", ticks = FALSE, width = "100%",
              animate = animationOptions(interval = 900, loop = FALSE)
            ),
            uiOutput("fleet_year_status")
          )
        } else {
          fold_note(tagList(tags$b("Filo serisi üretilmemiş.")),
                    "Çalıştırın: ",
                    tags$code("Rscript scripts/04_build_fleet_timeline.R"))
        },

        uiOutput("fleet_strip"),

        # Sticky for the same reason as the map tab: the policy calendar and the
        # coverage panel beside this map are several times its height, and
        # scrolling to read them used to take the map off the screen.
        tags$div(
          class = "row sticky-row",
          column(
            width = 7,
            tags$div(
              class = "sticky-map",
              atlas_panel(
                label = textOutput("fleet_map_title", inline = TRUE), num = "02",
                tags$div(class = "map-wrap", leafletOutput("fleet_map", height = 520))
              )
            )
          ),
          column(
            width = 5,
            atlas_panel(
              label = "Kümülatif Kurulu Güç ve Tesis Sayısı", num = "03",
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
              checkboxInput("show_policy", "Politika olaylarını göster", TRUE),
              uiOutput("fleet_plot_note")
            ),
            atlas_panel(label = "Politika Takvimi", num = "04",
                        fold = TRUE, open = TRUE, uiOutput("policy_panel")),
            atlas_panel(label = "Kapsama — bu eğriler ne kadarını görüyor",
                        num = "05", fold = TRUE, uiOutput("fleet_coverage_panel"))
          )
        )
      ),

      # ---- TAB: GRID INTENSITY ---------------------------------------------
      # The disagreement between estimates is itself a result (§7), so it gets a
      # tab rather than a footnote.
      tabItem(
        tabName = "grid",
        atlas_panel(label = "Şebeke Karbon Yoğunluğu — Üç Bağımsız Tahmin",
                    num = "01", uiOutput("grid_panel"))
      ),

      # ---- TAB: SOURCES ----------------------------------------------------
      # Climate TRACE, GEM and Ember are all CC BY 4.0 — attribution is a
      # licence condition, so this tab is required, not decorative (§10).
      tabItem(
        tabName = "sources",
        atlas_panel(label = "Veri Kaynakları ve Atıf", num = "01",
                    uiOutput("sources_panel"))
      )
    )
  )
)

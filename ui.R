
library(shiny)
library(dplyr)
library(plotly)
library(bslib)

app_header <- function() {
  tags$div(
    class = "app-header",
    tags$div(
      class = "app-header-main",
      tags$div(
        tags$div(class = "app-kicker", "Svensk prisstatistik"),
        tags$h1(class = "app-title", "KPI Voyager"),
        tags$p(
          class = "app-subtitle",
          "Analysera svenskt Konsumentprisindex, månads- och årsförändringar, bidrag enligt SCB-länkberäkning, säsongsmönster och vikter."
        )
      ),
      tags$div(
        class = "app-header-badges",
        tags$span(class = "app-version-pill", "KPI-version"),
        tags$span(class = "app-meta-pill", "Land: Sverige"),
        tags$span(class = "app-meta-pill", "Källa: lokala SCB-filer")
      )
    )
  )
}

control_group <- function(title, ..., note = NULL) {
  tags$div(
    class = "control-group",
    tags$div(class = "control-group-title", title),
    ...,
    if (!is.null(note)) tags$p(class = "control-note", note)
  )
}

chart_section <- function(title, subtitle, controls, plot_id, badges = character(0), height = "560px") {
  tags$div(
    class = "chart-section",
    tags$div(
      class = "chart-hero",
      tags$div(
        tags$h2(title),
        tags$p(subtitle),
        if (length(badges) > 0) {
          tags$div(
            class = "tab-badges",
            lapply(badges, function(x) tags$span(class = "tab-badge", x))
          )
        }
      )
    ),
    fluidRow(
      column(
        width = 3,
        tags$div(class = "control-card", controls)
      ),
      column(
        width = 9,
        tags$div(class = "plot-card", plotlyOutput(plot_id, height = height))
      )
    )
  )
}

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),

  tags$style(HTML("        
    body {
      background: #fbfdfc;
      color: #263238;
    }

    .app-header {
      max-width: 1250px;
      margin: 14px auto 14px auto;
      padding: 24px 28px;
      background: linear-gradient(135deg, #f7fbf9 0%, #eef7f4 100%);
      border: 1px solid #dcebe6;
      border-radius: 22px;
      box-shadow: 0 3px 14px rgba(0,0,0,0.035);
      color: #263238;
    }

    .app-header-main {
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 24px;
      flex-wrap: wrap;
    }

    .app-kicker {
      font-size: 12px;
      font-weight: 700;
      color: #3f8f7a;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      margin-bottom: 5px;
    }

    .app-title {
      margin: 0;
      font-size: 32px;
      font-weight: 800;
      letter-spacing: -0.03em;
      color: #20322f;
    }

    .app-subtitle {
      margin-top: 7px;
      margin-bottom: 0;
      max-width: 780px;
      font-size: 15px;
      line-height: 1.55;
      color: #526064;
    }

    .app-header-badges {
      display: flex;
      gap: 8px;
      flex-wrap: wrap;
      justify-content: flex-end;
    }

    .app-version-pill,
    .app-meta-pill {
      display: inline-block;
      border-radius: 999px;
      padding: 6px 12px;
      font-size: 12px;
      font-weight: 700;
      white-space: nowrap;
    }

    .app-version-pill {
      background: #3f8f7a;
      color: white;
      border: 1px solid #337968;
    }

    .app-meta-pill {
      background: #ffffff;
      color: #34514b;
      border: 1px solid #d7e7e2;
    }

    .navbar {
      max-width: 1250px;
      margin: 0 auto 18px auto !important;
      padding: 8px 10px !important;
      background: rgba(255,255,255,0.96) !important;
      border: 1px solid #e1e7e5 !important;
      border-radius: 18px !important;
      box-shadow: 0 2px 10px rgba(0,0,0,0.04);
      min-height: unset !important;
    }

    .navbar-brand {
      padding-top: 4px !important;
      padding-bottom: 4px !important;
      margin-right: 18px !important;
      font-weight: 800 !important;
      color: #20322f !important;
    }

    .navbar-brand-custom {
      display: inline-flex;
      align-items: center;
      gap: 8px;
    }

    .navbar-brand-mark {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      width: 26px;
      height: 26px;
      border-radius: 8px;
      background: #3f8f7a;
      color: white;
      font-size: 14px;
      font-weight: 800;
    }

    .navbar-nav .nav-link,
    .navbar-nav > li > a {
      border-radius: 999px !important;
      padding: 8px 14px !important;
      margin: 2px 3px !important;
      color: #3f4a4d !important;
      font-size: 14px !important;
      font-weight: 650 !important;
      border: 1px solid transparent !important;
      transition: all 0.15s ease-in-out;
    }

    .navbar-nav .nav-link:hover,
    .navbar-nav > li > a:hover {
      background: #eef7f4 !important;
      color: #24584b !important;
      border-color: #d2e7df !important;
    }

    .navbar-nav .nav-link.active,
    .navbar-nav > li.active > a,
    .navbar-nav > .active > a,
    .navbar-nav > .show > a {
      background: #3f8f7a !important;
      color: white !important;
      border-color: #337968 !important;
      box-shadow: 0 2px 6px rgba(63,143,122,0.25);
    }

    .app-tab-page {
      max-width: 1250px;
      margin: 0 auto;
      padding: 18px 8px 40px 8px;
      color: #263238;
    }

    .global-control-card {
      background: #ffffff;
      border: 1px solid #e1e7e5;
      border-radius: 18px;
      padding: 18px 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.035);
      margin-bottom: 18px;
    }

    .global-control-card h2 {
      margin-top: 0;
      margin-bottom: 6px;
      font-size: 20px;
      font-weight: 750;
    }

    .global-control-card p {
      margin-top: 0;
      color: #526064;
      font-size: 13.5px;
      line-height: 1.5;
    }

    .chart-section {
      background: #ffffff;
      border: 1px solid #e1e7e5;
      border-radius: 20px;
      padding: 18px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.035);
      margin-bottom: 22px;
    }

    .chart-hero {
      background: linear-gradient(135deg, #f7fbf9 0%, #eef7f4 100%);
      border: 1px solid #dcebe6;
      border-radius: 16px;
      padding: 16px 20px;
      margin-bottom: 16px;
    }

    .chart-hero h2 {
      margin-top: 0;
      margin-bottom: 6px;
      font-size: 21px;
      font-weight: 750;
    }

    .chart-hero p {
      margin-bottom: 0;
      font-size: 13.5px;
      line-height: 1.55;
      max-width: 860px;
    }

    .tab-badges {
      margin-top: 11px;
    }

    .tab-badge {
      display: inline-block;
      background: #e8f4f0;
      color: #24584b;
      border: 1px solid #cfe6dd;
      border-radius: 999px;
      padding: 4px 10px;
      font-size: 12px;
      font-weight: 600;
      margin-right: 6px;
      margin-bottom: 5px;
    }

    .control-card {
      background: #ffffff;
      border: 1px solid #edf1f0;
      border-radius: 16px;
      padding: 15px 16px;
      box-shadow: none;
      margin-bottom: 16px;
    }

    .control-card h3 {
      margin-top: 0;
      margin-bottom: 12px;
      font-size: 17px;
      font-weight: 700;
    }

    .control-group {
      border-top: 1px solid #edf1f0;
      padding-top: 13px;
      margin-top: 13px;
    }

    .control-group:first-of-type {
      border-top: none;
      padding-top: 0;
      margin-top: 0;
    }

    .control-group-title {
      font-size: 13px;
      font-weight: 700;
      color: #34514b;
      margin-bottom: 8px;
      text-transform: uppercase;
      letter-spacing: 0.03em;
    }

    .control-note {
      color: #687579;
      font-size: 12.5px;
      line-height: 1.45;
      margin-top: 6px;
      margin-bottom: 0;
    }

    .plot-card {
      background: transparent;
      border: none;
      border-radius: 0;
      box-shadow: none;
      padding: 0;
      min-height: 0;
      overflow: visible;
    }

    .help-page {
      max-width: 1100px;
      margin: 0 auto;
      padding: 22px 8px 45px 8px;
      color: #263238;
    }

    .help-card {
      background: #ffffff;
      border: 1px solid #e1e7e5;
      border-radius: 16px;
      padding: 18px 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.035);
      margin-bottom: 16px;
    }

    .help-card h2,
    .help-card h3 {
      margin-top: 0;
      font-weight: 750;
    }

    .help-card p,
    .help-card li {
      font-size: 14px;
      line-height: 1.55;
    }

    .form-group {
      margin-bottom: 12px;
    }

    @media (max-width: 768px) {
      .app-header {
        margin: 10px 8px 12px 8px;
        padding: 20px 18px;
      }

      .app-title {
        font-size: 26px;
      }

      .app-header-badges {
        justify-content: flex-start;
      }

      .navbar {
        margin-left: 8px !important;
        margin-right: 8px !important;
      }
    }
  ")),

  app_header(),

  navbarPage(
    title = tags$span(
      class = "navbar-brand-custom",
      tags$span(class = "navbar-brand-mark", "K"),
      tags$span("KPI Voyager")
    ),
    id = "main_tabs",
    collapsible = TRUE,

    tabPanel(
      "Dashboard",
      tags$div(
        class = "app-tab-page",

        tags$div(
          class = "global-control-card",
          tags$h2("Gemensamt aggregat"),
          tags$p("Landet är fast till Sverige. Välj om dashboarden ska styras av vanlig COICOP-hierarki eller av ett specialaggregat."),
          radioButtons(
            "aggregate_kind",
            NULL,
            choices = c("COICOP" = "coicop", "Specialaggregat" = "special_aggregate"),
            selected = "coicop",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.aggregate_kind == 'coicop'",
            selectInput(
              "aggregate",
              "Välj aggregat",
              choices = setNames(kpi_hierarchy$KOD, kpi_hierarchy$code_label),
              selected = total_kod,
              multiple = FALSE
            )
          ),
          conditionalPanel(
            condition = "input.aggregate_kind == 'special_aggregate'",
            selectInput(
              "special_aggregate",
              "Välj specialaggregat",
              choices = special_aggregate_select_choices,
              selected = "Tjänster exkl ägda boendet",
              multiple = FALSE
            ),
            tags$p(
              class = "control-note",
              "När specialaggregat används visas endast bidragsgraferna. Indexutveckling, säsongsmönster och vikter döljs."
            )
          )
        ),

        conditionalPanel(
          condition = "input.aggregate_kind == 'coicop'",
          chart_section(
            title = "Indexutveckling",
          subtitle = "Visar valt KPI-aggregat omräknat till vald referensperiod. Endast Sverige visas.",
          badges = c("Index", "Ombasering", "KPI"),
          plot_id = "plot_index",
          controls = tagList(
            tags$h3("Indexinställningar"),
            control_group(
              "Referensperiod",
              radioButtons(
                "index_ref_type",
                NULL,
                choices = c("Helår" = "year", "Månad" = "month"),
                selected = "year",
                inline = TRUE
              ),
              selectInput("index_ref", "Välj referensperiod", choices = NULL, multiple = FALSE),
              note = "Referensperioden sätts till 100. Serien visas från vald referensperiod och framåt."
            )
          )
        ),
        ),

        chart_section(
          title = "Bidrag till månadsförändring, M/M-1",
          subtitle = "Staplarna visar komponenternas bidrag till månadsförändringen. I COICOP-läget kan du välja presentationssätt; i specialaggregatsläget visas topp-X subklasser plus Övrigt.",
          badges = c("M/M-1", "Effekt_manad", "COICOP / specialaggregat"),
          plot_id = "plot_mr",
          controls = tagList(
            tags$h3("Månadsinställningar"),
            conditionalPanel(
              condition = "input.aggregate_kind == 'coicop'",
              control_group(
                "Bidragstyp",
                radioButtons(
                  "contribution_type_mr",
                  "Bidrag till",
                  choices = c(
                    "valt aggregat" = "selected higher aggregate",
                    "hela KPI" = "all-items KPI"
                  ),
                  selected = "selected higher aggregate"
                ),
                note = "Vid 'valt aggregat' räknas effekterna om mot det valda aggregatet. Vid 'hela KPI' används total KPI som mål."
              )
            ),
            control_group(
              "Presentationssätt",
              conditionalPanel(
                condition = "input.aggregate_kind == 'coicop'",
                radioButtons(
                  "presentation_mr",
                  NULL,
                  choices = c(
                    "Standard COICOP" = "standard_coicop",
                    "Specialaggregat" = "special_aggregates",
                    "Viktigaste grupperna" = "important_groups",
                    "Viktigaste klasserna" = "important_classes",
                    "Viktigaste subklasserna" = "important_subclasses"
                  ),
                  selected = "standard_coicop"
                )
              ),
              conditionalPanel(
                condition = "input.aggregate_kind == 'special_aggregate' || (input.aggregate_kind == 'coicop' && ['important_groups', 'important_classes', 'important_subclasses'].indexOf(input.presentation_mr) >= 0)",
                sliderInput(
                  "important_n_mr",
                  "Antal viktiga subklasser som ska visas separat",
                  min = 1,
                  max = 30,
                  value = 10,
                  step = 1,
                  ticks = FALSE
                ),
                sliderInput(
                  "important_rank_months_mr",
                  "Antal senaste månader som rankingen ska baseras på",
                  min = 1,
                  max = 24,
                  value = 3,
                  step = 1,
                  ticks = FALSE
                )
              ),
              note = "I COICOP-läget fungerar presentationssätten som tidigare. I specialaggregatsläget visas de X subklasser som har störst genomsnittligt absolutbidrag under de Y senaste valda månaderna; resterande subklasser summeras till Övrigt."
            ),
            control_group(
              "Perioder",
              selectInput("months_mr", "Välj månader", choices = NULL, multiple = TRUE)
            )
          )
        ),

        chart_section(
          title = "Bidrag till årsförändring, M/M-12",
          subtitle = "Visar bidrag över tid till årsförändringen. I COICOP-läget kan du välja presentationssätt; i specialaggregatsläget visas topp-X subklasser plus Övrigt.",
          badges = c("M/M-12", "Effekt_ar", "COICOP / specialaggregat"),
          plot_id = "plot_ar",
          height = "620px",
          controls = tagList(
            tags$h3("Årsinställningar"),
            conditionalPanel(
              condition = "input.aggregate_kind == 'coicop'",
              control_group(
                "Bidragstyp",
                radioButtons(
                  "contribution_type_ar",
                  "Bidrag till",
                  choices = c(
                    "valt aggregat" = "selected higher aggregate",
                    "hela KPI" = "all-items KPI"
                  ),
                  selected = "selected higher aggregate"
                )
              )
            ),
            control_group(
              "Presentationssätt",
              conditionalPanel(
                condition = "input.aggregate_kind == 'coicop'",
                radioButtons(
                  "presentation_ar",
                  NULL,
                  choices = c(
                    "Standard COICOP" = "standard_coicop",
                    "Specialaggregat" = "special_aggregates",
                    "Viktigaste grupperna" = "important_groups",
                    "Viktigaste klasserna" = "important_classes",
                    "Viktigaste subklasserna" = "important_subclasses"
                  ),
                  selected = "standard_coicop"
                )
              ),
              conditionalPanel(
                condition = "input.aggregate_kind == 'special_aggregate' || (input.aggregate_kind == 'coicop' && ['important_groups', 'important_classes', 'important_subclasses'].indexOf(input.presentation_ar) >= 0)",
                sliderInput(
                  "important_n_ar",
                  "Antal viktiga subklasser som ska visas separat",
                  min = 1,
                  max = 30,
                  value = 10,
                  step = 1,
                  ticks = FALSE
                ),
                sliderInput(
                  "important_rank_months_ar",
                  "Antal senaste månader som rankingen ska baseras på",
                  min = 1,
                  max = 24,
                  value = 3,
                  step = 1,
                  ticks = FALSE
                )
              ),
              note = "I COICOP-läget fungerar presentationssätten som tidigare. I specialaggregatsläget visas de X subklasser som har störst genomsnittligt absolutbidrag under de Y senaste månaderna inom valt intervall; resterande subklasser summeras till Övrigt."
            ),
            control_group(
              "Tidsintervall",
              sliderInput(
                "range_ar",
                "Välj år",
                min = 2000,
                max = 2026,
                value = c(2021, 2026),
                step = 1,
                ticks = FALSE,
                sep = ""
              )
            )
          )
        ),

        conditionalPanel(
          condition = "input.aggregate_kind == 'coicop'",
          chart_section(
            title = "Säsongsmönster",
          subtitle = "Jämför prisutvecklingen inom år. December föregående år sätts till 100.",
          badges = c("Dec Y-1 = 100", "Säsong", "Årsjämförelse"),
          plot_id = "plot_seasonality",
          controls = tagList(
            tags$h3("Säsongsinställningar"),
            control_group(
              "År",
              selectInput("years_seasonality", "Välj år", choices = NULL, multiple = TRUE),
              note = "December föregående år läggs till som gemensam startpunkt."
            )
          )
        ),
        ),

        conditionalPanel(
          condition = "input.aggregate_kind == 'coicop'",
          chart_section(
            title = "Vikter",
          subtitle = "Visar vikter för direkta undergrupper till valt aggregat. Om aggregatet saknar undergrupper visas aggregatets egen vikt.",
          badges = c("Vikter", "Per mille", "Konsumtionsstruktur"),
          plot_id = "plot_weights",
          controls = tagList(
            tags$h3("Viktinställningar"),
            control_group(
              "Tidsintervall",
              sliderInput(
                "range_weights",
                "Välj år",
                min = 2000,
                max = 2026,
                value = c(2021, 2026),
                step = 1,
                ticks = FALSE,
                sep = ""
              ),
              note = "Från och med 2004 hämtas vikten som första icke-saknade år-månadsvikt per år. Före 2004 används Korttidsvikt."
            )
          )
        ),
        )
      )
    ),

    tabPanel(
      "Help",
      tags$div(
        class = "help-page",
        tags$div(
          class = "help-card",
          tags$h2("KPI Voyager"),
          tags$p("Den här varianten är avskalad för svenska KPI-data. Landet är fast till Sverige och alla grafer ligger på samma dashboard-flik."),
          tags$ul(
            tags$li("Ingen data hämtas från Eurostat."),
            tags$li("Data läses från TAB6612_sv.csv och TAB6622_sv.csv."),
            tags$li("Bidragen beräknas med samma länkstruktur som i Testdata_prep.R."),
            tags$li("I fältet Gemensamt aggregat kan du välja vanlig COICOP-hierarki eller ett specialaggregat."),
            tags$li("I COICOP-läget fungerar dashboarden som tidigare, inklusive val mellan Standard COICOP, Specialaggregat och Viktigaste grupperna, klasserna eller subklasserna i bidragsgraferna."),
            tags$li("I specialaggregatsläget visas bara M/M-1 och M/M-12. Graferna visar de X viktigaste subklasserna i valt specialaggregat plus Övrigt."),
            tags$li("Specialaggregat summerar subklasser enligt suffixen IV, DV, V och T, men använder särskilda undantag för Livsmedel, Ägda boendet och Energi."),
            tags$li("I topp-X-lägena visas de X komponenter på vald nivå som har störst genomsnittligt absolutbidrag under de Y senaste månaderna; resten summeras till Övrigt."),
            tags$li("För specialaggregat sätts månadsförändring och årsförändring i linjeserien till summan av de underliggande subklassbidragen."),
            tags$li("Viktigaste grupperna och Viktigaste klasserna är bara valbara när valt aggregat är total KPI eller en COICOP-avdelning, till exempel 01, 02 eller 03."),
            tags$li("I bidragsgraferna används både färg och mönster i staplarna och i legendmarkörerna."),
            tags$li("Ett aggregat väljs åt gången och används för samtliga grafer."),
            tags$li("Vid bidrag till valt aggregat räknas komponentvikterna om relativt valt aggregat."),
            tags$li("Vid bidrag till hela KPI används total KPI som målaggregat.")
          )
        ),
        tags$div(
          class = "help-card",
          tags$h3("Körning"),
          tags$p("Lägg ui.R, server.R och global.R i samma Shiny-mapp. Lägg också CSV-filerna i samma mapp, eller sätt KPI_DATA_DIR innan appen startas:"),
          tags$pre("Sys.setenv(KPI_DATA_DIR = '/sökväg/till/mappen')\nshiny::runApp()")
        )
      )
    )
  )
)


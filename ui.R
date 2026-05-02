library(shiny)
library(eurostat)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(bslib)

# Read eurostat_countries dataframe

additional_areas <- data.frame(code = c("EU", "EA","BA","XK","RS","MK","AL","ME","TR","US","UK"), name = c("European union", "Euro area","Bosnia-Herzegovina", "Kosovo", "Serbia","North Macedonia","Albania","Montenegro","Turkey","United States", "United Kingdom"), label = c("European union", "Euro area","Bosnia-Herzegovina", "Kosovo", "Serbia", "North Macedonia","Albania","Montenegro","Turkey","United States", "United Kingdom"))
eurostat_countries <- rbind(eu_countries, efta_countries, additional_areas) %>% 
        select(code, name) %>% arrange(name) %>% 
        filter(name != "Liechtenstein")

country_group_buttons <- function(prefix) {
        tags$div(
                style = "margin-bottom: 10px;",
                tags$label("Country group quick selection:"),
                br(),
                actionButton(paste0(prefix, "_eu"), "EU", class = "quick-btn"),
                actionButton(paste0(prefix, "_efta"), "EFTA", class = "quick-btn"),
                actionButton(paste0(prefix, "_eu_efta"), "EU + EFTA", class = "quick-btn"),
                actionButton(paste0(prefix, "_euro"), "Euro area", class = "quick-btn"),
                actionButton(paste0(prefix, "_med"), "Mediterranean", class = "quick-btn"),
                actionButton(paste0(prefix, "_nordic"), "Nordic", class = "quick-btn"),
                actionButton(paste0(prefix, "_balkan"), "Balkan", class = "quick-btn"),
                br(),
                actionButton(paste0(prefix, "_central"), "Central", class = "quick-btn"),
                actionButton(paste0(prefix, "_eastern"), "Eastern", class = "quick-btn"),
                actionButton(paste0(prefix, "_western"), "Western", class = "quick-btn"),
                actionButton(paste0(prefix, "_benelux"), "Benelux", class = "quick-btn"),
                br(),
                actionButton(paste0(prefix, "_clear"), "Clear selection", class = "quick-btn-clear")
        )
}

tab_hero <- function(title, subtitle, badges = character(0)) {
        tags$div(
                class = "tab-hero",
                tags$h2(title),
                tags$p(subtitle),
                if (length(badges) > 0) {
                        tags$div(
                                class = "tab-badges",
                                lapply(badges, function(x) {
                                        tags$span(class = "tab-badge", x)
                                })
                        )
                }
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

download_panel <- function(condition, download_id) {
        conditionalPanel(
                condition = condition,
                tags$div(
                        class = "download-panel",
                        downloadLink(download_id, "Download data")
                )
        )
}

ui <- fluidPage(theme = bs_theme(bootswatch = "minty"),
                tags$style(HTML("
                
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
          max-width: 750px;
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
        
        .navbar .container-fluid,
        .navbar .container {
          padding-left: 8px !important;
          padding-right: 8px !important;
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
        
        .navbar-nav {
          gap: 6px;
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
        
        .navbar-toggler {
          border: 1px solid #d7e7e2 !important;
          border-radius: 10px !important;
          padding: 6px 9px !important;
        }
        
        .navbar-collapse {
          padding-top: 4px;
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
        
          .navbar-nav .nav-link,
          .navbar-nav > li > a {
            border-radius: 10px !important;
            margin: 3px 0 !important;
          }
        }
    .plot-container .legend .traces .legendtext {
    white-space: normal !important;
    word-break: break-word !important;
    }
  .quick-btn {
  background-color: #f8f9fa !important;
  color: #4a4a4a !important;
  border: 1px solid #d6d6d6 !important;
  border-radius: 8px !important;
  padding: 6px 12px !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  box-shadow: none !important;
  margin-right: 6px;
  margin-bottom: 6px;
}

.quick-btn:hover {
  background-color: #eef2f5 !important;
  border-color: #bfc7cf !important;
  color: #2f2f2f !important;
}

.quick-btn:focus,
.quick-btn:active {
  outline: none !important;
  box-shadow: none !important;
}

.quick-btn-clear {
  background-color: #fafafa !important;
  color: #777 !important;
  border: 1px solid #d6d6d6 !important;
  border-radius: 8px !important;
  padding: 6px 12px !important;
  font-size: 13px !important;
  box-shadow: none !important;
  margin-top: 4px;
}

.quick-btn-clear:hover {
  background-color: #f1f1f1 !important;
  color: #555 !important;
}

.app-tab-page {
  max-width: 1250px;
  margin: 0 auto;
  padding: 18px 8px 40px 8px;
  color: #263238;
}

.tab-hero {
  background: linear-gradient(135deg, #f7fbf9 0%, #eef7f4 100%);
  border: 1px solid #dcebe6;
  border-radius: 18px;
  padding: 20px 26px;
  margin-bottom: 18px;
}

.tab-hero h2 {
  margin-top: 0;
  margin-bottom: 6px;
  font-size: 24px;
  font-weight: 700;
}

.tab-hero p {
  margin-bottom: 0;
  font-size: 14px;
  line-height: 1.55;
  max-width: 850px;
}

.tab-badges {
  margin-top: 12px;
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
  border: 1px solid #e1e7e5;
  border-radius: 16px;
  padding: 16px 18px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.035);
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

.primary-action-btn {
  width: 100%;
  background-color: #3f8f7a !important;
  color: white !important;
  border: 1px solid #337968 !important;
  border-radius: 10px !important;
  padding: 8px 12px !important;
  font-weight: 700 !important;
  margin-top: 10px;
}

.primary-action-btn:hover {
  background-color: #347866 !important;
}

.secondary-action-btn {
  width: 100%;
  background-color: #f8f9fa !important;
  color: #3f4a4d !important;
  border: 1px solid #d6d6d6 !important;
  border-radius: 10px !important;
  padding: 8px 12px !important;
  font-weight: 600 !important;
  margin-top: 8px;
}

.secondary-action-btn:hover {
  background-color: #eef2f5 !important;
}

.download-panel {
  margin-top: 14px;
  padding: 12px 14px;
  background: #f7fbf9;
  border: 1px solid #dcebe6;
  border-radius: 12px;
  font-size: 13px;
  font-weight: 600;
}

.info-label {
  border-bottom: 1px dotted #6c7a7d;
  cursor: help;
}

.side-note {
  background: #fffaf0;
  border: 1px solid #f0dfb5;
  border-radius: 14px;
  padding: 11px 13px;
  font-size: 12.5px;
  color: #5f4b16;
  line-height: 1.45;
  margin-top: 12px;
}

.form-group {
  margin-bottom: 12px;
}

.help-page {
  max-width: 1100px;
  margin: 0 auto;
  padding: 22px 8px 45px 8px;
  color: #263238;
}

.help-hero {
  background: linear-gradient(135deg, #f7fbf9 0%, #eef7f4 100%);
  border: 1px solid #dcebe6;
  border-radius: 18px;
  padding: 26px 30px;
  margin-bottom: 18px;
}

.help-hero h2 {
  margin-top: 0;
  margin-bottom: 8px;
  font-size: 26px;
  font-weight: 700;
}

.help-hero p {
  font-size: 15px;
  line-height: 1.6;
  max-width: 850px;
  margin-bottom: 0;
}

.help-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(285px, 1fr));
  gap: 16px;
  margin-top: 16px;
  margin-bottom: 20px;
}

.help-card {
  background: #ffffff;
  border: 1px solid #e1e7e5;
  border-radius: 16px;
  padding: 18px 20px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.035);
}

.help-card h3,
.help-card h4 {
  margin-top: 0;
  font-weight: 700;
}

.help-card h3 {
  font-size: 19px;
  margin-bottom: 10px;
}

.help-card h4 {
  font-size: 16px;
  margin-bottom: 8px;
}

.help-card p,
.help-card li {
  font-size: 14px;
  line-height: 1.55;
}

.help-card ul {
  padding-left: 19px;
  margin-bottom: 0;
}

.help-section-title {
  margin-top: 30px;
  margin-bottom: 12px;
  font-size: 22px;
  font-weight: 700;
}

.help-badge {
  display: inline-block;
  background: #e8f4f0;
  color: #24584b;
  border: 1px solid #cfe6dd;
  border-radius: 999px;
  padding: 4px 10px;
  font-size: 12px;
  font-weight: 600;
  margin-bottom: 10px;
}

.help-callout {
  background: #fffaf0;
  border: 1px solid #f0dfb5;
  border-radius: 16px;
  padding: 16px 20px;
  margin-top: 16px;
  margin-bottom: 20px;
}

.help-callout strong {
  color: #6a4b00;
}

.help-muted {
  color: #687579;
  font-size: 13px;
}

.help-table {
  width: 100%;
  border-collapse: collapse;
  margin-top: 10px;
  font-size: 14px;
}

.help-table th {
  background: #f4f8f7;
  border-bottom: 1px solid #dce5e2;
  padding: 10px;
  text-align: left;
  font-weight: 700;
}

.help-table td {
  border-bottom: 1px solid #edf1f0;
  padding: 10px;
  vertical-align: top;
}

.help-details {
  margin-top: 10px;
}

.help-details summary {
  cursor: pointer;
  font-weight: 700;
  margin-bottom: 8px;
}

.help-kpi {
  font-size: 13px;
  color: #526064;
  margin-top: 8px;
}
  ")),        
                tags$div(
                        class = "app-header",
                        tags$div(
                                class = "app-header-main",
                                tags$div(
                                        tags$div(class = "app-kicker", "European price statistics dashboard"),
                                        tags$h1(class = "app-title", "HICP Voyager"),
                                        tags$p(
                                                class = "app-subtitle",
                                                "Analyse HICP indices, inflation rates, contributions, seasonality and weights across Europe."
                                        )
                                ),
                                tags$div(
                                        class = "app-header-badges",
                                        tags$span(class = "app-version-pill", "Version 2.1"),
                                        tags$span(class = "app-meta-pill", "Source: Eurostat HICP data")
                                )
                        )
                ),
                
                navbarPage(
                        title = tags$span(
                                class = "navbar-brand-custom",
                                tags$span(class = "navbar-brand-mark", "H"),
                                tags$span("HICP Voyager")
                        ),
                        id = "main_tabs",
                        collapsible = TRUE,
                tabPanel(
                        "Index",
                        tags$div(
                                class = "app-tab-page",
                                
                                tab_hero(
                                        "Index",
                                        "Compare HICP and HICP-CT index developments across countries and product categories. Rebase series to a common reference period and optionally adjust lower-level indices for all-items inflation.",
                                        badges = c("HICP", "HICP-CT", "Rebasing", "Relative price index")
                                ),
                                
                                fluidRow(
                                        column(
                                                width = 3,
                                                tags$div(
                                                        class = "control-card",
                                                        tags$h3("Index settings"),
                                                        
                                                        control_group(
                                                                "Countries",
                                                                country_group_buttons("index"),
                                                                selectInput(
                                                                        "countries",
                                                                        "Custom countries selection",
                                                                        choices = setNames(eurostat_countries$code, eurostat_countries$name),
                                                                        multiple = TRUE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Product categories",
                                                                selectInput(
                                                                        "coicops",
                                                                        "Select product categories",
                                                                        choices = setNames(coicop_set$coicop18_code, coicop_set$code_label),
                                                                        multiple = TRUE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Index measure",
                                                                checkboxGroupInput(
                                                                        "index_measure",
                                                                        NULL,
                                                                        choices = c(
                                                                                "HICP" = "HICP",
                                                                                "HICP-CT" = "HICP_CT"
                                                                        ),
                                                                        selected = "HICP"
                                                                ),
                                                                note = "If both HICP and HICP-CT are selected, only one product category can be shown."
                                                        ),
                                                        
                                                        control_group(
                                                                "Display options",
                                                                checkboxInput(
                                                                        "index_backdrop_eu",
                                                                        "Other EU countries as backdrop",
                                                                        value = TRUE
                                                                ),
                                                                checkboxInput(
                                                                        "index_adjust_total",
                                                                        label = tags$span(
                                                                                class = "info-label",
                                                                                title = paste(
                                                                                        "Divides each selected lower-level index by the all-items HICP",
                                                                                        "index for the same country, month and measure, then multiplies by 100.",
                                                                                        "This gives a relative price index and makes it easier to compare",
                                                                                        "lower-level price developments across countries with different",
                                                                                        "overall inflation environments."
                                                                                ),
                                                                                "Adjust lower level index with total inflation"
                                                                        ),
                                                                        value = FALSE
                                                                )
                                                        ),
                                                        
                                                        actionButton(
                                                                "update",
                                                                "Retrieve data",
                                                                class = "primary-action-btn"
                                                        ),
                                                        
                                                        control_group(
                                                                "Reference period",
                                                                radioButtons(
                                                                        "period_type",
                                                                        "Index reference period",
                                                                        choices = c("Full year", "Month"),
                                                                        selected = "Full year"
                                                                ),
                                                                selectInput(
                                                                        "select_years",
                                                                        "Select index reference period",
                                                                        choices = "",
                                                                        multiple = FALSE
                                                                ),
                                                                actionButton(
                                                                        "rebase",
                                                                        "Update plot",
                                                                        class = "secondary-action-btn"
                                                                ),
                                                                note = "The selected reference period is set to 100 for all displayed series."
                                                        ),
                                                        
                                                        download_panel("output.plot", "downloadData")
                                                )
                                        ),
                                        
                                        column(
                                                width = 9,
                                                tags$div(
                                                        class = "plot-card",
                                                        plotlyOutput("plot")
                                                )
                                        )
                                )
                        )
                ),
                tabPanel(
                        "M/M-1",
                        tags$div(
                                class = "app-tab-page",
                                
                                tab_hero(
                                        "Monthly rate of change, M/M-1",
                                        "Analyse monthly price changes and decompose them into component contributions. This tab is especially useful for understanding short-term inflation movements.",
                                        badges = c("Monthly rate", "Ribe contributions", "Country comparison")
                                ),
                                
                                fluidRow(
                                        column(
                                                width = 3,
                                                tags$div(
                                                        class = "control-card",
                                                        tags$h3("Monthly analysis settings"),
                                                        
                                                        control_group(
                                                                "Display mode",
                                                                radioButtons(
                                                                        "mr_view",
                                                                        NULL,
                                                                        choices = c(
                                                                                "One graph per period" = "period",
                                                                                "One graph per country" = "country"
                                                                        ),
                                                                        selected = "period"
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Countries",
                                                                country_group_buttons("mr"),
                                                                selectInput(
                                                                        "countries_mr",
                                                                        "Custom countries selection",
                                                                        choices = setNames(eurostat_countries$code, eurostat_countries$name),
                                                                        multiple = TRUE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Classification and aggregate",
                                                                radioButtons(
                                                                        "classification_mr",
                                                                        "Classification",
                                                                        choices = c(
                                                                                "ECOICOP ver. 2" = "ecoicop",
                                                                                "Special aggregats" = "sa"
                                                                        ),
                                                                        selected = "ecoicop"
                                                                ),
                                                                selectInput(
                                                                        "coicop_mr",
                                                                        "Select product category",
                                                                        choices = setNames(label_set$coicop18_code, label_set$code_label),
                                                                        multiple = FALSE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Contribution target",
                                                                radioButtons(
                                                                        "contribution_type_mr",
                                                                        "Contribution to",
                                                                        choices = c(
                                                                                "selected higher aggregate",
                                                                                "all-items HICP"
                                                                        ),
                                                                        selected = "selected higher aggregate"
                                                                ),
                                                                note = "Choose whether contributions should sum to the selected aggregate or to all-items HICP."
                                                        ),
                                                        
                                                        actionButton(
                                                                "update_mr",
                                                                "Retrieve data",
                                                                class = "primary-action-btn"
                                                        ),
                                                        
                                                        control_group(
                                                                "Periods",
                                                                selectInput(
                                                                        "select_period_mr",
                                                                        "Select months to compare",
                                                                        choices = "",
                                                                        multiple = TRUE
                                                                )
                                                        ),
                                                        
                                                        download_panel("output.plot_mr", "downloadData_mr")
                                                )
                                        ),
                                        
                                        column(
                                                width = 9,
                                                tags$div(
                                                        class = "plot-card",
                                                        plotlyOutput("plot_mr")
                                                )
                                        )
                                )
                        )
                ),
                tabPanel(
                        "M/M-12",
                        tags$div(
                                class = "app-tab-page",
                                
                                tab_hero(
                                        "Annual rate of change, M/M-12",
                                        "Analyse annual inflation rates and decompose them into component contributions. Use this tab to compare annual inflation drivers across countries or over time.",
                                        badges = c("Annual rate", "Ribe contributions", "Inflation decomposition")
                                ),
                                
                                fluidRow(
                                        column(
                                                width = 3,
                                                tags$div(
                                                        class = "control-card",
                                                        tags$h3("Annual analysis settings"),
                                                        
                                                        control_group(
                                                                "Display mode",
                                                                radioButtons(
                                                                        "ar_view",
                                                                        NULL,
                                                                        choices = c(
                                                                                "One graph per period" = "period",
                                                                                "One graph per country" = "country"
                                                                        ),
                                                                        selected = "period"
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Countries",
                                                                country_group_buttons("ar"),
                                                                selectInput(
                                                                        "countries_ar",
                                                                        "Custom countries selection",
                                                                        choices = setNames(eurostat_countries$code, eurostat_countries$name),
                                                                        multiple = TRUE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Classification and aggregate",
                                                                radioButtons(
                                                                        "classification_ar",
                                                                        "Classification",
                                                                        choices = c(
                                                                                "ECOICOP ver. 2" = "ecoicop",
                                                                                "Special aggregats" = "sa"
                                                                        ),
                                                                        selected = "ecoicop"
                                                                ),
                                                                selectInput(
                                                                        "coicop_ar",
                                                                        "Select product category",
                                                                        choices = setNames(label_set$coicop18_code, label_set$code_label),
                                                                        multiple = FALSE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Contribution target",
                                                                radioButtons(
                                                                        "contribution_type_ar",
                                                                        "Contribution to",
                                                                        choices = c(
                                                                                "selected higher aggregate",
                                                                                "all-items HICP"
                                                                        ),
                                                                        selected = "selected higher aggregate"
                                                                ),
                                                                note = "Use all-items HICP when you want to see how the selected components contribute to total inflation."
                                                        ),
                                                        
                                                        actionButton(
                                                                "update_ar",
                                                                "Retrieve data",
                                                                class = "primary-action-btn"
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "output.plot_ar && input.ar_view == 'country'",
                                                                control_group(
                                                                        "Period range",
                                                                        sliderInput(
                                                                                "range_slider",
                                                                                "Select period",
                                                                                min = 2000,
                                                                                max = 2023,
                                                                                value = c(2015, 2023),
                                                                                ticks = FALSE,
                                                                                sep = ""
                                                                        )
                                                                )
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "output.plot_ar && input.ar_view == 'period'",
                                                                control_group(
                                                                        "Period",
                                                                        selectInput(
                                                                                "select_period_ar",
                                                                                "Select month",
                                                                                choices = "",
                                                                                multiple = FALSE
                                                                        )
                                                                )
                                                        ),
                                                        
                                                        download_panel("output.plot_ar", "downloadData_ar")
                                                )
                                        ),
                                        
                                        column(
                                                width = 9,
                                                tags$div(
                                                        class = "plot-card",
                                                        plotlyOutput("plot_ar")
                                                )
                                        )
                                )
                        )
                ),
                tabPanel(
                        "Seasonality",
                        tags$div(
                                class = "app-tab-page",
                                
                                tab_hero(
                                        "Seasonality",
                                        "Compare within-year price patterns by setting December of the previous year to 100. This is useful for identifying recurring seasonal movements in selected product categories.",
                                        badges = c("December Y-1 = 100", "Seasonal pattern", "Year comparison")
                                ),
                                
                                fluidRow(
                                        column(
                                                width = 3,
                                                tags$div(
                                                        class = "control-card",
                                                        tags$h3("Seasonality settings"),
                                                        
                                                        control_group(
                                                                "Countries",
                                                                country_group_buttons("se"),
                                                                selectInput(
                                                                        "countries_se",
                                                                        "Select countries",
                                                                        choices = setNames(eurostat_countries$code, eurostat_countries$name),
                                                                        multiple = TRUE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Product category",
                                                                selectInput(
                                                                        "coicop_se",
                                                                        "Select product category",
                                                                        choices = setNames(coicop_set$coicop18_code, coicop_set$code_label),
                                                                        multiple = FALSE
                                                                )
                                                        ),
                                                        
                                                        actionButton(
                                                                "update_se",
                                                                "Retrieve data",
                                                                class = "primary-action-btn"
                                                        ),
                                                        
                                                        control_group(
                                                                "Years",
                                                                selectInput(
                                                                        "select_years_se",
                                                                        "Select years to compare",
                                                                        choices = "",
                                                                        multiple = TRUE
                                                                ),
                                                                note = "Each selected year is shown as a separate line. December of the previous year is added as the common reference point."
                                                        ),
                                                        
                                                        download_panel("output.plot_se", "downloadData_se")
                                                )
                                        ),
                                        
                                        column(
                                                width = 9,
                                                tags$div(
                                                        class = "plot-card",
                                                        plotlyOutput("plot_se")
                                                )
                                        )
                                )
                        )
                ),
                tabPanel(
                        "Weights",
                        tags$div(
                                class = "app-tab-page",
                                
                                tab_hero(
                                        "Weights",
                                        "Explore HICP expenditure weights across countries, years and product categories. Weights are shown in per mille, where all-items HICP equals 1000.",
                                        badges = c("Expenditure weights", "Per mille", "Consumption structure")
                                ),
                                
                                fluidRow(
                                        column(
                                                width = 3,
                                                tags$div(
                                                        class = "control-card",
                                                        tags$h3("Weight settings"),
                                                        
                                                        control_group(
                                                                "Display mode",
                                                                radioButtons(
                                                                        "weights_view",
                                                                        NULL,
                                                                        choices = c(
                                                                                "One graph per year" = "year",
                                                                                "One graph per country" = "country"
                                                                        ),
                                                                        selected = "year"
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Countries",
                                                                country_group_buttons("weights"),
                                                                selectInput(
                                                                        "countries_w",
                                                                        "Countries custom selection",
                                                                        choices = setNames(eurostat_countries$code, eurostat_countries$name),
                                                                        multiple = TRUE
                                                                )
                                                        ),
                                                        
                                                        control_group(
                                                                "Product category",
                                                                selectInput(
                                                                        "coicop_w",
                                                                        "Select product category",
                                                                        choices = setNames(label_set$coicop18_code, label_set$code_label),
                                                                        multiple = FALSE
                                                                ),
                                                                note = "If a parent aggregate is selected, the chart shows its direct lower-level components where available."
                                                        ),
                                                        
                                                        actionButton(
                                                                "update_w",
                                                                "Retrieve data",
                                                                class = "primary-action-btn"
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "output.plot_w",
                                                                control_group(
                                                                        "Period",
                                                                        sliderInput(
                                                                                "range_slider_w",
                                                                                "Select period",
                                                                                min = 2000,
                                                                                max = 2023,
                                                                                value = c(2015, 2023),
                                                                                ticks = FALSE,
                                                                                sep = ""
                                                                        )
                                                                )
                                                        ),
                                                        
                                                        download_panel("output.plot_w", "downloadData_w")
                                                )
                                        ),
                                        
                                        column(
                                                width = 9,
                                                tags$div(
                                                        class = "plot-card",
                                                        plotlyOutput("plot_w")
                                                )
                                        )
                                )
                        )
                ),
                
                
                tabPanel(
                        "Help",
                        withMathJax(
                        tags$div(
                                class = "help-page",
                                
                                tags$div(
                                        class = "help-hero",
                                        tags$span(class = "help-badge", "User guide"),
                                        tags$h2("HICP Voyager 2.1"),
                                        tags$p(
                                                "HICP Voyager is an interactive dashboard for exploring European consumer price statistics. ",
                                                "The app lets you analyse HICP indices, HICP-CT indices, monthly and annual rates of change, ",
                                                "Ribe contributions, seasonal patterns and expenditure weights across countries and product categories."
                                        )
                                ),
                                
                                tags$div(
                                        class = "help-grid",
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h3("What the app is for"),
                                                tags$p(
                                                        "The dashboard is designed for analytical comparison of price developments across countries, ",
                                                        "COICOP aggregates and time periods."
                                                ),
                                                tags$ul(
                                                        tags$li("Compare HICP and HICP-CT index developments."),
                                                        tags$li("Rebase index series to a common reference year or month."),
                                                        tags$li("Analyse monthly and annual rates of change."),
                                                        tags$li("Calculate contributions to selected aggregates or to all-items HICP."),
                                                        tags$li("Study seasonality and country differences in expenditure weights.")
                                                )
                                        ),
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h3("Recommended workflow"),
                                                tags$ul(
                                                        tags$li("Select one or more countries."),
                                                        tags$li("Select a product category or aggregate."),
                                                        tags$li("Choose the relevant index measure or calculation mode."),
                                                        tags$li("Click ", tags$strong("Retrieve data"), " to download data from Eurostat."),
                                                        tags$li("Choose reference period or comparison period where applicable."),
                                                        tags$li("Use ", tags$strong("Download data"), " to export calculated results.")
                                                )
                                        ),
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h3("Data source"),
                                                tags$p(
                                                        "Data is retrieved from Eurostat using the R package ",
                                                        tags$code("eurostat"),
                                                        ". The app uses HICP index data, HICP-CT index data and HICP weights, depending on the selected tab."
                                                ),
                                                tags$p(
                                                        class = "help-muted",
                                                        "All calculations are performed inside the app after the Eurostat data has been retrieved."
                                                )
                                        )
                                ),
                                
                                tags$h3(class = "help-section-title", "Tabs and functionality"),
                                
                                tags$div(
                                        class = "help-card",
                                        tags$h3("Index"),
                                        tags$p(
                                                "The Index tab is used to compare price index developments across countries and product categories. ",
                                                "You can select one or more countries, one or more COICOP aggregates, and either HICP, HICP-CT, or both."
                                        ),
                                        tags$table(
                                                class = "help-table",
                                                tags$tr(
                                                        tags$th("Function"),
                                                        tags$th("Description")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Country group quick selection")),
                                                        tags$td("Select predefined groups such as EU, Euro area, Nordic countries, Balkan countries or Benelux.")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Custom countries selection")),
                                                        tags$td("Manually select the countries or areas to include in the graph.")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Select product categories")),
                                                        tags$td("Select one or more COICOP aggregates. These can be all-items HICP or lower-level product categories.")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Select index measure")),
                                                        tags$td("Choose HICP, HICP-CT, or both. If both HICP and HICP-CT are selected, only one COICOP aggregate can be shown.")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Other EU countries as backdrop")),
                                                        tags$td("Shows non-selected EU countries in grey as contextual background series. This makes it easier to compare selected countries against the broader EU distribution.")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Adjust lower level index with total inflation")),
                                                        tags$td(
                                                                "Divides each selected lower-level index by the all-items HICP index for the same country and month, then multiplies by 100. ",
                                                                "This creates a relative price index and can be useful when comparing product-level price developments across countries with different overall inflation environments."
                                                        )
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Index reference period")),
                                                        tags$td("Rebases the selected series to either a full year average or to a specific month.")
                                                )
                                        ),
                                        tags$div(
                                                class = "help-callout",
                                                tags$strong("Interpretation tip: "),
                                                "When the total-inflation adjustment is used, the graph should be interpreted as relative price development. ",
                                                "A value above 100 after the reference period means that the product group has increased more than the country's all-items HICP. ",
                                                "A value below 100 means that it has developed more weakly than the country's all-items HICP."
                                        )
                                ),
                                
                                tags$div(
                                        class = "help-grid",
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h3("M/M-1"),
                                                tags$p(
                                                        "The M/M-1 tab analyses monthly rates of change and contributions to monthly inflation. ",
                                                        "It is useful when you want to understand what drove the latest monthly movement in a selected aggregate."
                                                ),
                                                tags$ul(
                                                        tags$li(tags$strong("One graph per period:"), " compares countries for one or more selected months."),
                                                        tags$li(tags$strong("One graph per country:"), " compares selected months within each country."),
                                                        tags$li(tags$strong("Selected higher aggregate:"), " calculates contributions to the selected parent aggregate."),
                                                        tags$li(tags$strong("All-items HICP:"), " calculates contributions to total HICP inflation.")
                                                ),
                                                tags$p(
                                                        class = "help-kpi",
                                                        "The black marker shows the monthly rate of change for the selected aggregate when relevant."
                                                )
                                        ),
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h3("M/M-12"),
                                                tags$p(
                                                        "The M/M-12 tab analyses annual rates of change and annual-rate contributions. ",
                                                        "It is useful for understanding which subcomponents explain annual inflation in a selected country or across countries."
                                                ),
                                                tags$ul(
                                                        tags$li(tags$strong("One graph per period:"), " compares countries for a selected month."),
                                                        tags$li(tags$strong("One graph per country:"), " shows the development over time by country."),
                                                        tags$li(tags$strong("Selected higher aggregate:"), " shows contributions to the selected aggregate."),
                                                        tags$li(tags$strong("All-items HICP:"), " shows contributions to total HICP inflation.")
                                                ),
                                                tags$p(
                                                        class = "help-kpi",
                                                        "The dashed black line or black marker shows the annual rate of change for the selected aggregate when relevant."
                                                )
                                        )
                                ),
                                
                                tags$div(
                                        class = "help-grid",
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h3("Seasonality"),
                                                tags$p(
                                                        "The Seasonality tab shows the monthly path of an index within each year, using December of the previous year as reference period."
                                                ),
                                                tags$ul(
                                                        tags$li("The index is expressed as December Y-1 = 100."),
                                                        tags$li("This makes it easier to compare seasonal price patterns between years."),
                                                        tags$li("Useful for categories with strong seasonal behaviour, such as package holidays, clothing or energy.")
                                                ),
                                                tags$p(
                                                        class = "help-kpi",
                                                        "Example: a value of 105 in July means that the index is 5 percent higher than in December of the previous year."
                                                )
                                        ),
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h3("Weights"),
                                                tags$p(
                                                        "The Weights tab shows HICP expenditure weights for selected countries and product categories."
                                                ),
                                                tags$ul(
                                                        tags$li(tags$strong("One graph per year:"), " compares countries within selected years."),
                                                        tags$li(tags$strong("One graph per country:"), " shows the development of weights over time for each country."),
                                                        tags$li("Weights are shown in per mille, where total HICP equals 1000.")
                                                ),
                                                tags$p(
                                                        class = "help-kpi",
                                                        "The tab can be used to compare consumption structures across countries."
                                                )
                                        )
                                ),
                                
                                tags$h3(class = "help-section-title", "Classification options"),
                                
                                tags$div(
                                        class = "help-card",
                                        tags$p(
                                                "Some tabs allow you to choose between the standard ECOICOP classification and selected special aggregates."
                                        ),
                                        tags$table(
                                                class = "help-table",
                                                tags$tr(
                                                        tags$th("Classification"),
                                                        tags$th("Use case")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("ECOICOP ver. 2")),
                                                        tags$td("Use this for standard COICOP-based analysis of HICP product categories.")
                                                ),
                                                tags$tr(
                                                        tags$td(tags$strong("Special aggregates")),
                                                        tags$td("Use this for broader analytical aggregates such as energy, services, food, processed food and non-energy industrial goods.")
                                                )
                                        )
                                ),
                                
                                tags$h3(class = "help-section-title", "Methodological notes"),
                                
                                tags$div(
                                        class = "help-card",
                                        tags$h3("Index rebasing"),
                                        tags$p(
                                                "When an index reference period is selected, the app calculates a new index base for all selected series. ",
                                                "For a full-year reference period, the average index value in that year is set to 100. ",
                                                "For a monthly reference period, the selected month is set to 100."
                                        ),
                                        tags$p(
                                                tags$strong("Formula: "),
                                                tags$code("rebased index = original index / reference-period index * 100")
                                        ),
                                        
                                        tags$hr(),
                                        
                                        tags$h3("Total-inflation adjustment"),
                                        tags$p(
                                                "The total-inflation adjustment transforms a lower-level index into a relative price index. ",
                                                "The selected index is divided by the all-items HICP index for the same country, month and measure."
                                        ),
                                        tags$p(
                                                tags$strong("Formula: "),
                                                tags$code("relative price index = lower-level index / all-items HICP * 100")
                                        ),
                                        tags$p(
                                                "The relative price index is then rebased using the same reference-period logic as ordinary index series."
                                        ),
                                        
                                        tags$hr(),
                                        
                                        tags$h3("Ribe contributions"),
                                        tags$p(
                                                "The M/M-1 and M/M-12 tabs calculate Ribe-type contributions. ",
                                                "The purpose is to decompose the rate of change of a selected aggregate into contributions from lower-level components."
                                        ),
                                        
                                        tags$h4("Notation"),
                                        
                                        tags$table(
                                                class = "help-table",
                                                tags$tr(
                                                        tags$th("Symbol"),
                                                        tags$th("Meaning")
                                                ),
                                                tags$tr(
                                                        tags$td("\\(j\\)"),
                                                        tags$td("Lower-level component, for example a COICOP subclass or special aggregate component.")
                                                ),
                                                tags$tr(
                                                        tags$td("\\(A\\)"),
                                                        tags$td("Target aggregate. This is either the selected higher aggregate or all-items HICP, depending on the selected contribution mode.")
                                                ),
                                                tags$tr(
                                                        tags$td("\\(I_{j,t}\\)"),
                                                        tags$td("Index for component \\(j\\) in month \\(t\\).")
                                                ),
                                                tags$tr(
                                                        tags$td("\\(I_{A,t}\\)"),
                                                        tags$td("Index for the target aggregate \\(A\\) in month \\(t\\).")
                                                ),
                                                tags$tr(
                                                        tags$td("\\(w_{j,y}^{A}\\)"),
                                                        tags$td("Annual weight for component \\(j\\), expressed in per mille relative to the target aggregate \\(A\\).")
                                                ),
                                                tags$tr(
                                                        tags$td("\\(m,y\\)"),
                                                        tags$td("Month \\(m\\) in year \\(y\\).")
                                                )
                                        ),
                                        
                                        tags$h4("Weight used for the selected contribution target"),
                                        
                                        tags$p(
                                                "If contributions are calculated to the selected higher aggregate, the component weights are rescaled to that aggregate:"
                                        ),
                                        
                                        tags$div(
                                                "$$",
                                                "w_{j,y}^{A} = \\frac{w_{j,y}^{00}}{w_{A,y}^{00}} \\times 1000",
                                                "$$"
                                        ),
                                        
                                        tags$p(
                                                "If contributions are calculated to all-items HICP, the ordinary all-items HICP weights are used:"
                                        ),
                                        
                                        tags$div(
                                                "$$",
                                                "w_{j,y}^{A} = w_{j,y}^{00}",
                                                "$$"
                                        ),
                                        
                                        tags$h4("Monthly contribution, M/M-1"),
                                        
                                        tags$p("First, the annual weight is price-updated to the previous month:"),
                                        
                                        tags$div(
                                                "$$",
                                                "\\widetilde{w}_{j,t-1}^{A} = ",
                                                "\\frac{w_{j,y}^{A}}{1000} \\times ",
                                                "\\frac{I_{j,t-1}}{I_{A,t-1}}",
                                                "$$"
                                        ),
                                        
                                        tags$p("The monthly rate of change for component \\(j\\) is:"),
                                        
                                        tags$div(
                                                "$$",
                                                "r_{j,t}^{m} = ",
                                                "\\left( \\frac{I_{j,t}}{I_{j,t-1}} - 1 \\right) \\times 100",
                                                "$$"
                                        ),
                                        
                                        tags$p("The monthly contribution from component \\(j\\) is then:"),
                                        
                                        tags$div(
                                                "$$",
                                                "C_{j,t}^{m} = ",
                                                "\\widetilde{w}_{j,t-1}^{A} \\times r_{j,t}^{m}",
                                                "$$"
                                        ),
                                        
                                        tags$p(
                                                "This is the formula used in the M/M-1 tab. ",
                                                "In the graph, contributions are shown in percentage points."
                                        ),
                                        
                                        tags$h4("Annual contribution, M/M-12"),
                                        
                                        tags$p(
                                                "For annual rates of change, the Ribe contribution is calculated in two parts. ",
                                                "The first part captures the price change from December of the previous year to the current month. ",
                                                "The second part adjusts for the price change between the same month of the previous year and December of the previous year."
                                        ),
                                        
                                        tags$div(
                                                "$$",
                                                "C_{j,m,y}^{12} = ",
                                                "100 \\times ",
                                                "\\frac{I_{A,12,y-1}}{I_{A,m,y-1}} \\times ",
                                                "\\frac{w_{j,y}^{A}}{1000} \\times ",
                                                "\\frac{I_{j,m,y} - I_{j,12,y-1}}{I_{j,12,y-1}}",
                                                "$$"
                                        ),
                                        
                                        tags$div(
                                                "$$",
                                                "\\quad + ",
                                                "100 \\times ",
                                                "\\frac{I_{A,12,y-2}}{I_{A,m,y-1}} \\times ",
                                                "\\frac{w_{j,y-1}^{A}}{1000} \\times ",
                                                "\\frac{I_{j,12,y-1} - I_{j,m,y-1}}{I_{j,12,y-2}}",
                                                "$$"
                                        ),
                                        
                                        tags$p(
                                                "The first term uses the current year's weight and the movement from December Y-1 to the current month. ",
                                                "The second term uses the previous year's weight and adjusts for the movement between month M in Y-1 and December Y-1."
                                        ),
                                        
                                        tags$h4("Aggregate rate of change"),
                                        
                                        tags$p("The aggregate monthly rate shown in the M/M-1 tab is:"),
                                        
                                        tags$div(
                                                "$$",
                                                "r_{A,t}^{m} = ",
                                                "\\left( \\frac{I_{A,t}}{I_{A,t-1}} - 1 \\right) \\times 100",
                                                "$$"
                                        ),
                                        
                                        tags$p("The aggregate annual rate shown in the M/M-12 tab is:"),
                                        
                                        tags$div(
                                                "$$",
                                                "r_{A,m,y}^{12} = ",
                                                "\\left( \\frac{I_{A,m,y}}{I_{A,m,y-1}} - 1 \\right) \\times 100",
                                                "$$"
                                        ),
                                        
                                        tags$p(
                                                class = "help-muted",
                                                "When all required component indices and weights are available, the sum of the component contributions corresponds to the rate of change of the selected contribution target, apart from rounding effects."
                                        )
                                ),
                                
                                tags$h3(class = "help-section-title", "How to interpret results"),
                                
                                tags$div(
                                        class = "help-grid",
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h4("Index graphs"),
                                                tags$p(
                                                        "Index graphs show price development over time. After rebasing, all selected series start from the same reference value, ",
                                                        "which makes relative movements easier to compare."
                                                )
                                        ),
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h4("Rates of change"),
                                                tags$p(
                                                        "Monthly rates show short-term price changes. Annual rates show price changes compared with the same month one year earlier."
                                                )
                                        ),
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h4("Contributions"),
                                                tags$p(
                                                        "Contributions show how much each component contributes to the rate of change of the selected aggregate. ",
                                                        "Positive contributions push inflation up; negative contributions push inflation down."
                                                )
                                        ),
                                        
                                        tags$div(
                                                class = "help-card",
                                                tags$h4("Weights"),
                                                tags$p(
                                                        "Weights indicate the relative importance of product groups in the HICP basket. ",
                                                        "Differences in weights can explain why similar price movements have different inflation impacts across countries."
                                                )
                                        )
                                ),
                                
                                tags$h3(class = "help-section-title", "Limitations"),
                                
                                tags$div(
                                        class = "help-card",
                                        tags$ul(
                                                tags$li("The app depends on data availability in Eurostat's HICP database."),
                                                tags$li("Some countries may publish flash estimates for all-items HICP before detailed lower-level aggregates are available."),
                                                tags$li("Contribution calculations require both index values and corresponding weights."),
                                                tags$li("Comparisons across countries should take into account differences in consumption weights, tax systems, market structures and data availability."),
                                                tags$li("The total-inflation adjustment is an analytical transformation and should not be interpreted as an official Eurostat indicator.")
                                        )
                                ),
                                
                                tags$div(
                                        class = "help-callout",
                                        tags$strong("Suggested citation text: "),
                                        "Data source: Eurostat HICP database. Calculations and visualisations: own calculations using R Shiny app HICP Voyager."
                                )
                        )
                ))
                
        )
)
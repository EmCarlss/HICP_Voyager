library(shiny)
library(eurostat)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(bslib)

# Read eurostat_countries dataframe
additional_areas <- data.frame(
        code = c("EU", "EA", "BA", "XK", "RS", "MK", "AL", "ME", "TR", "US", "UK"),
        name = c(
                "European union", "Euro area", "Bosnia-Herzegovina", "Kosovo", "Serbia",
                "North Macedonia", "Albania", "Montenegro", "Turkey", "United States", "United Kingdom"
        ),
        label = c(
                "European union", "Euro area", "Bosnia-Herzegovina", "Kosovo", "Serbia",
                "North Macedonia", "Albania", "Montenegro", "Turkey", "United States", "United Kingdom"
        )
)

eurostat_countries <- rbind(eu_countries, efta_countries, additional_areas) %>%
        select(code, name) %>%
        arrange(name) %>%
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
                actionButton(paste0(prefix, "_baltic"), "Baltic", class = "quick-btn"),
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
                                lapply(badges, function(x) tags$span(class = "tab-badge", x))
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

ui <- fluidPage(
        theme = bs_theme(bootswatch = "minty"),
        tags$head(tags$link(rel = "icon", type = "image/png", href = "app_logo_favicon.png")),
        tags$style(HTML("
body { background: linear-gradient(180deg, #f8fbfa 0%, #fbfdfc 42%, #ffffff 100%); }
.app-header {
  max-width: 1250px;
  margin: 12px auto 10px auto;
  padding: 15px 22px;
  background: radial-gradient(circle at top left, rgba(63,143,122,0.075) 0, rgba(63,143,122,0.022) 35%, transparent 62%), linear-gradient(135deg, #f9fcfb 0%, #f3faf7 100%);
  border: 1px solid #dcebe6;
  border-radius: 19px;
  box-shadow: 0 4px 14px rgba(32,50,47,0.035);
  color: #263238;
}
.app-header-main { display: flex; justify-content: space-between; align-items: center; gap: 22px; flex-wrap: nowrap; }
.app-brand-block { display: flex; align-items: center; gap: 14px; min-width: 0; flex: 1 1 auto; }
.app-brand-text { min-width: 0; }
.app-logo-card {
  flex: 0 0 auto;
  width: 58px;
  height: 58px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 15px;
  background: rgba(255,255,255,0.74);
  border: 1px solid rgba(215,231,226,0.95);
  box-shadow: 0 6px 16px rgba(32,50,47,0.065);
  overflow: hidden;
}
.app-logo { width: 100%; height: 100%; object-fit: cover; padding: 0; display: block; }
.app-kicker { font-size: 11.5px; font-weight: 800; color: #3f8f7a; text-transform: uppercase; letter-spacing: 0.12em; margin-bottom: 3px; }
.app-title { margin: 0; font-size: 28px; font-weight: 800; letter-spacing: -0.035em; color: #20322f; line-height: 1.05; }
.app-subtitle { margin-top: 6px; margin-bottom: 0; max-width: 780px; font-size: 14.2px; line-height: 1.45; color: #526064; }
.app-header-badges { display: flex; gap: 8px; flex-wrap: wrap; justify-content: flex-end; flex: 0 0 auto; }
.app-version-pill, .app-meta-pill { display: inline-block; border-radius: 999px; padding: 6px 12px; font-size: 12px; font-weight: 700; white-space: nowrap; }
.app-version-pill { background: #3f8f7a; color: white; border: 1px solid #337968; }
.app-meta-pill { background: #ffffff; color: #34514b; border: 1px solid #d7e7e2; }
.navbar { max-width: 1250px; margin: 0 auto 14px auto !important; padding: 7px 12px !important; background: rgba(255,255,255,0.97) !important; border: 1px solid #e1e7e5 !important; border-radius: 17px !important; box-shadow: 0 2px 10px rgba(32,50,47,0.035); min-height: unset !important; }
.navbar .container-fluid, .navbar .container { padding-left: 0 !important; padding-right: 0 !important; }
.navbar-brand { display: none !important; }
.navbar-brand-custom { display: inline-flex; align-items: center; gap: 8px; }
.navbar-brand-icon-only { gap: 0; }
.navbar-logo { display: none; }
.navbar-brand-mark { display: inline-flex; align-items: center; justify-content: center; width: 26px; height: 26px; border-radius: 8px; background: #3f8f7a; color: white; font-size: 14px; font-weight: 800; }
.navbar-nav { gap: 6px; }
.navbar-nav .nav-link, .navbar-nav > li > a { border-radius: 999px !important; padding: 8px 15px !important; margin: 2px 4px !important; color: #3f4a4d !important; font-size: 14px !important; font-weight: 650 !important; border: 1px solid transparent !important; transition: all 0.15s ease-in-out; }
.navbar-nav .nav-link:hover, .navbar-nav > li > a:hover { background: #eef7f4 !important; color: #24584b !important; border-color: #d2e7df !important; }
.navbar-nav .nav-link.active, .navbar-nav > li.active > a, .navbar-nav > .active > a, .navbar-nav > .show > a { background: #3f8f7a !important; color: white !important; border-color: #337968 !important; box-shadow: 0 2px 6px rgba(63,143,122,0.25); }
.navbar-toggler { border: 1px solid #d7e7e2 !important; border-radius: 10px !important; padding: 6px 9px !important; }
.navbar-collapse { padding-top: 0; }
.plot-card {
  position: relative;
  background: transparent;
  border: none;
  border-radius: 0;
  box-shadow: none;
  padding: 0;
  min-height: 620px;
  overflow: visible;
}
.plot-card::before {
  content: 'Select settings and retrieve data to display chart';
  position: absolute;
  inset: 0;
  min-height: 360px;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 24px;
  text-align: center;
  color: #6d7a7e;
  font-size: 14px;
  font-weight: 600;
  letter-spacing: 0.01em;
  background: #ffffff;
  border: 1px dashed #d7e7e2;
  border-radius: 16px;
  pointer-events: none;
  z-index: 0;
}
.plot-card.has-plot::before,
.plot-card:has(.js-plotly-plot)::before { display: none; }
.plot-card > .html-widget-output,
.plot-card > .shiny-plot-output {
  position: relative;
  z-index: 1;
  min-height: 620px;
  height: 620px;
  width: 100%;
  overflow: visible;
}
.plot-card .js-plotly-plot {
  max-width: 100%;
  overflow: visible;
}
.plot-container .legend .traces .legendtext { white-space: normal !important; word-break: break-word !important; }
.quick-btn {
  background-color: #fbfdfc !important;
  color: #3f4a4d !important;
  border: 1px solid #d8e3e0 !important;
  border-radius: 10px !important;
  padding: 6px 12px !important;
  min-height: 32px;
  font-size: 12.8px !important;
  font-weight: 600 !important;
  box-shadow: inset 0 1px 0 rgba(255,255,255,0.75), 0 1px 2px rgba(32,50,47,0.025) !important;
  margin-right: 5px;
  margin-bottom: 7px;
  transition: all 0.14s ease-in-out;
}
.quick-btn:hover {
  background-color: #eef7f4 !important;
  border-color: #c8dfd8 !important;
  color: #24584b !important;
  transform: translateY(-1px);
}
.quick-btn:focus, .quick-btn:active { outline: none !important; box-shadow: 0 0 0 3px rgba(63,143,122,0.12) !important; }
.quick-btn-clear {
  background-color: #ffffff !important;
  color: #6c777a !important;
  border: 1px solid #d8e3e0 !important;
  border-radius: 10px !important;
  padding: 6px 12px !important;
  min-height: 32px;
  font-size: 12.8px !important;
  font-weight: 600 !important;
  box-shadow: none !important;
  margin-top: 4px;
}
.quick-btn-clear:hover { background-color: #f6faf8 !important; color: #4d5a5d !important; border-color: #cbdad6 !important; }
.app-tab-page { max-width: 1250px; margin: 0 auto; padding: 14px 8px 40px 8px; color: #263238; }
.tab-hero { background: rgba(255,255,255,0.82); border: 1px solid #e1ebe7; border-radius: 16px; padding: 16px 22px; margin-bottom: 16px; box-shadow: 0 2px 8px rgba(32,50,47,0.025), inset 0 1px 0 rgba(255,255,255,0.82); }
.tab-hero h2 { margin-top: 0; margin-bottom: 5px; font-size: 22px; font-weight: 750; letter-spacing: -0.02em; }
.tab-hero p { margin-bottom: 0; font-size: 13.8px; line-height: 1.5; max-width: 900px; }
.tab-badges { margin-top: 10px; }
.tab-badge { display: inline-block; background: #edf7f4; color: #24584b; border: 1px solid #d4e9e1; border-radius: 999px; padding: 4px 10px; font-size: 11.8px; font-weight: 650; margin-right: 6px; margin-bottom: 5px; }
.control-card { background: #ffffff; border: 1px solid #e1e7e5; border-radius: 16px; padding: 16px 18px; box-shadow: 0 5px 16px rgba(32,50,47,0.04); margin-bottom: 16px; }
.control-card h3 { margin-top: 0; margin-bottom: 12px; font-size: 16.5px; font-weight: 750; letter-spacing: -0.01em; }
.control-group { border-top: 1px solid #edf1f0; padding-top: 12px; margin-top: 12px; }
.control-group:first-of-type { border-top: none; padding-top: 0; margin-top: 0; }
.control-group-title { font-size: 12.4px; font-weight: 750; color: #34514b; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.06em; }
.control-note { color: #687579; font-size: 12.5px; line-height: 1.45; margin-top: 6px; margin-bottom: 0; }
.control-card label { color: #273437; font-weight: 600; }
.form-control, .selectize-input {
  border-color: #d8e3e0 !important;
  border-radius: 10px !important;
  box-shadow: none !important;
}
.form-control:focus, .selectize-input.focus {
  border-color: #9ecdc0 !important;
  box-shadow: 0 0 0 3px rgba(63,143,122,0.12) !important;
}
.checkbox label, .radio label { font-weight: 500; color: #3f4a4d; }

.primary-action-btn { width: 100%; background-color: #3f8f7a !important; color: white !important; border: 1px solid #337968 !important; border-radius: 10px !important; padding: 8px 12px !important; font-weight: 700 !important; margin-top: 10px; }
.primary-action-btn:hover { background-color: #347866 !important; }
.secondary-action-btn { width: 100%; background-color: #f8f9fa !important; color: #3f4a4d !important; border: 1px solid #d6d6d6 !important; border-radius: 10px !important; padding: 8px 12px !important; font-weight: 600 !important; margin-top: 8px; }
.secondary-action-btn:hover { background-color: #eef2f5 !important; }
.download-panel { margin-top: 14px; padding: 12px 14px; background: #f7fbf9; border: 1px solid #dcebe6; border-radius: 12px; font-size: 13px; font-weight: 600; }
.info-label { border-bottom: 1px dotted #6c7a7d; cursor: help; }
.help-page { max-width: 1100px; margin: 0 auto; padding: 22px 8px 45px 8px; color: #263238; }
.help-hero, .help-card, .help-callout { background: #ffffff; border: 1px solid #e1e7e5; border-radius: 16px; padding: 18px 20px; margin-bottom: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.035); }
.help-hero { background: linear-gradient(135deg, #f7fbf9 0%, #eef7f4 100%); }
.help-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(285px, 1fr)); gap: 16px; margin-top: 16px; margin-bottom: 20px; }
.help-card h3, .help-card h4 { margin-top: 0; font-weight: 700; }
.help-card p, .help-card li { font-size: 14px; line-height: 1.55; }
.help-card ul { padding-left: 19px; margin-bottom: 0; }
.help-section-title { margin-top: 30px; margin-bottom: 12px; font-size: 22px; font-weight: 700; }
.help-badge { display: inline-block; background: #e8f4f0; color: #24584b; border: 1px solid #cfe6dd; border-radius: 999px; padding: 4px 10px; font-size: 12px; font-weight: 600; margin-bottom: 10px; }
.help-callout { background: #fffaf0; border-color: #f0dfb5; }
.help-muted { color: #687579; font-size: 13px; }
.help-table { width: 100%; border-collapse: collapse; margin-top: 10px; font-size: 14px; }
.help-table th { background: #f4f8f7; border-bottom: 1px solid #dce5e2; padding: 10px; text-align: left; font-weight: 700; }
.help-table td { border-bottom: 1px solid #edf1f0; padding: 10px; vertical-align: top; }
@media (max-width: 900px) {
  .app-header-main { flex-wrap: wrap; }
  .app-header-badges { justify-content: flex-start; margin-left: 84px; }
}
@media (max-width: 768px) {
  .app-header { margin: 10px 8px 10px 8px; padding: 16px 16px; }
  .app-title { font-size: 25px; }
  .app-brand-block { gap: 12px; align-items: flex-start; }
  .app-logo-card { width: 52px; height: 52px; border-radius: 14px; }
  .app-header-badges { justify-content: flex-start; margin-left: 64px; }
  .navbar { margin-left: 8px !important; margin-right: 8px !important; padding: 7px 8px !important; }
  .navbar-nav .nav-link, .navbar-nav > li > a { border-radius: 10px !important; margin: 3px 0 !important; }
  .tab-hero { padding: 15px 16px; }
  .plot-card { min-height: 480px; padding: 0; }
  .plot-card::before { min-height: 300px; }
  .plot-card > .html-widget-output,
  .plot-card > .shiny-plot-output { min-height: 480px; height: 480px; }
}
@media (max-width: 520px) {
  .app-brand-block { align-items: center; }
  .app-logo-card { width: 50px; height: 50px; border-radius: 13px; }
  .app-header-badges { margin-left: 0; }
  .app-subtitle { font-size: 13.5px; }
}
")),
        tags$script(HTML("
(function() {
  function syncPlotCardHeight(root) {
    if (!root) return;
    var gd = root.classList && root.classList.contains('js-plotly-plot') ? root : root.querySelector && root.querySelector('.js-plotly-plot');
    if (!gd) return;

    var output = gd.closest('.html-widget-output, .shiny-plot-output');
    var card = gd.closest('.plot-card');
    var layoutHeight = gd._fullLayout && gd._fullLayout.height ? gd._fullLayout.height : 0;
    var boxHeight = gd.getBoundingClientRect ? gd.getBoundingClientRect().height : 0;
    var plotHeight = Math.max(layoutHeight, boxHeight, gd.offsetHeight || 0);
    if (!plotHeight) return;

    var reservedHeight = Math.ceil(plotHeight + 18);

    if (output) {
      output.style.height = reservedHeight + 'px';
      output.style.minHeight = reservedHeight + 'px';
      output.style.overflow = 'visible';
    }
    if (card) {
      card.classList.add('has-plot');
      card.style.minHeight = reservedHeight + 'px';
      card.style.overflow = 'visible';
    }
  }

  function syncAllPlotCards() {
    document.querySelectorAll('.plot-card .js-plotly-plot').forEach(syncPlotCardHeight);
  }

  document.addEventListener('plotly_afterplot', function(e) {
    window.requestAnimationFrame(function() { syncPlotCardHeight(e.target); });
  }, true);

  document.addEventListener('shiny:value', function(e) {
    window.setTimeout(function() {
      syncPlotCardHeight(e.target);
      if (e.target && e.target.querySelectorAll) {
        e.target.querySelectorAll('.js-plotly-plot').forEach(syncPlotCardHeight);
      }
      syncAllPlotCards();
    }, 120);
  }, true);

  var observer = new MutationObserver(function() {
    window.setTimeout(syncAllPlotCards, 120);
  });

  document.addEventListener('DOMContentLoaded', function() {
    observer.observe(document.body, { childList: true, subtree: true });
    syncAllPlotCards();
  });

  window.addEventListener('resize', function() {
    window.setTimeout(syncAllPlotCards, 80);
  });
})();
")),
        tags$div(
                class = "app-header",
                tags$div(
                        class = "app-header-main",
                        tags$div(
                                class = "app-brand-block",
                                tags$div(
                                        class = "app-logo-card",
                                        tags$img(src = "app_logo.png", class = "app-logo", alt = "HICP Voyager logo")
                                ),
                                tags$div(
                                        class = "app-brand-text",
                                        tags$div(class = "app-kicker", "European price statistics dashboard"),
                                        tags$h1(class = "app-title", "HICP Voyager"),
                                        tags$p(
                                                class = "app-subtitle",
                                                "Analyse HICP indices, HICP-CT indices, inflation rates, contributions, seasonality and weights across Europe."
                                        )
                                )
                        ),
                        tags$div(
                                class = "app-header-badges",
                                tags$span(class = "app-version-pill", "Version 2.4"),
                                tags$span(class = "app-meta-pill", "Source: Eurostat HICP data")
                        )
                )
        ),
        navbarPage(
                title = tags$span(class = "navbar-brand-custom navbar-brand-icon-only", ""),
                id = "main_tabs",
                collapsible = TRUE,
                tabPanel(
                        "Index",
                        tags$div(
                                class = "app-tab-page",
                                tab_hero(
                                        "Index",
                                        "Compare HICP and HICP-CT index developments across countries and product categories. Rebase series to a common reference period, adjust lower-level indices for all-items inflation or compare selected country indices with the corresponding EU27 index.",
                                        badges = c("HICP", "HICP-CT", "Rebasing", "Relative price index", "EU27 comparison")
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
                                                                        choices = c("HICP" = "HICP", "HICP-CT" = "HICP_CT"),
                                                                        selected = "HICP"
                                                                ),
                                                                note = "If both HICP and HICP-CT are selected, only one product category can be shown."
                                                        ),
                                                        
                                                        control_group(
                                                                "Display options",
                                                                checkboxInput("index_backdrop_eu", "Other EU countries as backdrop", value = TRUE),
                                                                checkboxInput(
                                                                        "index_adjust_total",
                                                                        label = tags$span(
                                                                                class = "info-label",
                                                                                title = paste(
                                                                                        "Divides each selected lower-level index by the all-items index",
                                                                                        "for the same country, month and measure, then multiplies by 100.",
                                                                                        "This option cannot be combined with the EU adjustment."
                                                                                ),
                                                                                "Adjust lower level index with total inflation"
                                                                        ),
                                                                        value = FALSE
                                                                ),
                                                                conditionalPanel(
                                                                        condition = "!input.countries || input.countries.indexOf('EU') == -1",
                                                                        checkboxInput(
                                                                                "index_adjust_eu27",
                                                                                label = tags$span(
                                                                                        class = "info-label",
                                                                                        title = paste(
                                                                                                "Divides each selected country index by the corresponding EU index",
                                                                                                "for the same product category, month and measure, then multiplies by 100.",
                                                                                                "This option is hidden when EU is selected as a country and cannot be combined with the total-inflation adjustment."
                                                                                        ),
                                                                                        "Adjust selected aggregate with EU index"
                                                                                ),
                                                                                value = FALSE
                                                                        )
                                                                )
                                                        ),
                                                        checkboxInput(
                                                                "index_show_title",
                                                                "Show plot title",
                                                                value = TRUE
                                                        ),
                                                        actionButton("update", "Retrieve data", class = "primary-action-btn"),
                                                        control_group(
                                                                "Reference period",
                                                                radioButtons(
                                                                        "period_type",
                                                                        "Index reference period",
                                                                        choices = c("Full year", "Month"),
                                                                        selected = "Full year"
                                                                ),
                                                                selectInput("select_years", "Select index reference period", choices = "", multiple = FALSE),
                                                                actionButton("rebase", "Update plot", class = "secondary-action-btn"),
                                                                note = "The selected reference period is set to 100 for all displayed series."
                                                        ),
                                                        download_panel("output.plot", "downloadData")
                                                )
                                        ),
                                        column(width = 9, tags$div(class = "plot-card", plotlyOutput("plot", height = "620px")))
                                )
                        )
                ),
                tabPanel(
                        "M/M-1",
                        tags$div(
                                class = "app-tab-page",
                                tab_hero(
                                        "Monthly rate of change, M/M-1",
                                        "Analyse monthly price changes and decompose them into component contributions. Choose whether the index calculations are based on HICP or HICP-CT.",
                                        badges = c("HICP", "HICP-CT", "Monthly rate", "Ribe contributions", "Administered prices")
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
                                                                        choices = c("One graph per period" = "period", "One graph per country" = "country"),
                                                                        selected = "period"
                                                                )
                                                        ),
                                                        control_group(
                                                                "Index measure",
                                                                radioButtons(
                                                                        "mr_measure",
                                                                        NULL,
                                                                        choices = c("HICP" = "HICP", "HICP-CT" = "HICP_CT"),
                                                                        selected = "HICP",
                                                                        inline = TRUE
                                                                ),
                                                                note = "Only one measure is shown at a time."
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
                                                                        choices = c("ECOICOP ver. 2" = "ecoicop", "Special aggregates" = "sa", "Administered prices" = "ap"),
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
                                                                                "selected higher aggregate" = "selected higher aggregate",
                                                                                "all-items selected index" = "all-items HICP"
                                                                        ),
                                                                        selected = "selected higher aggregate"
                                                                ),
                                                                note = "Choose whether contributions should sum to the selected aggregate or to all-items for the selected index measure."
                                                        ),
                                                        actionButton("update_mr", "Retrieve data", class = "primary-action-btn"),
                                                        control_group(
                                                                "Periods",
                                                                selectInput("select_period_mr", "Select months to compare", choices = "", multiple = TRUE)
                                                        ),
                                                        download_panel("output.plot_mr", "downloadData_mr")
                                                )
                                        ),
                                        column(width = 9, tags$div(class = "plot-card", plotlyOutput("plot_mr", height = "620px")))
                                )
                        )
                ),
                tabPanel(
                        "M/M-12",
                        tags$div(
                                class = "app-tab-page",
                                tab_hero(
                                        "Annual rate of change, M/M-12",
                                        "Analyse annual inflation rates and decompose them into component contributions. Choose whether the index calculations are based on HICP or HICP-CT.",
                                        badges = c("HICP", "HICP-CT", "Annual rate", "Ribe contributions", "Administered prices")
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
                                                                        choices = c("One graph per period" = "period", "One graph per country" = "country"),
                                                                        selected = "period"
                                                                )
                                                        ),
                                                        control_group(
                                                                "Index measure",
                                                                radioButtons(
                                                                        "ar_measure",
                                                                        NULL,
                                                                        choices = c("HICP" = "HICP", "HICP-CT" = "HICP_CT"),
                                                                        selected = "HICP",
                                                                        inline = TRUE
                                                                ),
                                                                note = "Only one measure is shown at a time."
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
                                                                        choices = c("ECOICOP ver. 2" = "ecoicop", "Special aggregates" = "sa", "Administered prices" = "ap"),
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
                                                                                "selected higher aggregate" = "selected higher aggregate",
                                                                                "all-items selected index" = "all-items HICP"
                                                                        ),
                                                                        selected = "selected higher aggregate"
                                                                ),
                                                                note = "Use all-items selected index when you want to see how the selected components contribute to total inflation for HICP or HICP-CT."
                                                        ),
                                                        actionButton("update_ar", "Retrieve data", class = "primary-action-btn"),
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
                                                                        selectInput("select_period_ar", "Select month", choices = "", multiple = FALSE)
                                                                )
                                                        ),
                                                        download_panel("output.plot_ar", "downloadData_ar")
                                                )
                                        ),
                                        column(width = 9, tags$div(class = "plot-card", plotlyOutput("plot_ar", height = "620px")))
                                )
                        )
                ),
                tabPanel(
                        "Seasonality",
                        tags$div(
                                class = "app-tab-page",
                                tab_hero(
                                        "Seasonality",
                                        "Compare within-year price patterns by setting December of the previous year to 100.",
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
                                                        actionButton("update_se", "Retrieve data", class = "primary-action-btn"),
                                                        control_group(
                                                                "Years",
                                                                selectInput("select_years_se", "Select years to compare", choices = "", multiple = TRUE),
                                                                note = "Each selected year is shown as a separate line. December of the previous year is added as the common reference point."
                                                        ),
                                                        download_panel("output.plot_se", "downloadData_se")
                                                )
                                        ),
                                        column(width = 9, tags$div(class = "plot-card", plotlyOutput("plot_se", height = "620px")))
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
                                                                        choices = c("One graph per year" = "year", "One graph per country" = "country"),
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
                                                                "Classification and aggregate",
                                                                radioButtons(
                                                                        "classification_w",
                                                                        "Classification",
                                                                        choices = c(
                                                                                "ECOICOP ver. 2" = "ecoicop",
                                                                                "Special aggregates" = "sa",
                                                                                "Administered prices" = "ap"
                                                                        ),
                                                                        selected = "ecoicop"
                                                                ),
                                                                selectInput(
                                                                        "coicop_w",
                                                                        "Select product category",
                                                                        choices = setNames(label_set$coicop18_code, label_set$code_label),
                                                                        multiple = FALSE
                                                                ),
                                                                note = "If a parent aggregate is selected, the chart shows its direct lower-level components where available."
                                                        ),
                                                        actionButton("update_w", "Retrieve data", class = "primary-action-btn"),
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
                                        column(width = 9, tags$div(class = "plot-card", plotlyOutput("plot_w", height = "620px")))
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
                                                tags$h2("HICP Voyager 2.4"),
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
                                                                "COICOP aggregates, special aggregates, administered-price aggregates and time periods."
                                                        ),
                                                        tags$ul(
                                                                tags$li("Compare HICP and HICP-CT index developments."),
                                                                tags$li("Rebase index series to a common reference year or month."),
                                                                tags$li("Analyse monthly and annual rates of change."),
                                                                tags$li("Calculate contributions to selected aggregates or to all-items selected index."),
                                                                tags$li("Compare countries with the EU27 index as a denominator on the Index tab."),
                                                                tags$li("Study seasonality and country differences in expenditure weights.")
                                                        )
                                                ),

                                                tags$div(
                                                        class = "help-card",
                                                        tags$h3("Recommended workflow"),
                                                        tags$ul(
                                                                tags$li("Select one or more countries."),
                                                                tags$li("Select a product category or aggregate."),
                                                                tags$li("Choose classification where applicable: ECOICOP, special aggregates or administered prices."),
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
                                                                "Ordinary HICP index data are retrieved from ", tags$code("prc_hicp_minr"),
                                                                ". HICP-CT index data are retrieved from ", tags$code("prc_hicp_ct"),
                                                                ". Weights are retrieved from ", tags$code("prc_hicp_iw"), "."
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
                                                        tags$tr(tags$th("Function"), tags$th("Description")),
                                                        tags$tr(
                                                                tags$td(tags$strong("Country group quick selection")),
                                                                tags$td("Select predefined groups such as EU, Euro area, Nordic, Baltic, Balkan or Benelux.")
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
                                                                tags$td(tags$strong("Index measure")),
                                                                tags$td("Choose HICP, HICP-CT, or both. If both HICP and HICP-CT are selected, only one COICOP aggregate can be shown.")
                                                        ),
                                                        tags$tr(
                                                                tags$td(tags$strong("Other EU countries as backdrop")),
                                                                tags$td("Shows non-selected EU countries in grey as contextual background series. This makes it easier to compare selected countries against the broader EU distribution.")
                                                        ),
                                                        tags$tr(
                                                                tags$td(tags$strong("Adjust lower level index with total inflation")),
                                                                tags$td(
                                                                        "Divides each selected lower-level index by the all-items index for the same country, month and selected measure, then multiplies by 100. ",
                                                                        "This creates a relative price index and can be useful when comparing product-level price developments across countries with different overall inflation environments."
                                                                )
                                                        ),
                                                        tags$tr(
                                                                tags$td(tags$strong("Compare selected aggregate with EU27 index")),
                                                                tags$td(
                                                                        "Divides each selected country series by the corresponding EU27 index for the same aggregate, month and measure, then multiplies by 100. ",
                                                                        "This makes it easier to see whether a country's price development has been stronger or weaker than the EU27 benchmark. ",
                                                                        "This option is not available when EU27 itself is selected as a country."
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
                                                        "When the total-inflation adjustment is used, the graph should be interpreted as relative price development within each country. ",
                                                        "When the EU27 adjustment is used, the graph should be interpreted as the selected country aggregate relative to the same EU27 aggregate. ",
                                                        "The two adjustments cannot be combined, because the resulting index would be difficult to interpret."
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
                                                                tags$li(tags$strong("Index measure:"), " choose HICP or HICP-CT. Only one measure is shown at a time."),
                                                                tags$li(tags$strong("Classification:"), " choose ECOICOP, special aggregates or administered prices."),
                                                                tags$li(tags$strong("One graph per period:"), " compares countries for one or more selected months."),
                                                                tags$li(tags$strong("One graph per country:"), " compares selected months within each country."),
                                                                tags$li(tags$strong("Selected higher aggregate:"), " calculates contributions to the selected parent aggregate."),
                                                                tags$li(tags$strong("All-items selected index:"), " calculates contributions to all-items HICP or all-items HICP-CT, depending on selected measure.")
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
                                                                tags$li(tags$strong("Index measure:"), " choose HICP or HICP-CT. Only one measure is shown at a time."),
                                                                tags$li(tags$strong("Classification:"), " choose ECOICOP, special aggregates or administered prices."),
                                                                tags$li(tags$strong("One graph per period:"), " compares countries for a selected month."),
                                                                tags$li(tags$strong("One graph per country:"), " shows the development over time by country."),
                                                                tags$li(tags$strong("Selected higher aggregate:"), " shows contributions to the selected aggregate."),
                                                                tags$li(tags$strong("All-items selected index:"), " shows contributions to total inflation for the selected measure.")
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
                                                                "The Weights tab shows HICP expenditure weights for selected countries and product categories. It can use ECOICOP, special aggregates or administered prices as the selected classification."
                                                        ),
                                                        tags$ul(
                                                                tags$li(tags$strong("One graph per year:"), " compares countries within selected years."),
                                                                tags$li(tags$strong("One graph per country:"), " shows the development of weights over time for each country."),
                                                                tags$li("Weights are shown in per mille, where all-items HICP equals 1000."),
                                                                tags$li("The classification selector controls which hierarchy is used for the selected aggregate and its direct lower-level components.")
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
                                                        "The M/M-1, M/M-12 and Weights tabs allow you to choose between three classification structures. ",
                                                        "For M/M-1 and M/M-12, the same contribution logic is used for all three; only the hierarchy of components changes. For Weights, the same hierarchy controls which direct lower-level weights are displayed."
                                                ),
                                                tags$table(
                                                        class = "help-table",
                                                        tags$tr(tags$th("Classification"), tags$th("Hierarchy"), tags$th("Use case")),
                                                        tags$tr(
                                                                tags$td(tags$strong("ECOICOP ver. 2")),
                                                                tags$td("Standard COICOP product hierarchy."),
                                                                tags$td("Use this for ordinary product-category analysis of HICP or HICP-CT.")
                                                        ),
                                                        tags$tr(
                                                                tags$td(tags$strong("Special aggregates")),
                                                                tags$td("Examples: energy, services, food, processed food, unprocessed food and non-energy industrial goods."),
                                                                tags$td("Use this for broader analytical aggregates that cut across parts of the ordinary COICOP structure.")
                                                        ),
                                                        tags$tr(
                                                                tags$td(tags$strong("Administered prices")),
                                                                tags$td("APF Fully administered prices; APM Mainly administered prices; TOT_X_AP Overall index excluding administered prices; CP00 Total."),
                                                                tags$td("Use this to decompose total inflation into administered-price categories and the remaining overall index excluding administered prices.")
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
                                                tags$p(tags$strong("Formula: "), tags$code("rebased index = original index / reference-period index * 100")),

                                                tags$hr(),

                                                tags$h3("Total-inflation adjustment"),
                                                tags$p(
                                                        "The total-inflation adjustment transforms a lower-level index into a relative price index. ",
                                                        "The selected index is divided by the all-items index for the same country, month and measure."
                                                ),
                                                tags$p(tags$strong("Formula: "), tags$code("relative price index = lower-level index / all-items selected index * 100")),

                                                tags$hr(),

                                                tags$h3("EU27 adjustment"),
                                                tags$p(
                                                        "The EU27 adjustment compares each selected country with the corresponding EU27 series. ",
                                                        "The selected country index is divided by the EU27 index for the same aggregate, month and measure."
                                                ),
                                                tags$p(tags$strong("Formula: "), tags$code("country relative to EU27 = country index / EU27 index * 100")),
                                                tags$p(
                                                        "The resulting series is then rebased using the same reference-period logic as ordinary index series. ",
                                                        "This makes the graph show deviations from the EU27 development rather than the absolute index level."
                                                ),

                                                tags$hr(),

                                                tags$h3("Administered prices hierarchy"),
                                                tags$p(
                                                        "For the administered prices classification, CP00 is the total. The direct lower-level components are:"
                                                ),
                                                tags$ul(
                                                        tags$li(tags$strong("APF:"), " Fully administered prices."),
                                                        tags$li(tags$strong("APM:"), " Mainly administered prices."),
                                                        tags$li(tags$strong("TOT_X_AP:"), " Overall index excluding administered prices.")
                                                ),
                                                tags$p(
                                                        "When CP00 is selected with contribution target set to selected higher aggregate, the contributions from APF, APM and TOT_X_AP are calculated analogously to the other special-aggregate hierarchies."
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
                                                        tags$tr(tags$th("Symbol"), tags$th("Meaning")),
                                                        tags$tr(tags$td("\\(j\\)"), tags$td("Lower-level component, for example a COICOP subclass, special aggregate or administered-price component.")),
                                                        tags$tr(tags$td("\\(A\\)"), tags$td("Target aggregate. This is either the selected higher aggregate or all-items selected index, depending on selected contribution mode.")),
                                                        tags$tr(tags$td("\\(I_{j,t}\\)"), tags$td("Index for component \\(j\\) in month \\(t\\).")),
                                                        tags$tr(tags$td("\\(I_{A,t}\\)"), tags$td("Index for the target aggregate \\(A\\) in month \\(t\\).")),
                                                        tags$tr(tags$td("\\(w_{j,y}^{A}\\)"), tags$td("Annual weight for component \\(j\\), expressed in per mille relative to the target aggregate \\(A\\).")),
                                                        tags$tr(tags$td("\\(m,y\\)"), tags$td("Month \\(m\\) in year \\(y\\)."))
                                                ),

                                                tags$h4("Weight used for the selected contribution target"),
                                                tags$p("If contributions are calculated to the selected higher aggregate, the component weights are rescaled to that aggregate:"),
                                                tags$div("$$", "w_{j,y}^{A} = \\frac{w_{j,y}^{00}}{w_{A,y}^{00}} \\times 1000", "$$"),
                                                tags$p("If contributions are calculated to all-items selected index, the ordinary all-items HICP weights are used:"),
                                                tags$div("$$", "w_{j,y}^{A} = w_{j,y}^{00}", "$$"),

                                                tags$h4("Monthly contribution, M/M-1"),
                                                tags$p("First, the annual weight is price-updated to the previous month:"),
                                                tags$div("$$", "\\widetilde{w}_{j,t-1}^{A} = ", "\\frac{w_{j,y}^{A}}{1000} \\times ", "\\frac{I_{j,t-1}}{I_{A,t-1}}", "$$"),
                                                tags$p("The monthly rate of change for component \\(j\\) is:"),
                                                tags$div("$$", "r_{j,t}^{m} = ", "\\left( \\frac{I_{j,t}}{I_{j,t-1}} - 1 \\right) \\times 100", "$$"),
                                                tags$p("The monthly contribution from component \\(j\\) is then:"),
                                                tags$div("$$", "C_{j,t}^{m} = ", "\\widetilde{w}_{j,t-1}^{A} \\times r_{j,t}^{m}", "$$"),
                                                tags$p("This is the formula used in the M/M-1 tab. In the graph, contributions are shown in percentage points."),

                                                tags$h4("Annual contribution, M/M-12"),
                                                tags$p(
                                                        "For annual rates of change, the Ribe contribution is calculated in two parts. ",
                                                        "The first part captures the price change from December of the previous year to the current month. ",
                                                        "The second part adjusts for the price change between the same month of the previous year and December of the previous year."
                                                ),
                                                tags$div("$$", "C_{j,m,y}^{12} = ", "100 \\times ", "\\frac{I_{A,12,y-1}}{I_{A,m,y-1}} \\times ", "\\frac{w_{j,y}^{A}}{1000} \\times ", "\\frac{I_{j,m,y} - I_{j,12,y-1}}{I_{j,12,y-1}}", "$$"),
                                                tags$div("$$", "\\quad + ", "100 \\times ", "\\frac{I_{A,12,y-2}}{I_{A,m,y-1}} \\times ", "\\frac{w_{j,y-1}^{A}}{1000} \\times ", "\\frac{I_{j,12,y-1} - I_{j,m,y-1}}{I_{j,12,y-2}}", "$$"),
                                                tags$p(
                                                        "The first term uses the current year's weight and the movement from December Y-1 to the current month. ",
                                                        "The second term uses the previous year's weight and adjusts for the movement between month M in Y-1 and December Y-1."
                                                ),

                                                tags$h4("Aggregate rate of change"),
                                                tags$p("The aggregate monthly rate shown in the M/M-1 tab is:"),
                                                tags$div("$$", "r_{A,t}^{m} = ", "\\left( \\frac{I_{A,t}}{I_{A,t-1}} - 1 \\right) \\times 100", "$$"),
                                                tags$p("The aggregate annual rate shown in the M/M-12 tab is:"),
                                                tags$div("$$", "r_{A,m,y}^{12} = ", "\\left( \\frac{I_{A,m,y}}{I_{A,m,y-1}} - 1 \\right) \\times 100", "$$"),
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
                                                        tags$p("Monthly rates show short-term price changes. Annual rates show price changes compared with the same month one year earlier.")
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
                                                        tags$li("HICP-CT and administered-price series may have more limited country or aggregate coverage than ordinary HICP series."),
                                                        tags$li("Comparisons across countries should take into account differences in consumption weights, tax systems, market structures and data availability."),
                                                        tags$li("The total-inflation adjustment and EU27 adjustment are analytical transformations and should not be interpreted as official Eurostat indicators.")
                                                )
                                        ),

                                        tags$div(
                                                class = "help-callout",
                                                tags$strong("Suggested citation text: "),
                                                "Data source: Eurostat HICP database. Calculations and visualisations: own calculations using R Shiny app HICP Voyager."
                                        )
                                )
                        )
                )
        )
)

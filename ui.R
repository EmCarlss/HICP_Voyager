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

ui <- fluidPage(theme = bs_theme(bootswatch = "minty"),
                tags$style(HTML("
    .navbar {
      min-height: 15px;
      padding-top: 0px;
      padding-bottom: 0px;
    }
    .navbar-brand {
      padding-top: 0px;
      padding-bottom: 0px;
    }
    .nav-link {
      padding-top: 3px;
      padding-bottom: 3px;
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
  ")),        
        tags$div(
                tags$h1("HICP Voyager 2.1", style = "text-align: left; margin-bottom: 10px; margin-top: 10px;font-size: 20px;")
        ),
        navbarPage(
                title = "",
                tabPanel("Index",
                         fluidRow(
                                 column(width = 3,
                                        country_group_buttons("index"),
                                        selectInput("countries", "Custom countries selection", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicops", "Select product categories:", choices = setNames(coicop_set$coicop18_code, coicop_set$code_label), multiple = TRUE),
                                        checkboxGroupInput(
                                                "index_measure",
                                                "Select index measure:",
                                                choices = c(
                                                        "HICP" = "HICP",
                                                        "HICP-CT" = "HICP_CT"
                                                ),
                                                selected = "HICP"
                                        ),
                                        br(),
                                        p(" "),
                                        checkboxInput(
                                                "index_backdrop_eu",
                                                "Other EU countries as backdrop",
                                                value = TRUE
                                        ),
                                        actionButton("update", "Retrieve data"),
                                        br(),
                                        p(" "),
                                        br(),
                                        radioButtons("period_type", "Index reference period:", choices = c("Full year", "Month"), selected = "Full year"),
                                        p(" "),
                                        condition = "update",selectInput("select_years", "Select index reference period:", choices = "", multiple = FALSE),
                                        actionButton("rebase", "Update plot"),
                                        br(),
                                        p(" "),
                                        br(),
                                        conditionalPanel(
                                                condition = "output.plot",
                                                downloadLink('downloadData', 'Download data')
                                        )
                                 ),
                                 column(width = 8, div(style = "max-width: 700px; width: 100%;",
                                        plotlyOutput("plot")))
                                )
                ),
                tabPanel("M/M-1",
                         fluidRow(
                                 column(width = 3,
                                        radioButtons(
                                                "mr_view",
                                                "Display mode:",
                                                choices = c("One graph per period" = "period",
                                                            "One graph per country" = "country"),
                                                selected = "period"
                                        ),
                                        country_group_buttons("mr"),
                                        selectInput(
                                                "countries_mr",
                                                "Custom countries selection",
                                                choices = setNames(eurostat_countries$code, eurostat_countries$name),
                                                multiple = TRUE
                                        ),
                                        
                                        radioButtons(
                                                "classification_mr",
                                                "Classification:",
                                                choices = c(
                                                        "ECOICOP ver. 2" = "ecoicop",
                                                        "Special aggregats" = "sa"
                                                ),
                                                selected = "ecoicop"
                                        ),
                                        
                                        selectInput(
                                                "coicop_mr",
                                                "Select product category:",
                                                choices = setNames(label_set$coicop18_code, label_set$code_label),
                                                multiple = FALSE
                                        ),
                                        br(),
                                        radioButtons("contribution_type_mr", "Contribution to", choices = c("selected higher aggregate", "all-items HICP"), selected = "selected higher aggregate"),
                                        br(),
                                        actionButton("update_mr", "Retrieve data"), 
                                        br(),
                                        br(),
                                        selectInput("select_period_mr", "Select months to compare:", choices = "", multiple = TRUE),
                                        p(" "),
                                        p(" "),
                                        br(),
                                        p(" "),
                                        br(),
                                        conditionalPanel(
                                                condition = "output.plot_mr",
                                                downloadLink('downloadData_mr', 'Download data')
                                        )
                                 ),
                                 column(width = 8,
                                        plotlyOutput("plot_mr")
                                 )
                         )
                ),
                tabPanel("M/M-12",
                         fluidRow(
                                 column(width =3,
                                        radioButtons(
                                                "ar_view",
                                                "Display mode:",
                                                choices = c("One graph per period" = "period",
                                                            "One graph per country" = "country"),
                                                selected = "period"
                                        ),
                                        country_group_buttons("ar"),
                                        selectInput(
                                                "countries_ar",
                                                "Custom countries selection:",
                                                choices = setNames(eurostat_countries$code, eurostat_countries$name),
                                                multiple = TRUE
                                        ),
                                        
                                        radioButtons(
                                                "classification_ar",
                                                "Classification:",
                                                choices = c(
                                                        "ECOICOP ver. 2" = "ecoicop",
                                                        "Special aggregats" = "sa"
                                                ),
                                                selected = "ecoicop"
                                        ),
                                        
                                        selectInput(
                                                "coicop_ar",
                                                "Select product category:",
                                                choices = setNames(label_set$coicop18_code, label_set$code_label),
                                                multiple = FALSE
                                        ),
                                        br(),
                                        radioButtons("contribution_type_ar", "Contribution to", choices = c("selected higher aggregate", "all-items HICP"), selected = "selected higher aggregate"),
                                        br(),
                                        actionButton("update_ar", "Retrieve data"), 
                                        br(),
                                        br(),
                                        
                                        p(" "),
                                        conditionalPanel(
                                                condition = "output.plot_ar && input.ar_view == 'country'",
                                                sliderInput(
                                                        "range_slider",
                                                        "Select period:",
                                                        min = 2000,
                                                        max = 2023,
                                                        value = c(2015, 2023),
                                                        ticks = FALSE,
                                                        sep = ""
                                                )
                                        ),
                                        
                                        conditionalPanel(
                                                condition = "output.plot_ar && input.ar_view == 'period'",
                                                selectInput(
                                                        "select_period_ar",
                                                        "Select month:",
                                                        choices = "",
                                                        multiple = FALSE
                                                )
                                        ),
                                        p(" "),
                                        br(),
                                        p(" "),
                                        br(),
                                        conditionalPanel(
                                                condition = "output.plot_ar",
                                                downloadLink('downloadData_ar', 'Download data')
                                        )
                                 ),
                                 column(width = 8,
                                        plotlyOutput("plot_ar")
                                 )
                         )
                ),
                tabPanel("Seasonality",
                         fluidRow(
                                 column(width =3,
                                        country_group_buttons("se"),
                                        selectInput("countries_se", "Select countries:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicop_se", "Select product category:", choices = setNames(coicop_set$coicop18_code, coicop_set$code_label), multiple = FALSE),
                                        
                                        br(),
                                        actionButton("update_se", "Retrieve data"), 
                                        br(),
                                        br(),
                                        
                                        p(" "),
                                        br(),
                                        selectInput("select_years_se", "Select years to compare:", choices = "", multiple = TRUE),
                                        p(" "),
                                        br(),
                                        p(" "),
                                        br(),
                                        conditionalPanel(
                                                condition = "output.plot_se",
                                                downloadLink('downloadData_se', 'Download data')
                                        )
                                 ),
                                 column(width = 8,
                                        plotlyOutput("plot_se")
                                 )
                         )
                ),
                tabPanel("Weights",
                         fluidRow(
                                 column(width = 3,
                                        radioButtons(
                                                "weights_view",
                                                "Display mode:",
                                                choices = c("One graph per year" = "year",
                                                            "One graph per country" = "country"),
                                                selected = "year"
                                        ),
                                        country_group_buttons("weights"),
                                        selectInput("countries_w", "Countries custom selection:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicop_w", "Select product category:", choices = setNames(label_set$coicop18_code, label_set$code_label), multiple = FALSE),
                                        actionButton("update_w", "Retrieve data"), 
                                        br(),
                                        p(" "),
                                        p(" "),
                                        conditionalPanel(
                                                condition = "output.plot_w",
                                                sliderInput("range_slider_w", "Select period:", min = 2000, max = 2023, value = c(2015, 2023), ticks=FALSE, sep="")
                                        ),
                                        p(" "),
                                        br(),
                                        p(" "),
                                        br(),
                                        conditionalPanel(
                                                condition = "output.plot_w",
                                                downloadLink('downloadData_w', 'Download data')
                                        )
                                 ),
                                 column(width = 8,
                                        plotlyOutput("plot_w")
                                 )
                         )
                ),
                
                
                tabPanel("Help",
                         h5("Navigate seamlessly in the universe of european price statistics"),
                         br(),p("This app enables to produce graphs for the all-items level or any subcomponent(s) in the european HICP (Harmonized Index of Consumer Prices). Currently graphs for indices, weights and annual rates can be produced.",
                                br(),p(" "),"For the Index tab, users can set a new reference period (month or year), common for all selected index series.",br(),p(" "),
                                "For the M/M-12 (annual rates of change) and M/M-1 (monthly rates of change), Ribe contributions are calculated for the level directly below the selected aggregate (following formula in Eurostat's HICP Methodological manual, p 278-280)."),
                         br(),p(" "), "Data is retrieved using the 'eurostat' package from the Eurostat HICP database. Additional calculations are performed where needed." 
                         
                         )
                
        )
)
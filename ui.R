library(shiny)
library(eurostat)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(bslib)

# Read eurostat_countries dataframe
eu_areas <- data.frame(code = c("EU", "EA"), name = c("European union", "Euro area"), label = c("European union", "Euro area"))
eurostat_countries <- rbind(eu_countries, efta_countries, eu_areas) %>% 
        select(code, name) %>% arrange(name) %>% 
        filter(name != "Liechtenstein")

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
  ")),        
        tags$div(
                tags$h1("HICP Voyager 1.4", style = "text-align: left; margin-bottom: 10px; margin-top: 10px;font-size: 20px;")
        ),
        navbarPage(
                title = "",
                tabPanel("Index",
                         fluidRow(
                                 column(width = 3,
                                        selectInput("countries", "Select countries:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicops", "Select product categories:", choices = setNames(coicop_set$coicop_code, coicop_set$code_label), multiple = TRUE),
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
                                        selectInput("countries_mr", "Select countries:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicop_mr", "Select product category:", choices = setNames(label_set$coicop_code, label_set$code_label), multiple = FALSE),
                                        br(),
                                        radioButtons("contribution_type", "Contribution to", choices = c("selected higher aggregate", "all-items HICP"), selected = "selected higher aggregate"),
                                        br(),
                                        actionButton("update_mr", "Retrieve data"), 
                                        br(),
                                        br(),
                                        selectInput("select_period_mr", "Select months to compare:", choices = "", multiple = TRUE),
                                        p(" "),
                                        #conditionalPanel(
                                        #        condition = "output.plot_mr",
                                        #       sliderInput("range_slider_m", "Select period:", min = 2000, max = 2023, value = c(2015, 2023), ticks=FALSE, sep="")),
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
                                        selectInput("countries_ar", "Select countries:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicop_ar", "Select product category:", choices = setNames(label_set$coicop_code, label_set$code_label), multiple = FALSE),
                                        br(),
                                        radioButtons("contribution_type", "Contribution to", choices = c("selected higher aggregate", "all-items HICP"), selected = "selected higher aggregate"),
                                        br(),
                                        actionButton("update_ar", "Retrieve data"), 
                                        br(),
                                        br(),
                                        
                                        p(" "),
                                        conditionalPanel(
                                                condition = "output.plot_ar",
                                                sliderInput("range_slider", "Select period:", min = 2000, max = 2023, value = c(2015, 2023), ticks=FALSE, sep="")),
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
                                        selectInput("countries_se", "Select countries:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicop_se", "Select product category:", choices = setNames(coicop_set$coicop_code, coicop_set$code_label), multiple = FALSE),
                                        
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
                                        selectInput("countries_w", "Select countries:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicop_w", "Select product category:", choices = setNames(label_set$coicop_code, label_set$code_label), multiple = FALSE),
                                        actionButton("update_w", "Retrieve data"), 
                                        br(),
                                        p(" "),
                                        p(" "),
                                        conditionalPanel(
                                                condition = "output.plot_w",
                                                sliderInput("range_slider_w", "Select period:", min = 2000, max = 2023, value = c(2015, 2023), ticks=FALSE, sep="")),
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
                         br(),p("This app enables to produce graphs for the all-items level or any subcomponent(s) in the european HICP (Harmonized Index of Consumer Prices). Currently graphs for indices, monthly rates, annual rates, seasonality and weights can be produced.",
                                br(),p(" "),"For the Index tab, users can set a new reference period (month or year), common for all selected index series.",br(),p(" "),
                                "For the M/M-12 (annual rates of change) and M/M-1 (monthly rates of change), Ribe contributions are calculated for the level directly below the selected aggregate (following formula in Eurostat's HICP Methodological manual, p 182-183). The Ribe contribution is also described in"),
                         br(),p(" "),"For the Seasonaliy tab, indices with December previous year =100 are produced.",br(),p(" "),
                                HTML('<a href="https://www.oecd.org/sdd/prices-ppp/OECD-calculation-contributions-annual-inflation.pdf"), target="_blank">https://www.oecd.org/sdd/prices-ppp/OECD-calculation-contributions-annual-inflation.pdf</a>'),
                         br(),p(" "), "Data is retrieved using the 'eurostat' package from the Eurostat HICP database. Additional calculations are performed where needed." 
                         
                         )
                
        )
)
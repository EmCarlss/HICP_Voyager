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
  ")),        
        tags$div(
                tags$h1("HICP Voyager 1.0", style = "text-align: left; margin-bottom: 10px; margin-top: 10px;font-size: 20px;")
        ),
        navbarPage(
                title = "",
                tabPanel("Index",
                         fluidRow(
                                 column(width = 4,
                                        selectInput("countries", "Select countries:", choices = setNames(eurostat_countries$code, eurostat_countries$name), multiple = TRUE),
                                        selectInput("coicops", "Select product categories:", choices = setNames(coicop_set$coicop_code, coicop_set$code_label), multiple = TRUE),
                                        actionButton("update", "Retrieve data"),
                                        br(),
                                        p(" "),
                                        br(),
                                        condition = "update",selectInput("select_years", "Select index reference year:", choices = "", multiple = FALSE),
                                        actionButton("rebase", "Update plot"),
                                        br(),
                                        p(" "),
                                        br(),
                                        conditionalPanel(
                                                condition = "output.plot",
                                                downloadLink('downloadData', 'Download data')
                                        )
                                 ),
                                 column(width = 8,
                                        plotlyOutput("plot")
                                 )
                         )
                ),
                tabPanel("Help",
                         h5("Navigate seamlessly in the universe of european price statistics"),br(),p("This app enables to easily produce a graph for the all-items level or any subcomponent(s) in the european HICP (Harmonized Index of Consumer Prices). Users can also choose countries and set a new reference period, common for all selected index series.",br(), p("How to use:"), p("1. Select countries from the list."),p("2. Select product categories and press Retrieve data."), p("3. Set a reference/base year for the index series and press Update plot."), br(),p("Note:"),p("It's possible to zoom in on a certain time period by just press down the left mouse button and drag the pointer over a certain area in the graph. It's also possible to save the re-referenced data to CSV using the Download data-button."), p("In some cases a country may lack data for a subcomponent, if so no data will be shown in the graph.") # Lägg till din hjälptext här
                ))
                
        )
)
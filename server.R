library(shiny)
library(eurostat)
library(dplyr)
library(zoo)
library(plotly)

function(input, output, session) {
        
        hikp_data <- eventReactive(input$update, {
                req(input$countries, input$coicops)
                data <- get_eurostat("prc_hicp_midx", filters = list(unit = "I15",
                                                                     geo = input$countries,
                                                                     coicop = input$coicops))
                clean_eurostat_cache()
                data$time <- as.yearmon(data$time)
                new_plot_data<<-TRUE
                print(paste("hikp_data new_plot_data: ", new_plot_data))
                return(data)
        })
        
        
        # Function to create a list of selected years
        selected_data <- reactive({
                req(hikp_data())
                data <- hikp_data()
                data <- data[data$geo %in% input$countries, ]

                # Remove NAs in column values
                data_no_na <- data %>%
                        filter(!is.na(values), substr(time, 1, 3) == "Jan" | substr(time, 1, 3) == "Dec")
                
                # Group by "geo" and "coicop" and find the earliest common year with index values
                max_years <- data_no_na %>%
                        group_by(geo, coicop) %>%
                        summarise(max_year = max(time))
                
                # Remove NAs in "values"
                data_no_na <- data %>%
                        filter(!is.na(values), substr(time, 1, 3) == "Jan")
                
                # Gruppera data efter "geo" och "coicop", och hitta det minsta året med observationer för varje grupp
                min_years <- data_no_na %>%
                        group_by(geo, coicop) %>%
                        summarise(min_year = min(time))
                
                # Hitta det första gemensamma året med observationer för alla länder och COICOP-grupper
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                # Hitta det sista gemensamma året med observationer för alla länder och COICOP-grupper
                last_non_na_year <- max_years %>%
                        summarise(last_common_year = min(max_year)) %>%
                        pull(min(last_common_year))
                
                data <- filter(data, time >= max(first_non_na_year) & time<min(last_non_na_year))
                return(data$time)
        })
        
        # Uppdatera selectInput för att visa listan över valda år
        observe({
                updateSelectInput(session, "select_years", choices = as.character(unique(substr(selected_data(), start = 5, stop = 8))))
                req(input$countries, input$coicops)
        })

        plot_data <- eventReactive(input$rebase,{
                req(hikp_data())
                data<-hikp_data()
                coicop<-input$coicops
   
                geo<-input$countries
                        
                avg_refyear<-data %>%
                        filter(as.numeric(substring(time, nchar(time) - 3)) == as.numeric(input$select_years)) %>%
                        group_by(coicop, geo) %>%
                        summarise(mean_values = mean(values))
                
                rebased_data <-left_join(hikp_data(),avg_refyear, by = c("coicop", "geo"))
                
                rebased_data$newbase <- ifelse(!is.na(rebased_data$values) & !is.na(rebased_data$mean_values),
                                        rebased_data$values/rebased_data$mean_values*100,NA)
                new_rebased_data<<-TRUE
                new_plot_data<<-TRUE
                return(rebased_data)
        })
        
        # Skapa en plot
        observeEvent(input$rebase, {
                output$plot <- renderPlotly({
                        req(plot_data)
                        data <- plot_data()
                        
                        data <- data[data$geo %in% input$countries, ]
                        
                        # Hitta det första året där variabeln "values" innehåller numeriska tal
                        first_non_na_year <- min(data$time[!is.na(data$values)])
                        
                        # Filtrera bort rader före det första året där variabeln "values" innehåller numeriska tal
                        data <- filter(data, time >= first_non_na_year)
                        data$time <- as.Date(as.yearmon(data$time), format="%Y %B")
                        
                        
                        if(is.null(input$coicops)==FALSE && new_plot_data==TRUE && new_rebased_data==TRUE){
                                plotly_plot <- plot_ly(data, x = ~time, y = ~newbase, color = ~geo, linetype = ~coicop, type = 'scatter', mode = 'lines')
                                
                                y_label <- paste("Index", input$select_years, "=100")
                                plotly_plot <- plotly_plot %>% 
                                        layout(
                                                yaxis = list(
                                                        title = y_label
                                                ),
                                                xaxis = list(
                                                        title = ""
                                                ), 
                                                legend = list(font = list(color = "black"),x = 1.1, y = 1.0),
                                                annotations = list(
                                                        x = 1,
                                                        y = 0,
                                                        xref="paper",
                                                        yref="paper",
                                                        text = "Source: Eurostat and own calculations.",
                                                        showarrow = FALSE,
                                                        font = list(
                                                                color = "black",
                                                                size = 10  # Ange den önskade fontstorleken här
                                                        )
                                                ))
                                        
                                labled_input_coicop<-left_join(data.frame(input$coicops),coicop_set, by = c("input.coicops"="coicop_code"))
                                
                                # Hämta värden från input$coicops och separera dem med kommatecken
                              
                                coicop_values <- paste(labled_input_coicop$code_label, collapse = ", ")
                                #coicop_values <- paste(input$coicops, collapse = ", ")
                                
                                # Ersätt sista kommatecknet med ordet "and"
                                coicop_values <- gsub(",([^,]*)$", " and\\1", coicop_values)
                                
                                # Skapa rubriktexten med hjälp av textsträngen och värdena från input$coicops
                                plot_title <- paste("Price development for", coicop_values)
                                
                                # Skapa rubriktexten med hjälp av textsträngen och värdena från input$coicops med automatisk radbrytning
                                plot_title <- paste("Price development for", coicop_values)
                                
                                # Lägg till radbrytning i rubriktexten för att uppnå word-wrapping
                                plot_title <- gsub("(.{1,70})(\\s+|$)", "\\1\n", plot_title)
                                
                                # Lägg till rubriken i mitten av plotten
                                plotly_plot <- plotly_plot %>% 
                                        layout(
                                                paper_bgcolor = "white",
                                                plot_bgcolor = "white",
                                                title = list(
                                                        text = plot_title,
                                                        x = 0.15,
                                                        y = 0.9,
                                                        font = list(
                                                                size = 12,
                                                                family = "sans-serif",
                                                                color = "black",
                                                                weight = "bold"
                                                        )
                                                )
                                        )
                                new_plot_data<<-FALSE
                                new_rebased_data<-FALSE
                                
                                return(plotly_plot)
                                
                                }
                        })
                
        })
        
        
        
        output$downloadData <- downloadHandler(
                   filename = function() {
                     paste('data-', Sys.Date(), '.csv', sep='')
                   },
                   content = function(con) {
                     data<-plot_data()
                     
                     data <- data[data$geo %in% input$countries, ]
                     
                     # Hitta det första året där variabeln "values" innehåller numeriska tal
                     first_non_na_year <- min(data$time[!is.na(data$values)])
                     
                     # Filtrera bort rader före det första året där variabeln "values" innehåller numeriska tal
                     data <- filter(data, time >= first_non_na_year)
                     ddata<-data%>%select(coicop,geo,time,newbase)
                     data$time <- toString(as.Date(as.yearmon(ddata$time), format="%Y %B"))
                
                     write.csv(ddata, con)
                   })

}


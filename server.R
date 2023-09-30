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
    
                return(data)
                
        })
        
        #Weights data
        hikp_w_data <- eventReactive(input$update_w, {
                
                req(input$countries_w, input$coicop_w)
                filtered_data <- coicop_set_hierarchy[coicop_set_hierarchy$parent_code == input$coicop_w, ]
                
                result <- unique(filtered_data$coicop_code)
                
                result <- result[!is.na(result)]
                
                if (length(result) == 0) {
                        result <- input$coicop_w
                }
                
                data <- get_eurostat("prc_hicp_inw", filters = list(geo = input$countries_w,
                                                                    #time = c(years),
                                                                    coicop =result),update_cache = TRUE)
                
                
                data_no_na <- data %>%
                        filter(!is.na(values))
                
                
                # Group by "geo" & "coicop", find the first year with observations for every group
                min_years <- data_no_na %>%
                        group_by(geo, coicop) %>%
                        summarise(min_year = min(time))
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                
                data <- filter(data, time >= max(first_non_na_year))

                
                
                label_set<-select(coicop_set_hierarchy, coicop_code, code_label)
                
                # Merged datasets based on ID-column
                merged_data <- merge(data, label_set, by.x = "coicop", by.y = "coicop_code", all.x = TRUE)
                
                return(merged_data)
        })
        
        # Function to create a list of selected years for index
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
                
                # Group by "geo" & "coicop", find the first year with observations for every group
                min_years <- data_no_na %>%
                        group_by(geo, coicop) %>%
                        summarise(min_year = min(time))
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                # Find the last common year
                last_non_na_year <- max_years %>%
                        summarise(last_common_year = min(max_year)) %>%
                        pull(min(last_common_year))
                
                data <- filter(data, time >= max(first_non_na_year) & time<min(last_non_na_year))
                return(data$time)
        })
        
        # Update selectInput to show the list of available years (index)
        observe({
                updateSelectInput(session, "select_years", choices = as.character(unique(substr(selected_data(), start = 5, stop = 8))))
                req(input$countries, input$coicops)
        })
        
        #Create index data for plot
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
        
        # Create the plot (weights)
        observeEvent(input$update_w, {
                output$plot_w <- renderPlotly({
                        req(hikp_w_data())
                        data <- hikp_w_data()
                        
                        
                        if(is.null(input$coicop_w)==FALSE){
                                plotly_plot <- plot_ly(data, x = ~time, y = ~newbase, color = ~geo, linetype = ~coicop, type = 'scatter', mode = 'lines')
                                
                                #Create list to store subplots
                                subplots <- list()
                                
                                # Loopa igenom varje unikt värde i "geo"
                                count<-0
                                
                                for (geo_value in unique(data$geo)) {
                                        
                                        # Filter dataset for the current geo value
                                        count<-count+1
                                        filtered_data <- data[data$geo == geo_value, ]
                                        filtered_data$values <- ifelse(is.na(filtered_data$values), 0, filtered_data$values)
                                        # Create a stacked bar chart for current country
                                        
                                        if(count==1){subpl <- plot_ly(filtered_data, x = ~time, y = ~values,color=~code_label, type = "bar", legendgroup=~code_label)}
                                        if(count>1){subpl <- plot_ly(filtered_data, x = ~time, y = ~values,color=~code_label, type = "bar", legendgroup=~code_label, showlegend=F)}
                                        
                                        subpl <- subpl %>% layout(barmode = 'stack',annotations=list(text=geo_value,xref="paper",yref="paper",yanchor="bottom",xanchor="center",align="center",x=0.5,y=0.95,showarrow=FALSE))
                                        
                                        # Add axis labels
                                        subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Weight, per mille"))
                                        
                                        # Add the subplot in the list
                                        subplots[[geo_value]] <- subpl
                                }
                                
                                plot_rows=1
                                
                                if(length(subplots)>2){
                                
                                plot_rows=length(subplots)-1}
                                
                                if((length(subplots) %% 2) == 0) {
                                        #Even number of subplots
                                        plot_rows=length(subplots)/2
                                } else {
                                        #Odd number of subplots
                                        plot_rows=(length(subplots)+1)/2
                                }
                                
                                
                                # 
                                # Create layout for the frame graph, size depending on number of selected countries
                                layout <- plotly::subplot(subplots, nrows = plot_rows, titleX=TRUE, shareX=TRUE,titleY=TRUE,shareY=TRUE)
                                layout <- layout %>% layout(height = plot_rows*250,width=1000)
                                
                                # Create the frame graph
                                plotly_plot_w <- plotly_build(layout)
                                
                                # Add the class "plot-container" to the labels box
                                plotly_plot_w$x$attrs$legend$x$class <- "plot-container"
                                
                                
                                return(plotly_plot_w)
                                
                        }
                })
                
        })
        
        # Create the index plot
        observeEvent(input$rebase, {
                output$plot <- renderPlotly({
                        req(plot_data)
                        data <- plot_data()
                        
                        data <- data[data$geo %in% input$countries, ]
                        
                        # Find the first year where values contains index numbers
                        first_non_na_year <- min(data$time[!is.na(data$values)])
                        
                        # Remove rows prior to the first year where values contains numbers
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
                                                legend = list(font = list(color = "black")),
                                                annotations = list(
                                                        x = 1,
                                                        y = 0,
                                                        xref="paper",
                                                        yref="paper",
                                                        text = "Source: Eurostat and own calculations.",
                                                        showarrow = FALSE,
                                                        font = list(
                                                                color = "black",
                                                                size = 10  
                                                        )
                                                ))
                                        
                                labled_input_coicop<-left_join(data.frame(input$coicops),coicop_set, by = c("input.coicops"="coicop_code"))
                                
                                # Retrieve values from input$coicops and separate them with commas
                              
                                coicop_values <- paste(labled_input_coicop$code_label, collapse = ", ")
                                
                                # Replace final comma with "and"
                                coicop_values <- gsub(",([^,]*)$", " and\\1", coicop_values)
                                
                                # Create the explanatory text
                                plot_title <- paste("Price development for", coicop_values)
                                
                                # Add word-wrapping
                                plot_title <- gsub("(.{1,70})(\\s+|$)", "\\1\n", plot_title)
                                
                                # Add the text in the plot
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
        
            
        #Download index data
        output$downloadData <- downloadHandler(
                   filename = function() {
                     paste('data-', Sys.Date(), '.csv', sep='')
                   },
                   content = function(con) {
                     data<-plot_data()
                     
                     data <- data[data$geo %in% input$countries, ]
                     
                     # Find the first year where "values" contains numbers
                     first_non_na_year <- min(data$time[!is.na(data$values)])
                     
                     # Remove rows with NAs
                     data <- filter(data, time >= first_non_na_year)
                     ddata<-data%>%select(coicop,geo,time,newbase)
                     data$time <- toString(as.Date(as.yearmon(ddata$time), format="%Y %B"))
                
                     write.csv(ddata, con)
                   })
        
        #Download weights data
        output$downloadData_w <- downloadHandler(
                filename = function() {
                        paste('data-', Sys.Date(), '.csv', sep='')
                },
                content = function(con_w) {
                        data <- hikp_w_data()
                        
                        data$time <- substr(data$time, 1, 4)
                        
                        ddata<-data%>%select(code_label,geo,time,values)
                        
                        write.csv(ddata, con_w)
                })

}


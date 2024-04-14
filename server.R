library(shiny)
library(eurostat)
library(dplyr)
library(stringr)
library(zoo)
library(lubridate)
library(plotly)

function(input, output, session) {
        
        hikp_data <- eventReactive(input$update, {
                req(input$countries, input$coicops)
                
                tryCatch({
                        data <- get_eurostat("prc_hicp_midx", filters = list(unit = "I15",
                                                                             geo = input$countries,
                                                                             coicop = input$coicops))
                        clean_eurostat_cache()
                        data$time <- as.yearmon(data$time)
                        
                        new_plot_data <<- TRUE
                        
                        return(data)
                }, error = function(e) {
                        #Message if no data
                        print(paste("No data available:", e$message))
                        
                remove("hikp_data", envir = .GlobalEnv)
                })
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
                
                new_plot_w_data <<- TRUE
                
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
        
        #Annual rates data
        hikp_ar_data <- eventReactive(input$update_ar, {
                
                req(input$countries_ar, input$coicop_ar)
                filtered_data <- coicop_set_hierarchy[coicop_set_hierarchy$parent_code == input$coicop_ar, ]
                
                result <- unique(filtered_data$coicop_code)
                
                result <- result[!is.na(result)]
                
                if (input$contribution_type_ar=="selected higher aggregate"){
                        full_coicop <- c(result, input$coicop_ar)
                }

                if (input$contribution_type_ar=="all-items HICP" && input$coicop_ar !="CP00"){
                        full_coicop <- c(result, input$coicop_ar,"CP00")
       
                }
                
                if (input$contribution_type_ar=="all-items HICP" && input$coicop_ar == "CP00"){
                        full_coicop <- c(result, input$coicop_ar)
       
                }
                
                
                if (length(result) == 0) {
                        
                                result <- input$coicop_ar
                                full_coicop<-result

                }
                
                #Get the index data
                data_I <- get_eurostat("prc_hicp_midx", filters = list(geo = input$countries_ar,
                                                                       #time = c(years),
                                                                       unit="I15",
                                                                       coicop =full_coicop),update_cache = TRUE)
                
                # Get weights
                data_W <- get_eurostat("prc_hicp_inw", filters = list(geo = input$countries_ar,
                                                                      #time = c(years),
                                                                      coicop =full_coicop),update_cache = TRUE)
                
                new_plot_ar_data <<- TRUE
                
                # Index data for aggregates j
                data_I_j <- filter(data_I, coicop %in% result)
                
                data_I_j$year<-year(data_I_j$time)
                data_I_j<-select(data_I_j,-freq, -unit)
                data_I_j<-rename(data_I_j,IX_j=values)
                
                # Weights for j
                data_W_j <- filter(data_W, coicop %in% result)
                data_W_j$year<-year(data_W_j$time)
                data_W_j<-select(data_W_j, -freq, -time)
                data_W_j<-rename(data_W_j,WT_j_pre=values)
                
                # Weights for higher aggregate TOT
                
                data_W_TOT <- filter(data_W, coicop %in% input$coicop_ar)
                data_W_TOT$year<-year(data_W_TOT$time)
                data_W_TOT<-select(data_W_TOT, -freq, -time, -coicop)
                data_W_TOT<-rename(data_W_TOT,WT_TOT=values)
                data_W_jTOT <- left_join(data_W_j, data_W_TOT, by = c("year", "geo"))
                
                #Calculate weight for j in selected higher aggregate
                #...depending on the type of contribution selected
                if (input$contribution_type_ar == "selected higher aggregate") {
                        data_W_jTOT$WT_j<-data_W_jTOT$WT_j_pre/data_W_TOT$WT_TOT*1000
                } else if (input$contribution_type_ar == "all-items HICP") {
                        data_W_jTOT$WT_j<-data_W_jTOT$WT_j_pre
                }
                
                data_W_jTOT<-select(data_W_jTOT,-WT_j_pre,-WT_TOT)
                
                result_j <- left_join(data_I_j, data_W_jTOT, by = c("year", "coicop", "geo"))
                
                # Annual rate for higher aggregate TOT
                
                
                if (input$contribution_type_ar == "selected higher aggregate") {
                        data_AR <- filter(data_I, coicop %in% input$coicop_ar)
                } else if (input$contribution_type_ar == "all-items HICP") {
                        data_AR <- filter(data_I, coicop %in% "CP00")
                }
                
                data_AR$year <- year(data_AR$time)
                data_AR$month <- month(data_AR$time)
                
                m_AR_ymin1 <- data_AR
                m_AR_ymin1$year_plus1<-m_AR_ymin1$year+1
                m_AR_ymin1<-select(m_AR_ymin1,-year, -unit,-time, -freq)
                m_AR_ymin1<-rename(m_AR_ymin1,IX_j_m_ymin1=values,year=year_plus1)
                
                data_AR <- left_join(data_AR, m_AR_ymin1, by = c("year","month","geo","coicop"))
                data_AR$ann_rate_00<-data_AR$values/data_AR$IX_j_m_ymin1*100-100
                data_AR<-select(data_AR, -freq,-unit,-values,-IX_j_m_ymin1)

                # Indices for the higher aggregate TOT
                
                data_I_TOT <- filter(data_I, coicop %in% input$coicop_ar)
                
                data_I_TOT<-select(data_I_TOT,-freq, -unit, -coicop)
                data_I_TOT<-rename(data_I_TOT,IX_TOT=values)
                
                result_jTOT <- left_join(result_j, data_I_TOT, by = c("time", "geo"))
                result_jTOT$year <- year(result_jTOT$time)
                result_jTOT$month <- month(result_jTOT$time)
                
                
                #December y-1 data for TOT
                
                dec_data_ymin1 <- result_jTOT %>%
                        filter(month == 12)
                dec_data_ymin1<-select(dec_data_ymin1,-month)
                dec_data_ymin1$year_plus1<-dec_data_ymin1$year+1
                dec_data_ymin1<-select(dec_data_ymin1,-year, -time)
                dec_data_ymin1<-rename(dec_data_ymin1,IX_j_12_ymin1=IX_j,WT_j_12_ymin1=WT_j,IX_TOT_12_ymin1=IX_TOT,year=year_plus1)
                
                result_jTOT2 <- left_join(result_jTOT, dec_data_ymin1, by = c("year", "geo","coicop"))
                
                #December y-2 data for TOT
                
                dec_data_ymin2 <- result_jTOT %>%
                        filter(month == 12)
                dec_data_ymin2<-select(dec_data_ymin2,-month)
                dec_data_ymin2$year_plus2<-dec_data_ymin2$year+2
                dec_data_ymin2<-select(dec_data_ymin2,-year, -time)
                dec_data_ymin2<-rename(dec_data_ymin2,IX_j_12_ymin2=IX_j,WT_j_12_ymin2=WT_j,IX_TOT_12_ymin2=IX_TOT,year=year_plus2)
                
                result_jTOT2 <- left_join(result_jTOT2, dec_data_ymin2, by = c("year", "geo","coicop"))
                
                #Data for current month m y-1 for TOT
                
                m_data_ymin1 <- result_jTOT
                m_data_ymin1$year_plus1<-m_data_ymin1$year+1
                m_data_ymin1<-select(m_data_ymin1,-year, -time)
                m_data_ymin1<-rename(m_data_ymin1,IX_j_m_ymin1=IX_j,WT_j_m_ymin1=WT_j,IX_TOT_m_ymin1=IX_TOT,year=year_plus1)
                
                result_jTOT2 <- left_join(result_jTOT2, m_data_ymin1, by = c("year", "month","geo","coicop"))
                
                numeric_vars <- c("IX_j", "WT_j", "IX_TOT", "IX_j_12_ymin1", "WT_j_12_ymin1", "IX_TOT_12_ymin1", 
                                  "IX_j_12_ymin2", "WT_j_12_ymin2", "IX_TOT_12_ymin2", "IX_j_m_ymin1", 
                                  "WT_j_m_ymin1", "IX_TOT_m_ymin1")
                
                result_jTOT2 <- result_jTOT2 %>%
                        mutate(
                                Contr_j = if_else(
                                        rowSums(is.na(across(all_of(numeric_vars)))) > 0,
                                        NA,
                                        (100 * (IX_TOT_12_ymin1 / IX_TOT_m_ymin1 * WT_j_12_ymin1 / 1000) * ((IX_j - IX_j_12_ymin1) / IX_j_12_ymin1)) +
                                                100 * ((IX_TOT_12_ymin2 / IX_TOT_m_ymin1 * WT_j_12_ymin2 / 1000) * ((IX_j_12_ymin1 - IX_j_m_ymin1) / IX_j_12_ymin2))
                                )
                        )
                
                
                result_jTOT2 <- full_join(result_jTOT2, data_AR, by = c("year","time", "month", "geo", "coicop"))
                
                data<-select(result_jTOT2,year,month,time, geo, coicop, ann_rate_00, Contr_j)

                
                data_no_na <- data %>%
                        filter(is.numeric(Contr_j) | is.numeric(ann_rate_00))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time))

                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))

                
                data <- filter(data, time >= max(first_non_na_year))
                
                #data <- data %>%
                #        filter(!is.na(ann_rate_00) | !is.na(Contr_j))
                
                label_set<-select(coicop_set_hierarchy, coicop_code, code_label)
                
                if (input$contribution_type_ar == "selected higher aggregate") {
                        # Merged datasets based on ID-column
                        merged_data <- merge(data, label_set, by.x = "coicop", by.y = "coicop_code", all.x = TRUE)
                        merged_data <- merged_data %>%
                                mutate(code_label = ifelse(coicop == input$coicop_ar, NA, code_label))
                        
                } else if (input$contribution_type_ar == "all-items HICP") {
                        # Merged datasets based on ID-column
                        merged_data <- merge(data, label_set, by.x = "coicop", by.y = "coicop_code", all.x = TRUE)
                }
                
                
                
                return(merged_data)
        })
        
        #Monthly rates data
        hikp_mr_data <- eventReactive(input$update_mr, {
                
                req(input$countries_mr, input$coicop_mr)
                
                filtered_data <- coicop_set_hierarchy[coicop_set_hierarchy$parent_code == input$coicop_mr, ]
                
                result <- unique(filtered_data$coicop_code)
                
                result <- result[!is.na(result)]
                
                
                result <- unique(filtered_data$coicop_code)
                
                result <- result[!is.na(result)]
                
                if (input$contribution_type=="selected higher aggregate"){
                        full_coicop <- c(result, input$coicop_mr)
                }
                
                if (input$contribution_type=="all-items HICP" && input$coicop_mr !="CP00"){
                        full_coicop <- c(result, input$coicop_mr,"CP00")
                        
                }
                
                if (input$contribution_type=="all-items HICP" && input$coicop_mr == "CP00"){
                        full_coicop <- c(result, input$coicop_mr)
                        
                }
                
                
                if (length(result) == 0) {
                        
                        result <- input$coicop_mr
                        full_coicop<-result
                        
                }
                
                
                #Get the index data
                data_I <- get_eurostat("prc_hicp_midx", filters = list(geo = input$countries_mr,
                                                                       #time = c(years),
                                                                       unit="I15",
                                                                       coicop =full_coicop),update_cache = TRUE)
                
                
                
                # Get weights
                data_W <- get_eurostat("prc_hicp_inw", filters = list(geo = input$countries_mr,
                                                                      #time = c(years),
                                                                      coicop =full_coicop),update_cache = TRUE)
                
                new_plot_mr_data <<- TRUE
                
                # Index data for aggregates j -OK
                data_I_j <- filter(data_I, coicop %in% result)
                
                data_I_j$year<-year(data_I_j$time)
                data_I_j<-select(data_I_j,-freq, -unit)
                data_I_j<-rename(data_I_j,IX_j=values)
                
                # Weights for j -OK
                data_W_j <- filter(data_W, coicop %in% result)
                
                data_W_j$year<-year(data_W_j$time)
                data_W_j<-select(data_W_j, -freq, -time)
                data_W_j<-rename(data_W_j,WT_j_pre=values)
                
                # Weights for higher aggregate TOT -OK
                
                data_W_TOT <- filter(data_W, coicop %in% input$coicop_mr)
                data_W_TOT$year<-year(data_W_TOT$time)
                data_W_TOT<-select(data_W_TOT, -freq, -time, -coicop)
                data_W_TOT<-rename(data_W_TOT,WT_TOT=values)
                
                #Calculate weight for j in selected higher aggregate
                data_W_jTOT <- left_join(data_W_j, data_W_TOT, by = c("year", "geo"))
                
                #...depending on the type of contribution selected
                if (input$contribution_type == "selected higher aggregate") {
                        data_W_jTOT$WT_j<-data_W_jTOT$WT_j_pre/data_W_TOT$WT_TOT*1000
                } else if (input$contribution_type == "all-items HICP") {
                        data_W_jTOT$WT_j<-data_W_jTOT$WT_j_pre
                }
                
                data_W_jTOT<-select(data_W_jTOT,-WT_j_pre,-WT_TOT)
                
                result_j <- left_join(data_I_j, data_W_jTOT, by = c("year", "coicop", "geo"))
                
                # Monthly rate for higher aggregate TOT
                
                if (input$contribution_type == "selected higher aggregate") {
                        data_MR <- filter(data_I, coicop %in% input$coicop_mr)
                } else if (input$contribution_type == "all-items HICP") {
                        data_MR <- filter(data_I, coicop %in% "CP00")
                }
                
                data_MR$year <- year(data_MR$time)
                data_MR$month <- month(data_MR$time)
                
                #Find the monthly rate of change for total index
                m_MR_mmin1 <- data_MR
                
                m_MR_mmin1$new_year <- ifelse(m_MR_mmin1$month == 12, m_MR_mmin1$year + 1, m_MR_mmin1$year)
                m_MR_mmin1$new_month <- ifelse(m_MR_mmin1$month == 12, 1, m_MR_mmin1$month %% 12 + 1)
            
                m_MR_mmin1<-select(m_MR_mmin1,-year,-month, -unit,-time, -freq)
                m_MR_mmin1<-rename(m_MR_mmin1,IX_j_m_mmin1=values,year=new_year,month=new_month)
                
                data_MR <- left_join(data_MR, m_MR_mmin1, by = c("year","month","geo","coicop"))
                data_MR$m_rate_00<-data_MR$values/data_MR$IX_j_m_mmin1*100-100
                
                data_MR<-select(data_MR, -freq,-unit,-values,-IX_j_m_mmin1)
                
                # Indices for the higher aggregate TOT
                
                data_I_TOT <- filter(data_I, coicop %in% input$coicop_mr)
                data_I_TOT<-select(data_I_TOT,-freq, -unit, -coicop)
                data_I_TOT<-rename(data_I_TOT,IX_TOT=values)
                result_jTOT <- left_join(result_j, data_I_TOT, by = c("time", "geo"))
                result_jTOT$year <- year(result_jTOT$time)
                result_jTOT$month <- month(result_jTOT$time)
                
                #Previous month indices
                
                data_mmin1 <- result_jTOT
                
                data_mmin1$new_year <- ifelse(data_mmin1$month == 12, data_mmin1$year + 1, data_mmin1$year)
                data_mmin1$new_month <- ifelse(data_mmin1$month == 12, 1, data_mmin1$month %% 12 + 1)
                data_mmin1<-select(data_mmin1,-year,-month, -time, -WT_j)
                data_mmin1<-rename(data_mmin1,IX_TOT_m_mmin1=IX_TOT, IX_j_m_mmin1=IX_j,year=new_year,month=new_month)
                result_jTOT2 <- left_join(result_jTOT, data_mmin1, by = c("year","month", "geo","coicop"))
               
                
                #Contribution to monthly rate of change
                numeric_vars <- c("IX_j", "WT_j", "IX_TOT_m_mmin1", "IX_j_m_mmin1")
                
                result_jTOT2 <- result_jTOT2 %>%
                        mutate(
                                Contr_j = if_else(
                                        rowSums(is.na(across(all_of(numeric_vars)))) > 0,
                                        NA,
                                        (100 * (WT_j/1000*(IX_j - IX_j_m_mmin1))/IX_TOT_m_mmin1)
                                )
                        )
                
                result_jTOT2 <- full_join(result_jTOT2, data_MR, by = c("year","time", "month", "geo", "coicop"))
                
                data<-select(result_jTOT2,year,month,time, geo, coicop, m_rate_00, Contr_j)
             
                
                data_no_na <- data %>%
                        filter(is.numeric(Contr_j) | is.numeric(m_rate_00))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time))
                
                #Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                
                data <- filter(data, time >= max(first_non_na_year))
                
                #data <- data %>%
                #        filter(!is.na(m_rate_00) | !is.na(Contr_j))
               
                label_set<-select(coicop_set_hierarchy, coicop_code, code_label)
                
                # Merged datasets based on ID-column
                #...depending on contribution type selected
                if (input$contribution_type == "selected higher aggregate") {
                        # Merged datasets based on ID-column
                        merged_data <- merge(data, label_set, by.x = "coicop", by.y = "coicop_code", all.x = TRUE)
                        merged_data <- merged_data %>%
                                mutate(code_label = ifelse(coicop == input$coicop_mr, NA, code_label))
                        
                } else if (input$contribution_type == "all-items HICP") {
                        # Merged datasets based on ID-column
                        merged_data <- merge(data, label_set, by.x = "coicop", by.y = "coicop_code", all.x = TRUE)
                }
                
                return(merged_data)
                
        })
        
        #Seasonality data
        hikp_se_data <- eventReactive(input$update_se, {
                
                req(input$countries_se, input$coicop_se)
                
                #Get the index data
                data_I <- get_eurostat("prc_hicp_midx", filters = list(geo = input$countries_se,
                                                                       #time = c(years),
                                                                       unit="I15",
                                                                       coicop =input$coicop_se),update_cache = TRUE)
                
                
                new_plot_se_data <<- TRUE
                
                # Index data for aggregates j
                data_I_j <- data_I
                
                data_I_j$year<-year(data_I_j$time)
                data_I_j$month<-month(data_I_j$time)
                data_I_j<-select(data_I_j,-freq, -unit)
                data_I_j<-rename(data_I_j,IX_j=values)
                
                result_j <- data_I_j
                
                #December y-1 data for TOT
                
                dec_data_ymin1 <- result_j %>%
                        filter(month == 12)
                dec_data_ymin1<-select(dec_data_ymin1,-month)
                dec_data_ymin1$year_plus1<-dec_data_ymin1$year+1
                dec_data_ymin1<-select(dec_data_ymin1,-year, -time)
                dec_data_ymin1<-rename(dec_data_ymin1,IX_j_12_ymin1=IX_j,year=year_plus1)
                
                result_j2 <- left_join(result_j, dec_data_ymin1, by = c("year", "geo","coicop"))
                result_j2$index_dec_prv_yr<-result_j2$IX_j/result_j2$IX_j_12_ymin1*100
                data<-result_j2
                
                data_no_na <- data %>%
                        filter(is.numeric(index_dec_prv_yr))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time))
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                
                data <- filter(data, time >= max(first_non_na_year))
                
                data <- data %>%
                        filter(!is.na(index_dec_prv_yr))
                
                label_set<-select(coicop_set_hierarchy, coicop_code, code_label)
                
                # Merged datasets based on ID-column
                merged_data <- merge(data, label_set, by.x = "coicop", by.y = "coicop_code", all.x = TRUE)
                merged_data <- merged_data %>%
                        mutate(code_label = ifelse(coicop == input$coicop_se, NA, code_label))
                
                return(merged_data)
        })
        
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
                
                # Common filtering based on the first and last common years
                if (input$period_type == "Full year") {
                        data <- filter(data, time > max(first_non_na_year) & time < min(last_non_na_year))
                } else if (input$period_type == "Month") {
                        data <- filter(data, time >= max(first_non_na_year) & time < min(last_non_na_year))
                }
                
                if (input$period_type == "Full year") {
                        year_list<-unique(format(data$time, "%Y"))
                        return(year_list)
                } else if (input$period_type == "Month") {
                        # Create a list of Year + Month combinations
                        year_month_list <- unique(paste(format(data$time, "%Y"), format(data$time, "%B"), sep = " "))
                        return(year_month_list)
                }
        })
        
        
        # Update selectInput to show the list of available years (index)
        observe({
                updateSelectInput(session, "select_years", choices = selected_data())
                req(input$countries, input$coicops)
        })
        
        selected_w_data <- reactive({
                req(hikp_w_data())
                data <- hikp_w_data()
                data$time <- as.yearmon(data$time)
                data <- data[data$geo %in% input$countries_w, ]
                
                # Remove NAs in column values
                data_no_na <- data %>%
                        filter(is.numeric(values))
                
                max_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(max_year = max(time,na.rm=TRUE))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time))
                
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                # Find the last common year
                last_non_na_year <- max_years %>%
                        summarise(last_common_year = min(max_year)) %>%
                        pull(min(last_common_year))
                
                # Common filtering based on the first and last common years
                
                data <- filter(data, time >= max(first_non_na_year) & time <= min(last_non_na_year))
                year_list<-unique(format(data$time, "%Y"))
                
                return(year_list)
                
        })
        
        observe({
                req(hikp_w_data())
                year_choices <- selected_w_data()
                
                max_year<-as.numeric(max(year_choices))
                min_year<-as.numeric(min(year_choices))
                updateSliderInput(session, "range_slider_w", min = min_year, max = max_year,step=1, value = c(max(c(max_year-5, min_year)), max_year))
                req(hikp_w_data, input$update_w, input$countries_w, input$coicop_w)
        })
        
        selected_ar_data <- reactive({
                req(hikp_ar_data())
                data <- hikp_ar_data()
                data$time <- as.yearmon(data$time)
                data <- data[data$geo %in% input$countries_ar, ]
                
                # Remove NAs in column values
                data_no_na <- data %>%
                        filter(is.numeric(Contr_j) | is.numeric(ann_rate_00), month == 1 | month == 12)
                
                max_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(max_year = max(time,na.rm=TRUE))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time))
                
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                # Find the last common year
                last_non_na_year <- max_years %>%
                        summarise(last_common_year = min(max_year)) %>%
                        pull(min(last_common_year))
                
                # Common filtering based on the first and last common years
                
                data <- filter(data, time >= max(first_non_na_year) & time <= min(last_non_na_year))
                year_list<-unique(format(data$time, "%Y"))
                return(year_list)
        
        })
        
        observe({
                req(hikp_ar_data())
                year_choices <- selected_ar_data()
                
                max_year<-as.numeric(max(year_choices))
                min_year<-as.numeric(min(year_choices))
                updateSliderInput(session, "range_slider", min = min_year, max = max_year,step=1, value = c(max(c(max_year-2, min_year)), max_year))
                req(hikp_ar_data, input$update_ar, input$countries_ar, input$coicop_ar)
        })
        
        selected_mr_data <- reactive({
                req(hikp_mr_data())
                data <- hikp_mr_data()
                data$time <- as.yearmon(data$time)
                data <- data[data$geo %in% input$countries_mr, ]
                
                # Remove NAs in column values
                
                data_no_na <- data %>%
                        filter(!is.na(Contr_j) | !is.na(m_rate_00))
                
                max_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(max_year = max(time,na.rm=TRUE))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time))
                
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                # Find the last common year
                last_non_na_year <- max_years %>%
                        summarise(last_common_year = min(max_year)) %>%
                        pull(min(last_common_year))
                
                # Common filtering based on the first and last common years
                
                data <- filter(data, time >= max(first_non_na_year) & time <= min(last_non_na_year))
                period_list<-unique(format(rev(data$time), "%Y %B"))
                
                return(period_list)
                
        })
        
        # Update selectInput to show the list of available periods (monthly rate)
        observe({
                updateSelectInput(session, "select_period_mr", choices = selected_mr_data(),selected = selected_mr_data()[1])
                req(input$countries_mr, input$coicop_mr)
        })
        
        selected_se_data <- reactive({
                req(hikp_se_data())
                data <- hikp_se_data()
                data$time <- as.yearmon(data$time)
                data <- data[data$geo %in% input$countries_se, ]
                
                # Remove NAs in column values
                
                data_no_na <- data %>%
                        filter(is.numeric(index_dec_prv_yr))
                
                
                max_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(max_year = max(time,na.rm=TRUE))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time))
                
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                # Find the last common year
                last_non_na_year <- max_years %>%
                        summarise(last_common_year = min(max_year)) %>%
                        pull(min(last_common_year))
                
                # Common filtering based on the first and last common years
                
                data <- filter(data, time >= max(first_non_na_year) & time <= min(last_non_na_year))
                year_list<-unique(format(rev(data$time), "%Y"))
                
                return(year_list)
                
        })
        
        # Update selectInput to show the list of available periods (monthly rate)
        observe({
                updateSelectInput(session, "select_years_se", choices = selected_se_data(),selected = selected_se_data()[1])
                req(input$countries_se, input$coicop_se)
        })
        
        #Create index data for plot
        plot_data <- eventReactive(input$rebase,{
                req(hikp_data())
                data<-hikp_data()
                coicop<-input$coicops
   
                geo<-input$countries
                        
                # Adjust filtering depending on choice of period type
                if (input$period_type == "Full year") {
                        avg_refyear <- data %>%
                                filter(as.numeric(format(time, "%Y")) == as.numeric(input$select_years)) %>%
                                group_by(coicop, geo) %>%
                                summarise(mean_values = mean(values))
                } else if (input$period_type == "Month") {
                        avg_refyear <- data %>%
                                filter(as.character(format(time, "%Y %B")) == input$select_years) %>%
                                group_by(coicop, geo) %>%
                                summarise(mean_values = mean(values))
                }

                rebased_data <-left_join(hikp_data(),avg_refyear, by = c("coicop", "geo"))
                
                rebased_data$newbase <- ifelse(!is.na(rebased_data$values) & !is.na(rebased_data$mean_values),
                                        rebased_data$values/rebased_data$mean_values*100,NA)
                
                
                # Adjust filtering based on if Full year or Month is selected as reference period
                if (input$period_type == "Full year") {
                        rebased_data <- rebased_data %>% filter(as.numeric(format(time, "%Y")) >= as.numeric(input$select_years))
                } else if (input$period_type == "Month") {
                        
                        # Extract year and month
                        selected_year <- as.numeric(str_extract(input$select_years, "\\d{4}"))
                        selected_month <- match(tolower(str_extract(input$select_years, "[A-Za-z]+")), tolower(month.name))
                        
                        # Filter data
                        rebased_data <- rebased_data %>% 
                                filter(year(time) > selected_year | (year(time) == selected_year & month(time) >= selected_month))
                }
                
                new_rebased_data<<-TRUE
                new_plot_data<<-TRUE
                return(rebased_data)
        })
        
        # Create the plot (weights)
        observeEvent(input$update_w, {
                output$plot_w <- renderPlotly({
                        req(hikp_w_data())
                        data <- hikp_w_data()
                        
                        # Filter data based on selected years
                        time_filtered_data <- data[as.numeric(format(data$time, "%Y")) >= input$range_slider_w[1] & as.numeric(format(data$time, "%Y")) <= input$range_slider_w[2], ]
                        
                        
                        # Determine common max and min on y-axis
                        weight_by_geo <- time_filtered_data %>%
                                group_by(geo, time) %>%
                                summarise(sum = sum(values, na.rm = TRUE))
                        
                        max_weight <-max(weight_by_geo$sum)
                        
                        if(is.null(input$coicop_w)==FALSE && is.null(input$countries_w)==FALSE){
                                
                                #Create list to store subplots
                                subplots <- list()
                                
                                # Loop through each unique value in geo "geo"
                                count <- 0
                                for (geo_value in unique(time_filtered_data$geo)) {
                                        
                                        # Filter dataset for the current geo value
                                        count <- count + 1
                                        filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                        
                                        filtered_data$values <- ifelse(is.na(filtered_data$values), 0, filtered_data$values)
                                        
                                        # Check if all values in "values" are 0
                                        if(all(filtered_data$values == 0)) {
                                                
                                                # If all values are 0, add text "Data unavailable" in the middle
                                                text_annotation <- list(
                                                        x = 0.25,
                                                        y = 0.75,
                                                        xref = "paper",
                                                        yref = "paper",
                                                        text = "Data unavailable",
                                                        showarrow = FALSE,
                                                        font = list(size = 14, color = "black")
                                                )
                                                
                                                subpl <- plot_ly(filtered_data,x = ~time, y = ~values, showlegend = FALSE,color = ~code_label,legendgroup = ~code_label)  # Empty subplot
                                                subpl <- subpl %>% layout(barmode = 'stack', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                
                                                
                                                # Add axis labels
                                                subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Weight, per mille"))
                                                subpl <- subpl %>% layout(annotations = list(text_annotation))
                                        } else {
                                                # Create a stacked bar chart for current country
                                                if(count == 1) {
                                                        subpl <- plot_ly(filtered_data, x = ~time, y = ~values, color = ~code_label, type = "bar", legendgroup = ~code_label)
                                                } else {
                                                        subpl <- plot_ly(filtered_data, x = ~time, y = ~values, color = ~code_label, type = "bar", legendgroup = ~code_label, showlegend = FALSE)
                                                }
                                                
                                                subpl <- subpl %>% layout(yaxis = list(range = c(0, max_weight*1.05)),barmode = 'stack', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                # Add axis labels
                                                subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Weight, per mille"))
                                        }
                                        
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
                                
                                if(length(subplots)>2){
                                layout <- layout %>% layout(height = plot_rows*300,width=1100)
                                }
                                if(length(subplots)<=2){
                                        layout <- layout %>% layout(height = plot_rows*475,width=1100)
                                }
                                
                                # Create the frame graph
                                plotly_plot_w <- plotly_build(layout)
                                
                                # Add the class "plot-container" to the labels box
                                plotly_plot_w$x$attrs$legend$x$class <- "plot-container"
                                
                                return(plotly_plot_w)
                                
                        }
                })
                
        })
        
        # Create the plot (annual rate of change)
        observeEvent(input$update_ar, {
                output$plot_ar <- renderPlotly({
                        
                        req(hikp_ar_data(), input$coicop_ar)
                        data <- hikp_ar_data()
                        
                        # Filter data based on selected year range
                        time_filtered_data <- data[as.numeric(format(data$time, "%Y")) >= input$range_slider[1] & as.numeric(format(data$time, "%Y")) <= input$range_slider[2], ]
                        
                        time_filtered_data$sign <- ifelse(time_filtered_data$Contr_j > 0, 1, 
                                                          ifelse(time_filtered_data$Contr_j < 0, -1, 1))
                        
                        # Determine common max and min on y-axis
                        contr_by_geo <- time_filtered_data %>%
                                                group_by(geo, time, sign) %>%
                                                summarise(sum_value = sum(Contr_j, na.rm = TRUE))
                
                        max_contr <-max(contr_by_geo$sum_value)
                        
                        min_contr <-min(contr_by_geo$sum_value)
                        
                        ar_00_by_geo <- time_filtered_data %>%
                                group_by(geo, time) %>%
                                summarise(max_value = max(ann_rate_00, na.rm = TRUE),min_value = min(ann_rate_00, na.rm = TRUE))
                        
                        max_ar_00 <-max(ar_00_by_geo$max_value)
                        
                        min_ar_00 <-min(ar_00_by_geo$min_value)
                        
                        #...depending on the type of contribution selected
                        if (input$contribution_type_ar == "selected higher aggregate") {
                                max_ar<-max(max_ar_00,max_contr)
                                min_ar<-min(min_ar_00,min_contr,0)
                                
                        } else if (input$contribution_type_ar == "all-items HICP") {
                                max_ar<-max(max_contr)
                                min_ar<-min(min_contr,0)
                        }
                        

                        if(is.null(input$coicop_ar)==FALSE && is.null(input$countries_ar)==FALSE){
                               
                                #Create list to store subplots
                                subplots <- list()
                                
                                # Loop through each unique value in geo "geo"
                                count <- 0
                                for (geo_value in unique(time_filtered_data$geo)) {
                                        
                                        # Filter dataset for the current geo value
                                        count <- count + 1
                                        filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                        
                                        filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)
                                        
                                        # Check if all values in "Contr_j" are 0
                                        if(all(filtered_data$Contr_j == 0 & is.na(filtered_data$ann_rate_00))) {
                                                
                                                # If all values are 0, add text "Data unavailable" in the middle
                                                text_annotation <- list(
                                                        x = 0.25,
                                                        y = 0.75,
                                                        xref = "paper",
                                                        yref = "paper",
                                                        text = "Data unavailable",
                                                        showarrow = FALSE,
                                                        font = list(size = 14, color = "black")
                                                )
                                                
                                                subpl <- plot_ly(filtered_data,x = ~time, y = ~Contr_j, showlegend = FALSE,color = ~code_label,legendgroup = ~code_label)  # Empty subplot
                                                subpl <- subpl %>% layout(barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE),xaxis = list(tickformat = "%Y %B"))
                                                
                                                
                                                #Add axis labels
                                                if (input$contribution_type_ar == "selected higher aggregate") {
                                                        subpl <- subpl %>% layout(yaxis = list(range = c(min_ar-2, max_ar+2)),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE),xaxis = list(tickformat = "%Y %B",tickangle = 45))
                                                        # Add a line for annual rate
                                                        subpl <- subpl %>% add_trace(filtered_data ,x = ~time, y = ~ann_rate_00, type = 'scatter', mode = 'lines', name = 'Annual rate M/M-12', line = list(color = 'black', dash = 'dash',width = 1.5))
                                                        # Add axis labels
                                                        subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Annual rate of change, %"))
                                                }
                                                
                                                if (input$contribution_type_ar == "all-items HICP") {
                                                        subpl <- subpl %>% layout(yaxis = list(range = c(ifelse(min_ar==0,min_ar, min_ar*1.05), ifelse(max_ar==0,max_ar,max_ar*1.05))),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE),xaxis = list(tickformat = "%Y %B",tickangle = 45))
                                                        print(min_ar)
                                                        # Add a line for annual rate
                                                        subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Contrib. to HICP M/M-12, %-points"))     
                                                }
                                                
        
                                                subpl <- subpl %>% layout(annotations = list(text_annotation))
                                        } else {
                                                # Create a stacked bar chart for current country
                                                
                                                if(count == 1) {
                                                        
                                                        subpl <- plot_ly(filtered_data, x = ~time, y = ~Contr_j, color = ~code_label, type = "bar", legendgroup = ~code_label)
                                                } else {
                                                        subpl <- plot_ly(filtered_data, x = ~time, y = ~Contr_j, color = ~code_label, type = "bar", legendgroup = ~code_label, showlegend = FALSE)
                                                }
                                                
                                                
                                                
                                                if (input$contribution_type_ar == "selected higher aggregate") {
                                                        subpl <- subpl %>% layout(yaxis = list(range = c(ifelse(min_ar==0,min_ar, min_ar*1.05), ifelse(max_ar==0,max_ar,max_ar*1.05))),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE),xaxis = list(tickformat = "%Y %B",tickangle = 45))
                                                        # Add a line for annual rate
                                                        subpl <- subpl %>% add_trace(filtered_data ,x = ~time, y = ~ann_rate_00, type = 'scatter', mode = 'lines', name = 'Annual rate M/M-12', line = list(color = 'black', dash = 'dash',width = 1.5))
                                                        # Add axis labels
                                                        subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Annual rate of change, %"))
                                                }  
                                                
                                                if (input$contribution_type_ar == "all-items HICP") {
                                                        subpl <- subpl %>% layout(yaxis = list(range = c(ifelse(min_ar==0,min_ar, min_ar*1.05), ifelse(max_ar==0,max_ar,max_ar*1.05))),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE),xaxis = list(tickformat = "%Y %B",tickangle = 45))
                                                        # Add a line for annual rate
                                                        subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Contrib. to HICP M/M-12, %-points"))
                                                }  
                                                
                                        }
                                        
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
                                
                                # Customize the legend text with word-wrapping
                                layout <- layout %>% layout(legend.text = geo_value,font = list(size = 11), legend.traceorder = "reversed")
                                
                                if(length(subplots)>2){
                                        layout <- layout %>% layout(height = plot_rows*300,width=1100)
                                }
                                if(length(subplots)<=2){
                                        layout <- layout %>% layout(height = plot_rows*475,width=1100)
                                }
                                
                                
                                # Create the frame graph
                                plotly_plot_ar <- plotly_build(layout)
                                
                                # Add the class "plot-container" to the labels box
                                plotly_plot_ar$x$attrs$legend$x$class <- "plot-container"
                                
                                #Reset the new_plot_ar_data variable to ensure that plot is not shown unless new data is available
                                
                                
                                return(plotly_plot_ar)
                                
                        }
                })
                
        })
        
        # Create the plot (monthly rate of change)
        observeEvent(input$update_mr, {
                output$plot_mr <- renderPlotly({
                        
                        req(hikp_mr_data(), input$coicop_mr,input$select_period_mr)
                        data <- hikp_mr_data()
                        
                        # Filter data based on selected year range
                        
                        time_filtered_data <- data[format(data$time, "%Y %B") %in% input$select_period_mr, ]
                        
                        time_filtered_data$sign <- ifelse(time_filtered_data$Contr_j > 0, 1, 
                                                          ifelse(time_filtered_data$Contr_j < 0, -1, 1))
                        
                        # Determine common max and min on y-axis
                        contr_by_geo <- time_filtered_data %>%
                                group_by(geo, time, sign) %>%
                                summarise(sum_value = sum(Contr_j, na.rm = TRUE))
                        
                        max_contr <-max(contr_by_geo$sum_value)
                        
                        min_contr <-min(contr_by_geo$sum_value)
                        
                        mr_00_by_geo <- time_filtered_data %>%
                                group_by(geo, time) %>%
                                summarise(max_value = max(m_rate_00, na.rm = TRUE),min_value = min(m_rate_00, na.rm = TRUE))

                        max_mr_00 <-max(mr_00_by_geo$max_value)
                        
                        min_mr_00 <-min(mr_00_by_geo$min_value)
                        
                        #...depending on the type of contribution selected
                        if (input$contribution_type == "selected higher aggregate") {
                                max_mr<-max(max_mr_00,max_contr,0)
                                min_mr<-min(min_mr_00,min_contr,0)
                                
                        } else if (input$contribution_type == "all-items HICP") {
                                max_mr<-max(max_contr,0)
                                min_mr<-min(min_contr,0)
                        }
                        

                        
                        if(is.null(input$coicop_mr)==FALSE && is.null(input$countries_mr)==FALSE){
                                
                                #Create list to store subplots
                                subplots <- list()
                                
                                # Loop through each unique value in geo "geo"
                                count <- 0
                                for (geo_value in unique(time_filtered_data$geo)) {
                                        
                                        # Filter dataset for the current geo value
                                        count <- count + 1
                                        filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                        
                                        filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)
                                        
                                        # Sort data based on time and then on Contr_j
                                        filtered_data <- filtered_data[order(filtered_data$time, filtered_data$Contr_j), ]
                                        
                                        # Filter unique time values for the x-axis tick labels
                                        x_axis_tick_labels <- unique(filtered_data$time)
                                        
                                        # Filter unique time values for the x-axis tick labels
                                        x_axis_tick_labels <- unique(filtered_data$time)
                                        
                                        # Define the x-axis layout with categoryarray
                                        x_axis_layout <- list(
                                                title = "",
                                                tickmode = "array",
                                                tickvals = x_axis_tick_labels,
                                                #ticktext = x_axis_tick_labels,
                                                tickformat = "%Y %B",
                                                tickangle = 45
                                        )
                                        
                                        # Check if all values in "Contr_j" are 0
                                        if(all(filtered_data$Contr_j == 0 & is.na(filtered_data$m_rate_00))) {
                                                # If all values are 0, add text "Data unavailable" in the middle
                                                text_annotation <- list(
                                                        x = 0.25,
                                                        y = 0.75,
                                                        xref = "paper",
                                                        yref = "paper",
                                                        text = "Data unavailable",
                                                        showarrow = FALSE,
                                                        font = list(size = 14, color = "black")
                                                )
                                                
                                                subpl <- plot_ly(filtered_data,x = ~time, y = ~Contr_j, showlegend = FALSE,color = ~code_label,legendgroup = ~code_label)  # Empty subplot
                                                subpl <- subpl %>% layout(barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                
                                                # Add axis labels
                                                #subpl <- subpl %>% layout(xaxis = x_axis_layout, yaxis = list(title = "Monthly rate of change, %"))
                                                
                                                if (input$contribution_type == "selected higher aggregate") {
                                                        subpl <- subpl %>% layout(xaxis = x_axis_layout,yaxis = list(title = "Monthly rate of change, %",range = c(ifelse(min_mr==0,min_mr, min_mr*1.05), ifelse(max_mr==0,max_mr,max_mr*1.05))),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                }  
                                                
                                                if (input$contribution_type == "all-items HICP") {
                                                        subpl <- subpl %>% layout(xaxis = x_axis_layout,yaxis = list(range = c(ifelse(min_mr==0,min_mr, min_mr*1.05), ifelse(max_mr==0,max_mr,max_mr*1.05))),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                        
                                                        # Add axis labels
                                                        subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Contr. to all-items M/M-1, %"))
                                                }  
                                                
                                                subpl <- subpl %>% layout(annotations = list(text_annotation),xaxis = x_axis_layout)
                                        } else {
                                                # Create a stacked bar chart for current country
                                                
                                                if(count == 1) {
                                                        subpl <- plot_ly(filtered_data, x = ~time, y = ~Contr_j, color = ~code_label, type = "bar", legendgroup = ~code_label)
                                                } else {
                                                        subpl <- plot_ly(filtered_data, x = ~time, y = ~Contr_j, color = ~code_label, type = "bar", legendgroup = ~code_label, showlegend = FALSE)
                                                }
                                                
                                                
                                                
                                                # Add axis labels
                                                subpl <- subpl %>% layout(xaxis = x_axis_layout, yaxis = list(title = "Monthly rate of change, %"))
                                                
                                                if (input$contribution_type == "selected higher aggregate") {
                                                        subpl <- subpl %>% layout(yaxis = list(range = c(ifelse(min_mr==0,min_mr, min_mr*1.05), ifelse(max_mr==0,max_mr,max_mr*1.05))),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                        
                                                        # Lägg till linjediagram
                                                        subpl <- subpl %>% add_trace(filtered_data ,x = ~time, y = ~m_rate_00, type = 'scatter', mode = 'markers', name = 'Monthly rate M/M-1', marker = list(color = 'black', size = 10))
                                                        
                                                }  
                                                
                                                if (input$contribution_type == "all-items HICP") {
                                                        subpl <- subpl %>% layout(yaxis = list(range = c(ifelse(min_mr==0,min_mr, min_mr*1.05), ifelse(max_mr==0,max_mr,max_mr*1.05))),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                        
                                                        # Add axis labels
                                                        subpl <- subpl %>% layout(xaxis = list(title = ""), yaxis = list(title = "Contr. to all-items M/M-1, %"))
                                                }  
                                                
                                        }
                                        
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
                                
                                # Customize the legend text with word-wrapping
                                layout <- layout %>% layout(legend.text = geo_value,font = list(size = 11), legend.traceorder = "reversed")
                                
                                if(length(subplots)>2){
                                        layout <- layout %>% layout(height = plot_rows*300,width=1100)
                                }
                                if(length(subplots)<=2){
                                        layout <- layout %>% layout(height = plot_rows*475,width=1100)
                                }
                                
                                
                                # Create the frame graph
                                plotly_plot_mr <- plotly_build(layout)
                                
                                # Add the class "plot-container" to the labels box
                                plotly_plot_mr$x$attrs$legend$x$class <- "plot-container"
                                
                                #Reset the new_plot_ar_data variable to ensure that plot is not shown unless new data is available
                                
                                
                                return(plotly_plot_mr)
                                
                        }
                })
                
        })
        
        # Create the plot (seasonality)
        observeEvent(input$update_se, {
                output$plot_se <- renderPlotly({
                        
                        req(hikp_se_data(), input$coicop_se)
                        data <- hikp_se_data()
                        
                        # Create a named vector for month
                        month_abbrev <- c("Dec Y-1", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                        names(month_abbrev) <- 0:12
                        
                        # Step 1: Filter rows where month == 1
                        filtered_data <- data %>%
                                filter(month == 1)
                        
                        # Step 2 & 3: Create copies and modify them 
                        modified_data <- filtered_data %>%
                                mutate(
                                        month = 0,  
                                        time = time - months(1),  
                                        IX_j = IX_j_12_ymin1,  
                                        index_dec_prv_yr = 100.00000  
                                )
                        
                        # Step 4: Combine the rows
                        data <- bind_rows(data, modified_data)
                        
                        # Step 5: Sort after coicop, geo, year and month
                        data <- data %>%
                                arrange(coicop, geo, year,month)
                        
                        time_filtered_data <- data[data$year %in% input$select_years_se, ]
                        
                        # Check number of rows in time_filtered_data
                        nrows_time_filtered_data <- nrow(time_filtered_data)
                        
                        # If number of rows>0 then we can create min/max values for the y-axis
                        if (nrows_time_filtered_data > 0) {
                                # Determine common max and min on y-axis
                                ix_by_geo <- time_filtered_data %>%
                                        group_by(geo, time) %>%
                                        summarise(max_value = max(index_dec_prv_yr, na.rm = TRUE), min_value=min(index_dec_prv_yr, na.rm = TRUE))
                                
                                max_se <-max(ix_by_geo$max_value)
                                
                                min_se <-min(ix_by_geo$min_value)
                        } else {
                                max_se<-125
                                min_se<-75
                        }
                        
                        nrows_time_filtered_data<-0
                            
                        
                        if(is.null(input$coicop_se)==FALSE && is.null(input$countries_se)==FALSE){
                                
                                #Create list to store subplots
                                subplots <- list()
                                
                                # Loop through each unique value in geo "geo"
                                count <- 0
                                for (geo_value in unique(time_filtered_data$geo)) {
                                        
                                        # Filter dataset for the current geo value
                                        count <- count + 1
                                        filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                        
                                        filtered_data$index_dec_prv_yr <- ifelse(is.na(filtered_data$index_dec_prv_yr), 0, filtered_data$index_dec_prv_yr)
                                        
                                        # Check if all values in "index_dec_prv_yr" are 0
                                        if(all(filtered_data$index_dec_prv_yr == 0 & is.na(filtered_data$index_dec_prv_yr))) {
                                                
                                                # If all values are 0, add text "Data unavailable" in the middle
                                                text_annotation <- list(
                                                        x = 0.25,
                                                        y = 0.75,
                                                        xref = "paper",
                                                        yref = "paper",
                                                        text = "Data unavailable",
                                                        showarrow = FALSE,
                                                        font = list(size = 14, color = "black")
                                                )
                                                
                                                subpl <- plot_ly(filtered_data,x = ~month, y = ~index_dec_prv_yr, showlegend = FALSE,color = ~year,legendgroup =~as.factor(year),type = 'scatter', mode = 'lines')  # Empty subplot
                                                subpl <- subpl %>% layout(annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                
                                                # Add axis labels
                                                subpl <- subpl %>% layout(xaxis = list(range = c(0, 12),zeroline= FALSE,showgrid = TRUE,showline=FALSE,title = "",tickvals = 0:12, ticktext = month_abbrev, tickangle = 45), yaxis = list(zeroline= FALSE,showgrid = TRUE,showline=FALSE,title = "Index Dec. Y-1=100"))
                                                subpl <- subpl %>% layout(annotations = list(text_annotation))
                                        } else {
                                                # Create a stacked bar chart for current country
                                                
                                                if(count == 1) {
                                                        subpl <- plot_ly(filtered_data, x = ~month, y = ~index_dec_prv_yr, color = ~as.factor(year),legendgroup =~as.factor(year),type = 'scatter', mode = 'lines')
                                                } else {
                                                        subpl <- plot_ly(filtered_data, x = ~month, y = ~index_dec_prv_yr, color = ~as.factor(year),legendgroup =~as.factor(year), showlegend = FALSE,type = 'scatter', mode = 'lines')
                                                }
                                                
                                                subpl <- subpl %>% layout(yaxis = list(title = "Index Dec. Y-1=100",showgrid = TRUE,showline=FALSE,zeroline= FALSE, range = c(min_se-2, max_se+2)), annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                
                                                # Add axis labels
                                                subpl <- subpl %>% layout(xaxis = list(range = c(0, 12),showgrid = TRUE,showline=FALSE,zeroline= FALSE, title = "",tickvals = 0:12, ticktext = month_abbrev, tickangle = 45))
                                        }
                                        
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
                                if(length(subplots)>0){
                                frame_layout <- plotly::subplot(subplots, nrows = plot_rows, titleX=TRUE, shareX=TRUE,titleY=TRUE,shareY=TRUE)
                                }
                                
                                if(length(subplots)>2){
                                        frame_layout <- frame_layout %>% layout(height = plot_rows*300,width=1100)
                                }
                                if(length(subplots)<=2 && (length(subplots)>0)){
                                        frame_layout <- frame_layout %>% layout(height = plot_rows*475,width=1100)
                                }
                                
                                # Create the frame graph
                                if(length(subplots)>0){
                                        plotly_plot_se <- plotly_build(frame_layout)
                                        # Add the class "plot-container" to the labels box
                                        
                                        plotly_plot_se$x$attrs$legend$x$class <- "plot-container"
                                        return(plotly_plot_se)
                                }

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
                                                        title = ""), 
                                                legend = list(font = list(color = "black",size=11)),
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
        
        #Download annual rate data
        output$downloadData_ar <- downloadHandler(
                filename = function() {
                        paste('data-', Sys.Date(), '.csv', sep='')
                },
                content = function(con_ar) {
                        data <- hikp_ar_data()
                        data$time <- substr(data$time, 1, 4)
                        ddata<-data%>%select(coicop,geo,time,Contr_j,ann_rate_00)
                        
                        write.csv(ddata, con_ar)
                })
        
        #Download monthly rates data
        output$downloadData_mr <- downloadHandler(
                filename = function() {
                        paste('data-', Sys.Date(), '.csv', sep='')
                },
                content = function(con_mr) {
                        data <- hikp_mr_data()
                        #data$time <- substr(data$time, 1, 4)
                        data$year <- year(data$time)
                        data$month <- month(data$time)
                        ddata<-data%>%select(coicop,geo,time,month,year,Contr_j,m_rate_00)
                        
                        write.csv(ddata, con_mr)
                })
        
        #Download seasonal data
        output$downloadData_se <- downloadHandler(
                filename = function() {
                        paste('data-', Sys.Date(), '.csv', sep='')
                },
                content = function(se_data) {
                        data <- hikp_se_data()

                        ddata<-data%>%select(coicop,geo,time,month,year,index_dec_prv_yr)
                        
                        write.csv(ddata, se_data)
                })

}


library(shiny)
library(eurostat)
library(dplyr)
library(stringr)
library(zoo)
library(lubridate)
library(plotly)

function(input, output, session) {
        
        add_country_group_observers <- function(prefix, input_id, session, input, country_groups) {
                observeEvent(input[[paste0(prefix, "_eu")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["All EU countries"]])
                })
                
                observeEvent(input[[paste0(prefix, "_efta")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["EFTA"]])
                })
                
                observeEvent(input[[paste0(prefix, "_eu_efta")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["EU + EFTA"]])
                })
                
                observeEvent(input[[paste0(prefix, "_med")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Mediterranean"]])
                })
                
                observeEvent(input[[paste0(prefix, "_nordic")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Nordic"]])
                })
                
                observeEvent(input[[paste0(prefix, "_balkan")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Balkan"]])
                })
                
                observeEvent(input[[paste0(prefix, "_central")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Central"]])
                })
                
                observeEvent(input[[paste0(prefix, "_eastern")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Eastern"]])
                })
                
                observeEvent(input[[paste0(prefix, "_western")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Western"]])
                })
                
                observeEvent(input[[paste0(prefix, "_benelux")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Benelux"]])
                })
                
                observeEvent(input[[paste0(prefix, "_clear")]], {
                        updateSelectInput(session, input_id, selected = character(0))
                })
        }
        
        
        add_country_group_observers("index", "countries", session, input, country_groups)
        add_country_group_observers("mr", "countries_mr", session, input, country_groups)
        add_country_group_observers("ar", "countries_ar", session, input, country_groups)
        add_country_group_observers("se", "countries_se", session, input, country_groups)
        add_country_group_observers("weights", "countries_w", session, input, country_groups)
        
        wrap_legend <- function(x, width = 45) {
                x %>%
                        stringr::str_wrap(width = width) %>%
                        gsub("\n", "<br>", .)
        }
        
        hikp_data <- eventReactive(input$update, {
                req(input$countries, input$coicops, input$index_measure)
                
                validate(
                        need(
                                !(length(input$index_measure) == 2 && length(input$coicops) > 1),
                                "When both HICP and HICP-CT are selected, please select only one COICOP aggregate."
                        )
                )
                
                coicops <- ifelse(input$coicops == "CP00", "TOTAL", input$coicops)
                
                selected_countries <- input$countries
                
                backdrop_countries <- if (isTRUE(input$index_backdrop_eu)) {
                        setdiff(country_groups[["All EU countries"]], selected_countries)
                } else {
                        character(0)
                }
                
                countries_to_fetch <- union(selected_countries, backdrop_countries)
                
                tryCatch({
                        
                        data_list <- list()
                        
                        if ("HICP" %in% input$index_measure) {
                                data_hicp <- get_eurostat(
                                        "prc_hicp_minr",
                                        filters = list(
                                                unit = "I25",
                                                geo = countries_to_fetch,
                                                coicop18 = coicops
                                        ),
                                        update_cache = TRUE
                                )
                                
                                data_hicp <- data_hicp %>%
                                        mutate(measure = "HICP")
                                
                                data_list[["HICP"]] <- data_hicp
                        }
                        
                        if ("HICP_CT" %in% input$index_measure) {
                                data_ct <- get_eurostat(
                                        "prc_hicp_ct",
                                        filters = list(
                                                unit = "I25",
                                                geo = countries_to_fetch,
                                                coicop18 = coicops
                                        ),
                                        update_cache = TRUE
                                )
                                
                                data_ct <- data_ct %>%
                                        mutate(measure = "HICP-CT")
                                
                                data_list[["HICP_CT"]] <- data_ct
                        }
                        
                        data <- bind_rows(data_list)
                        clean_eurostat_cache()
                        
                        data$time <- as.yearmon(data$time)
                        
                        new_plot_data <<- TRUE
                        
                        return(data)
                        
                }, error = function(e) {
                        print(paste("No data available:", e$message))
                        return(NULL)
                })
        })
        
        
        #Weights data
        hikp_w_data <- eventReactive(input$update_w, {
                
                req(input$countries_w, input$coicop_w)
                
                filtered_data <- coicop_set_hierarchy[coicop_set_hierarchy$parent_code == input$coicop_w, ]
                coicops <- if (input$coicop_w == "CP00") "TOTAL" else input$coicop_w
                print(filtered_data)
                result <- unique(filtered_data$coicop18_code)
                
                result <- result[!is.na(result)]
                
                if (length(result) == 0) {
                        result <- coicops
                }
                
                data <- get_eurostat("prc_hicp_iw", filters = list(geo = input$countries_w,
                                                                    #time = c(years),
                                                                    coicop18 =result),update_cache = TRUE)
                
                new_plot_w_data <<- TRUE
                
                data_no_na <- data %>%
                        filter(!is.na(values))
                
                
                # Group by "geo" & "coicop", find the first year with observations for every group
                min_years <- data_no_na %>%
                        group_by(geo, coicop18) %>%
                        summarise(min_year = min(time))
                
                # Find the first common year
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year)) %>%
                        pull(max(first_common_year))
                
                
                data <- filter(data, time >= max(first_non_na_year))

                label_set<-select(coicop_set_hierarchy, coicop18_code, code_label)
                
                # Merged datasets based on ID-column
                merged_data <- merge(data, label_set, by.x = "coicop18", by.y = "coicop18_code", all.x = TRUE)
                
                return(merged_data)
        })
        
        #Annual rates data
        hikp_ar_data <- eventReactive(input$update_ar, {
                
                req(input$countries_ar, input$coicop_ar)
                filtered_data <- coicop_set_hierarchy[coicop_set_hierarchy$parent_code == input$coicop_ar, ]
                coicops <- if (input$coicop_ar == "CP00") "TOTAL" else input$coicop_ar

                result <- unique(filtered_data$coicop18_code)
                
                result <- result[!is.na(result)]
                
                if (input$contribution_type_ar=="selected higher aggregate"){
                        full_coicop <- c(result, coicops)
                }

                if (input$contribution_type_ar=="all-items HICP" && coicops !="TOTAL"){
                        full_coicop <- c(result, coicops,"TOTAL")
       
                }
                
                if (input$contribution_type_ar=="all-items HICP" && coicops == "TOTAL"){
                        full_coicop <- c(result, coicops)
       
                }
                
                
                if (length(result) == 0) {
                        
                                result <- coicops
                                full_coicop<-result

                }
                
                #Get the index data
                data_I <- get_eurostat("prc_hicp_minr", filters = list(geo = input$countries_ar,
                                                                       #time = c(years),
                                                                       unit="I25",
                                                                       coicop18 =full_coicop),update_cache = TRUE)
                
                # Get weights
                data_W <- get_eurostat("prc_hicp_iw", filters = list(geo = input$countries_ar,
                                                                      #time = c(years),
                                                                      coicop18 =full_coicop),update_cache = TRUE)
                data_W_check<<-data_W
                result_check<<-result
                
                data_W<-select(data_W, -statinfo)
                
                new_plot_ar_data <<- TRUE
                
                # Index data for aggregates j
                data_I_j <- filter(data_I, coicop18 %in% result)
                
                data_I_j$year<-year(data_I_j$time)
                data_I_j<-select(data_I_j,-freq, -unit)
                data_I_j<-rename(data_I_j,IX_j=values)
                
                # Weights for j
                data_W_j <- filter(data_W, coicop18 %in% result)
                
                data_W_j$year<-year(data_W_j$time)
                data_W_j<-select(data_W_j, -freq, -time)
                data_W_j<-rename(data_W_j,WT_j_pre=values)

                
                # Weights for higher aggregate TOT
                
                inputcoicop_ar_check<<-coicops
                data_W_TOT_check <<- filter(data_W, coicop18 %in% coicops)
                data_W_TOT <- filter(data_W, coicop18 %in% coicops)
                
                
                
                
                data_W_TOT$year<-year(data_W_TOT$time)
                data_W_TOT<-select(data_W_TOT, -freq, -time, -coicop18)
                data_W_TOT<-rename(data_W_TOT,WT_TOT=values)
                
                data_W_TOT_check<<-data_W_TOT
                
                #Calculate weight for j in selected higher aggregate
                data_W_jTOT <- left_join(data_W_j, data_W_TOT, by = c("year", "geo"))
                data_W_jTOT_check <<- left_join(data_W_j, data_W_TOT, by = c("year", "geo"))
                
                #...depending on the type of contribution selected
                if (input$contribution_type_ar == "selected higher aggregate") {
                        
                        data_W_jTOT <- data_W_jTOT %>%
                                mutate(WT_j = WT_j_pre / WT_TOT * 1000)
                } else if (input$contribution_type_ar == "all-items HICP") {
                        data_W_jTOT$WT_j<-data_W_jTOT$WT_j_pre
                }
                
                
                
                data_W_jTOT<-select(data_W_jTOT,-WT_j_pre,-WT_TOT)
                
                result_j <- left_join(data_I_j, data_W_jTOT, by = c("year", "coicop18", "geo"))
                
                # Annual rate for higher aggregate TOT
                
                
                if (input$contribution_type_ar == "selected higher aggregate") {
                        data_AR <- filter(data_I, coicop18 %in% coicops)
                } else if (input$contribution_type_ar == "all-items HICP") {
                        data_AR <- filter(data_I, coicop18 %in% "TOTAL")
                }
                
                data_AR$year <- year(data_AR$time)
                data_AR$month <- month(data_AR$time)
                
                m_AR_ymin1 <- data_AR
                m_AR_ymin1$year_plus1<-m_AR_ymin1$year+1
                m_AR_ymin1<-select(m_AR_ymin1,-year, -unit,-time, -freq)
                m_AR_ymin1<-rename(m_AR_ymin1,IX_j_m_ymin1=values,year=year_plus1)
                
                data_AR <- left_join(data_AR, m_AR_ymin1, by = c("year","month","geo","coicop18"))
                data_AR$ann_rate_00<-data_AR$values/data_AR$IX_j_m_ymin1*100-100
                data_AR<-select(data_AR, -freq,-unit,-values,-IX_j_m_ymin1)

                # Indices for the higher aggregate TOT
                
                data_I_TOT <- filter(data_I, coicop18 %in% coicops)
                
                data_I_TOT<-select(data_I_TOT,-freq, -unit, -coicop18)
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
                
                result_jTOT2 <- left_join(result_jTOT, dec_data_ymin1, by = c("year", "geo","coicop18"))
                
                #December y-2 data for TOT
                
                dec_data_ymin2 <- result_jTOT %>%
                        filter(month == 12)
                dec_data_ymin2<-select(dec_data_ymin2,-month)
                dec_data_ymin2$year_plus2<-dec_data_ymin2$year+2
                dec_data_ymin2<-select(dec_data_ymin2,-year, -time)
                dec_data_ymin2<-rename(dec_data_ymin2,IX_j_12_ymin2=IX_j,WT_j_12_ymin2=WT_j,IX_TOT_12_ymin2=IX_TOT,year=year_plus2)
                
                result_jTOT2 <- left_join(result_jTOT2, dec_data_ymin2, by = c("year", "geo","coicop18"))
                
                #Data for current month m y-1 for TOT
                
                m_data_ymin1 <- result_jTOT
                m_data_ymin1$year_plus1<-m_data_ymin1$year+1
                m_data_ymin1<-select(m_data_ymin1,-year, -time)
                m_data_ymin1<-rename(m_data_ymin1,IX_j_m_ymin1=IX_j,WT_j_m_ymin1=WT_j,IX_TOT_m_ymin1=IX_TOT,year=year_plus1)
                
                result_jTOT2 <- left_join(result_jTOT2, m_data_ymin1, by = c("year", "month","geo","coicop18"))
                
                numeric_vars <- c("IX_j", "WT_j", "IX_TOT", "IX_j_12_ymin1", "WT_j_12_ymin1", "IX_TOT_12_ymin1", 
                                  "IX_j_12_ymin2", "WT_j_12_ymin2", "IX_TOT_12_ymin2", "IX_j_m_ymin1", 
                                  "WT_j_m_ymin1", "IX_TOT_m_ymin1")
                
                result_jTOT2 <- result_jTOT2 %>%
                        mutate(
                                Contr_j = if_else(
                                        rowSums(is.na(across(all_of(numeric_vars)))) > 0,
                                        NA,
                                        (100 * (IX_TOT_12_ymin1 / IX_TOT_m_ymin1 * WT_j / 1000) * ((IX_j - IX_j_12_ymin1) / IX_j_12_ymin1)) +
                                                100 * ((IX_TOT_12_ymin2 / IX_TOT_m_ymin1 * WT_j_12_ymin1 / 1000) * ((IX_j_12_ymin1 - IX_j_m_ymin1) / IX_j_12_ymin2))
                                )
                        )
                
                result_jTOT2 <- full_join(result_jTOT2, data_AR, by = c("year","time", "month", "geo", "coicop18"))
                
                data<-select(result_jTOT2,year,month,time, geo, coicop18, ann_rate_00, Contr_j)

                
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
                
                label_set<-select(coicop_set_hierarchy, coicop18_code, code_label)
                
                # Merged datasets based on ID-column
                merged_data <- merge(data, label_set, by.x = "coicop18", by.y = "coicop18_code", all.x = TRUE)
                merged_data <- merged_data %>%
                        mutate(
                                code_label = ifelse(coicop18 == coicops, NA, code_label),
                                code_label_wrapped = ifelse(
                                        is.na(code_label),
                                        NA,
                                        wrap_legend(code_label, width = 45)
                                )
                        )
                
                return(merged_data)
        })
        
        #Seasonality data
        hikp_se_data <- eventReactive(input$update_se, {
                
                req(input$countries_se, input$coicop_se)
                
                coicops <- if (input$coicop_se == "CP00") "TOTAL" else input$coicop_se
                
                #Get the index data
                data_I <- get_eurostat("prc_hicp_minr", filters = list(geo = input$countries_se,
                                                                       #time = c(years),
                                                                       unit="I25",
                                                                       coicop18 =coicops),update_cache = TRUE)
                
                
                
                
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
                
                result_j2 <- left_join(result_j, dec_data_ymin1, by = c("year", "geo","coicop18"))
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
                
                label_set<-select(coicop_set_hierarchy, coicop18_code, code_label)
                
                # Merged datasets based on ID-column
                merged_data <- merge(data, label_set, by.x = "coicop18", by.y = "coicop18_code", all.x = TRUE)
                merged_data <- merged_data %>%
                        mutate(code_label = ifelse(coicop18 == coicops, NA, code_label))
                
                return(merged_data)
        })
        
        #Monthly rates data
        hikp_mr_data <- eventReactive(input$update_mr, {
                
                req(input$countries_mr, input$coicop_mr)
                
                filtered_data <- coicop_set_hierarchy[coicop_set_hierarchy$parent_code == input$coicop_mr, ]
                
                coicops <- if (input$coicop_mr == "CP00") "TOTAL" else input$coicop_mr
                print(coicops)
                
                result <- unique(filtered_data$coicop18_code)
                                
                result <- result[!is.na(result)]
                print(result)
                
                if (input$contribution_type_mr=="selected higher aggregate"){
                        full_coicop <- c(result, coicops)
                }
                
                if (input$contribution_type_mr=="all-items HICP" && coicops !="TOTAL"){
                        full_coicop <- c(result, coicops,"TOTAL")
                        
                }
                
                if (input$contribution_type_mr=="all-items HICP" && coicops == "TOTAL"){
                        full_coicop <- c(result, coicops)
                        
                }
                
                
                if (length(result) == 0) {
                        
                        result <- coicops
                        full_coicop<-result
                        
                }
                
                
                #Get the index data
                data_I <- get_eurostat("prc_hicp_minr", filters = list(geo = input$countries_mr,
                                                                       #time = c(years),
                                                                       unit="I25",
                                                                       coicop18 =full_coicop),update_cache = TRUE)
                
                # Get weights
                data_W <- get_eurostat("prc_hicp_iw", filters = list(geo = input$countries_mr,
                                                                      #time = c(years),
                                                                      coicop18 =full_coicop),update_cache = TRUE)
                
                new_plot_mr_data <<- TRUE
                
                # Index data for aggregates j 
                data_I_j <- filter(data_I, coicop18 %in% result)
                
               
                data_I_j <- data_I_j %>%
                        mutate(
                                year = year(time),
                                month = month(time)
                        ) %>%
                        select(-any_of(c("freq", "unit"))) %>%
                        rename(IX_j = values)
                
               
                # Weights for j -OK
                data_W_j <- data_W %>%
                        filter(coicop18 %in% result) %>%
                        mutate(year = year(time)) %>%
                        select(-any_of(c("freq", "time"))) %>%
                        rename(WT_j_pre = values)
                
                # Weights for higher aggregate TOT -OK
                
                data_W_TOT <- data_W %>%
                        filter(coicop18 %in% coicops) %>%
                        mutate(year = year(time)) %>%
                        select(-any_of(c("freq", "time", "coicop18"))) %>%
                        rename(WT_TOT = values)
                
                #Calculate weight for j in selected higher aggregate
                data_W_jTOT <- left_join(data_W_j, data_W_TOT, by = c("year", "geo"))
                
                #...depending on the type of contribution selected
                if (input$contribution_type_mr == "selected higher aggregate") {
                        data_W_jTOT <- data_W_jTOT %>%
                                mutate(WT_j = WT_j_pre / WT_TOT * 1000)
                } else if (input$contribution_type_mr == "all-items HICP") {
                        data_W_jTOT$WT_j<-data_W_jTOT$WT_j_pre
                }
                
                
                data_W_jTOT<-select(data_W_jTOT,-WT_j_pre,-WT_TOT)
                
                result_j <- left_join(data_I_j, data_W_jTOT, by = c("year", "coicop18", "geo"))
                
                # Monthly rate for higher aggregate TOT
                
                if (input$contribution_type_mr == "selected higher aggregate") {
                        data_MR <- filter(data_I, coicop18 %in% coicops)
                } else if (input$contribution_type_mr == "all-items HICP") {
                        data_MR <- filter(data_I, coicop18 %in% "CP00")
                }
                
                data_MR$year <- year(data_MR$time)
                data_MR$month <- month(data_MR$time)
                
                #Find the monthly rate of change for total index
                m_MR_mmin1 <- data_MR
                
                m_MR_mmin1$new_year <- ifelse(m_MR_mmin1$month == 12, m_MR_mmin1$year + 1, m_MR_mmin1$year)
                m_MR_mmin1$new_month <- ifelse(m_MR_mmin1$month == 12, 1, m_MR_mmin1$month %% 12 + 1)
            
                m_MR_mmin1<-select(m_MR_mmin1,-year,-month, -unit,-time, -freq)
                m_MR_mmin1<-rename(m_MR_mmin1,IX_j_m_mmin1=values,year=new_year,month=new_month)
                
                data_MR <- left_join(data_MR, m_MR_mmin1, by = c("year","month","geo","coicop18"))
                data_MR$m_rate_00<-data_MR$values/data_MR$IX_j_m_mmin1*100-100
                
                data_MR<-select(data_MR, -freq,-unit,-values,-IX_j_m_mmin1)
                
                # Indices for the higher aggregate TOT
                
                data_I_TOT <- filter(data_I, coicop18 %in% coicops)
                
                data_I_TOT <- data_I_TOT %>%
                        mutate(
                                year = year(time),
                                month = month(time)
                        ) %>%
                        select(-any_of(c("freq", "unit", "coicop18"))) %>%
                        rename(IX_TOT = values)
                
                #Fixing
                result_jTOT <- left_join(result_j, data_I_TOT, by = c("time", "geo"))
                result_jTOT$year <- year(result_jTOT$time)
                result_jTOT$month <- month(result_jTOT$time)
                
                
                #Previous month indices
                
                data_mmin1 <- result_jTOT
                
                data_mmin1$new_year <- ifelse(data_mmin1$month == 12, data_mmin1$year + 1, data_mmin1$year)
                data_mmin1$new_month <- ifelse(data_mmin1$month == 12, 1, data_mmin1$month %% 12 + 1)
                data_mmin1<-select(data_mmin1,-year,-month, -time, -WT_j)
                data_mmin1<-rename(data_mmin1,IX_TOT_m_mmin1=IX_TOT, IX_j_m_mmin1=IX_j,year=new_year,month=new_month)
                result_jTOT2 <- left_join(result_jTOT, data_mmin1, by = c("year","month", "geo","coicop18"))
                
                # Compute contributions to the monthly rate of change following the HICP methodology:
                # 1) Update weights to the previous month (m-1) by price-updating base weights:
                #    weight_j_mmin1 = w_j * (I_j^{m-1} / I_TOT^{m-1})
                # 2) Calculate the monthly rate of change for each sub-index:
                #    mr_j = (I_j^{m} / I_j^{m-1} - 1) * 100
                # 3) Derive contributions as the product of the price-updated weight and the sub-index rate:
                #    Contr_j = mr_j * weight_j_mmin1
                # Missing values are propagated to ensure contributions are only computed when all required inputs are available.
                
                #Contribution to monthly rate of change
                result_jTOT2 <- result_jTOT2 %>%
                        mutate(
                                weight_j_mmin1 = if_else(
                                        is.na(WT_j) | is.na(IX_j_m_mmin1) | is.na(IX_TOT_m_mmin1),
                                        NA_real_,
                                        (WT_j / 1000) * (IX_j_m_mmin1 / IX_TOT_m_mmin1)
                                ),
                                
                                mr_j = if_else(
                                        is.na(IX_j) | is.na(IX_j_m_mmin1),
                                        NA_real_,
                                        (IX_j / IX_j_m_mmin1 - 1) * 100
                                ),
                                
                                Contr_j = mr_j * weight_j_mmin1
                        )
               
                result_jTOT2 <- full_join(result_jTOT2, data_MR, by = c("year","time", "month", "geo", "coicop18"))
                

                
                
                data<-select(result_jTOT2,year,month,time, geo, coicop18, m_rate_00, Contr_j)
             
                
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
                
                data <- data %>%
                        filter(!is.na(m_rate_00) | !is.na(Contr_j))
               
                label_set<-select(coicop_set_hierarchy, coicop18_code, code_label)
                
                # Merged datasets based on ID-column
                merged_data <- merge(data, label_set, by.x = "coicop18", by.y = "coicop18_code", all.x = TRUE)
                
                merged_data <- merged_data %>%
                        mutate(
                                code_label = ifelse(coicop18 == coicops, NA, code_label),
                                code_label_wrapped = ifelse(
                                        is.na(code_label),
                                        NA,
                                        wrap_legend(code_label, width = 45)
                                )
                        )
                
                return(merged_data)
                
        })
        
        selected_data <- reactive({
                req(hikp_data())
                data <- hikp_data()
                
                data <- data[data$geo %in% input$countries, ]
                
                # Remove NAs in column values
                data_no_na <- data %>%
                        filter(!is.na(values), substr(time, 1, 3) == "Jan" | substr(time, 1, 3) == "Dec")
                
                # Group by "geo" and "coicop18" and find the earliest common year with index values
                max_years <- data_no_na %>%
                        group_by(geo, coicop18) %>%
                        summarise(max_year = max(time))
                
                # Group by "geo" & "coicop18", find the first year with observations for every group
                min_years <- data_no_na %>%
                        group_by(geo, coicop18) %>%
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
        
        #When both HICP and HICP-CT are selected, only one COICOP aggregate can be shown.
        observeEvent(input$index_measure, {
                req(input$index_measure)
                
                if (length(input$index_measure) == 2 && length(input$coicops) > 1) {
                        updateSelectInput(
                                session,
                                "coicops",
                                selected = input$coicops[1]
                        )
                        
                        showNotification(
                                "When both HICP and HICP-CT are selected, only one COICOP aggregate can be shown.",
                                type = "message"
                        )
                }
        })
        
        #When both HICP and HICP-CT are selected, only one COICOP aggregate can be shown (also after first selection).
        observeEvent(input$coicops, {
                req(input$index_measure)
                
                if (length(input$index_measure) == 2 && length(input$coicops) > 1) {
                        updateSelectInput(
                                session,
                                "coicops",
                                selected = input$coicops[1]
                        )
                }
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
                req(hikp_w_data(), input$weights_view)
                year_choices <- selected_w_data()
                
                max_year <- as.numeric(max(year_choices))
                min_year <- as.numeric(min(year_choices))
                
                slider_value <- if (input$weights_view == "country") {
                        c(max(max_year - 5, min_year), max_year)
                } else {
                        c(max_year, max_year)
                }
                
                updateSliderInput(
                        session,
                        "range_slider_w",
                        min = min_year,
                        max = max_year,
                        step = 1,
                        value = slider_value
                )
                
                req(input$update_w, input$countries_w, input$coicop_w)
        })
        
        selected_ar_data <- reactive({
                req(hikp_ar_data(), input$ar_view)
                
                data <- hikp_ar_data() %>%
                        mutate(time = as.yearmon(time)) %>%
                        filter(geo %in% input$countries_ar)
                
                data_no_na <- data %>%
                        filter(!is.na(Contr_j) | !is.na(ann_rate_00))
                
                min_times <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_time = min(time, na.rm = TRUE), .groups = "drop")
                
                max_times <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(max_time = max(time, na.rm = TRUE), .groups = "drop")
                
                first_common_time <- max(min_times$min_time)
                last_common_time  <- min(max_times$max_time)
                
                available_times <- data %>%
                        filter(time >= first_common_time,
                               time <= last_common_time) %>%
                        distinct(time) %>%
                        arrange(desc(time)) %>%
                        pull(time)
                
                if (input$ar_view == "period") {
                        period_list <- format(available_times, "%Y %B")
                        
                        return(list(
                                choices = period_list,
                                selected = period_list[1]
                        ))
                }
                
                year_list <- unique(format(available_times, "%Y"))
                
                return(list(
                        choices = year_list,
                        selected = NULL
                ))
        })
        
        
        observe({
                req(hikp_ar_data(), input$ar_view)
                
                ar_choices <- selected_ar_data()
                
                if (input$ar_view == "country") {
                        year_choices <- ar_choices$choices
                        
                        max_year <- as.numeric(max(year_choices))
                        min_year <- as.numeric(min(year_choices))
                        
                        updateSliderInput(
                                session,
                                "range_slider",
                                min = min_year,
                                max = max_year,
                                step = 1,
                                value = c(max(max_year - 2, min_year), max_year)
                        )
                }
                
                if (input$ar_view == "period") {
                        updateSelectInput(
                                session,
                                "select_period_ar",
                                choices = ar_choices$choices,
                                selected = ar_choices$selected
                        )
                }
        })
        
        
        
        selected_mr_data <- reactive({
                req(hikp_mr_data())
                
                data <- hikp_mr_data() %>%
                        mutate(time = as.yearmon(time)) %>%
                        filter(geo %in% input$countries_mr)
                
                data_no_na <- data %>%
                        filter(!is.na(Contr_j) | !is.na(m_rate_00))
                
                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_time = min(time, na.rm = TRUE), .groups = "drop")
                
                max_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(max_time = max(time, na.rm = TRUE), .groups = "drop")
                
                first_common_time <- max(min_years$min_time)
                last_common_time  <- min(max_years$max_time)
                
                available_times <- data %>%
                        filter(time >= first_common_time,
                               time <= last_common_time) %>%
                        distinct(time) %>%
                        arrange(desc(time)) %>%
                        pull(time)
                
                period_list <- format(available_times, "%Y %B")
                
                latest_time <- max(available_times)
                
                if (input$mr_view == "period") {
                        # Graph by period: only last month
                        default_periods <- format(latest_time, "%Y %B")
                        
                } else {
                        # Graph by country: same month last 5 years
                        
                        wanted_times <- latest_time - 0:4
                        
                        default_periods <- format(
                                available_times[available_times %in% wanted_times],
                                "%Y %B"
                        )
                        
                        if (length(default_periods) < 5) {
                                default_periods <- format(latest_time, "%Y %B")
                        }
                }
                
                list(
                        choices = period_list,
                        selected = default_periods
                )
        })
        
        # Update selectInput to show the list of available periods (monthly rate)
        observe({
                req(input$countries_mr, input$coicop_mr, input$mr_view)
                
                periods <- selected_mr_data()
                
                updateSelectInput(
                        session,
                        "select_period_mr",
                        choices = periods$choices,
                        selected = periods$selected
                )
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
        
        
        # Update selectInput to show the list of available periods (seasonality)
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
                                group_by(coicop18, geo, measure) %>%
                                summarise(mean_values = mean(values, na.rm = TRUE), .groups = "drop")
                } else if (input$period_type == "Month") {
                        avg_refyear <- data %>%
                                filter(as.character(format(time, "%Y %B")) == input$select_years) %>%
                                group_by(coicop18, geo, measure) %>%
                                summarise(mean_values = mean(values, na.rm = TRUE), .groups = "drop")
                }

                rebased_data <- left_join(
                        hikp_data(),
                        avg_refyear,
                        by = c("coicop18", "geo", "measure")
                )
                
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
                        req(hikp_w_data(), input$weights_view)
                        data <- hikp_w_data()
                        
                        # Filter data based on selected years
                        time_filtered_data <- data[
                                as.numeric(format(data$time, "%Y")) >= input$range_slider_w[1] &
                                        as.numeric(format(data$time, "%Y")) <= input$range_slider_w[2], ]
                        
                        if (is.null(input$coicop_w) == FALSE && is.null(input$countries_w) == FALSE) {
                                
                                subplots <- list()
                                
                                # ---------------------------------------------------
                                # 1) ONE GRAPH PER COUNTRY
                                # ---------------------------------------------------
                                if (input$weights_view == "country") {
                                        
                                        weight_by_geo <- time_filtered_data %>%
                                                group_by(geo, time) %>%
                                                summarise(sum = sum(values, na.rm = TRUE), .groups = "drop")
                                        
                                        max_weight <- max(weight_by_geo$sum, na.rm = TRUE)
                                        
                                        count <- 0
                                        for (geo_value in unique(time_filtered_data$geo)) {
                                                
                                                count <- count + 1
                                                filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                                filtered_data$values <- ifelse(is.na(filtered_data$values), 0, filtered_data$values)
                                                
                                                if (all(filtered_data$values == 0)) {
                                                        
                                                        text_annotation <- list(
                                                                x = 0.25,
                                                                y = 0.75,
                                                                xref = "paper",
                                                                yref = "paper",
                                                                text = "Data unavailable",
                                                                showarrow = FALSE,
                                                                font = list(size = 14, color = "black")
                                                        )
                                                        
                                                        subpl <- plot_ly(
                                                                filtered_data,
                                                                x = ~time,
                                                                y = ~values,
                                                                showlegend = FALSE,
                                                                color = ~code_label,
                                                                legendgroup = ~code_label
                                                        )
                                                        
                                                        subpl <- subpl %>%
                                                                layout(
                                                                        barmode = 'stack',
                                                                        annotations = list(
                                                                                text = geo_value, xref = "paper", yref = "paper",
                                                                                yanchor = "bottom", xanchor = "center",
                                                                                align = "center", x = 0.5, y = 0.95, showarrow = FALSE
                                                                        ),
                                                                        xaxis = list(title = ""),
                                                                        yaxis = list(title = "Weight, per mille")
                                                                ) %>%
                                                                layout(annotations = list(text_annotation))
                                                        
                                                } else {
                                                        
                                                        if (count == 1) {
                                                                subpl <- plot_ly(
                                                                        filtered_data,
                                                                        x = ~time,
                                                                        y = ~values,
                                                                        color = ~code_label,
                                                                        type = "bar",
                                                                        legendgroup = ~code_label
                                                                )
                                                        } else {
                                                                subpl <- plot_ly(
                                                                        filtered_data,
                                                                        x = ~time,
                                                                        y = ~values,
                                                                        color = ~code_label,
                                                                        type = "bar",
                                                                        legendgroup = ~code_label,
                                                                        showlegend = FALSE
                                                                )
                                                        }
                                                        
                                                        subpl <- subpl %>%
                                                                layout(
                                                                        yaxis = list(range = c(0, max_weight * 1.05), title = "Weight, per mille"),
                                                                        xaxis = list(title = ""),
                                                                        barmode = 'stack',
                                                                        annotations = list(
                                                                                text = geo_value, xref = "paper", yref = "paper",
                                                                                yanchor = "bottom", xanchor = "center",
                                                                                align = "center", x = 0.5, y = 0.95, showarrow = FALSE
                                                                        )
                                                                )
                                                }
                                                
                                                subplots[[geo_value]] <- subpl
                                        }
                                }
                                
                                # ---------------------------------------------------
                                # 2) ONE GRAPH PER YEAR
                                # ---------------------------------------------------
                                if (input$weights_view == "year") {
                                        
                                        time_filtered_data$year <- as.numeric(format(time_filtered_data$time, "%Y"))
                                        
                                        weight_by_year_geo <- time_filtered_data %>%
                                                group_by(year, geo) %>%
                                                summarise(sum = sum(values, na.rm = TRUE), .groups = "drop")
                                        
                                        max_weight <- max(weight_by_year_geo$sum, na.rm = TRUE)
                                        
                                        count <- 0
                                        for (year_value in sort(unique(time_filtered_data$year))) {
                                                
                                                count <- count + 1
                                                filtered_data <- time_filtered_data[time_filtered_data$year == year_value, ]
                                                filtered_data$values <- ifelse(is.na(filtered_data$values), 0, filtered_data$values)
                                                
                                                # Sort countries by selected aggregate weight.
                                                # Special case:
                                                # If selected aggregate is CP00, sort by CP01 instead of total weight,
                                                # since CP00 is always 1000 for all countries.
                                                if (input$coicop_w == "CP00") {
                                                        sort_data <- hikp_w_data() %>%
                                                                mutate(year = as.numeric(format(time, "%Y"))) %>%
                                                                filter(year == year_value, coicop18 == "CP01", geo %in% unique(filtered_data$geo)) %>%
                                                                group_by(geo) %>%
                                                                summarise(sort_weight = sum(values, na.rm = TRUE), .groups = "drop")
                                                } else {
                                                        # For all other selected aggregates, sort by the total stacked height
                                                        # in the current graph (= sum of shown child weights for each country)
                                                        sort_data <- filtered_data %>%
                                                                group_by(geo) %>%
                                                                summarise(sort_weight = sum(values, na.rm = TRUE), .groups = "drop")
                                                }
                                                
                                                geo_order <- sort_data %>%
                                                        arrange(sort_weight, geo) %>%
                                                        pull(geo)
                                                
                                                # Keep any countries not found in sort_data at the end
                                                remaining_geo <- setdiff(unique(as.character(filtered_data$geo)), geo_order)
                                                geo_order <- c(geo_order, sort(remaining_geo))
                                                
                                                filtered_data$geo <- factor(filtered_data$geo, levels = geo_order)
                                                
                                                if (all(filtered_data$values == 0)) {
                                                        
                                                        text_annotation <- list(
                                                                x = 0.25,
                                                                y = 0.75,
                                                                xref = "paper",
                                                                yref = "paper",
                                                                text = "Data unavailable",
                                                                showarrow = FALSE,
                                                                font = list(size = 14, color = "black")
                                                        )
                                                        
                                                        subpl <- plot_ly(
                                                                filtered_data,
                                                                x = ~geo,
                                                                y = ~values,
                                                                showlegend = FALSE,
                                                                color = ~code_label,
                                                                legendgroup = ~code_label
                                                        )
                                                        
                                                        subpl <- subpl %>%
                                                                layout(
                                                                        barmode = 'stack',
                                                                        annotations = list(
                                                                                text = as.character(year_value), xref = "paper", yref = "paper",
                                                                                yanchor = "bottom", xanchor = "center",
                                                                                align = "center", x = 0.5, y = 0.95, showarrow = FALSE
                                                                        ),
                                                                        xaxis = list(title = "", categoryorder = "array", categoryarray = geo_order),
                                                                        yaxis = list(title = "Weight, per mille")
                                                                ) %>%
                                                                layout(annotations = list(text_annotation))
                                                        
                                                } else {
                                                        
                                                        if (count == 1) {
                                                                subpl <- plot_ly(
                                                                        filtered_data,
                                                                        x = ~geo,
                                                                        y = ~values,
                                                                        color = ~code_label,
                                                                        type = "bar",
                                                                        legendgroup = ~code_label
                                                                )
                                                        } else {
                                                                subpl <- plot_ly(
                                                                        filtered_data,
                                                                        x = ~geo,
                                                                        y = ~values,
                                                                        color = ~code_label,
                                                                        type = "bar",
                                                                        legendgroup = ~code_label,
                                                                        showlegend = FALSE
                                                                )
                                                        }
                                                        
                                                        subpl <- subpl %>%
                                                                layout(
                                                                        yaxis = list(range = c(0, max_weight * 1.05), title = "Weight, per mille"),
                                                                        xaxis = list(
                                                                                title = "",
                                                                                categoryorder = "array",
                                                                                categoryarray = geo_order
                                                                        ),
                                                                        barmode = 'stack',
                                                                        annotations = list(
                                                                                text = as.character(year_value), xref = "paper", yref = "paper",
                                                                                yanchor = "bottom", xanchor = "center",
                                                                                align = "center", x = 0.5, y = 0.95, showarrow = FALSE
                                                                        )
                                                                )
                                                }
                                                
                                                subplots[[as.character(year_value)]] <- subpl
                                        }
                                }
                                
                                # ---------------------------------------------------
                                # Common subplot layout
                                # ---------------------------------------------------
                                plot_rows <- 1
                                
                                if (length(subplots) > 2) {
                                        plot_rows <- length(subplots) - 1
                                }
                                
                                if ((length(subplots) %% 2) == 0) {
                                        plot_rows <- length(subplots) / 2
                                } else {
                                        plot_rows <- (length(subplots) + 1) / 2
                                }
                                
                                layout <- plotly::subplot(
                                        subplots,
                                        nrows = plot_rows,
                                        titleX = TRUE,
                                        shareX = FALSE,
                                        titleY = TRUE,
                                        shareY = TRUE
                                )
                                
                                if (length(subplots) > 2) {
                                        layout <- layout %>% layout(height = plot_rows * 300, width = 1100)
                                }
                                if (length(subplots) <= 2) {
                                        layout <- layout %>% layout(height = plot_rows * 475, width = 1100)
                                }
                                
                                plotly_plot_w <- plotly_build(layout)
                                plotly_plot_w$x$attrs$legend$x$class <- "plot-container"
                                
                                return(plotly_plot_w)
                        }
                })
        })
        
        # Create the plot (annual rate of change)
        observeEvent(input$update_ar, {
                output$plot_ar <- renderPlotly({
                        
                        req(hikp_ar_data(), input$coicop_ar, input$ar_view)
                        
                        data <- hikp_ar_data()
                        
                        if (input$ar_view == "country") {
                                req(input$range_slider)
                                
                                time_filtered_data <- data[
                                        as.numeric(format(data$time, "%Y")) >= input$range_slider[1] &
                                                as.numeric(format(data$time, "%Y")) <= input$range_slider[2],
                                ]
                        }
                        
                        if (input$ar_view == "period") {
                                req(input$select_period_ar)
                                
                                time_filtered_data <- data[
                                        format(data$time, "%Y %B") == input$select_period_ar,
                                ]
                        }
                        
                        # Check: do contribution sum to +/- 5 % of the annual rates of change?
                        
                        ar_check <- time_filtered_data %>%
                                group_by(geo, time) %>%
                                summarise(
                                        sum_contr = sum(Contr_j, na.rm = TRUE),
                                        ann_rate = first(ann_rate_00[!is.na(ann_rate_00)]),
                                        n_contr = sum(!is.na(Contr_j)),
                                        .groups = "drop"
                                ) %>%
                                mutate(
                                        valid_sum = case_when(
                                                is.na(ann_rate) ~ FALSE,
                                                n_contr == 0 ~ FALSE,
                                                TRUE ~ abs(sum_contr - ann_rate) <= 0.05
                                        )
                                )
                                time_filtered_check<<-time_filtered_data
                                ar_check2<<-ar_check
                        
                        valid_geos <- ar_check %>%
                                group_by(geo) %>%
                                summarise(all_valid = all(valid_sum), .groups = "drop") %>%
                                filter(all_valid) %>%
                                pull(geo)
                        
                        time_filtered_data <- time_filtered_data %>%
                                filter(geo %in% valid_geos)
                        
                        validate(
                                need(nrow(time_filtered_data) > 0,
                                     "Due to data gaps, the annual contributions cannot be calculated.")
                        )
                        
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
                        
                        if (input$ar_view == "period") {
                                
                                period_label <- input$select_period_ar
                                
                                time_filtered_data$sign <- ifelse(
                                        time_filtered_data$Contr_j > 0, 1,
                                        ifelse(time_filtered_data$Contr_j < 0, -1, 1)
                                )
                                
                                filtered_data <- time_filtered_data %>%
                                        mutate(geo_chr = as.character(geo))
                                
                                filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)
                                
                                geo_order <- filtered_data %>%
                                        group_by(geo_chr) %>%
                                        summarise(
                                                sort_rate = ifelse(
                                                        all(is.na(ann_rate_00)),
                                                        NA_real_,
                                                        first(na.omit(ann_rate_00))
                                                ),
                                                .groups = "drop"
                                        ) %>%
                                        arrange(sort_rate, geo_chr) %>%
                                        pull(geo_chr)
                                
                                geo_order <- unique(as.character(geo_order))
                                
                                filtered_data <- filtered_data %>%
                                        mutate(geo_chr = factor(geo_chr, levels = geo_order))
                                
                                bar_data <- filtered_data %>%
                                        filter(!is.na(code_label_wrapped))
                                
                                subpl <- plot_ly(
                                        bar_data,
                                        x = ~geo_chr,
                                        y = ~Contr_j,
                                        color = ~code_label_wrapped,
                                        legendgroup = ~code_label_wrapped,
                                        type = "bar",
                                        showlegend = TRUE
                                )
                                
                                if (input$contribution_type_ar == "selected higher aggregate") {
                                        
                                        line_data <- filtered_data %>%
                                                group_by(geo_chr) %>%
                                                summarise(
                                                        ann_rate_00 = first(na.omit(ann_rate_00)),
                                                        .groups = "drop"
                                                ) %>%
                                                mutate(geo_chr = factor(as.character(geo_chr), levels = geo_order))
                                        
                                        subpl <- subpl %>%
                                                add_trace(
                                                        data = line_data,
                                                        x = ~geo_chr,
                                                        y = ~ann_rate_00,
                                                        type = "scatter",
                                                        mode = "markers",
                                                        inherit = FALSE,
                                                        name = "Annual rate M/M-12",
                                                        marker = list(color = "black", size = 10),
                                                        showlegend = TRUE
                                                )
                                }
                                
                                y_title <- if (input$contribution_type_ar == "selected higher aggregate") {
                                        "Annual rate of change, %"
                                } else {
                                        "Contrib. to HICP M/M-12, %-points"
                                }
                                
                                subpl <- subpl %>%
                                        layout(
                                                yaxis = list(
                                                        range = c(
                                                                ifelse(min_ar == 0, min_ar, min_ar * 1.05),
                                                                ifelse(max_ar == 0, max_ar, max_ar * 1.05)
                                                        ),
                                                        title = y_title
                                                ),
                                                xaxis = list(
                                                        title = "",
                                                        type = "category",
                                                        categoryorder = "array",
                                                        categoryarray = geo_order
                                                ),
                                                barmode = "relative",
                                                annotations = list(
                                                        text = period_label,
                                                        xref = "paper",
                                                        yref = "paper",
                                                        yanchor = "bottom",
                                                        xanchor = "center",
                                                        align = "center",
                                                        x = 0.5,
                                                        y = 0.95,
                                                        showarrow = FALSE
                                                ),
                                                font = list(size = 11),
                                                legend.traceorder = "reversed",
                                                height = 600,
                                                width = 1100
                                        )
                                
                                plotly_plot_ar <- plotly_build(subpl)
                                plotly_plot_ar$x$attrs$legend$x$class <- "plot-container"
                                
                                return(plotly_plot_ar)
                        }
                        
                        if (input$ar_view == "country") {
                        
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
                                                
                                                bar_data <- filtered_data %>%
                                                        filter(!is.na(code_label_wrapped))
                                                
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
                                                        
                                                        
                                                        subpl <- plot_ly(bar_data, x = ~time, y = ~Contr_j, showlegend = FALSE,
                                                                         color = ~code_label_wrapped, legendgroup = ~code_label_wrapped)
                                                        
                                                        # Add axis labels
                                                        if (input$contribution_type_ar == "selected higher aggregate") {
                                                                subpl <- subpl %>% layout(
                                                                        xaxis = list(title = ""),
                                                                        yaxis = list(title = "Annual rate of change, %")
                                                                )
                                                        }
                                                        
                                                        if (input$contribution_type_ar == "all-items HICP") {
                                                                subpl <- subpl %>% layout(
                                                                        xaxis = list(title = ""),
                                                                        yaxis = list(title = "Contrib. to HICP M/M-12, %-points")
                                                                )
                                                        }
                                                        
                                                        subpl <- subpl %>% layout(annotations = list(text_annotation))
                                                } else {
                                                        # Create a stacked bar chart for current country
                                                        
                                                        if(count == 1) {
                                                                subpl <- plot_ly(bar_data, x = ~time, y = ~Contr_j, color = ~code_label_wrapped,
                                                                                 type = "bar", legendgroup = ~code_label_wrapped)
                                                        } else {
                                                                subpl <- plot_ly(bar_data, x = ~time, y = ~Contr_j, color = ~code_label_wrapped,
                                                                                 type = "bar", legendgroup = ~code_label_wrapped, showlegend = FALSE)
                                                        }
                                                        
                                                        
                                                        if (input$contribution_type_ar == "selected higher aggregate") {
                                                                
                                                                subpl <- subpl %>% layout(
                                                                        yaxis = list(range = c(
                                                                                ifelse(min_ar == 0, min_ar, min_ar * 1.05),
                                                                                ifelse(max_ar == 0, max_ar, max_ar * 1.05)
                                                                        )),
                                                                        barmode = "relative",
                                                                        annotations = list(
                                                                                text = geo_value,
                                                                                xref = "paper",
                                                                                yref = "paper",
                                                                                yanchor = "bottom",
                                                                                xanchor = "center",
                                                                                align = "center",
                                                                                x = 0.5,
                                                                                y = 0.95,
                                                                                showarrow = FALSE
                                                                        ),
                                                                        xaxis = list(tickformat = "%Y %B", tickangle = 45)
                                                                )
                                                                
                                                                line_data <- filtered_data %>%
                                                                        filter(!is.na(ann_rate_00)) %>%
                                                                        distinct(time, geo, .keep_all = TRUE) %>%
                                                                        arrange(time)
                                                                
                                                                subpl <- subpl %>% add_trace(
                                                                        data = line_data,
                                                                        x = ~time,
                                                                        y = ~ann_rate_00,
                                                                        type = "scatter",
                                                                        mode = "lines",
                                                                        inherit = FALSE,
                                                                        name = "Annual rate M/M-12",
                                                                        line = list(color = "black", dash = "dash", width = 1.5),
                                                                        showlegend = (count == 1)
                                                                ) %>%
                                                                        layout(xaxis = list(title = ""), yaxis = list(title = "Annual rate of change, %"))
                                                        }
                                                        
                                                        if (input$contribution_type_ar == "all-items HICP") {
                                                                
                                                                subpl <- subpl %>% layout(
                                                                        yaxis = list(range = c(
                                                                                ifelse(min_ar == 0, min_ar, min_ar * 1.05),
                                                                                ifelse(max_ar == 0, max_ar, max_ar * 1.05)
                                                                        )),
                                                                        barmode = "relative",
                                                                        annotations = list(
                                                                                text = geo_value,
                                                                                xref = "paper",
                                                                                yref = "paper",
                                                                                yanchor = "bottom",
                                                                                xanchor = "center",
                                                                                align = "center",
                                                                                x = 0.5,
                                                                                y = 0.95,
                                                                                showarrow = FALSE
                                                                        ),
                                                                        xaxis = list(tickformat = "%Y %B", tickangle = 45)
                                                                ) %>%
                                                                        layout(xaxis = list(title = ""), yaxis = list(title = "Contrib. to HICP M/M-12, %-points"))
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
                                        layout <- layout %>%
                                                layout(
                                                        font = list(size = 11),
                                                        legend.traceorder = "reversed"
                                                )
                                        
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
                                
                
                                
                                
                        }
                })
                
        })
        
        # Create the plot (monthly rate of change)
        observeEvent(input$update_mr, {
                output$plot_mr <- renderPlotly({
                        
                        #req(hikp_mr_data(), input$coicop_mr,input$select_period_mr)
                        req(hikp_mr_data(), input$coicop_mr, input$select_period_mr, input$mr_view)
                        
                        data <- hikp_mr_data()
                        
                        # Filter data based on selected year range
                        
                        time_filtered_data <- data[format(data$time, "%Y %B") %in% input$select_period_mr, ]
                        
                        # Determine common max and min on y-axis
                        contr_by_geo <- time_filtered_data %>%
                                group_by(geo, time) %>%
                                summarise(sum_value = sum(Contr_j, na.rm = TRUE))
                        
                        max_contr <-max(contr_by_geo$sum_value)
                        
                        min_contr <-min(contr_by_geo$sum_value)
                        
                        mr_00_by_geo <- time_filtered_data %>%
                                group_by(geo, time) %>%
                                summarise(max_value = max(m_rate_00, na.rm = TRUE),min_value = min(m_rate_00, na.rm = TRUE))

                        max_mr_00 <-max(mr_00_by_geo$max_value)
                        
                        min_mr_00 <-min(mr_00_by_geo$min_value)
                        
                        max_mr<-max(max_mr_00,max_contr)
                        
                        min_mr<-min(min_mr_00,min_contr,0)
                        
                        if (input$mr_view == "period") {
                                
                                subplots <- list()
                                count <- 0
                                
                                period_order <- time_filtered_data %>%
                                        mutate(time_ym = as.yearmon(time)) %>%
                                        distinct(time_ym) %>%
                                        arrange(desc(time_ym)) %>%
                                        pull(time_ym)
                                
                                for (period_value in period_order) {
                                        
                                        count <- count + 1
                                        
                                        filtered_data <- time_filtered_data %>%
                                                mutate(
                                                        time_ym = as.yearmon(time),
                                                        geo_chr = as.character(geo)
                                                ) %>%
                                                filter(time_ym == as.yearmon(period_value))
                                        
                                        filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)
                                        
                                        geo_order <- filtered_data %>%
                                                group_by(geo_chr) %>%
                                                summarise(
                                                        sort_rate = first(na.omit(m_rate_00)),
                                                        .groups = "drop"
                                                ) %>%
                                                arrange(sort_rate, geo_chr) %>%
                                                pull(geo_chr)
                                        
                                        geo_order <- unique(as.character(geo_order))
                                        
                                        filtered_data <- filtered_data %>%
                                                mutate(geo_chr = factor(geo_chr, levels = geo_order))
                                        
                                        period_label <- format(as.yearmon(period_value), "%Y %B")
                                        
                                        bar_data <- filtered_data %>%
                                                filter(!is.na(code_label_wrapped))
                                        
                                        subpl <- plot_ly(
                                                bar_data,
                                                x = ~geo_chr,
                                                y = ~Contr_j,
                                                color = ~code_label_wrapped,
                                                legendgroup = ~code_label_wrapped,
                                                type = "bar",
                                                showlegend = count == 1
                                        )
                                        
                                        line_data <- filtered_data %>%
                                                group_by(geo_chr) %>%
                                                summarise(
                                                        m_rate_00 = first(na.omit(m_rate_00)),
                                                        .groups = "drop"
                                                ) %>%
                                                mutate(geo_chr = factor(as.character(geo_chr), levels = geo_order))
                                        
                                        subpl <- subpl %>%
                                                add_trace(
                                                        data = line_data,
                                                        x = ~geo_chr,
                                                        y = ~m_rate_00,
                                                        type = "scatter",
                                                        mode = "markers",
                                                        inherit = FALSE,
                                                        name = "Monthly rate M/M-1",
                                                        marker = list(color = "black", size = 10),
                                                        showlegend = count == 1
                                                ) %>%
                                                layout(
                                                        yaxis = list(
                                                                range = c(min_mr - 1.0, max_mr + 1.0),
                                                                title = "Monthly rate of change, %"
                                                        ),
                                                        xaxis = list(
                                                                title = "",
                                                                type = "category",
                                                                categoryorder = "array",
                                                                categoryarray = geo_order
                                                        ),
                                                        barmode = "relative",
                                                        annotations = list(
                                                                text = period_label,
                                                                xref = "paper",
                                                                yref = "paper",
                                                                yanchor = "bottom",
                                                                xanchor = "center",
                                                                align = "center",
                                                                x = 0.5,
                                                                y = 0.95,
                                                                showarrow = FALSE
                                                        )
                                                )
                                        
                                        subplots[[period_label]] <- subpl
                                }
                                
                                plot_rows <- if ((length(subplots) %% 2) == 0) {
                                        length(subplots) / 2
                                } else {
                                        (length(subplots) + 1) / 2
                                }
                                
                                layout <- plotly::subplot(
                                        subplots,
                                        nrows = plot_rows,
                                        titleX = TRUE,
                                        shareX = FALSE,
                                        titleY = TRUE,
                                        shareY = TRUE
                                ) %>%
                                        layout(
                                                font = list(size = 11),
                                                legend.traceorder = "reversed"
                                        )
                                
                                if (length(subplots) > 2) {
                                        layout <- layout %>% layout(height = plot_rows * 300, width = 1100)
                                }
                                if (length(subplots) <= 2) {
                                        layout <- layout %>% layout(height = plot_rows * 475, width = 1100)
                                }
                                
                                plotly_plot_mr <- plotly_build(layout)
                                plotly_plot_mr$x$attrs$legend$x$class <- "plot-container"
                                
                                return(plotly_plot_mr)
                        }
                        
                        if (input$mr_view == "country") {
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
                                        
                                        bar_data <- filtered_data %>%
                                                filter(!is.na(code_label_wrapped))
                                        
                                        # Filter unique time values for the x-axis tick labels
                                        x_axis_tick_labels <- unique(filtered_data$time)
                                        
                                        # Filter unique time values for the x-axis tick labels
                                        x_axis_tick_labels <- unique(filtered_data$time)
                                        
                                        # Define the x-axis layout with categoryarray
                                        x_axis_layout <- list(
                                                title = "",
                                                tickmode = "array",
                                                tickvals = x_axis_tick_labels,
                                                ticktext = x_axis_tick_labels,
                                                tickformat = "%Y %B"
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
                                                
                                                subpl <- plot_ly(bar_data,x = ~time, y = ~Contr_j, showlegend = FALSE,color = ~code_label_wrapped,legendgroup = ~code_label_wrapped)  # Empty subplot
                                                subpl <- subpl %>% layout(barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                
                                                # Add axis labels
                                                subpl <- subpl %>% layout(xaxis = x_axis_layout, yaxis = list(title = "Monthly rate of change, %"))
                                                subpl <- subpl %>% layout(annotations = list(text_annotation),xaxis = x_axis_layout)
                                        } else {
                                                # Create a stacked bar chart for current country
                                                
                                                if(count == 1) {
                                                        subpl <- plot_ly(bar_data, x = ~time, y = ~Contr_j, color = ~code_label_wrapped, type = "bar", legendgroup = ~code_label_wrapped)
                                                } else {
                                                        subpl <- plot_ly(bar_data, x = ~time, y = ~Contr_j, color = ~code_label_wrapped, type = "bar", legendgroup = ~code_label_wrapped, showlegend = FALSE)
                                                }
                                                
                                                subpl <- subpl %>% layout(yaxis = list(range = c((min_mr-1.0), max_mr + 1.0)),barmode = 'relative', annotations = list(text = geo_value, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center", x = 0.5, y = 0.95, showarrow = FALSE))
                                                
                                                # Lägg till linjediagram
                                                
                                                line_data <- filtered_data %>%
                                                        filter(!is.na(m_rate_00)) %>%
                                                        distinct(time, geo, .keep_all = TRUE) %>%
                                                        arrange(time)
                                                
                                                subpl <- subpl %>%
                                                        add_trace(
                                                                data = line_data,
                                                                x = ~time,
                                                                y = ~m_rate_00,
                                                                type = "scatter",
                                                                mode = "markers",
                                                                inherit = FALSE,
                                                                name = "Monthly rate M/M-1",
                                                                marker = list(color = "black", size = 10),
                                                                showlegend = count == 1
                                                        )
                                                
                                                # Add axis labels
                                                subpl <- subpl %>% layout(xaxis = x_axis_layout, yaxis = list(title = "Monthly rate of change, %"))
                                        }
                                        
                                        # Add the subplot in the list
                                        subplots[[geo_value]] <- subpl
                                }}
                                
                                
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
                                arrange(coicop18, geo, year,month)
                        
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
                        
                        selected_countries <- input$countries
                        
                        backdrop_countries <- if (isTRUE(input$index_backdrop_eu)) {
                                setdiff(country_groups[["All EU countries"]], selected_countries)
                        } else {
                                character(0)
                        }
                        
                        countries_to_plot <- union(selected_countries, backdrop_countries)
                        
                        data <- data %>%
                                filter(geo %in% countries_to_plot) %>%
                                mutate(is_backdrop = geo %in% backdrop_countries)
                        
                        # Find the first year where values contains index numbers
                        first_non_na_year <- min(data$time[!is.na(data$values)])
                        
                        # Remove rows prior to the first year where values contains numbers
                        data <- filter(data, time >= first_non_na_year)
                        
                        data$time <- as.Date(as.yearmon(data$time), format="%Y %B")
                        
                        if(is.null(input$coicops)==FALSE && new_plot_data==TRUE && new_rebased_data==TRUE){
                                #plotly_plot <- plot_ly(data, x = ~time, y = ~newbase, color = ~geo, linetype = ~coicop18, type = 'scatter', mode = 'lines')
                                
                                data <- data %>%
                                        mutate(
                                                series_label = case_when(
                                                        measure == "HICP-CT" ~ paste0(geo, " CT"),
                                                        measure == "HICP" ~ as.character(geo),
                                                        TRUE ~ paste(geo, measure)
                                                ),
                                                line_group = paste(geo, coicop18, measure, sep = "_")
                                        )
                                
                                plotly_plot <- plot_ly()
                                
                                plotly_plot <- plot_ly()
                                
                                # Draw backdrop first, so selected countries will come out on top
                                for (series in unique(data$line_group[data$is_backdrop])) {
                                        
                                        series_data <- data %>%
                                                filter(line_group == series) %>%
                                                arrange(time)
                                        
                                        plotly_plot <- plotly_plot %>%
                                                add_trace(
                                                        data = series_data,
                                                        x = ~time,
                                                        y = ~newbase,
                                                        type = "scatter",
                                                        mode = "lines",
                                                        name = unique(series_data$series_label),
                                                        line = list(
                                                                color = "rgba(150,150,150,0.35)",
                                                                width = 0.8,
                                                                dash = ifelse(unique(series_data$measure) == "HICP-CT", "dash", "solid")
                                                        ),
                                                        hoverinfo = "text",
                                                        text = ~paste0(
                                                                geo, "<br>",
                                                                measure, "<br>",
                                                                coicop18, "<br>",
                                                                format(time, "%Y-%m"), ": ",
                                                                round(newbase, 1)
                                                        ),
                                                        showlegend = FALSE
                                                )
                                }
                                
                                # Draw selected countries by normal colors
                                for (series in unique(data$line_group[!data$is_backdrop])) {
                                        
                                        series_data <- data %>%
                                                filter(line_group == series) %>%
                                                arrange(time)
                                        
                                        plotly_plot <- plotly_plot %>%
                                                add_trace(
                                                        data = series_data,
                                                        x = ~time,
                                                        y = ~newbase,
                                                        type = "scatter",
                                                        mode = "lines",
                                                        name = unique(series_data$series_label),
                                                        line = list(
                                                                dash = ifelse(unique(series_data$measure) == "HICP-CT", "dash", "solid"),
                                                                width = 2
                                                        ),
                                                        showlegend = TRUE
                                                )
                                }
                                
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
                                        
                                labled_input_coicop<-left_join(data.frame(input$coicops),coicop_set, by = c("input.coicops"="coicop18_code"))
                                
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
                     ddata<-data%>%select(coicop18,geo,time,newbase)
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
                        #data$time <- substr(data$time, 1, 4)
                        data$year <- year(data$time)
                        data$month <- month(data$time)
                        ddata<-data%>%select(coicop18,geo,year,month,Contr_j,ann_rate_00)
                        
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
                        ddata<-data%>%select(coicop18,geo,time,month,year,Contr_j,m_rate_00)
                        
                        write.csv(ddata, con_mr)
                })

}


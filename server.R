library(shiny)
library(eurostat)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(zoo)
library(lubridate)
library(plotly)

function(input, output, session) {
        add_country_group_observers <- function(prefix, input_id, session, input, country_groups) {
                observeEvent(input[[paste0(prefix, "_eu")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["EU"]])
                })
                observeEvent(input[[paste0(prefix, "_efta")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["EFTA"]])
                })
                observeEvent(input[[paste0(prefix, "_eu_efta")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["EU + EFTA"]])
                })
                observeEvent(input[[paste0(prefix, "_euro")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Euro area"]])
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
                observeEvent(input[[paste0(prefix, "_baltic")]], {
                        updateSelectInput(session, input_id, selected = country_groups[["Baltic"]])
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

        selected_hierarchy <- function(classification) {
                if (identical(classification, "sa")) {
                        sa_hierarchy
                } else if (identical(classification, "ap")) {
                        ap_hierarchy
                } else {
                        coicop_set_hierarchy
                }
        }

        selected_label_set <- function(classification) {
                selected_hierarchy(classification) %>%
                        select(coicop18_code, code_label)
        }

        eurostat_coicop_code <- function(x) {
                ifelse(x == "CP00", "TOTAL", x)
        }

        app_coicop_code <- function(x) {
                ifelse(x == "TOTAL", "CP00", x)
        }

        selected_hicp_index_dataset <- function(measure) {
                if (identical(measure, "HICP_CT")) {
                        "prc_hicp_ct"
                } else {
                        "prc_hicp_minr"
                }
        }

        selected_hicp_measure_label <- function(measure) {
                if (identical(measure, "HICP_CT")) {
                        "HICP-CT"
                } else {
                        "HICP"
                }
        }

        selected_target_coicop <- function(contribution_type, selected_coicop) {
                if (identical(contribution_type, "all-items HICP")) {
                        "TOTAL"
                } else {
                        selected_coicop
                }
        }

        null_coalesce <- function(x, y) {
                if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
        }

        get_direct_children <- function(hierarchy, selected_code) {
                hierarchy %>%
                        filter(parent_code == selected_code) %>%
                        arrange(coicop18_code)
        }

        get_descendants <- function(hierarchy, selected_code) {
                selected_code <- as.character(selected_code)
                all_descendants <- character(0)
                frontier <- selected_code

                repeat {
                        children <- hierarchy$coicop18_code[hierarchy$parent_code %in% frontier]
                        children <- setdiff(unique(children[!is.na(children)]), all_descendants)

                        if (length(children) == 0) break

                        all_descendants <- c(all_descendants, children)
                        frontier <- children
                }

                hierarchy %>%
                        filter(coicop18_code %in% all_descendants) %>%
                        arrange(coicop18_code)
        }

        coicop_level_name <- function(level) {
                if (is.null(level) || length(level) == 0 || is.na(level)) {
                        return(NA_character_)
                }

                if (level == 1) {
                        "total"
                } else if (level == 2) {
                        "division"
                } else if (level == 3) {
                        "group"
                } else if (level == 4) {
                        "class"
                } else {
                        "subclass"
                }
        }

        ecoicop_prefix_descendants <- function(hierarchy, selected_code) {
                selected_code <- as.character(selected_code)

                if (identical(selected_code, "CP00")) {
                        hierarchy %>%
                                filter(coicop18_code != "CP00") %>%
                                arrange(coicop18_code)
                } else {
                        hierarchy %>%
                                filter(
                                        coicop18_code != selected_code,
                                        startsWith(coicop18_code, selected_code)
                                ) %>%
                                arrange(coicop18_code)
                }
        }

        ecoicop_leaf_descendants <- function(hierarchy, selected_code) {
                # Use prefix logic rather than only parent_code. Some ECOICOP codes
                # contain zeroes or skip an intermediate published aggregate. If the
                # parent_code relation is imperfect, a parent and its child can both
                # be treated as leaves, which double-counts the basket and creates a
                # large negative aggregation residual. Prefix pruning keeps only
                # terminal, non-overlapping components under the selected aggregate.
                descendants <- ecoicop_prefix_descendants(hierarchy, selected_code)
                if (nrow(descendants) == 0) return(descendants)

                codes <- descendants$coicop18_code
                has_child <- vapply(
                        codes,
                        function(code) any(startsWith(codes, code) & nchar(codes) > nchar(code)),
                        logical(1)
                )

                descendants %>%
                        mutate(.has_child = has_child) %>%
                        filter(!.has_child) %>%
                        select(-.has_child) %>%
                        arrange(coicop18_code)
        }

        first_non_na <- function(x) {
                y <- x[!is.na(x)]
                if (length(y) == 0) NA_real_ else y[1]
        }

        finite_range <- function(x, default = c(-1, 1), pad = 0.06, min_padding = NULL) {
                x <- x[is.finite(x)]
                if (length(x) == 0) return(default)

                mn <- min(x, na.rm = TRUE)
                mx <- max(x, na.rm = TRUE)

                if (mn == mx) {
                        padding <- if (!is.null(min_padding)) min_padding else max(abs(mn) * pad, 1)
                        return(c(mn - padding, mx + padding))
                }

                # Expand the range around the observed min and max instead of
                # multiplying the end points. Multiplication works poorly for
                # seasonality indices around 100 because it can move the lower
                # limit above the observed minimum and visually clip the chart.
                pad_fraction <- ifelse(pad > 1, pad - 1, pad)
                padding <- (mx - mn) * pad_fraction
                if (!is.null(min_padding)) {
                        padding <- max(padding, min_padding)
                }

                c(mn - padding, mx + padding)
        }

        visible_y_range <- function(x, default = c(-1, 1), pad_fraction = 0.08, min_pad = 0.08) {
                x <- x[is.finite(x)]
                if (length(x) == 0) return(default)

                mn <- min(c(x, 0), na.rm = TRUE)
                mx <- max(c(x, 0), na.rm = TRUE)

                if (mn == mx) {
                        pad <- max(abs(mx) * pad_fraction, min_pad)
                        return(c(mn - pad, mx + pad))
                }

                pad <- max((mx - mn) * pad_fraction, min_pad)
                c(mn - pad, mx + pad)
        }

        stacked_component_range <- function(data, value_col, group_cols) {
                if (nrow(data) == 0 || !(value_col %in% names(data))) {
                        return(data.frame(
                                positive_sum = numeric(0),
                                negative_sum = numeric(0)
                        ))
                }

                tmp <- data
                tmp$.range_value <- tmp[[value_col]]
                tmp$.range_value <- ifelse(is.na(tmp$.range_value), 0, tmp$.range_value)
                tmp$.range_positive <- ifelse(tmp$.range_value > 0, tmp$.range_value, 0)
                tmp$.range_negative <- ifelse(tmp$.range_value < 0, tmp$.range_value, 0)

                tmp %>%
                        group_by(across(all_of(group_cols))) %>%
                        summarise(
                                positive_sum = sum(.range_positive, na.rm = TRUE),
                                negative_sum = sum(.range_negative, na.rm = TRUE),
                                .groups = "drop"
                        )
        }

        subplot_dims <- function(
                n_panels,
                ncols = NULL,
                container_width = 960,
                min_ratio = 1.1,
                max_ratio = 1.5,
                target_ratio = 1.3
        ) {
                if (is.null(ncols)) {
                        ncols <- if (n_panels == 1) 1 else 2
                }

                nrows <- ceiling(n_panels / ncols)
                panel_width <- container_width / ncols
                panel_height <- panel_width / target_ratio
                min_height <- panel_width / max_ratio
                max_height <- panel_width / min_ratio
                panel_height <- max(min_height, min(panel_height, max_height))

                # Plotly subplots need extra vertical room for row spacing,
                # facet titles, tick labels and margins. The visual frame around
                # the chart has intentionally been removed in the UI, but the page
                # still needs to reserve enough height for multi-panel charts.
                row_gap <- if (nrows > 1) (nrows - 1) * 70 else 0
                outer_margin <- 90
                total_height <- max(620, ceiling(nrows * panel_height + row_gap + outer_margin))

                list(
                        ncols = ncols,
                        nrows = nrows,
                        panel_width = panel_width,
                        panel_height = panel_height,
                        total_height = total_height
                )
        }

        component_style_map <- function(labels) {
                labels <- labels[!is.na(labels) & labels != ""]
                labels <- unique(as.character(labels))
                labels <- sort(labels)

                if (length(labels) == 0) {
                        return(data.frame(
                                label = character(0),
                                color = character(0),
                                pattern = character(0),
                                stringsAsFactors = FALSE
                        ))
                }

                # High-contrast colours first. Later entries reuse a nearby hue and
                # therefore get a subtle pattern overlay. This keeps small charts clean
                # while still separating components when many similar shades are needed.
                style_palette <- data.frame(
                        color = c(
                                "#0072B2", "#D55E00", "#009E73", "#CC79A7",
                                "#56B4E9", "#E69F00", "#009FB7", "#7B61A8",
                                "#3B6EA8", "#C95D63", "#5E8C31", "#B07AA1",
                                "#2A9D8F", "#A6761D", "#6A4C93", "#8C564B",
                                "#4E79A7", "#F28E2B", "#59A14F", "#AF7AA1"
                        ),
                        pattern = c(
                                "", "", "", "",
                                "/", "/", "\\", "\\",
                                "x", "x", "/", "/",
                                "-", "-", "+", "+",
                                ".", ".", "|", "|"
                        ),
                        stringsAsFactors = FALSE
                )

                palette_index <- ((seq_along(labels) - 1) %% nrow(style_palette)) + 1

                data.frame(
                        label = labels,
                        color = style_palette$color[palette_index],
                        pattern = style_palette$pattern[palette_index],
                        stringsAsFactors = FALSE
                )
        }

        add_styled_bar_traces <- function(
                plot,
                data,
                x_col,
                y_col,
                style_map,
                showlegend = TRUE,
                legend_col = "code_label_wrapped"
        ) {
                if (nrow(data) == 0 || nrow(style_map) == 0) return(plot)

                for (label_value in style_map$label) {
                        trace_data <- data[data[[legend_col]] == label_value & !is.na(data[[legend_col]]), , drop = FALSE]
                        if (nrow(trace_data) == 0) next

                        style_row <- style_map[match(label_value, style_map$label), , drop = FALSE]
                        marker_style <- list(
                                color = style_row$color,
                                line = list(color = "rgba(255,255,255,0.70)", width = 0.45)
                        )

                        if (!is.na(style_row$pattern) && style_row$pattern != "") {
                                marker_style$pattern <- list(
                                        shape = style_row$pattern,
                                        fillmode = "overlay",
                                        fgcolor = "rgba(35,35,35,0.36)",
                                        bgcolor = style_row$color,
                                        size = 8,
                                        solidity = 0.16
                                )
                        }

                        plot <- plot %>%
                                add_trace(
                                        data = trace_data,
                                        x = trace_data[[x_col]],
                                        y = trace_data[[y_col]],
                                        type = "bar",
                                        name = label_value,
                                        legendgroup = label_value,
                                        showlegend = showlegend,
                                        legendrank = 100 + match(label_value, style_map$label),
                                        marker = marker_style,
                                        hoverinfo = "all",
                                        inherit = FALSE
                                )
                }

                plot
        }

        observeEvent(input$classification_mr, {
                hierarchy <- selected_hierarchy(input$classification_mr)
                old_selection <- isolate(input$coicop_mr)
                new_selection <- if (!is.null(old_selection) && old_selection %in% hierarchy$coicop18_code) {
                        old_selection
                } else {
                        "CP00"
                }
                updateSelectInput(
                        session,
                        "coicop_mr",
                        choices = setNames(hierarchy$coicop18_code, hierarchy$code_label),
                        selected = new_selection
                )
        }, ignoreInit = FALSE)

        observeEvent(input$classification_ar, {
                hierarchy <- selected_hierarchy(input$classification_ar)
                old_selection <- isolate(input$coicop_ar)
                new_selection <- if (!is.null(old_selection) && old_selection %in% hierarchy$coicop18_code) {
                        old_selection
                } else {
                        "CP00"
                }
                updateSelectInput(
                        session,
                        "coicop_ar",
                        choices = setNames(hierarchy$coicop18_code, hierarchy$code_label),
                        selected = new_selection
                )
        }, ignoreInit = FALSE)

        observeEvent(input$classification_w, {
                hierarchy <- selected_hierarchy(input$classification_w)
                old_selection <- isolate(input$coicop_w)
                new_selection <- if (!is.null(old_selection) && old_selection %in% hierarchy$coicop18_code) {
                        old_selection
                } else {
                        "CP00"
                }
                updateSelectInput(
                        session,
                        "coicop_w",
                        choices = setNames(hierarchy$coicop18_code, hierarchy$code_label),
                        selected = new_selection
                )
        }, ignoreInit = FALSE)

        # ------------------------------------------------------------
        # Index tab data
        # ------------------------------------------------------------

        hikp_data <- eventReactive(input$update, {
                req(input$countries, input$coicops, input$index_measure)

                validate(
                        need(
                                !(length(input$index_measure) == 2 && length(input$coicops) > 1),
                                "When both HICP and HICP-CT are selected, please select only one COICOP aggregate."
                        )
                )

                coicops_selected <- eurostat_coicop_code(input$coicops)

                adjust_total <- isTRUE(input$index_adjust_total)
                adjust_eu27 <- isTRUE(input$index_adjust_eu27) && !("EU" %in% input$countries)

                validate(
                        need(
                                !(adjust_total && adjust_eu27),
                                "Please choose either the total-inflation adjustment or the EU27 adjustment, not both."
                        ),
                        need(
                                !(adjust_total && "TOTAL" %in% coicops_selected),
                                "The total-inflation adjustment is only meaningful for lower-level aggregates. Please remove all-items HICP from the selected product categories."
                        )
                )

                coicops_to_fetch <- if (adjust_total) {
                        union(coicops_selected, "TOTAL")
                } else {
                        coicops_selected
                }

                selected_countries <- input$countries
                backdrop_countries <- if (isTRUE(input$index_backdrop_eu)) {
                        setdiff(country_groups[["EU"]], selected_countries)
                } else {
                        character(0)
                }
                countries_to_fetch <- union(selected_countries, backdrop_countries)
                if (adjust_eu27) {
                        countries_to_fetch <- union(countries_to_fetch, "EU")
                }

                tryCatch({
                        data_list <- list()

                        if ("HICP" %in% input$index_measure) {
                                data_hicp <- get_eurostat(
                                        "prc_hicp_minr",
                                        filters = list(unit = "I25", geo = countries_to_fetch, coicop18 = coicops_to_fetch),
                                        update_cache = TRUE
                                ) %>%
                                        mutate(measure = "HICP")
                                data_list[["HICP"]] <- data_hicp
                        }

                        if ("HICP_CT" %in% input$index_measure) {
                                data_ct <- get_eurostat(
                                        "prc_hicp_ct",
                                        filters = list(unit = "I25", geo = countries_to_fetch, coicop18 = coicops_to_fetch),
                                        update_cache = TRUE
                                ) %>%
                                        mutate(measure = "HICP-CT")
                                data_list[["HICP_CT"]] <- data_ct
                        }

                        data <- bind_rows(data_list)
                        clean_eurostat_cache()
                        data$time <- as.yearmon(data$time)

                        new_plot_data <<- TRUE
                        data
                }, error = function(e) {
                        print(paste("No data available:", e$message))
                        NULL
                })
        })

        selected_data <- reactive({
                req(hikp_data())

                selected_countries_for_periods <- input$countries
                if (isTRUE(input$index_adjust_eu27) && !("EU" %in% input$countries)) {
                        selected_countries_for_periods <- union(selected_countries_for_periods, "EU")
                }

                data <- hikp_data() %>%
                        filter(geo %in% selected_countries_for_periods)

                data_no_na <- data %>%
                        filter(!is.na(values), substr(time, 1, 3) == "Jan" | substr(time, 1, 3) == "Dec")

                validate(need(nrow(data_no_na) > 0, "No index data available."))

                max_years <- data_no_na %>%
                        group_by(geo, coicop18, measure) %>%
                        summarise(max_year = max(time), .groups = "drop")

                min_years <- data_no_na %>%
                        group_by(geo, coicop18, measure) %>%
                        summarise(min_year = min(time), .groups = "drop")

                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year), .groups = "drop") %>%
                        pull(first_common_year)

                last_non_na_year <- max_years %>%
                        summarise(last_common_year = min(max_year), .groups = "drop") %>%
                        pull(last_common_year)

                if (input$period_type == "Full year") {
                        data <- filter(data, time > max(first_non_na_year) & time < min(last_non_na_year))
                        unique(format(data$time, "%Y"))
                } else {
                        data <- filter(data, time >= max(first_non_na_year) & time < min(last_non_na_year))
                        unique(paste(format(data$time, "%Y"), format(data$time, "%B"), sep = " "))
                }
        })

        observe({
                req(input$countries, input$coicops)
                updateSelectInput(session, "select_years", choices = selected_data())
        })

        observeEvent(input$index_measure, {
                req(input$index_measure)
                if (length(input$index_measure) == 2 && length(input$coicops) > 1) {
                        updateSelectInput(session, "coicops", selected = input$coicops[1])
                        showNotification(
                                "When both HICP and HICP-CT are selected, only one COICOP aggregate can be shown.",
                                type = "message"
                        )
                }
        })

        observeEvent(input$coicops, {
                req(input$index_measure)
                if (length(input$index_measure) == 2 && length(input$coicops) > 1) {
                        updateSelectInput(session, "coicops", selected = input$coicops[1])
                }
        })

        observeEvent(input$index_adjust_total, {
                if (isTRUE(input$index_adjust_total) && isTRUE(input$index_adjust_eu27)) {
                        updateCheckboxInput(session, "index_adjust_eu27", value = FALSE)
                        showNotification(
                                "The total-inflation adjustment and the EU27 adjustment cannot be combined.",
                                type = "message"
                        )
                }
        }, ignoreInit = TRUE)

        observeEvent(input$index_adjust_eu27, {
                if (isTRUE(input$index_adjust_eu27) && isTRUE(input$index_adjust_total)) {
                        updateCheckboxInput(session, "index_adjust_total", value = FALSE)
                        showNotification(
                                "The EU27 adjustment and the total-inflation adjustment cannot be combined.",
                                type = "message"
                        )
                }
        }, ignoreInit = TRUE)

        observeEvent(input$countries, {
                if ("EU" %in% input$countries && isTRUE(input$index_adjust_eu27)) {
                        updateCheckboxInput(session, "index_adjust_eu27", value = FALSE)
                        showNotification(
                                "The EU27 adjustment is not available when EU27 itself is selected as a country.",
                                type = "message"
                        )
                }
        }, ignoreInit = TRUE)

        plot_data <- eventReactive(input$rebase, {
                req(hikp_data(), input$select_years)
                data <- hikp_data()
                coicops_selected <- eurostat_coicop_code(input$coicops)

                adjust_total <- isTRUE(input$index_adjust_total)
                adjust_eu27 <- isTRUE(input$index_adjust_eu27) && !("EU" %in% input$countries)

                validate(
                        need(
                                !(adjust_total && adjust_eu27),
                                "Please choose either the total-inflation adjustment or the EU27 adjustment, not both."
                        )
                )

                if (adjust_total) {
                        total_data <- data %>%
                                filter(coicop18 == "TOTAL") %>%
                                select(geo, time, measure, total_values = values)

                        data <- data %>%
                                filter(coicop18 %in% coicops_selected) %>%
                                left_join(total_data, by = c("geo", "time", "measure")) %>%
                                mutate(
                                        original_values = values,
                                        adjustment = "Country aggregate / country all-items index",
                                        values = if_else(!is.na(values) & !is.na(total_values) & total_values != 0, values / total_values * 100, NA_real_)
                                )
                } else if (adjust_eu27) {
                        eu27_data <- data %>%
                                filter(geo == "EU", coicop18 %in% coicops_selected) %>%
                                select(coicop18, time, measure, eu27_values = values)

                        data <- data %>%
                                filter(coicop18 %in% coicops_selected, geo != "EU") %>%
                                left_join(eu27_data, by = c("coicop18", "time", "measure")) %>%
                                mutate(
                                        original_values = values,
                                        adjustment = "Country aggregate / EU27 aggregate",
                                        values = if_else(!is.na(values) & !is.na(eu27_values) & eu27_values != 0, values / eu27_values * 100, NA_real_)
                                )
                } else {
                        data <- data %>%
                                filter(coicop18 %in% coicops_selected) %>%
                                mutate(adjustment = "None")
                }

                if (input$period_type == "Full year") {
                        avg_refyear <- data %>%
                                filter(as.numeric(format(time, "%Y")) == as.numeric(input$select_years)) %>%
                                group_by(coicop18, geo, measure) %>%
                                summarise(mean_values = mean(values, na.rm = TRUE), .groups = "drop")
                } else {
                        avg_refyear <- data %>%
                                filter(as.character(format(time, "%Y %B")) == input$select_years) %>%
                                group_by(coicop18, geo, measure) %>%
                                summarise(mean_values = mean(values, na.rm = TRUE), .groups = "drop")
                }

                rebased_data <- left_join(data, avg_refyear, by = c("coicop18", "geo", "measure")) %>%
                        mutate(newbase = if_else(!is.na(values) & !is.na(mean_values), values / mean_values * 100, NA_real_))

                if (input$period_type == "Full year") {
                        rebased_data <- rebased_data %>%
                                filter(as.numeric(format(time, "%Y")) >= as.numeric(input$select_years))
                } else {
                        selected_year <- as.numeric(str_extract(input$select_years, "\\d{4}"))
                        selected_month <- match(tolower(str_extract(input$select_years, "[A-Za-z]+")), tolower(month.name))
                        rebased_data <- rebased_data %>%
                                filter(year(time) > selected_year | (year(time) == selected_year & month(time) >= selected_month))
                }

                new_rebased_data <<- TRUE
                new_plot_data <<- TRUE
                rebased_data
        })

        observeEvent(input$rebase, {
                output$plot <- renderPlotly({
                        req(plot_data())
                        data <- plot_data()

                        adjust_eu27 <- isTRUE(input$index_adjust_eu27) && !("EU" %in% input$countries)

                        selected_countries <- input$countries
                        if (adjust_eu27) {
                                selected_countries <- setdiff(selected_countries, "EU")
                        }

                        backdrop_countries <- if (isTRUE(input$index_backdrop_eu)) {
                                setdiff(country_groups[["EU"]], selected_countries)
                        } else {
                                character(0)
                        }
                        countries_to_plot <- union(selected_countries, backdrop_countries)

                        data <- data %>%
                                filter(geo %in% countries_to_plot) %>%
                                mutate(is_backdrop = geo %in% backdrop_countries)

                        first_non_na_year <- min(data$time[!is.na(data$values)])
                        data <- filter(data, time >= first_non_na_year)
                        data$time <- as.Date(as.yearmon(data$time), format = "%Y %B")

                        validate(need(nrow(data) > 0, "No data available for the selected combination."))

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

                        for (series in unique(data$line_group[data$is_backdrop])) {
                                series_data <- data %>% filter(line_group == series) %>% arrange(time)
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
                                                text = ~paste0(geo, "<br>", measure, "<br>", coicop18, "<br>", format(time, "%Y-%m"), ": ", round(newbase, 1)),
                                                showlegend = FALSE
                                        )
                        }

                        for (series in unique(data$line_group[!data$is_backdrop])) {
                                series_data <- data %>% filter(line_group == series) %>% arrange(time)
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

                        y_label <- if (isTRUE(input$index_adjust_total)) {
                                paste("Relative price index", input$select_years, "=100")
                        } else if (adjust_eu27) {
                                paste("Index relative to EU27", input$select_years, "=100")
                        } else {
                                paste("Index", input$select_years, "=100")
                        }

                        labelled_input_coicop <- left_join(
                                data.frame(input.coicops = input$coicops),
                                coicop_set,
                                by = c("input.coicops" = "coicop18_code")
                        )
                        coicop_values <- paste(labelled_input_coicop$code_label, collapse = ", ")
                        coicop_values <- gsub(",([^,]*)$", " and\\1", coicop_values)
                        plot_title <- paste("Price development for", coicop_values)
                        
                        if (isTRUE(input$index_adjust_total)) {
                                plot_title <- paste0(plot_title, " adjusted for total inflation (all-items selected index)")
                        } else if (adjust_eu27) {
                                plot_title <- paste0(plot_title, " relative to the corresponding EU27 index")
                        }
                        plot_title <- gsub("(.{1,70})(\\s+|$)", "\\1\n", plot_title)
                        
                        show_index_title <- isTRUE(input$index_show_title)
                        
                        plotly_plot <- plotly_plot %>%
                                layout(
                                        paper_bgcolor = "white",
                                        plot_bgcolor = "white",
                                        yaxis = list(title = y_label),
                                        xaxis = list(title = ""),
                                        legend = list(font = list(color = "black", size = 11)),
                                        
                                        title = list(
                                                text = if (show_index_title) plot_title else "",
                                                x = 0.15,
                                                y = 0.9,
                                                font = list(size = 12, family = "sans-serif", color = "black")
                                        ),
                                        
                                        margin = list(
                                                t = if (show_index_title) 70 else 25
                                        )
                                )

                        new_plot_data <<- FALSE
                        new_rebased_data <<- FALSE
                        plotly_plot
                })
        })

        # ------------------------------------------------------------
        # Weights data and plot
        # ------------------------------------------------------------

        hikp_w_data <- eventReactive(input$update_w, {
                req(input$countries_w, input$coicop_w, input$classification_w)

                current_hierarchy <- selected_hierarchy(input$classification_w)
                filtered_data <- current_hierarchy[current_hierarchy$parent_code == input$coicop_w, ]
                coicops <- eurostat_coicop_code(input$coicop_w)
                result <- unique(filtered_data$coicop18_code)
                result <- result[!is.na(result)]

                if (length(result) == 0) {
                        result <- coicops
                }

                data <- get_eurostat(
                        "prc_hicp_iw",
                        filters = list(
                                geo = input$countries_w,
                                coicop18 = result
                        ),
                        update_cache = TRUE
                )

                validate(
                        need(
                                nrow(data) > 0,
                                "No weight data available for the selected countries and aggregate."
                        )
                )

                new_plot_w_data <<- TRUE

                data_no_na <- data %>% filter(!is.na(values))

                validate(
                        need(
                                nrow(data_no_na) > 0,
                                "No non-missing weight data available for the selected countries and aggregate."
                        )
                )

                min_years <- data_no_na %>%
                        group_by(geo, coicop18) %>%
                        summarise(min_year = min(time), .groups = "drop")

                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year), .groups = "drop") %>%
                        pull(first_common_year)

                data <- filter(data, time >= max(first_non_na_year))

                label_set_w <- selected_label_set(input$classification_w)

                data %>%
                        left_join(label_set_w, by = c("coicop18" = "coicop18_code")) %>%
                        mutate(
                                classification = input$classification_w,
                                selected_aggregate = input$coicop_w,
                                code_label_wrapped = ifelse(
                                        is.na(code_label),
                                        NA_character_,
                                        wrap_legend(code_label, width = 42)
                                )
                        )
        })

        selected_w_data <- reactive({
                req(hikp_w_data())
                data <- hikp_w_data()
                data$time <- as.yearmon(data$time)
                data <- data[data$geo %in% input$countries_w, ]
                data_no_na <- data %>% filter(is.numeric(values))

                max_years <- data_no_na %>% group_by(geo) %>% summarise(max_year = max(time, na.rm = TRUE), .groups = "drop")
                min_years <- data_no_na %>% group_by(geo) %>% summarise(min_year = min(time), .groups = "drop")

                first_non_na_year <- min_years %>% summarise(first_common_year = max(min_year), .groups = "drop") %>% pull(first_common_year)
                last_non_na_year <- max_years %>% summarise(last_common_year = min(max_year), .groups = "drop") %>% pull(last_common_year)

                data <- filter(data, time >= max(first_non_na_year) & time <= min(last_non_na_year))
                unique(format(data$time, "%Y"))
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
        })

        observeEvent(input$update_w, {
                output$plot_w <- renderPlotly({
                        req(hikp_w_data(), input$weights_view, input$range_slider_w)
                        data <- hikp_w_data()

                        time_filtered_data <- data[
                                as.numeric(format(data$time, "%Y")) >= input$range_slider_w[1] &
                                        as.numeric(format(data$time, "%Y")) <= input$range_slider_w[2],
                        ]

                        validate(need(nrow(time_filtered_data) > 0, "No data available for the selected countries and period."))

                        subplots <- list()
                        component_styles_w <- component_style_map(time_filtered_data$code_label_wrapped)

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

                                        subpl <- add_styled_bar_traces(
                                                plot_ly(),
                                                filtered_data,
                                                x_col = "time",
                                                y_col = "values",
                                                style_map = component_styles_w,
                                                showlegend = count == 1
                                        ) %>%
                                                layout(
                                                        yaxis = list(range = c(0, max_weight * 1.05), title = "Weight, per mille"),
                                                        xaxis = list(title = ""),
                                                        barmode = "stack",
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
                                                        )
                                                )
                                        subplots[[geo_value]] <- subpl
                                }
                        }

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

                                        # Sort countries in the "one graph per year" view.
                                        #
                                        # For CP00, total HICP weight is always 1000, so it cannot be used
                                        # as a sorting variable. Instead, use a meaningful child aggregate:
                                        # - ECOICOP ver. 2: CP01
                                        # - Special aggregates: FOOD
                                        # - Administered prices: APF
                                        
                                        sort_code <- case_when(
                                                input$classification_w == "sa" ~ "FOOD",
                                                input$classification_w == "ap" ~ "APF",
                                                input$classification_w == "ecoicop" & input$coicop_w == "CP00" ~ "CP01",
                                                TRUE ~ NA_character_
                                        )
                                        
                                        if (!is.na(sort_code)) {
                                                
                                                sort_data <- hikp_w_data() %>%
                                                        mutate(year = as.numeric(format(time, "%Y"))) %>%
                                                        filter(
                                                                year == year_value,
                                                                coicop18 == sort_code,
                                                                geo %in% unique(filtered_data$geo)
                                                        ) %>%
                                                        group_by(geo) %>%
                                                        summarise(sort_weight = sum(values, na.rm = TRUE), .groups = "drop")
                                                
                                        } else {
                                                
                                                sort_data <- filtered_data %>%
                                                        group_by(geo) %>%
                                                        summarise(sort_weight = sum(values, na.rm = TRUE), .groups = "drop")
                                        }

                                        geo_order <- sort_data %>% arrange(sort_weight, geo) %>% pull(geo)
                                        remaining_geo <- setdiff(unique(as.character(filtered_data$geo)), geo_order)
                                        geo_order <- c(geo_order, sort(remaining_geo))
                                        filtered_data$geo <- factor(filtered_data$geo, levels = geo_order)

                                        subpl <- add_styled_bar_traces(
                                                plot_ly(),
                                                filtered_data,
                                                x_col = "geo",
                                                y_col = "values",
                                                style_map = component_styles_w,
                                                showlegend = count == 1
                                        ) %>%
                                                layout(
                                                        yaxis = list(range = c(0, max_weight * 1.05), title = "Weight, per mille"),
                                                        xaxis = list(title = "", categoryorder = "array", categoryarray = geo_order),
                                                        barmode = "stack",
                                                        annotations = list(
                                                                text = as.character(year_value),
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
                                        subplots[[as.character(year_value)]] <- subpl
                                }
                        }

                        dims <- subplot_dims(length(subplots))
                        layout <- plotly::subplot(
                                subplots,
                                nrows = dims$nrows,
                                titleX = TRUE,
                                shareX = input$weights_view == "country",
                                titleY = TRUE,
                                shareY = TRUE
                        ) %>%
                                layout(
                                        autosize = FALSE,
                                        height = dims$total_height,
                                        font = list(size = 11),
                                        legend = list(traceorder = "normal", font = list(size = 11), x = 1.02, y = 1),
                                        margin = list(r = 180)
                                )

                        plotly_plot_w <- plotly_build(layout)
                        plotly_plot_w$x$attrs$legend$x$class <- "plot-container"
                        plotly_plot_w
                })
        })

        # ------------------------------------------------------------
        # Shared contribution data calculation
        # ------------------------------------------------------------

        get_contribution_setup <- function(countries, selected_code, classification, contribution_type, measure, component_codes = NULL) {
                current_hierarchy <- selected_hierarchy(classification)
                filtered_data <- current_hierarchy[current_hierarchy$parent_code == selected_code, ]
                coicops <- eurostat_coicop_code(selected_code)
                target_coicop <- selected_target_coicop(contribution_type, coicops)
                measure_label <- selected_hicp_measure_label(measure)
                index_dataset <- selected_hicp_index_dataset(measure)

                if (!is.null(component_codes)) {
                        result <- unique(component_codes)
                } else {
                        result <- unique(filtered_data$coicop18_code)
                }
                result <- result[!is.na(result)]

                if (length(result) == 0) {
                        result <- coicops
                }

                full_coicop <- unique(c(result, coicops, target_coicop))

                list(
                        countries = countries,
                        classification = classification,
                        coicops = coicops,
                        target_coicop = target_coicop,
                        result = result,
                        full_coicop = full_coicop,
                        measure_label = measure_label,
                        index_dataset = index_dataset
                )
        }

        eurostat_request_cache <- new.env(parent = emptyenv())

        normalise_eurostat_filters <- function(filters) {
                filter_names <- sort(names(filters))
                paste(
                        vapply(
                                filter_names,
                                function(filter_name) {
                                        filter_values <- filters[[filter_name]]
                                        filter_values <- sort(unique(as.character(filter_values[!is.na(filter_values)])))
                                        paste0(filter_name, "=", paste(filter_values, collapse = ","))
                                },
                                character(1)
                        ),
                        collapse = "|"
                )
        }

        get_eurostat_cached <- function(dataset, filters, update_cache = FALSE) {
                cache_key <- paste(dataset, normalise_eurostat_filters(filters), sep = "::")

                if (!isTRUE(update_cache) && exists(cache_key, envir = eurostat_request_cache, inherits = FALSE)) {
                        return(get(cache_key, envir = eurostat_request_cache, inherits = FALSE))
                }

                data <- get_eurostat(
                        dataset,
                        filters = filters,
                        update_cache = update_cache
                )

                assign(cache_key, data, envir = eurostat_request_cache)
                data
        }

        get_eurostat_chunked <- function(
                dataset,
                filters,
                chunk_var = "coicop18",
                chunk_size = 15,
                update_cache = FALSE,
                try_full_first = TRUE
        ) {
                # Fast path: try to fetch all requested values in one Eurostat call.
                # If Eurostat rejects the large request, fall back to adaptive chunking.
                values_to_chunk <- unique(filters[[chunk_var]])
                values_to_chunk <- values_to_chunk[!is.na(values_to_chunk)]

                if (length(values_to_chunk) == 0) {
                        return(data.frame())
                }

                fetch_direct <- function(values) {
                        local_filters <- filters
                        local_filters[[chunk_var]] <- values

                        tryCatch(
                                list(
                                        ok = TRUE,
                                        data = get_eurostat_cached(
                                                dataset,
                                                filters = local_filters,
                                                update_cache = update_cache
                                        )
                                ),
                                error = function(e) {
                                        list(ok = FALSE, data = data.frame(), message = conditionMessage(e))
                                }
                        )
                }

                fetch_adaptive <- function(values) {
                        if (length(values) == 0) {
                                return(data.frame())
                        }

                        result <- fetch_direct(values)
                        if (isTRUE(result$ok)) {
                                return(result$data)
                        }

                        if (length(values) == 1) {
                                warning(
                                        paste0(
                                                "Eurostat request failed for ", dataset, ", ",
                                                chunk_var, " = ", values, ": ", result$message
                                        ),
                                        call. = FALSE
                                )
                                return(data.frame())
                        }

                        split_at <- ceiling(length(values) / 2)
                        bind_rows(
                                fetch_adaptive(values[seq_len(split_at)]),
                                fetch_adaptive(values[(split_at + 1):length(values)])
                        )
                }

                if (isTRUE(try_full_first)) {
                        full_result <- fetch_direct(values_to_chunk)
                        if (isTRUE(full_result$ok)) {
                                return(full_result$data)
                        }
                }

                # Backwards-compatible fallback: if full retrieval fails, start with
                # moderately large chunks and only split further when a chunk fails.
                # This keeps the number of API calls much lower than one call per code.
                if (length(values_to_chunk) <= chunk_size) {
                        return(fetch_adaptive(values_to_chunk))
                }

                chunks <- split(values_to_chunk, ceiling(seq_along(values_to_chunk) / chunk_size))
                bind_rows(lapply(chunks, fetch_adaptive))
        }

        get_index_and_weight_data <- function(setup, progress_callback = NULL) {
                notify_progress <- function(stage, status, data = NULL) {
                        if (is.function(progress_callback)) {
                                progress_callback(stage = stage, status = status, data = data)
                        }
                }

                notify_progress("index", "start")
                data_I <- get_eurostat_chunked(
                        setup$index_dataset,
                        filters = list(geo = setup$countries, unit = "I25", coicop18 = setup$full_coicop),
                        chunk_var = "coicop18",
                        chunk_size = 75,
                        update_cache = FALSE,
                        try_full_first = TRUE
                )
                notify_progress("index", "done", data_I)

                validate(
                        need(
                                nrow(data_I) > 0,
                                paste0("No ", setup$measure_label, " data available for the selected countries and aggregate.")
                        )
                )

                notify_progress("weights", "start")
                data_W <- get_eurostat_chunked(
                        "prc_hicp_iw",
                        filters = list(geo = setup$countries, coicop18 = setup$full_coicop),
                        chunk_var = "coicop18",
                        chunk_size = 75,
                        update_cache = FALSE,
                        try_full_first = TRUE
                ) %>%
                        select(-any_of("statinfo"))
                notify_progress("weights", "done", data_W)

                validate(
                        need(
                                nrow(data_W) > 0,
                                "No HICP weight data available for the selected countries and aggregate."
                        )
                )

                list(data_I = data_I, data_W = data_W)
        }

        prepare_component_weights <- function(data_W, setup, contribution_type) {
                data_W_j <- data_W %>%
                        filter(coicop18 %in% setup$result) %>%
                        mutate(year = year(time)) %>%
                        select(-any_of(c("freq", "time"))) %>%
                        rename(WT_j_pre = values)

                if (contribution_type == "selected higher aggregate") {
                        data_W_TOT <- data_W %>%
                                filter(coicop18 %in% setup$coicops) %>%
                                mutate(year = year(time)) %>%
                                select(-any_of(c("freq", "time", "coicop18"))) %>%
                                rename(WT_TOT = values)

                        data_W_j %>%
                                left_join(data_W_TOT, by = c("year", "geo")) %>%
                                mutate(WT_j = WT_j_pre / WT_TOT * 1000) %>%
                                select(-any_of(c("WT_j_pre", "WT_TOT")))
                } else {
                        data_W_j %>%
                                mutate(WT_j = WT_j_pre) %>%
                                select(-any_of(c("WT_j_pre")))
                }
        }

        # ------------------------------------------------------------
        # Monthly rates data
        # ------------------------------------------------------------

        hikp_mr_data <- eventReactive(input$update_mr, {
                req(input$countries_mr, input$coicop_mr, input$classification_mr, input$mr_measure)

                setup <- get_contribution_setup(
                        countries = input$countries_mr,
                        selected_code = input$coicop_mr,
                        classification = input$classification_mr,
                        contribution_type = input$contribution_type_mr,
                        measure = input$mr_measure
                )

                raw_data <- get_index_and_weight_data(setup)
                data_I <- raw_data$data_I
                data_W <- raw_data$data_W

                new_plot_mr_data <<- TRUE

                data_I_j <- data_I %>%
                        filter(coicop18 %in% setup$result) %>%
                        mutate(year = year(time), month = month(time)) %>%
                        select(-any_of(c("freq", "unit"))) %>%
                        rename(IX_j = values)

                data_W_jTOT <- prepare_component_weights(data_W, setup, input$contribution_type_mr)

                result_j <- left_join(data_I_j, data_W_jTOT, by = c("year", "coicop18", "geo"))

                data_MR <- data_I %>%
                        filter(coicop18 %in% setup$target_coicop) %>%
                        mutate(year = year(time), month = month(time))

                m_MR_mmin1 <- data_MR %>%
                        mutate(
                                year = ifelse(month == 12, year + 1, year),
                                month = ifelse(month == 12, 1, month %% 12 + 1)
                        ) %>%
                        select(-any_of(c("unit", "time", "freq"))) %>%
                        rename(IX_A_mmin1 = values)

                data_MR <- data_MR %>%
                        left_join(m_MR_mmin1, by = c("year", "month", "geo", "coicop18")) %>%
                        mutate(m_rate_00 = values / IX_A_mmin1 * 100 - 100) %>%
                        select(-any_of(c("freq", "unit", "values", "IX_A_mmin1")))

                data_I_TARGET <- data_I %>%
                        filter(coicop18 %in% setup$target_coicop) %>%
                        mutate(year = year(time), month = month(time)) %>%
                        select(-any_of(c("freq", "unit", "coicop18"))) %>%
                        rename(IX_TOT = values)

                result_jTOT <- left_join(result_j, data_I_TARGET, by = c("time", "geo")) %>%
                        mutate(year = year(time), month = month(time))

                data_mmin1 <- result_jTOT %>%
                        mutate(
                                year = ifelse(month == 12, year + 1, year),
                                month = ifelse(month == 12, 1, month %% 12 + 1)
                        ) %>%
                        select(-any_of(c("time", "WT_j"))) %>%
                        rename(IX_TOT_m_mmin1 = IX_TOT, IX_j_m_mmin1 = IX_j)

                result_jTOT2 <- left_join(result_jTOT, data_mmin1, by = c("year", "month", "geo", "coicop18")) %>%
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

                result_jTOT2 <- full_join(result_jTOT2, data_MR, by = c("year", "time", "month", "geo", "coicop18"))

                data <- result_jTOT2 %>%
                        select(year, month, time, geo, coicop18, m_rate_00, Contr_j) %>%
                        mutate(measure = setup$measure_label, target_coicop = setup$target_coicop)

                data_no_na <- data %>% filter(!is.na(Contr_j) | !is.na(m_rate_00))

                validate(need(nrow(data_no_na) > 0, "No monthly rate or contribution data available for the selected combination."))

                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time), .groups = "drop")
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year), .groups = "drop") %>%
                        pull(first_common_year)

                data <- data %>%
                        filter(time >= max(first_non_na_year)) %>%
                        filter(!is.na(m_rate_00) | !is.na(Contr_j))

                label_set_mr <- selected_label_set(input$classification_mr)

                data %>%
                        left_join(label_set_mr, by = c("coicop18" = "coicop18_code")) %>%
                        mutate(
                                code_label = ifelse(coicop18 == target_coicop, NA_character_, code_label),
                                code_label_wrapped = ifelse(is.na(code_label), NA_character_, wrap_legend(code_label, width = 45))
                        )
        })

        selected_mr_data <- reactive({
                req(hikp_mr_data(), input$mr_view)

                data <- hikp_mr_data() %>%
                        mutate(time = as.yearmon(time)) %>%
                        filter(geo %in% input$countries_mr)

                data_no_na <- data %>% filter(!is.na(Contr_j) | !is.na(m_rate_00))
                validate(need(nrow(data_no_na) > 0, "No common periods with data for the selected countries."))

                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_time = min(time, na.rm = TRUE), .groups = "drop")
                max_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(max_time = max(time, na.rm = TRUE), .groups = "drop")

                first_common_time <- max(min_years$min_time)
                last_common_time <- min(max_years$max_time)

                available_times <- data %>%
                        filter(time >= first_common_time, time <= last_common_time) %>%
                        distinct(time) %>%
                        arrange(desc(time)) %>%
                        pull(time)

                period_list <- format(available_times, "%Y %B")
                latest_time <- max(available_times)

                if (input$mr_view == "period") {
                        default_periods <- format(latest_time, "%Y %B")
                } else {
                        wanted_times <- latest_time - 0:4
                        default_periods <- format(available_times[available_times %in% wanted_times], "%Y %B")
                        if (length(default_periods) < 5) default_periods <- format(latest_time, "%Y %B")
                }

                list(choices = period_list, selected = default_periods)
        })

        observe({
                req(input$countries_mr, input$coicop_mr, input$mr_view)
                periods <- selected_mr_data()
                updateSelectInput(session, "select_period_mr", choices = periods$choices, selected = periods$selected)
        })

        observeEvent(input$update_mr, {
                output$plot_mr <- renderPlotly({
                        req(hikp_mr_data(), input$coicop_mr, input$select_period_mr, input$mr_view)
                        data <- hikp_mr_data()
                        measure_label <- unique(na.omit(data$measure))[1]
                        monthly_marker_name <- paste0("Monthly rate M/M-1 (", measure_label, ")")
                        y_title <- paste0("Monthly rate of change (", measure_label, "), %")

                        time_filtered_data <- data[format(data$time, "%Y %B") %in% input$select_period_mr, ]

                        validate(
                                need(
                                        any(!is.na(time_filtered_data$Contr_j)) | any(!is.na(time_filtered_data$m_rate_00)),
                                        "No data for the selected country/countries and period."
                                )
                        )

                        stacked_range_mr <- stacked_component_range(
                                time_filtered_data,
                                value_col = "Contr_j",
                                group_cols = c("geo", "time")
                        )
                        mr_00_values <- time_filtered_data %>%
                                filter(!is.na(m_rate_00)) %>%
                                distinct(geo, time, m_rate_00)
                        y_rng <- visible_y_range(
                                c(stacked_range_mr$positive_sum, stacked_range_mr$negative_sum, mr_00_values$m_rate_00, 0),
                                default = c(-1, 1),
                                pad_fraction = 0.08,
                                min_pad = 0.08
                        )
                        component_styles_mr <- component_style_map(time_filtered_data$code_label_wrapped)

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
                                                mutate(time_ym = as.yearmon(time), geo_chr = as.character(geo)) %>%
                                                filter(time_ym == as.yearmon(period_value))
                                        filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)

                                        geo_order <- filtered_data %>%
                                                group_by(geo_chr) %>%
                                                summarise(sort_rate = first_non_na(m_rate_00), .groups = "drop") %>%
                                                arrange(sort_rate, geo_chr) %>%
                                                pull(geo_chr)
                                        geo_order <- unique(as.character(geo_order))
                                        filtered_data <- filtered_data %>% mutate(geo_chr = factor(geo_chr, levels = geo_order))
                                        period_label <- format(as.yearmon(period_value), "%Y %B")

                                        bar_data <- filtered_data %>% filter(!is.na(code_label_wrapped))

                                        subpl <- add_styled_bar_traces(
                                                plot_ly(),
                                                bar_data,
                                                x_col = "geo_chr",
                                                y_col = "Contr_j",
                                                style_map = component_styles_mr,
                                                showlegend = count == 1
                                        )

                                        line_data <- filtered_data %>%
                                                group_by(geo_chr) %>%
                                                summarise(m_rate_00 = first_non_na(m_rate_00), .groups = "drop") %>%
                                                mutate(geo_chr = factor(as.character(geo_chr), levels = geo_order))

                                        subpl <- subpl %>%
                                                add_trace(
                                                        data = line_data,
                                                        x = ~geo_chr,
                                                        y = ~m_rate_00,
                                                        type = "scatter",
                                                        mode = "markers",
                                                        inherit = FALSE,
                                                        name = monthly_marker_name,
                                                        marker = list(color = "black", size = 10),
                                                        showlegend = count == 1,
                                                        legendrank = 1
                                                ) %>%
                                                layout(
                                                        yaxis = list(range = y_rng, title = y_title),
                                                        xaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = geo_order),
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

                                dims <- subplot_dims(length(subplots))
                                layout <- plotly::subplot(
                                        subplots,
                                        nrows = dims$nrows,
                                        titleX = TRUE,
                                        shareX = FALSE,
                                        titleY = TRUE,
                                        shareY = TRUE
                                ) %>%
                                        layout(
                                                autosize = FALSE,
                                                height = dims$total_height,
                                                font = list(size = 11),
                                                legend = list(traceorder = "normal")
                                        )

                                plotly_plot_mr <- plotly_build(layout)
                                plotly_plot_mr$x$attrs$legend$x$class <- "plot-container"
                                return(plotly_plot_mr)
                        }

                        if (input$mr_view == "country") {
                                subplots <- list()
                                count <- 0
                                for (geo_value in unique(time_filtered_data$geo)) {
                                        count <- count + 1
                                        filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                        filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)
                                        filtered_data <- filtered_data[order(filtered_data$time, filtered_data$Contr_j), ]
                                        bar_data <- filtered_data %>% filter(!is.na(code_label_wrapped))

                                        x_axis_tick_labels <- unique(filtered_data$time)
                                        x_axis_layout <- list(
                                                title = "",
                                                tickmode = "array",
                                                tickvals = x_axis_tick_labels,
                                                ticktext = x_axis_tick_labels,
                                                tickformat = "%Y %B"
                                        )

                                        subpl <- add_styled_bar_traces(
                                                plot_ly(),
                                                bar_data,
                                                x_col = "time",
                                                y_col = "Contr_j",
                                                style_map = component_styles_mr,
                                                showlegend = count == 1
                                        ) %>%
                                                layout(
                                                        yaxis = list(range = y_rng),
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
                                                        )
                                                )

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
                                                        name = monthly_marker_name,
                                                        marker = list(color = "black", size = 10),
                                                        showlegend = count == 1,
                                                        legendrank = 1
                                                ) %>%
                                                layout(xaxis = x_axis_layout, yaxis = list(range = y_rng, title = y_title))

                                        subplots[[geo_value]] <- subpl
                                }

                                dims <- subplot_dims(length(subplots))
                                layout <- plotly::subplot(
                                        subplots,
                                        nrows = dims$nrows,
                                        titleX = TRUE,
                                        shareX = TRUE,
                                        titleY = TRUE,
                                        shareY = TRUE
                                ) %>%
                                        layout(
                                                autosize = FALSE,
                                                height = dims$total_height,
                                                font = list(size = 11),
                                                legend = list(traceorder = "normal")
                                        )

                                plotly_plot_mr <- plotly_build(layout)
                                plotly_plot_mr$x$attrs$legend$x$class <- "plot-container"
                                return(plotly_plot_mr)
                        }
                })
        })

        # ------------------------------------------------------------
        # Annual rates data
        # ------------------------------------------------------------

        hikp_ar_data <- eventReactive(input$update_ar, {
                req(input$countries_ar, input$coicop_ar, input$classification_ar, input$ar_measure)

                setup <- get_contribution_setup(
                        countries = input$countries_ar,
                        selected_code = input$coicop_ar,
                        classification = input$classification_ar,
                        contribution_type = input$contribution_type_ar,
                        measure = input$ar_measure
                )

                raw_data <- get_index_and_weight_data(setup)
                data_I <- raw_data$data_I
                data_W <- raw_data$data_W

                new_plot_ar_data <<- TRUE

                data_I_j <- data_I %>%
                        filter(coicop18 %in% setup$result) %>%
                        mutate(year = year(time)) %>%
                        select(-any_of(c("freq", "unit"))) %>%
                        rename(IX_j = values)

                data_W_jTOT <- prepare_component_weights(data_W, setup, input$contribution_type_ar)
                result_j <- left_join(data_I_j, data_W_jTOT, by = c("year", "coicop18", "geo"))

                data_AR <- data_I %>%
                        filter(coicop18 %in% setup$target_coicop) %>%
                        mutate(year = year(time), month = month(time))

                m_AR_ymin1 <- data_AR %>%
                        mutate(year = year + 1) %>%
                        select(-any_of(c("unit", "time", "freq"))) %>%
                        rename(IX_A_m_ymin1 = values)

                data_AR <- data_AR %>%
                        left_join(m_AR_ymin1, by = c("year", "month", "geo", "coicop18")) %>%
                        mutate(ann_rate_00 = values / IX_A_m_ymin1 * 100 - 100) %>%
                        select(-any_of(c("freq", "unit", "values", "IX_A_m_ymin1")))

                data_I_TARGET <- data_I %>%
                        filter(coicop18 %in% setup$target_coicop) %>%
                        select(-any_of(c("freq", "unit", "coicop18"))) %>%
                        rename(IX_TOT = values)

                result_jTOT <- left_join(result_j, data_I_TARGET, by = c("time", "geo")) %>%
                        mutate(year = year(time), month = month(time))

                dec_data_ymin1 <- result_jTOT %>%
                        filter(month == 12) %>%
                        select(-month) %>%
                        mutate(year = year + 1) %>%
                        select(-time) %>%
                        rename(IX_j_12_ymin1 = IX_j, WT_j_12_ymin1 = WT_j, IX_TOT_12_ymin1 = IX_TOT)

                result_jTOT2 <- left_join(result_jTOT, dec_data_ymin1, by = c("year", "geo", "coicop18"))

                dec_data_ymin2 <- result_jTOT %>%
                        filter(month == 12) %>%
                        select(-month) %>%
                        mutate(year = year + 2) %>%
                        select(-time) %>%
                        rename(IX_j_12_ymin2 = IX_j, WT_j_12_ymin2 = WT_j, IX_TOT_12_ymin2 = IX_TOT)

                result_jTOT2 <- left_join(result_jTOT2, dec_data_ymin2, by = c("year", "geo", "coicop18"))

                m_data_ymin1 <- result_jTOT %>%
                        mutate(year = year + 1) %>%
                        select(-time) %>%
                        rename(IX_j_m_ymin1 = IX_j, WT_j_m_ymin1 = WT_j, IX_TOT_m_ymin1 = IX_TOT)

                result_jTOT2 <- left_join(result_jTOT2, m_data_ymin1, by = c("year", "month", "geo", "coicop18"))

                numeric_vars <- c(
                        "IX_j", "WT_j", "IX_TOT", "IX_j_12_ymin1", "WT_j_12_ymin1", "IX_TOT_12_ymin1",
                        "IX_j_12_ymin2", "WT_j_12_ymin2", "IX_TOT_12_ymin2", "IX_j_m_ymin1",
                        "WT_j_m_ymin1", "IX_TOT_m_ymin1"
                )

                result_jTOT2 <- result_jTOT2 %>%
                        mutate(
                                Contr_j = if_else(
                                        rowSums(is.na(across(all_of(numeric_vars)))) > 0,
                                        NA_real_,
                                        (100 * (IX_TOT_12_ymin1 / IX_TOT_m_ymin1 * WT_j / 1000) * ((IX_j - IX_j_12_ymin1) / IX_j_12_ymin1)) +
                                                100 * ((IX_TOT_12_ymin2 / IX_TOT_m_ymin1 * WT_j_12_ymin1 / 1000) * ((IX_j_12_ymin1 - IX_j_m_ymin1) / IX_j_12_ymin2))
                                )
                        )

                result_jTOT2 <- full_join(result_jTOT2, data_AR, by = c("year", "time", "month", "geo", "coicop18"))

                data <- result_jTOT2 %>%
                        select(year, month, time, geo, coicop18, ann_rate_00, Contr_j) %>%
                        mutate(measure = setup$measure_label, target_coicop = setup$target_coicop)

                data_no_na <- data %>% filter(!is.na(Contr_j) | !is.na(ann_rate_00))
                validate(need(nrow(data_no_na) > 0, "No annual rate or contribution data available for the selected combination."))

                min_years <- data_no_na %>%
                        group_by(geo) %>%
                        summarise(min_year = min(time), .groups = "drop")
                first_non_na_year <- min_years %>%
                        summarise(first_common_year = max(min_year), .groups = "drop") %>%
                        pull(first_common_year)

                data <- filter(data, time >= max(first_non_na_year))

                label_set_ar <- selected_label_set(input$classification_ar)

                data %>%
                        left_join(label_set_ar, by = c("coicop18" = "coicop18_code")) %>%
                        mutate(
                                code_label = ifelse(coicop18 == target_coicop, NA_character_, code_label),
                                code_label_wrapped = ifelse(is.na(code_label), NA_character_, wrap_legend(code_label, width = 45))
                        )
        })

        selected_ar_data <- reactive({
                req(hikp_ar_data(), input$ar_view, input$countries_ar)

                data <- hikp_ar_data() %>%
                        mutate(time = as.yearmon(time)) %>%
                        filter(geo %in% input$countries_ar)

                data_no_na <- data %>%
                        filter(!is.na(Contr_j) | !is.na(ann_rate_00))

                available_times <- data_no_na %>%
                        distinct(geo, time) %>%
                        count(time, name = "n_geo") %>%
                        filter(n_geo == length(input$countries_ar)) %>%
                        arrange(desc(time)) %>%
                        pull(time)

                validate(need(length(available_times) > 0, "No common periods with data for the selected countries."))

                if (input$ar_view == "period") {
                        period_list <- format(available_times, "%Y %B")
                        return(list(choices = period_list, selected = period_list[1]))
                }

                year_list <- unique(format(available_times, "%Y"))
                list(choices = year_list, selected = NULL)
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

        observeEvent(input$update_ar, {
                output$plot_ar <- renderPlotly({
                        req(hikp_ar_data(), input$coicop_ar, input$ar_view)
                        data <- hikp_ar_data()
                        measure_label <- unique(na.omit(data$measure))[1]
                        annual_marker_name <- paste0("Annual rate M/M-12 (", measure_label, ")")
                        all_items_axis_ar <- paste0("Contrib. to ", measure_label, " M/M-12, %-points")
                        selected_axis_ar <- paste0("Annual rate of change (", measure_label, "), %")

                        if (input$ar_view == "country") {
                                req(input$range_slider)
                                time_filtered_data <- data[
                                        as.numeric(format(data$time, "%Y")) >= input$range_slider[1] &
                                                as.numeric(format(data$time, "%Y")) <= input$range_slider[2],
                                ]
                        } else {
                                req(input$select_period_ar)
                                time_filtered_data <- data[format(data$time, "%Y %B") == input$select_period_ar, ]
                        }

                        validate(
                                need(
                                        any(!is.na(time_filtered_data$Contr_j)) | any(!is.na(time_filtered_data$ann_rate_00)),
                                        "No data for the selected country/countries and period."
                                )
                        )

                        time_filtered_data$sign <- ifelse(time_filtered_data$Contr_j > 0, 1, ifelse(time_filtered_data$Contr_j < 0, -1, 1))

                        stacked_range_ar <- stacked_component_range(
                                time_filtered_data,
                                value_col = "Contr_j",
                                group_cols = c("geo", "time")
                        )
                        ar_00_values <- time_filtered_data %>%
                                filter(!is.na(ann_rate_00)) %>%
                                distinct(geo, time, ann_rate_00)

                        if (input$contribution_type_ar == "selected higher aggregate") {
                                y_rng <- visible_y_range(
                                        c(stacked_range_ar$positive_sum, stacked_range_ar$negative_sum, ar_00_values$ann_rate_00, 0),
                                        default = c(-1, 1),
                                        pad_fraction = 0.08,
                                        min_pad = 0.20
                                )
                        } else {
                                y_rng <- visible_y_range(
                                        c(stacked_range_ar$positive_sum, stacked_range_ar$negative_sum, 0),
                                        default = c(-1, 1),
                                        pad_fraction = 0.08,
                                        min_pad = 0.20
                                )
                        }
                        component_styles_ar <- component_style_map(time_filtered_data$code_label_wrapped)

                        if (input$ar_view == "period") {
                                period_label <- input$select_period_ar
                                filtered_data <- time_filtered_data %>% mutate(geo_chr = as.character(geo))
                                filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)

                                geo_order <- filtered_data %>%
                                        group_by(geo_chr) %>%
                                        summarise(sort_rate = first_non_na(ann_rate_00), .groups = "drop") %>%
                                        arrange(sort_rate, geo_chr) %>%
                                        pull(geo_chr)
                                geo_order <- unique(as.character(geo_order))
                                filtered_data <- filtered_data %>% mutate(geo_chr = factor(geo_chr, levels = geo_order))
                                bar_data <- filtered_data %>% filter(!is.na(code_label_wrapped))

                                subpl <- add_styled_bar_traces(
                                        plot_ly(),
                                        bar_data,
                                        x_col = "geo_chr",
                                        y_col = "Contr_j",
                                        style_map = component_styles_ar,
                                        showlegend = TRUE
                                )

                                if (input$contribution_type_ar == "selected higher aggregate") {
                                        line_data <- filtered_data %>%
                                                group_by(geo_chr) %>%
                                                summarise(ann_rate_00 = first_non_na(ann_rate_00), .groups = "drop") %>%
                                                mutate(geo_chr = factor(as.character(geo_chr), levels = geo_order))

                                        subpl <- subpl %>%
                                                add_trace(
                                                        data = line_data,
                                                        x = ~geo_chr,
                                                        y = ~ann_rate_00,
                                                        type = "scatter",
                                                        mode = "markers",
                                                        inherit = FALSE,
                                                        name = annual_marker_name,
                                                        marker = list(color = "black", size = 10),
                                                        showlegend = TRUE,
                                                        legendrank = 1
                                                )
                                }

                                y_title <- if (input$contribution_type_ar == "selected higher aggregate") selected_axis_ar else all_items_axis_ar

                                plotly_plot_ar <- subpl %>%
                                        layout(
                                                yaxis = list(range = y_rng, title = y_title),
                                                xaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = geo_order),
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
                                                legend = list(traceorder = "normal"),
                                                autosize = FALSE,
                                                height = 600
                                        ) %>%
                                        plotly_build()

                                plotly_plot_ar$x$attrs$legend$x$class <- "plot-container"
                                return(plotly_plot_ar)
                        }

                        if (input$ar_view == "country") {
                                subplots <- list()
                                count <- 0
                                for (geo_value in unique(time_filtered_data$geo)) {
                                        count <- count + 1
                                        filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                        filtered_data$Contr_j <- ifelse(is.na(filtered_data$Contr_j), 0, filtered_data$Contr_j)
                                        bar_data <- filtered_data %>% filter(!is.na(code_label_wrapped))

                                        subpl <- add_styled_bar_traces(
                                                plot_ly(),
                                                bar_data,
                                                x_col = "time",
                                                y_col = "Contr_j",
                                                style_map = component_styles_ar,
                                                showlegend = count == 1
                                        ) %>%
                                                layout(
                                                        yaxis = list(range = y_rng),
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

                                        if (input$contribution_type_ar == "selected higher aggregate") {
                                                line_data <- filtered_data %>%
                                                        filter(!is.na(ann_rate_00)) %>%
                                                        distinct(time, geo, .keep_all = TRUE) %>%
                                                        arrange(time)

                                                subpl <- subpl %>%
                                                        add_trace(
                                                                data = line_data,
                                                                x = ~time,
                                                                y = ~ann_rate_00,
                                                                type = "scatter",
                                                                mode = "lines",
                                                                inherit = FALSE,
                                                                name = annual_marker_name,
                                                                line = list(color = "black", dash = "dash", width = 1.5),
                                                                showlegend = count == 1,
                                                                legendrank = 1
                                                        ) %>%
                                                        layout(xaxis = list(title = ""), yaxis = list(range = y_rng, title = selected_axis_ar))
                                        } else {
                                                subpl <- subpl %>%
                                                        layout(xaxis = list(title = ""), yaxis = list(range = y_rng, title = all_items_axis_ar))
                                        }

                                        subplots[[geo_value]] <- subpl
                                }

                                dims <- subplot_dims(length(subplots))
                                layout <- plotly::subplot(
                                        subplots,
                                        nrows = dims$nrows,
                                        titleX = TRUE,
                                        shareX = TRUE,
                                        titleY = TRUE,
                                        shareY = TRUE
                                ) %>%
                                        layout(
                                                autosize = FALSE,
                                                height = dims$total_height,
                                                font = list(size = 11),
                                                legend = list(traceorder = "normal")
                                        )

                                plotly_plot_ar <- plotly_build(layout)
                                plotly_plot_ar$x$attrs$legend$x$class <- "plot-container"
                                return(plotly_plot_ar)
                        }
                })
        })

        # ------------------------------------------------------------
        # Seasonality data and plot
        # ------------------------------------------------------------

        hikp_se_data <- eventReactive(input$update_se, {
                req(input$countries_se, input$coicop_se)
                coicops <- eurostat_coicop_code(input$coicop_se)

                data_I <- get_eurostat(
                        "prc_hicp_minr",
                        filters = list(geo = input$countries_se, unit = "I25", coicop18 = coicops),
                        update_cache = TRUE
                )

                new_plot_se_data <<- TRUE

                data_I_j <- data_I %>%
                        mutate(year = year(time), month = month(time)) %>%
                        select(-any_of(c("freq", "unit"))) %>%
                        rename(IX_j = values)

                dec_data_ymin1 <- data_I_j %>%
                        filter(month == 12) %>%
                        select(-month) %>%
                        mutate(year = year + 1) %>%
                        select(-time) %>%
                        rename(IX_j_12_ymin1 = IX_j)

                data <- data_I_j %>%
                        left_join(dec_data_ymin1, by = c("year", "geo", "coicop18")) %>%
                        mutate(index_dec_prv_yr = IX_j / IX_j_12_ymin1 * 100)

                data_no_na <- data %>% filter(is.numeric(index_dec_prv_yr))
                min_years <- data_no_na %>% group_by(geo) %>% summarise(min_year = min(time), .groups = "drop")
                first_non_na_year <- min_years %>% summarise(first_common_year = max(min_year), .groups = "drop") %>% pull(first_common_year)

                data <- data %>%
                        filter(time >= max(first_non_na_year)) %>%
                        filter(!is.na(index_dec_prv_yr))

                label_set_se <- selected_label_set(input$classification_mr)

                data %>%
                        left_join(label_set_se, by = c("coicop18" = "coicop18_code")) %>%
                        mutate(code_label = ifelse(coicop18 == coicops, NA_character_, code_label))
        })

        selected_se_data <- reactive({
                req(hikp_se_data())
                data <- hikp_se_data()
                data$time <- as.yearmon(data$time)
                data <- data[data$geo %in% input$countries_se, ]
                data_no_na <- data %>% filter(is.numeric(index_dec_prv_yr))

                max_years <- data_no_na %>% group_by(geo) %>% summarise(max_year = max(time, na.rm = TRUE), .groups = "drop")
                min_years <- data_no_na %>% group_by(geo) %>% summarise(min_year = min(time), .groups = "drop")

                first_non_na_year <- min_years %>% summarise(first_common_year = max(min_year), .groups = "drop") %>% pull(first_common_year)
                last_non_na_year <- max_years %>% summarise(last_common_year = min(max_year), .groups = "drop") %>% pull(last_common_year)

                data <- filter(data, time >= max(first_non_na_year) & time <= min(last_non_na_year))
                unique(format(rev(data$time), "%Y"))
        })

        observe({
                req(input$countries_se, input$coicop_se)
                years <- selected_se_data()
                updateSelectInput(session, "select_years_se", choices = years, selected = years[1])
        })

        observeEvent(input$update_se, {
                output$plot_se <- renderPlotly({
                        req(hikp_se_data(), input$coicop_se, input$select_years_se)
                        data <- hikp_se_data()

                        month_abbrev <- c("Dec Y-1", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                        names(month_abbrev) <- 0:12

                        filtered_data <- data %>% filter(month == 1)
                        modified_data <- filtered_data %>%
                                mutate(
                                        month = 0,
                                        time = time - months(1),
                                        IX_j = IX_j_12_ymin1,
                                        index_dec_prv_yr = 100.00000
                                )

                        data <- bind_rows(data, modified_data) %>%
                                arrange(coicop18, geo, year, month)

                        time_filtered_data <- data[data$year %in% input$select_years_se, ]
                        validate(need(nrow(time_filtered_data) > 0, "No data available for the selected years."))

                        visible_y_values <- time_filtered_data$index_dec_prv_yr
                        y_rng <- finite_range(
                                visible_y_values,
                                default = c(98, 102),
                                pad = 0.08,
                                min_padding = 0.75
                        )

                        subplots <- list()
                        count <- 0
                        for (geo_value in unique(time_filtered_data$geo)) {
                                count <- count + 1
                                filtered_data <- time_filtered_data[time_filtered_data$geo == geo_value, ]
                                filtered_data$index_dec_prv_yr <- ifelse(is.na(filtered_data$index_dec_prv_yr), 0, filtered_data$index_dec_prv_yr)

                                subpl <- plot_ly(
                                        filtered_data,
                                        x = ~month,
                                        y = ~index_dec_prv_yr,
                                        color = ~as.factor(year),
                                        legendgroup = ~as.factor(year),
                                        type = "scatter",
                                        mode = "lines",
                                        showlegend = count == 1
                                ) %>%
                                        layout(
                                                yaxis = list(title = "Index Dec. Y-1=100", showgrid = TRUE, showline = FALSE, zeroline = FALSE, range = y_rng),
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
                                                xaxis = list(
                                                        range = c(0, 12),
                                                        showgrid = TRUE,
                                                        showline = FALSE,
                                                        zeroline = FALSE,
                                                        title = "",
                                                        tickvals = 0:12,
                                                        ticktext = month_abbrev,
                                                        tickangle = 45
                                                )
                                        )
                                subplots[[geo_value]] <- subpl
                        }

                        dims <- subplot_dims(length(subplots))
                        layout <- plotly::subplot(
                                subplots,
                                nrows = dims$nrows,
                                titleX = TRUE,
                                shareX = TRUE,
                                titleY = TRUE,
                                shareY = TRUE
                        ) %>%
                                layout(
                                        autosize = FALSE,
                                        height = dims$total_height,
                                        font = list(size = 11),
                                        legend = list(traceorder = "reversed")
                                )

                        plotly_plot_se <- plotly_build(layout)

                        # Enforce the same tight range on all subplot y-axes after
                        # plotly has combined the individual panels. This avoids
                        # Plotly's subplot defaults reopening the vertical scale.
                        yaxis_names <- grep("^yaxis", names(plotly_plot_se$x$layout), value = TRUE)
                        for (axis_name in yaxis_names) {
                                plotly_plot_se$x$layout[[axis_name]]$range <- y_rng
                                plotly_plot_se$x$layout[[axis_name]]$autorange <- FALSE
                        }

                        plotly_plot_se$x$attrs$legend$x$class <- "plot-container"
                        plotly_plot_se
                })
        })

        # ------------------------------------------------------------
        # Downloads
        # ------------------------------------------------------------

        output$downloadData <- downloadHandler(
                filename = function() {
                        paste("data-", Sys.Date(), ".csv", sep = "")
                },
                content = function(con) {
                        data <- plot_data() %>%
                                filter(geo %in% input$countries)

                        if (nrow(data) == 0) {
                                ddata <- data
                        } else {
                                first_non_na_year <- min(data$time[!is.na(data$values)])
                                data <- filter(data, time >= first_non_na_year)
                                ddata <- data %>%
                                        select(any_of(c(
                                                "measure", "adjustment", "coicop18", "geo", "time",
                                                "original_values", "total_values", "eu27_values", "values", "newbase"
                                        )))
                        }

                        write.csv(ddata, con, row.names = FALSE)
                }
        )

        output$downloadData_w <- downloadHandler(
                filename = function() {
                        paste("data-", Sys.Date(), ".csv", sep = "")
                },
                content = function(con_w) {
                        data <- hikp_w_data()
                        data$time <- substr(data$time, 1, 4)
                        ddata <- data %>%
                                select(classification, selected_aggregate, coicop18, code_label, geo, time, values)
                        write.csv(ddata, con_w, row.names = FALSE)
                }
        )

        output$downloadData_ar <- downloadHandler(
                filename = function() {
                        paste("data-", Sys.Date(), ".csv", sep = "")
                },
                content = function(con_ar) {
                        data <- hikp_ar_data()
                        data$year <- year(data$time)
                        data$month <- month(data$time)
                        ddata <- data %>% select(measure, target_coicop, coicop18, geo, year, month, Contr_j, ann_rate_00)
                        write.csv(ddata, con_ar, row.names = FALSE)
                }
        )

        output$downloadData_mr <- downloadHandler(
                filename = function() {
                        paste("data-", Sys.Date(), ".csv", sep = "")
                },
                content = function(con_mr) {
                        data <- hikp_mr_data()
                        data$year <- year(data$time)
                        data$month <- month(data$time)
                        ddata <- data %>% select(measure, target_coicop, coicop18, geo, time, month, year, Contr_j, m_rate_00)
                        write.csv(ddata, con_mr, row.names = FALSE)
                }
        )
}

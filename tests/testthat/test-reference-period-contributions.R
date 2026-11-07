test_that("monthly references expose additive cumulative changes without level allocation", {
        data <- make_synthetic_hicp()
        result <- calculate_reference_period_contributions(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_type = "month",
                reference_period = "2021-01-01",
                residual_tolerance = 1e-10
        )
        monthly <- result[!duplicated(result$time), ]
        period_data <- split(result, result$time)

        expect_equal(
                monthly$parent_rebased - 100,
                monthly$total_change_pct,
                tolerance = 1e-10
        )
        expect_true(all(vapply(period_data, function(period) {
                isTRUE(all.equal(
                        sum(period$contribution_pp),
                        unique(period$contribution_sum_pp),
                        tolerance = 1e-10
                ))
        }, logical(1))))
        expect_false(any(c(
                "level_share", "base_level_allocation", "allocated_index_level",
                "allocated_index_level_plot", "allocated_level_sum",
                "level_allocation_complete", "negative_allocated_level",
                "reconstructed_index"
        ) %in% names(result)))
})

test_that("full-year transformation has the required annual means", {
        data <- make_synthetic_hicp()
        result <- calculate_reference_period_contributions(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_type = "year",
                reference_period = 2022,
                residual_tolerance = 1e-10
        )
        reference <- result[format(result$time, "%Y") == "2022", ]
        parent_reference <- reference[!duplicated(reference$time), ]
        monthly <- result[!duplicated(result$time), ]

        expect_equal(mean(parent_reference$parent_rebased), 100, tolerance = 1e-10)
        expect_equal(
                as.numeric(tapply(reference$contribution_pp, reference$component_code, mean)),
                c(0, 0),
                tolerance = 1e-10
        )
        expect_equal(
                monthly$parent_rebased - 100,
                monthly$total_change_pct,
                tolerance = 1e-10
        )
})

test_that("full-year contributions remain additive before and after the reference year", {
        data <- make_synthetic_hicp()
        result <- calculate_reference_period_contributions(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_type = "year",
                reference_period = 2022,
                residual_tolerance = 1e-10
        )
        monthly <- result[!duplicated(result$time), ]
        selected <- monthly[monthly$time %in% as.Date(c("2021-06-01", "2023-06-01")), ]

        expect_equal(
                selected$total_change_pct,
                selected$contribution_sum_pp,
                tolerance = 1e-10
        )
        expect_true(all(abs(selected$residual_pp) < 1e-10))
})

test_that("full-year anchors keep the earliest uninterrupted computable history", {
        data <- make_synthetic_hicp()
        anchor <- select_fixed_base_year_anchor(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_year = 2024
        )

        expect_true(anchor$valid)
        expect_equal(anchor$anchor_period, as.Date("2021-01-01"))
        expect_false(anchor$history_limited)
})

test_that("a missing intermediate annual weight moves the full-year anchor forward", {
        data <- make_synthetic_hicp()
        data$weights$values[
                data$weights$coicop18 == "B" & format(data$weights$time, "%Y") == "2022"
        ] <- NA_real_

        anchor <- select_fixed_base_year_anchor(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_year = 2024
        )
        available_years <- valid_fixed_base_years(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B")
        )
        result <- calculate_reference_period_contributions(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_type = "year",
                reference_period = 2024,
                residual_tolerance = 1e-10
        )
        reference <- result[format(result$time, "%Y") == "2024", ]
        parent_reference <- reference[!duplicated(reference$time), ]

        expect_true(anchor$valid)
        expect_equal(anchor$anchor_period, as.Date("2023-01-01"))
        expect_true(anchor$history_limited)
        expect_match(anchor$history_limit_reason, "incomplete through 2022")
        expect_true(2024 %in% available_years)
        expect_equal(unique(result$anchor_period), as.Date("2023-01-01"))
        expect_equal(min(result$time), as.Date("2023-01-01"))
        expect_true(any(format(result$time, "%Y") == "2023"))
        expect_equal(mean(parent_reference$parent_rebased), 100, tolerance = 1e-10)
})

test_that("a missing intermediate December link moves the full-year anchor forward", {
        data <- make_synthetic_hicp()
        data$index$values[
                data$index$coicop18 == "A" & data$index$time == as.Date("2022-12-01")
        ] <- NA_real_

        anchor <- select_fixed_base_year_anchor(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_year = 2024
        )

        expect_true(anchor$valid)
        expect_equal(anchor$anchor_period, as.Date("2024-01-01"))
        expect_true(anchor$history_limited)
        expect_match(anchor$history_limit_reason, "incomplete through 2023")
})

test_that("full-year references require twelve complete months", {
        data <- make_synthetic_hicp()
        data$index$values[
                data$index$coicop18 == "A" & data$index$time == as.Date("2022-05-01")
        ] <- NA_real_

        expect_false(2022 %in% valid_fixed_base_years(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B")
        ))
        expect_error(
                calculate_reference_period_contributions(
                        data$index,
                        data$weights,
                        parent_code = "P",
                        component_codes = c("A", "B"),
                        reference_type = "year",
                        reference_period = 2022
                ),
                "incomplete"
        )
})

test_that("incomplete target months keep missing contributions and diagnostics explicit", {
        data <- make_synthetic_hicp()
        data$index$values[
                data$index$coicop18 == "A" & data$index$time == as.Date("2023-05-01")
        ] <- NA_real_
        result <- calculate_reference_period_contributions(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                reference_type = "year",
                reference_period = 2022
        )
        target <- result[result$time == as.Date("2023-05-01"), ]

        expect_true(all(!target$complete))
        expect_true(is.na(target$contribution_pp[target$component_code == "A"]))
        expect_true(is.finite(target$contribution_pp[target$component_code == "B"]))
        expect_true(all(is.na(target$contribution_sum_pp)))
        expect_true(all(is.na(target$residual_pp)))
        expect_true(all(is.finite(target$parent_rebased)))
        expect_true(all(is.finite(target$total_change_pct)))
})

test_that("positive and negative contributions retain their algebraic sum", {
        dates <- as.Date(c("2020-12-01", "2021-01-01", "2021-02-01"))
        index_data <- rbind(
                data.frame(geo = "XX", coicop18 = "P", time = dates, values = c(100, 100, 170)),
                data.frame(geo = "XX", coicop18 = "A", time = dates, values = c(100, 100, 200)),
                data.frame(geo = "XX", coicop18 = "B", time = dates, values = c(100, 100, 50))
        )
        weight_data <- data.frame(
                geo = "XX",
                coicop18 = c("P", "A", "B"),
                time = as.Date("2021-01-01"),
                values = c(1000, 800, 200)
        )
        result <- calculate_fixed_base_contributions(
                index_data,
                weight_data,
                parent_code = "P",
                component_codes = c("A", "B"),
                base_period = as.Date("2021-01-01"),
                residual_tolerance = 1e-10
        )
        target <- result[result$time == as.Date("2021-02-01"), ]

        expect_equal(target$contribution_pp, c(80, -10), tolerance = 1e-12)
        expect_equal(unique(target$contribution_sum_pp), 70, tolerance = 1e-12)
        expect_equal(unique(target$total_change_pct), 70, tolerance = 1e-12)
        expect_equal(unique(target$residual_pp), 0, tolerance = 1e-12)
        expect_lt(target$contribution_pp[target$component_code == "B"], 0)
})

test_that("the fixed-base Plotly chart uses relative contribution bars and one published line", {
        server_lines <- readLines(testthat::test_path("..", "..", "server.R"), warn = FALSE)
        start <- grep("output\\$plot_fixed_base <- renderPlotly", server_lines)
        finish <- grep("hikp_w_data <- eventReactive", server_lines)
        plot_lines <- server_lines[start:(finish[finish > start][1] - 1L)]
        plot_text <- paste(plot_lines, collapse = "\n")

        expect_length(start, 1)
        expect_match(plot_text, 'y_col = "contribution_pp"', fixed = TRUE)
        expect_match(plot_text, 'barmode = "relative"', fixed = TRUE)
        expect_match(plot_text, "y = ~total_change_pct", fixed = TRUE)
        expect_match(plot_text, 'textposition = "none"', fixed = TRUE)
        expect_match(plot_text, 'hovermode = "closest"', fixed = TRUE)
        expect_match(plot_text, "Published rebased index:", fixed = TRUE)
        expect_match(plot_text, "Cumulative change:", fixed = TRUE)
        expect_match(plot_text, "rangeslider = list", fixed = TRUE)
        expect_match(plot_text, "zeroline = TRUE", fixed = TRUE)
        expect_match(plot_text, "ifelse(complete, contribution_pp, NA_real_)", fixed = TRUE)
        expect_false(grepl("allocated_index_level", plot_text, fixed = TRUE))
        expect_false(grepl("fixed_base_stacking_status", plot_text, fixed = TRUE))
})

test_that("the fixed-base CSV keeps contribution diagnostics and drops level allocations", {
        server_lines <- readLines(testthat::test_path("..", "..", "server.R"), warn = FALSE)
        start <- grep("output\\$downloadData_fixed_base <- downloadHandler", server_lines)
        finish <- grep("output\\$downloadData_w <- downloadHandler", server_lines)
        export_lines <- server_lines[start:(finish[finish > start][1] - 1L)]
        export_text <- paste(export_lines, collapse = "\n")

        expect_length(start, 1)
        for (field in c(
                "geo", "time", "parent_code", "component_code", "reference_type",
                "reference_period", "anchor_period", "contribution_pp", "parent_rebased",
                "total_change_pct", "contribution_sum_pp", "residual_pp", "complete",
                "history_limited", "history_limit_reason"
        )) {
                expect_match(export_text, field, fixed = TRUE)
        }
        for (field in c(
                "level_share", "base_level_allocation", "allocated_index_level",
                "allocated_index_level_plot", "allocated_level_sum",
                "level_allocation_complete", "negative_allocated_level",
                "reconstructed_index"
        )) {
                expect_false(grepl(field, export_text, fixed = TRUE))
        }
})

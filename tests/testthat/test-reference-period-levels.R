test_that("monthly reference periods allocate the full index level", {
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

        shares <- result[!duplicated(result$component_code), ]
        monthly <- result[!duplicated(result$time), ]

        expect_equal(sum(shares$level_share), 1, tolerance = 1e-12)
        expect_equal(sum(shares$base_level_allocation), 100, tolerance = 1e-10)
        expect_equal(
                monthly$allocated_level_sum,
                100 + monthly$contribution_sum_pp,
                tolerance = 1e-10
        )
        expect_true(all(monthly$level_allocation_complete))
})

test_that("monthly level allocations remain additive across several year boundaries", {
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
        target <- result[
                result$time == as.Date("2023-06-01"),
        ]

        expect_equal(
                unique(target$allocated_level_sum),
                100 + unique(target$contribution_sum_pp),
                tolerance = 1e-10
        )
        expect_equal(
                unique(target$parent_rebased),
                unique(target$allocated_level_sum),
                tolerance = 1e-10
        )
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

        expect_equal(mean(parent_reference$parent_rebased), 100, tolerance = 1e-10)
        expect_equal(
                as.numeric(tapply(reference$contribution_pp, reference$component_code, mean)),
                c(0, 0),
                tolerance = 1e-10
        )
        annual_shares <- result[!duplicated(result$component_code), "level_share"]
        expect_equal(sum(annual_shares), 1, tolerance = 1e-12)
})

test_that("full-year allocations are additive before and after the reference year", {
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
                selected$allocated_level_sum,
                100 + selected$contribution_sum_pp,
                tolerance = 1e-10
        )
        expect_equal(selected$parent_rebased, selected$allocated_level_sum, tolerance = 1e-10)
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
        monthly <- result[!duplicated(result$time), ]

        expect_true(anchor$valid)
        expect_equal(anchor$anchor_period, as.Date("2023-01-01"))
        expect_true(anchor$history_limited)
        expect_match(anchor$history_limit_reason, "incomplete through 2022")
        expect_true(2024 %in% available_years)
        expect_equal(unique(result$anchor_period), as.Date("2023-01-01"))
        expect_equal(min(result$time), as.Date("2023-01-01"))
        expect_true(any(format(result$time, "%Y") == "2023"))
        expect_equal(mean(parent_reference$parent_rebased), 100, tolerance = 1e-10)
        expect_equal(
                monthly$allocated_level_sum,
                100 + monthly$contribution_sum_pp,
                tolerance = 1e-10
        )
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

test_that("incomplete target months do not create apparently complete stacks", {
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
        expect_true(all(is.na(target$contribution_sum_pp)))
        expect_true(all(!target$level_allocation_complete))
        expect_true(all(is.na(target$allocated_index_level_plot)))
        expect_true(all(is.na(target$allocated_level_sum)))
})

test_that("negative contributions and negative allocated levels are distinguished", {
        contributions <- data.frame(
                geo = "XX",
                time = as.Date("2024-01-01"),
                component_code = c("A", "B"),
                contribution_pp = c(-15, 15),
                contribution_sum_pp = c(0, 0),
                complete = TRUE,
                stringsAsFactors = FALSE
        )
        shares <- data.frame(
                geo = "XX",
                component_code = c("A", "B"),
                level_share = c(0.10, 0.90),
                base_level_allocation = c(10, 90),
                stringsAsFactors = FALSE
        )
        negative_level <- allocate_fixed_base_levels(contributions, shares)
        status <- fixed_base_stacking_status(negative_level)

        expect_equal(sum(negative_level$allocated_index_level), 100, tolerance = 1e-12)
        expect_true(any(negative_level$negative_allocated_level))
        expect_false(status$supported)

        contributions$contribution_pp <- c(-5, 5)
        negative_contribution <- allocate_fixed_base_levels(contributions, shares)
        expect_true(all(negative_contribution$allocated_index_level >= 0))
        expect_true(fixed_base_stacking_status(negative_contribution)$supported)
})

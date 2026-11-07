test_that("same-year contributions are exactly additive", {
        data <- make_synthetic_hicp()
        result <- calculate_synthetic(data)
        same_year <- result[format(result$time, "%Y") == "2021", ]
        monthly <- same_year[!duplicated(same_year$time), ]

        expect_true(all(monthly$complete))
        expect_equal(monthly$contribution_sum_pp, monthly$total_change_pct, tolerance = 1e-10)
        expect_equal(monthly$residual_pp, rep(0, nrow(monthly)), tolerance = 1e-10)
})

test_that("one and several year boundaries remain exactly additive", {
        data <- make_synthetic_hicp()
        result <- calculate_synthetic(data)
        monthly <- result[!duplicated(result$time), ]
        one_boundary <- monthly[monthly$time == as.Date("2022-06-01"), ]
        several_boundaries <- monthly[monthly$time == as.Date("2023-06-01"), ]

        expect_equal(one_boundary$contribution_sum_pp, one_boundary$total_change_pct, tolerance = 1e-10)
        expect_equal(several_boundaries$contribution_sum_pp, several_boundaries$total_change_pct, tolerance = 1e-10)
        expect_false(one_boundary$significant_residual)
        expect_false(several_boundaries$significant_residual)
})

test_that("annual weight changes are used instead of frozen base weights", {
        data <- make_synthetic_hicp()
        changed_weights <- calculate_synthetic(data)

        frozen <- data
        for (year in 2022:2023) {
                frozen$weights$values[
                        frozen$weights$coicop18 == "A" & format(frozen$weights$time, "%Y") == year
                ] <- frozen$weights$values[
                        frozen$weights$coicop18 == "P" & format(frozen$weights$time, "%Y") == year
                ] * 0.60
                frozen$weights$values[
                        frozen$weights$coicop18 == "B" & format(frozen$weights$time, "%Y") == year
                ] <- frozen$weights$values[
                        frozen$weights$coicop18 == "P" & format(frozen$weights$time, "%Y") == year
                ] * 0.40
        }
        frozen_weights <- calculate_synthetic(frozen)

        changed_final <- changed_weights[
                changed_weights$time == as.Date("2023-06-01") & changed_weights$component_code == "A",
                "contribution_pp"
        ]
        frozen_final <- frozen_weights[
                frozen_weights$time == as.Date("2023-06-01") & frozen_weights$component_code == "A",
                "contribution_pp"
        ]
        expect_false(isTRUE(all.equal(changed_final, frozen_final, tolerance = 1e-12)))
})

test_that("results do not depend on Eurostat index reference scaling", {
        data <- make_synthetic_hicp()
        baseline <- calculate_synthetic(data)
        rescaled <- data
        scale <- c(P = 1.25, A = 0.80, B = 1.60)
        rescaled$index$values <- rescaled$index$values * scale[rescaled$index$coicop18]
        comparison <- calculate_synthetic(rescaled)

        expect_equal(comparison$contribution_pp, baseline$contribution_pp, tolerance = 1e-10)
        expect_equal(comparison$total_change_pct, baseline$total_change_pct, tolerance = 1e-10)
})

test_that("missing index and weight observations are explicit", {
        data <- make_synthetic_hicp()
        data$index$values[
                data$index$coicop18 == "A" & data$index$time == as.Date("2022-05-01")
        ] <- NA_real_
        data$weights$values[
                data$weights$coicop18 == "B" & data$weights$time == as.Date("2023-01-01")
        ] <- NA_real_
        result <- calculate_synthetic(data)

        may_2022 <- result[result$time == as.Date("2022-05-01"), ]
        june_2023 <- result[result$time == as.Date("2023-06-01"), ]
        expect_true(is.na(may_2022$contribution_pp[may_2022$component_code == "A"]))
        expect_true(all(!may_2022$complete))
        expect_true(all(is.na(may_2022$contribution_sum_pp)))
        expect_true(all(is.na(may_2022$residual_pp)))
        expect_match(unique(may_2022$missing_components), "A")
        expect_match(unique(june_2023$missing_components), "B")
})

test_that("published parent residual is measured and never forced away", {
        data <- make_synthetic_hicp()
        target <- data$index$coicop18 == "P" & data$index$time == as.Date("2022-05-01")
        data$index$values[target] <- data$index$values[target] + 0.25
        result <- calculate_synthetic(data)
        may_2022 <- result[result$time == as.Date("2022-05-01"), ]
        summary_row <- may_2022[1, ]

        expect_true(summary_row$complete)
        expect_gt(abs(summary_row$residual_pp), 0)
        expect_equal(
                summary_row$residual_pp,
                summary_row$total_change_pct - summary_row$contribution_sum_pp,
                tolerance = 1e-12
        )
        expect_true(summary_row$significant_residual)
})

test_that("a known same-year analytical result is reproduced", {
        data <- make_synthetic_hicp()
        result <- calculate_synthetic(data)
        component <- result[
                result$time == as.Date("2021-02-01") & result$component_code == "A",
        ]
        lookup <- function(code, date) {
                data$index$values[data$index$coicop18 == code & data$index$time == as.Date(date)]
        }
        updated_weight <- 0.60 *
                (lookup("A", "2021-01-01") / lookup("A", "2020-12-01")) /
                (lookup("P", "2021-01-01") / lookup("P", "2020-12-01"))
        expected <- updated_weight *
                (lookup("A", "2021-02-01") / lookup("A", "2021-01-01") - 1) * 100

        expect_equal(component$contribution_pp, expected, tolerance = 1e-12)
})

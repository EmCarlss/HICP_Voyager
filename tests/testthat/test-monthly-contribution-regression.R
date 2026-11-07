test_that("shared monthly helpers reproduce the previous M/M-1 calculation", {
        weight_share <- c(0.55, 0.45)
        previous_index <- c(104, 98)
        current_index <- c(105, 99.5)
        december_index <- c(100, 100)

        legacy_denominator <- sum(
                weight_share * (previous_index / december_index),
                na.rm = TRUE
        )
        legacy_weight <- weight_share *
                (previous_index / december_index) /
                legacy_denominator
        legacy_rate <- (current_index / previous_index - 1) * 100
        legacy_contribution <- legacy_rate * legacy_weight

        denominator <- monthly_chain_denominator(
                weight_share,
                previous_index,
                december_index
        )
        updated_weight <- monthly_price_updated_weight(
                weight_share,
                previous_index,
                december_index,
                denominator
        )
        rate <- monthly_component_rate(current_index, previous_index)
        contribution <- monthly_component_contribution(rate, updated_weight)

        expect_identical(denominator, legacy_denominator)
        expect_equal(updated_weight, legacy_weight, tolerance = 0)
        expect_equal(rate, legacy_rate, tolerance = 0)
        expect_equal(contribution, legacy_contribution, tolerance = 0)
})

test_that("shared monthly helpers preserve missing-value behaviour", {
        weight_share <- c(0.6, NA_real_)
        previous_index <- c(101, 99)
        current_index <- c(102, NA_real_)
        december_index <- c(100, 100)
        denominator <- monthly_chain_denominator(weight_share, previous_index, december_index)
        updated_weight <- monthly_price_updated_weight(
                weight_share,
                previous_index,
                december_index,
                denominator
        )
        rate <- monthly_component_rate(current_index, previous_index)

        expect_true(is.finite(denominator))
        expect_true(is.na(updated_weight[2]))
        expect_true(is.na(rate[2]))
        expect_true(is.na(monthly_component_contribution(rate, updated_weight)[2]))
        expect_true(is.infinite(monthly_component_rate(1, 0)))
})

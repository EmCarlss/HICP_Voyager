source(testthat::test_path("..", "..", "R", "contribution_core.R"))
source(testthat::test_path("..", "..", "R", "contribution_fixed_base.R"))

make_synthetic_hicp <- function() {
        dates <- seq.Date(as.Date("2020-12-01"), as.Date("2024-12-01"), by = "month")
        n <- length(dates)
        component_a <- 100 * cumprod(c(1, rep(c(1.010, 1.004, 0.998), length.out = n - 1)))
        component_b <- 100 * cumprod(c(1, rep(c(1.002, 1.007, 1.001), length.out = n - 1)))
        shares <- list(
                `2021` = c(A = 0.60, B = 0.40),
                `2022` = c(A = 0.45, B = 0.55),
                `2023` = c(A = 0.70, B = 0.30),
                `2024` = c(A = 0.50, B = 0.50)
        )

        parent <- rep(NA_real_, n)
        parent[dates == as.Date("2020-12-01")] <- 100
        for (year in 2021:2024) {
                reference_date <- as.Date(sprintf("%d-12-01", year - 1L))
                year_rows <- as.integer(format(dates, "%Y")) == year
                reference_row <- dates == reference_date
                weight <- shares[[as.character(year)]]
                parent[year_rows] <- parent[reference_row] * (
                        weight["A"] * component_a[year_rows] / component_a[reference_row] +
                                weight["B"] * component_b[year_rows] / component_b[reference_row]
                )
        }

        index_data <- rbind(
                data.frame(geo = "XX", coicop18 = "P", time = dates, values = parent),
                data.frame(geo = "XX", coicop18 = "A", time = dates, values = component_a),
                data.frame(geo = "XX", coicop18 = "B", time = dates, values = component_b)
        )

        parent_weights <- c(`2021` = 500, `2022` = 400, `2023` = 600, `2024` = 550)
        weight_data <- do.call(rbind, lapply(2021:2024, function(year) {
                weight <- shares[[as.character(year)]]
                parent_weight <- parent_weights[as.character(year)]
                data.frame(
                        geo = "XX",
                        coicop18 = c("P", "A", "B"),
                        time = as.Date(sprintf("%d-01-01", year)),
                        values = c(parent_weight, parent_weight * weight["A"], parent_weight * weight["B"])
                )
        }))

        list(index = index_data, weights = weight_data)
}

make_long_reference_hicp <- function() {
        years <- 2010:2016
        dates <- seq.Date(as.Date("2009-12-01"), as.Date("2016-12-01"), by = "month")
        n <- length(dates)
        component_a <- 100 * cumprod(c(1, rep(c(1.006, 1.002, 0.999), length.out = n - 1)))
        component_b <- 100 * cumprod(c(1, rep(c(1.001, 1.004, 1.002), length.out = n - 1)))
        shares <- lapply(years, function(year) {
                a_share <- 0.45 + 0.05 * ((year - min(years)) %% 3)
                c(A = a_share, B = 1 - a_share)
        })
        names(shares) <- as.character(years)

        parent <- rep(NA_real_, n)
        parent[dates == as.Date("2009-12-01")] <- 100
        for (year in years) {
                reference_date <- as.Date(sprintf("%d-12-01", year - 1L))
                year_rows <- as.integer(format(dates, "%Y")) == year
                reference_row <- dates == reference_date
                weight <- shares[[as.character(year)]]
                parent[year_rows] <- parent[reference_row] * (
                        weight["A"] * component_a[year_rows] / component_a[reference_row] +
                                weight["B"] * component_b[year_rows] / component_b[reference_row]
                )
        }

        index_data <- rbind(
                data.frame(geo = "XX", coicop18 = "P", time = dates, values = parent),
                data.frame(geo = "XX", coicop18 = "A", time = dates, values = component_a),
                data.frame(geo = "XX", coicop18 = "B", time = dates, values = component_b)
        )
        weight_data <- do.call(rbind, lapply(years, function(year) {
                weight <- shares[[as.character(year)]]
                data.frame(
                        geo = "XX",
                        coicop18 = c("P", "A", "B"),
                        time = as.Date(sprintf("%d-01-01", year)),
                        values = c(1000, 1000 * weight["A"], 1000 * weight["B"])
                )
        }))

        list(index = index_data, weights = weight_data)
}

calculate_synthetic <- function(data, base = "2021-01-01") {
        calculate_fixed_base_contributions(
                data$index,
                data$weights,
                parent_code = "P",
                component_codes = c("A", "B"),
                base_period = as.Date(base),
                residual_tolerance = 1e-10
        )
}

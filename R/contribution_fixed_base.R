# Exact contributions over arbitrary monthly periods for an annually chained
# Laspeyres-type HICP. Implements Schwind and Weinand (2026), equations (5)-(7).

fixed_base_month <- function(x) {
        dates <- if (inherits(x, "Date")) {
                x
        } else if (is.numeric(x)) {
                as.Date(x, origin = "1970-01-01")
        } else {
                as.Date(x)
        }
        as.Date(format(dates, "%Y-%m-01"))
}

fixed_base_lookup <- function(data, geo, code, period = NULL, year = NULL) {
        rows <- data$geo == geo & data$coicop18 == code
        if (!is.null(period)) rows <- rows & data$.period == fixed_base_month(period)
        if (!is.null(year)) rows <- rows & data$.year == year
        values <- data$values[rows]
        values <- values[!is.na(values)]
        if (length(values) != 1) NA_real_ else as.numeric(values)
}

fixed_base_component_term <- function(weight_share, scale, end_index, start_index) {
        if (is.na(weight_share) || is.na(scale)) return(NA_real_)
        if (weight_share == 0) return(0)
        if (is.na(end_index) || is.na(start_index) || start_index == 0) return(NA_real_)
        weight_share * scale * (end_index / start_index - 1) * 100
}

fixed_base_price_updated_weight <- function(
        index_data,
        weight_data,
        geo,
        parent_code,
        component_code,
        period
) {
        period <- fixed_base_month(period)
        year <- as.integer(format(period, "%Y"))
        december_previous <- as.Date(sprintf("%d-12-01", year - 1L))

        annual_share <- normalise_component_weight(
                fixed_base_lookup(weight_data, geo, component_code, year = year),
                fixed_base_lookup(weight_data, geo, parent_code, year = year)
        )

        price_update_weight(
                annual_share,
                fixed_base_lookup(index_data, geo, component_code, period = period),
                fixed_base_lookup(index_data, geo, component_code, period = december_previous),
                fixed_base_lookup(index_data, geo, parent_code, period = period),
                fixed_base_lookup(index_data, geo, parent_code, period = december_previous)
        )
}

fixed_base_contribution_one <- function(
        index_data,
        weight_data,
        geo,
        parent_code,
        component_code,
        base_period,
        target_period
) {
        base_period <- fixed_base_month(base_period)
        target_period <- fixed_base_month(target_period)
        base_year <- as.integer(format(base_period, "%Y"))
        target_year <- as.integer(format(target_period, "%Y"))
        parent_base <- fixed_base_lookup(index_data, geo, parent_code, period = base_period)
        component_base <- fixed_base_lookup(index_data, geo, component_code, period = base_period)
        base_weight <- fixed_base_price_updated_weight(
                index_data,
                weight_data,
                geo,
                parent_code,
                component_code,
                base_period
        )

        if (target_period == base_period) return(0)

        if (target_year == base_year) {
                component_target <- fixed_base_lookup(index_data, geo, component_code, period = target_period)
                return(fixed_base_component_term(base_weight, 1, component_target, component_base))
        }

        december_base_year <- as.Date(sprintf("%d-12-01", base_year))
        component_december_base <- fixed_base_lookup(
                index_data,
                geo,
                component_code,
                period = december_base_year
        )
        result <- fixed_base_component_term(
                base_weight,
                1,
                component_december_base,
                component_base
        )

        if (target_year - base_year > 1L) {
                for (middle_year in seq.int(base_year + 1L, target_year - 1L)) {
                        parent_december_previous <- fixed_base_lookup(
                                index_data,
                                geo,
                                parent_code,
                                period = as.Date(sprintf("%d-12-01", middle_year - 1L))
                        )
                        component_december_previous <- fixed_base_lookup(
                                index_data,
                                geo,
                                component_code,
                                period = as.Date(sprintf("%d-12-01", middle_year - 1L))
                        )
                        component_december_current <- fixed_base_lookup(
                                index_data,
                                geo,
                                component_code,
                                period = as.Date(sprintf("%d-12-01", middle_year))
                        )
                        middle_component_weight <- fixed_base_lookup(
                                weight_data,
                                geo,
                                component_code,
                                year = middle_year
                        )
                        middle_parent_weight <- fixed_base_lookup(
                                weight_data,
                                geo,
                                parent_code,
                                year = middle_year
                        )
                        middle_share <- normalise_component_weight(
                                middle_component_weight,
                                middle_parent_weight
                        )
                        middle_scale <- ifelse(
                                is.na(parent_december_previous) | is.na(parent_base) | parent_base == 0,
                                NA_real_,
                                parent_december_previous / parent_base
                        )
                        middle_term <- fixed_base_component_term(
                                middle_share,
                                middle_scale,
                                component_december_current,
                                component_december_previous
                        )
                        result <- if (is.na(result) || is.na(middle_term)) NA_real_ else result + middle_term
                }
        }

        current_reference_period <- as.Date(sprintf("%d-12-01", target_year - 1L))
        parent_current_reference <- fixed_base_lookup(
                index_data,
                geo,
                parent_code,
                period = current_reference_period
        )
        component_current_reference <- fixed_base_lookup(
                index_data,
                geo,
                component_code,
                period = current_reference_period
        )
        component_target <- fixed_base_lookup(
                index_data,
                geo,
                component_code,
                period = target_period
        )
        current_component_weight <- fixed_base_lookup(
                weight_data,
                geo,
                component_code,
                year = target_year
        )
        current_parent_weight <- fixed_base_lookup(
                weight_data,
                geo,
                parent_code,
                year = target_year
        )
        current_share <- normalise_component_weight(
                current_component_weight,
                current_parent_weight
        )
        current_scale <- ifelse(
                is.na(parent_current_reference) | is.na(parent_base) | parent_base == 0,
                NA_real_,
                parent_current_reference / parent_base
        )
        current_term <- fixed_base_component_term(
                current_share,
                current_scale,
                component_target,
                component_current_reference
        )

        if (is.na(result) || is.na(current_term)) NA_real_ else result + current_term
}

calculate_fixed_base_contributions <- function(
        index_data,
        weight_data,
        parent_code,
        component_codes,
        base_period,
        residual_tolerance = 0.05
) {
        required_index_columns <- c("geo", "coicop18", "time", "values")
        required_weight_columns <- c("geo", "coicop18", "time", "values")
        if (!all(required_index_columns %in% names(index_data))) {
                stop("Index data must contain geo, coicop18, time and values.")
        }
        if (!all(required_weight_columns %in% names(weight_data))) {
                stop("Weight data must contain geo, coicop18, time and values.")
        }
        if (length(component_codes) == 0 || anyDuplicated(component_codes)) {
                stop("Component codes must be a non-empty, unique direct-child partition.")
        }

        index_data <- index_data[, required_index_columns]
        weight_data <- weight_data[, required_weight_columns]
        index_data$.period <- fixed_base_month(index_data$time)
        weight_data$.year <- as.integer(format(as.Date(weight_data$time), "%Y"))
        base_period <- fixed_base_month(base_period)

        duplicate_index <- duplicated(index_data[c("geo", "coicop18", ".period")]) |
                duplicated(index_data[c("geo", "coicop18", ".period")], fromLast = TRUE)
        duplicate_weight <- duplicated(weight_data[c("geo", "coicop18", ".year")]) |
                duplicated(weight_data[c("geo", "coicop18", ".year")], fromLast = TRUE)
        if (any(duplicate_index)) stop("Index data contain duplicate geo/component/month observations.")
        if (any(duplicate_weight)) stop("Weight data contain duplicate geo/component/year observations.")

        geographies <- sort(unique(as.character(index_data$geo)))
        output <- list()
        output_index <- 0L

        for (geo in geographies) {
                parent_periods <- sort(unique(index_data$.period[
                        index_data$geo == geo &
                                index_data$coicop18 == parent_code &
                                !is.na(index_data$values) &
                                index_data$.period >= base_period
                ]))
                parent_base <- fixed_base_lookup(index_data, geo, parent_code, period = base_period)

                for (target_period in parent_periods) {
                        target_period <- fixed_base_month(target_period)
                        parent_target <- fixed_base_lookup(index_data, geo, parent_code, period = target_period)
                        total_change <- ifelse(
                                is.na(parent_base) | is.na(parent_target) | parent_base == 0,
                                NA_real_,
                                (parent_target / parent_base - 1) * 100
                        )
                        rebased_parent <- ifelse(is.na(total_change), NA_real_, 100 + total_change)
                        contributions <- vapply(
                                component_codes,
                                function(component_code) {
                                        fixed_base_contribution_one(
                                                index_data,
                                                weight_data,
                                                geo,
                                                parent_code,
                                                component_code,
                                                base_period,
                                                target_period
                                        )
                                },
                                numeric(1)
                        )
                        missing_components <- component_codes[is.na(contributions)]
                        complete <- length(missing_components) == 0
                        partial_sum <- sum(contributions, na.rm = TRUE)
                        contribution_sum <- if (complete) partial_sum else NA_real_
                        residual <- if (complete && !is.na(total_change)) {
                                total_change - contribution_sum
                        } else {
                                NA_real_
                        }

                        output_index <- output_index + 1L
                        output[[output_index]] <- data.frame(
                                geo = geo,
                                time = target_period,
                                base_period = base_period,
                                parent_code = parent_code,
                                component_code = component_codes,
                                contribution_pp = unname(contributions),
                                parent_rebased = rebased_parent,
                                total_change_pct = total_change,
                                partial_contribution_sum_pp = partial_sum,
                                contribution_sum_pp = contribution_sum,
                                residual_pp = residual,
                                missing_components = paste(missing_components, collapse = ", "),
                                complete = complete,
                                significant_residual = !is.na(residual) & abs(residual) > residual_tolerance,
                                stringsAsFactors = FALSE
                        )
                }
        }

        if (length(output) == 0) {
                return(data.frame())
        }
        do.call(rbind, output)
}

valid_fixed_base_months <- function(index_data, weight_data, parent_code, component_codes) {
        index_data$.period <- fixed_base_month(index_data$time)
        weight_data$.year <- as.integer(format(as.Date(weight_data$time), "%Y"))
        geographies <- unique(as.character(index_data$geo))
        candidates <- sort(unique(index_data$.period[
                index_data$coicop18 == parent_code & !is.na(index_data$values)
        ]))

        candidates[vapply(candidates, function(period) {
                period <- fixed_base_month(period)
                year <- as.integer(format(period, "%Y"))
                december_previous <- as.Date(sprintf("%d-12-01", year - 1L))
                all(vapply(geographies, function(geo) {
                        needed_codes <- c(parent_code, component_codes)
                        current_ok <- vapply(needed_codes, function(code) {
                                !is.na(fixed_base_lookup(index_data, geo, code, period = period))
                        }, logical(1))
                        reference_ok <- vapply(needed_codes, function(code) {
                                !is.na(fixed_base_lookup(index_data, geo, code, period = december_previous))
                        }, logical(1))
                        weights_ok <- vapply(needed_codes, function(code) {
                                !is.na(fixed_base_lookup(weight_data, geo, code, year = year))
                        }, logical(1))
                        all(current_ok) && all(reference_ok) && all(weights_ok)
                }, logical(1)))
        }, logical(1))]
}

valid_fixed_base_years <- function(index_data, weight_data, parent_code, component_codes) {
        months <- valid_fixed_base_months(
                index_data,
                weight_data,
                parent_code,
                component_codes
        )
        if (length(months) == 0) return(integer(0))

        years <- sort(unique(as.integer(format(months, "%Y"))))
        years[vapply(years, function(year) {
                year_months <- as.integer(format(months[format(months, "%Y") == as.character(year)], "%m"))
                identical(sort(unique(year_months)), 1:12)
        }, logical(1))]
}

fixed_base_level_shares <- function(
        index_data,
        weight_data,
        parent_code,
        component_codes,
        reference_type = c("month", "year"),
        reference_period
) {
        reference_type <- match.arg(reference_type)
        index_data <- index_data[, c("geo", "coicop18", "time", "values")]
        weight_data <- weight_data[, c("geo", "coicop18", "time", "values")]
        index_data$.period <- fixed_base_month(index_data$time)
        weight_data$.year <- as.integer(format(as.Date(weight_data$time), "%Y"))

        periods <- if (reference_type == "month") {
                fixed_base_month(reference_period)
        } else {
                reference_year <- if (inherits(reference_period, "Date")) {
                        as.integer(format(reference_period, "%Y"))
                } else {
                        as.integer(reference_period)
                }
                as.Date(sprintf("%d-%02d-01", reference_year, 1:12))
        }

        geographies <- sort(unique(as.character(index_data$geo)))
        output <- vector("list", length(geographies))

        for (geo_index in seq_along(geographies)) {
                geo <- geographies[geo_index]
                monthly_weights <- matrix(
                        unlist(lapply(periods, function(period) {
                                vapply(component_codes, function(component_code) {
                                        fixed_base_price_updated_weight(
                                                index_data,
                                                weight_data,
                                                geo,
                                                parent_code,
                                                component_code,
                                                period
                                        )
                                }, numeric(1))
                        })),
                        nrow = length(component_codes),
                        ncol = length(periods)
                )

                complete <- all(is.finite(monthly_weights))
                average_weights <- if (complete) rowMeans(monthly_weights) else rep(NA_real_, length(component_codes))
                weight_total <- sum(average_weights)
                shares <- if (!complete || !is.finite(weight_total) || weight_total <= 0) {
                        rep(NA_real_, length(component_codes))
                } else {
                        average_weights / weight_total
                }

                output[[geo_index]] <- data.frame(
                        geo = geo,
                        component_code = component_codes,
                        level_share = shares,
                        base_level_allocation = 100 * shares,
                        stringsAsFactors = FALSE
                )
        }

        do.call(rbind, output)
}

transform_fixed_base_to_full_year <- function(
        contribution_data,
        reference_year,
        residual_tolerance = 0.05
) {
        reference_year <- if (inherits(reference_year, "Date")) {
                as.integer(format(reference_year, "%Y"))
        } else {
                as.integer(reference_year)
        }
        required_columns <- c(
                "geo", "time", "component_code", "contribution_pp",
                "parent_rebased", "complete"
        )
        if (!all(required_columns %in% names(contribution_data))) {
                stop("Contribution data do not contain the fields required for a full-year transformation.")
        }

        output <- contribution_data
        output$anchor_period <- output$base_period
        output$anchor_contribution_pp <- output$contribution_pp
        output$anchor_parent_rebased <- output$parent_rebased
        output$reference_type <- "year"
        output$reference_period <- as.Date(sprintf("%d-01-01", reference_year))

        expected_months <- as.Date(sprintf("%d-%02d-01", reference_year, 1:12))
        for (geo in unique(as.character(output$geo))) {
                geo_rows <- output$geo == geo
                reference_rows <- geo_rows & format(output$time, "%Y") == as.character(reference_year)
                reference_periods <- sort(unique(output$time[reference_rows]))
                if (!identical(reference_periods, expected_months)) {
                        stop("A full-year reference period requires all twelve monthly parent observations.")
                }

                reference_complete <- output[reference_rows, , drop = FALSE]
                if (any(!reference_complete$complete) || any(is.na(reference_complete$contribution_pp))) {
                        stop("A full-year reference period requires complete component contributions for all twelve months.")
                }

                parent_reference <- output[reference_rows, c("time", "parent_rebased"), drop = FALSE]
                parent_reference <- parent_reference[!duplicated(parent_reference$time), , drop = FALSE]
                annual_parent_mean <- mean(parent_reference$parent_rebased)
                if (!is.finite(annual_parent_mean) || annual_parent_mean == 0) {
                        stop("The full-year parent-index average must be finite and non-zero.")
                }
                scale <- 100 / annual_parent_mean

                component_means <- tapply(
                        output$contribution_pp[reference_rows],
                        output$component_code[reference_rows],
                        mean
                )
                matched_means <- unname(component_means[output$component_code[geo_rows]])
                output$contribution_pp[geo_rows] <- scale * (
                        output$anchor_contribution_pp[geo_rows] - matched_means
                )
                output$parent_rebased[geo_rows] <- scale * output$anchor_parent_rebased[geo_rows]
                output$total_change_pct[geo_rows] <- output$parent_rebased[geo_rows] - 100

                geo_periods <- sort(unique(output$time[geo_rows]))
                for (period in geo_periods) {
                        period <- fixed_base_month(period)
                        period_rows <- geo_rows & output$time == period
                        contributions <- output$contribution_pp[period_rows]
                        missing_components <- output$component_code[period_rows][is.na(contributions)]
                        complete <- length(missing_components) == 0
                        partial_sum <- sum(contributions, na.rm = TRUE)
                        contribution_sum <- if (complete) partial_sum else NA_real_
                        total_change <- output$total_change_pct[period_rows][1]
                        residual <- if (complete && !is.na(total_change)) {
                                total_change - contribution_sum
                        } else {
                                NA_real_
                        }

                        output$partial_contribution_sum_pp[period_rows] <- partial_sum
                        output$contribution_sum_pp[period_rows] <- contribution_sum
                        output$residual_pp[period_rows] <- residual
                        output$missing_components[period_rows] <- paste(missing_components, collapse = ", ")
                        output$complete[period_rows] <- complete
                        output$significant_residual[period_rows] <- !is.na(residual) &
                                abs(residual) > residual_tolerance
                }
        }

        output
}

allocate_fixed_base_levels <- function(contribution_data, level_shares) {
        required_share_columns <- c("geo", "component_code", "level_share", "base_level_allocation")
        if (!all(required_share_columns %in% names(level_shares))) {
                stop("Level shares do not contain the required fields.")
        }

        output <- contribution_data
        contribution_key <- paste(output$geo, output$component_code, sep = "\r")
        share_key <- paste(level_shares$geo, level_shares$component_code, sep = "\r")
        share_rows <- match(contribution_key, share_key)
        output$level_share <- level_shares$level_share[share_rows]
        output$base_level_allocation <- level_shares$base_level_allocation[share_rows]
        output$allocated_index_level <- output$base_level_allocation + output$contribution_pp
        output$allocated_index_level_plot <- NA_real_
        output$allocated_level_sum <- NA_real_
        output$level_allocation_complete <- FALSE

        period_key <- paste(output$geo, output$time, sep = "\r")
        for (key in unique(period_key)) {
                rows <- period_key == key
                complete <- all(output$complete[rows]) &&
                        all(is.finite(output$allocated_index_level[rows]))
                output$level_allocation_complete[rows] <- complete
                if (complete) {
                        output$allocated_index_level_plot[rows] <- output$allocated_index_level[rows]
                        output$allocated_level_sum[rows] <- sum(output$allocated_index_level[rows])
                }
        }

        output$reconstructed_index <- ifelse(
                output$complete,
                100 + output$contribution_sum_pp,
                NA_real_
        )
        output$negative_allocated_level <- !is.na(output$allocated_index_level) &
                output$allocated_index_level < 0
        output
}

fixed_base_stacking_status <- function(data, tolerance = sqrt(.Machine$double.eps)) {
        negative_rows <- !is.na(data$allocated_index_level_plot) &
                data$allocated_index_level_plot < -tolerance
        list(
                supported = !any(negative_rows),
                negative_rows = negative_rows,
                message = if (any(negative_rows)) {
                        paste(
                                "One or more allocated component levels are negative.",
                                "Plotly would separate positive and negative bars, so the chart uses",
                                "non-stacked component lines instead of claiming a visual stacked total."
                        )
                } else {
                        NULL
                }
        )
}

calculate_reference_period_contributions <- function(
        index_data,
        weight_data,
        parent_code,
        component_codes,
        reference_type = c("month", "year"),
        reference_period,
        residual_tolerance = 0.05
) {
        reference_type <- match.arg(reference_type)

        if (reference_type == "month") {
                reference_month <- fixed_base_month(reference_period)
                contribution_data <- calculate_fixed_base_contributions(
                        index_data,
                        weight_data,
                        parent_code,
                        component_codes,
                        base_period = reference_month,
                        residual_tolerance = residual_tolerance
                )
                contribution_data$anchor_period <- contribution_data$base_period
                contribution_data$anchor_contribution_pp <- contribution_data$contribution_pp
                contribution_data$anchor_parent_rebased <- contribution_data$parent_rebased
                contribution_data$reference_type <- "month"
                contribution_data$reference_period <- reference_month
                level_shares <- fixed_base_level_shares(
                        index_data,
                        weight_data,
                        parent_code,
                        component_codes,
                        reference_type = "month",
                        reference_period = reference_month
                )
        } else {
                reference_year <- if (inherits(reference_period, "Date")) {
                        as.integer(format(reference_period, "%Y"))
                } else {
                        as.integer(reference_period)
                }
                valid_years <- valid_fixed_base_years(
                        index_data,
                        weight_data,
                        parent_code,
                        component_codes
                )
                if (!(reference_year %in% valid_years)) {
                        stop("The selected full-year reference period is incomplete.")
                }

                valid_months <- valid_fixed_base_months(
                        index_data,
                        weight_data,
                        parent_code,
                        component_codes
                )
                anchor_candidates <- valid_months[
                        valid_months <= as.Date(sprintf("%d-01-01", reference_year))
                ]
                if (length(anchor_candidates) == 0) {
                        stop("No complete monthly anchor is available before the full-year reference period.")
                }
                anchor_period <- min(anchor_candidates)
                anchored_data <- calculate_fixed_base_contributions(
                        index_data,
                        weight_data,
                        parent_code,
                        component_codes,
                        base_period = anchor_period,
                        residual_tolerance = residual_tolerance
                )
                contribution_data <- transform_fixed_base_to_full_year(
                        anchored_data,
                        reference_year,
                        residual_tolerance = residual_tolerance
                )
                level_shares <- fixed_base_level_shares(
                        index_data,
                        weight_data,
                        parent_code,
                        component_codes,
                        reference_type = "year",
                        reference_period = reference_year
                )
        }

        allocate_fixed_base_levels(contribution_data, level_shares)
}

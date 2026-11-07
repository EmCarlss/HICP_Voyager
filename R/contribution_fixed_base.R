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
        december_before_base_year <- as.Date(sprintf("%d-12-01", base_year - 1L))

        parent_base <- fixed_base_lookup(index_data, geo, parent_code, period = base_period)
        component_base <- fixed_base_lookup(index_data, geo, component_code, period = base_period)
        parent_reference <- fixed_base_lookup(index_data, geo, parent_code, period = december_before_base_year)
        component_reference <- fixed_base_lookup(index_data, geo, component_code, period = december_before_base_year)
        component_weight <- fixed_base_lookup(weight_data, geo, component_code, year = base_year)
        parent_weight <- fixed_base_lookup(weight_data, geo, parent_code, year = base_year)
        annual_share <- normalise_component_weight(component_weight, parent_weight)
        base_weight <- price_update_weight(
                annual_share,
                component_base,
                component_reference,
                parent_base,
                parent_reference
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

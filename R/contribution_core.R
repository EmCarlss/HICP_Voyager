# Shared, side-effect-free helpers for HICP contribution calculations.

normalise_component_weight <- function(component_weight, parent_weight) {
        ifelse(
                is.na(component_weight) | is.na(parent_weight) | parent_weight == 0,
                NA_real_,
                component_weight / parent_weight
        )
}

price_update_weight <- function(
        annual_weight_share,
        component_period_index,
        component_reference_index,
        parent_period_index,
        parent_reference_index
) {
        if (is.na(annual_weight_share)) return(NA_real_)
        if (annual_weight_share == 0) return(0)

        required <- c(
                component_period_index,
                component_reference_index,
                parent_period_index,
                parent_reference_index
        )
        if (any(is.na(required)) || component_reference_index == 0 ||
                parent_reference_index == 0 || parent_period_index == 0) {
                return(NA_real_)
        }

        annual_weight_share *
                (component_period_index / component_reference_index) /
                (parent_period_index / parent_reference_index)
}

monthly_chain_denominator <- function(weight_share, previous_index, december_index) {
        links <- previous_index / december_index
        sum(weight_share * links, na.rm = TRUE)
}

monthly_price_updated_weight <- function(
        weight_share,
        previous_index,
        december_index,
        denominator
) {
        ifelse(
                is.na(weight_share) |
                        is.na(previous_index) |
                        is.na(december_index) |
                        is.na(denominator) |
                        denominator <= 0,
                NA_real_,
                weight_share * (previous_index / december_index) / denominator
        )
}

monthly_component_rate <- function(current_index, previous_index) {
        ifelse(
                is.na(current_index) | is.na(previous_index),
                NA_real_,
                (current_index / previous_index - 1) * 100
        )
}

monthly_component_contribution <- function(monthly_rate, updated_weight) {
        ifelse(
                is.na(monthly_rate) | is.na(updated_weight),
                NA_real_,
                monthly_rate * updated_weight
        )
}

validate_direct_child_partition <- function(
        hierarchy,
        parent_code,
        component_codes = NULL,
        code_column = "coicop18_code",
        parent_column = "parent_code"
) {
        required_columns <- c(code_column, parent_column)
        missing_columns <- setdiff(required_columns, names(hierarchy))
        if (length(missing_columns) > 0) {
                return(list(
                        valid = FALSE,
                        components = character(0),
                        message = paste("Hierarchy is missing columns:", paste(missing_columns, collapse = ", "))
                ))
        }

        codes <- as.character(hierarchy[[code_column]])
        parents <- as.character(hierarchy[[parent_column]])
        expected <- sort(unique(codes[!is.na(parents) & parents == parent_code]))
        expected <- expected[!is.na(expected) & nzchar(expected)]

        if (length(expected) == 0) {
                return(list(
                        valid = FALSE,
                        components = character(0),
                        message = "The selected aggregate has no direct children in the ECOICOP hierarchy."
                ))
        }

        duplicated_codes <- unique(codes[duplicated(codes) & !is.na(codes)])
        if (length(duplicated_codes) > 0) {
                return(list(
                        valid = FALSE,
                        components = expected,
                        message = paste("Hierarchy contains duplicate codes:", paste(duplicated_codes, collapse = ", "))
                ))
        }

        overlapping <- vapply(
                expected,
                function(code) any(startsWith(expected, code) & nchar(expected) > nchar(code)),
                logical(1)
        )
        if (any(overlapping)) {
                return(list(
                        valid = FALSE,
                        components = expected,
                        message = paste(
                                "Direct children overlap in the hierarchy:",
                                paste(expected[overlapping], collapse = ", ")
                        )
                ))
        }

        if (!is.null(component_codes)) {
                supplied <- sort(unique(as.character(component_codes)))
                missing_children <- setdiff(expected, supplied)
                extra_children <- setdiff(supplied, expected)
                if (length(missing_children) > 0 || length(extra_children) > 0) {
                        details <- c(
                                if (length(missing_children) > 0) {
                                        paste("missing", paste(missing_children, collapse = ", "))
                                },
                                if (length(extra_children) > 0) {
                                        paste("unexpected", paste(extra_children, collapse = ", "))
                                }
                        )
                        return(list(
                                valid = FALSE,
                                components = expected,
                                message = paste("Component set is not the complete direct-child partition:", paste(details, collapse = "; "))
                        ))
                }
        }

        list(valid = TRUE, components = expected, message = NULL)
}

library(eurostat)
library(dplyr)
library(stringr)
hikp_coicop <- get_eurostat("prc_hicp_minr", filters = list(geo = "DE", unit = "I25"))
hicp_coicop2 <- label_eurostat(hikp_coicop, "coicop", eu_order = TRUE, code = c("coicop18", "geo"), fix_duplicated = TRUE)
coicop_set <- select(hicp_coicop2, coicop18_code, coicop18)
coicop_set <- unique(coicop_set)
coicop_set$coicop18_code[coicop_set$coicop18_code == "TOTAL"] <- "CP00"
coicop_set$code_label <- paste(coicop_set$coicop18_code, coicop_set$coicop18, sep = " ")

coicop_set_hierarchy <- coicop_set[grepl("^CP\\d{2,6}$", coicop_set$coicop18_code), ]
coicop_set_hierarchy$level <- nchar(gsub("\\D", "", coicop_set_hierarchy$coicop18_code))
coicop_set_hierarchy$level <- ifelse(coicop_set_hierarchy$coicop18_code == "CP00", 1, nchar(gsub("\\D", "", coicop_set_hierarchy$coicop18_code)))
coicop_set_hierarchy$parent_code <- ifelse(coicop_set_hierarchy$level > 2, substr(coicop_set_hierarchy$coicop18_code, 1, nchar(coicop_set_hierarchy$coicop18_code) - 1), NA)
coicop_set_hierarchy$parent_code <- ifelse(coicop_set_hierarchy$level == 2 & coicop_set_hierarchy$coicop18_code != "CP00", "CP00", ifelse(coicop_set_hierarchy$level > 2, substr(coicop_set_hierarchy$coicop18_code, 1, nchar(coicop_set_hierarchy$coicop18_code) - 1), NA))

# Uppdatera parent_code baserat på villkoret
coicop_set_hierarchy <- coicop_set_hierarchy %>%
        mutate(
                parent_code = ifelse(
                        grepl("^CP\\d{3}0\\d$", coicop18_code),
                        substr(coicop18_code, 1, 5),
                        parent_code
                )
        )

label_set<-select(coicop_set_hierarchy, coicop18_code, code_label)

# ------------------------------------------------------------
# Special aggregates hierarchy
# ------------------------------------------------------------

sa_hierarchy <- data.frame(
        coicop18_code = c(
                "IGD_NNRG",
                "SERV",
                "NRG",
                "FOOD",
                "FOOD_P",
                "FOOD_NP",
                "CP00"
        ),
        coicop18 = c(
                "Non-energy industrial goods",
                "Services (overall index excluding goods)",
                "Energy",
                "Food including alcohol and tobacco",
                "Processed food including alcohol and tobacco",
                "Unprocessed food",
                "Total"
        ),
        level = c(2, 2, 2, 2, 3, 3, 1),
        parent_code = c("CP00", "CP00", "CP00", "CP00", "FOOD", "FOOD", NA_character_),
        stringsAsFactors = FALSE
)

sa_hierarchy$code_label <- paste(
        sa_hierarchy$coicop18_code,
        sa_hierarchy$coicop18,
        sep = " "
)

label_set_ecoicop <- select(coicop_set_hierarchy, coicop18_code, code_label)
label_set_sa <- select(sa_hierarchy, coicop18_code, code_label)

# Keep existing default label_set for the rest of the app
label_set <- label_set_ecoicop

clean_eurostat_cache()

new_plot_data<-FALSE
new_rebased_data<-FALSE
new_plot_ar_data<-FALSE
new_plot_w_data<-FALSE

country_groups <- list(
        "EU" = c(
                "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
                "DE","EL","HU","IE","IT","LV","LT","LU","MT","NL",
                "PL","PT","RO","SK","SI","ES","SE"),
        "EFTA" = c("IS", "NO", "CH"),
        "EU + EFTA" = c(
                "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
                "DE","EL","HU","IE","IT","LV","LT","LU","MT","NL",
                "PL","PT","RO","SK","SI","ES","SE",
                "IS","NO","CH"
        ),
        "Euro area" = c(
                "AT","BE","HR","CY","EE","FI","FR","DE","EL","IE",
                "IT","LV","LT","LU","MT","NL","PT","SK","SI","ES"),
        "Mediterranean" = c("EL","ES", "IT", "PT", "CY", "MT"),
        "Nordic" = c("DK", "FI", "IS", "NO", "SE"),
        "Balkan" = c("AL", "BA", "HR", "ME", "MK", "RS", "SI", "XK"),
        "Central" = c("AT", "CH", "CZ", "DE", "HU", "PL", "SK"),
        "Eastern" = c("BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK"),
        "Western" = c("AT", "BE", "CH", "DE", "FR", "IE", "LU", "NL"),
        "Benelux" = c("BE", "LU", "NL")
)

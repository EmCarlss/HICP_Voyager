
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(lubridate)
library(plotly)
library(bslib)

# ------------------------------------------------------------
# Data source
# ------------------------------------------------------------
# Recommended: place TAB6612_sv.csv and TAB6622_sv.csv in the same
# folder as the app, or set KPI_DATA_DIR to the folder containing them.
# Example in R before running the app:
# Sys.setenv(KPI_DATA_DIR = "/Volumes/Lagring/Annat/Programmering/R/KPI makrogranskning")

kpi_data_dir <- Sys.getenv("KPI_DATA_DIR", unset = ".")
monthly_csv <- file.path(kpi_data_dir, "TAB6612_sv.csv")
annual_csv  <- file.path(kpi_data_dir, "TAB6622_sv.csv")

# Fallback to your current local path if the app is run from another folder.
if (!file.exists(monthly_csv)) {
  monthly_csv <- "/Volumes//Lagring//Annat//Programmering//R//KPI makrogranskning//TAB6612_sv.csv"
}
if (!file.exists(annual_csv)) {
  annual_csv <- "/Volumes//Lagring//Annat//Programmering//R//KPI makrogranskning//TAB6622_sv.csv"
}

if (!file.exists(monthly_csv)) {
  stop("Cannot find TAB6612_sv.csv. Put it in the app folder or set Sys.setenv(KPI_DATA_DIR = '...').")
}
if (!file.exists(annual_csv)) {
  stop("Cannot find TAB6622_sv.csv. Put it in the app folder or set Sys.setenv(KPI_DATA_DIR = '...').")
}

# ------------------------------------------------------------
# General helpers
# ------------------------------------------------------------
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

sum_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else sum(x)
}

weighted_mean_or_na <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
}

num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- str_trim(as.character(x))
  x <- na_if(x, "")
  x <- na_if(x, "..")
  x <- str_replace_all(x, "\\s+", "")
  x <- str_replace_all(x, ",", ".")
  as.numeric(x)
}

clean_names_swe <- function(x) {
  x %>%
    gsub("[åäÅÄ]", "a", .) %>%
    gsub("[öÖ]", "o", .) %>%
    gsub(" ", "_", .)
}

# Convert SCB's dotted product-group code to a display code on the
# CP/COICOP form used in the graphs. The first two digits are preserved
# for divisions 01-09, so 01 becomes CP01 rather than CP1.
to_eukod <- function(kod) {
  vapply(as.character(kod), function(x) {
    if (is.na(x) || !nzchar(str_trim(x))) return(NA_character_)

    x <- str_trim(x)

    if (str_to_upper(x) %in% c("TOTAL", "CP00") || str_detect(x, "^0+([.]0+)*$")) {
      return("TOTAL")
    }

    parts <- unlist(str_split(x, "\\."))
    if (length(parts) == 0) return(NA_character_)

    first_num <- suppressWarnings(as.integer(parts[1]))
    first_part <- if (!is.na(first_num)) sprintf("%02d", first_num) else parts[1]
    rest_parts <- if (length(parts) > 1) parts[-1] else character(0)

    paste0("CP", paste(c(first_part, rest_parts), collapse = ""))
  }, character(1))
}

get_col_num <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) {
    rep(NA_real_, nrow(df))
  } else {
    num(df[[found[1]]])
  }
}

wrap_legend <- function(x, width = 45) {
  x %>%
    stringr::str_wrap(width = width) %>%
    gsub("\n", "<br>", .)
}


# ------------------------------------------------------------
# Special aggregate helpers
# ------------------------------------------------------------
# Specialaggregat are built from leaf-level subklasser. Most subklasser
# are grouped by the suffix in the SCB label, but a few COICOP branches
# override the suffix-based rule.
special_aggregate_levels <- c(
  "Livsmedel",
  "Ägda boendet",
  "Energi",
  "Övriga icke-varaktiga varor",
  "Delvis varaktiga varor",
  "Varaktiga varor",
  "Tjänster exkl ägda boendet",
  "Övrigt"
)

special_aggregate_code_map <- tibble::tibble(
  special_group = special_aggregate_levels,
  special_code = sprintf("SP%02d", seq_along(special_aggregate_levels))
)

special_aggregate_select_choices <- setNames(
  special_aggregate_levels,
  special_aggregate_levels
)

special_aggregate_group <- function(kod, beskr) {
  kod <- as.character(kod)
  beskr <- as.character(beskr)
  suffix <- stringr::str_match(beskr, "\\((IV|DV|T|V)\\)\\s*$")[, 2]

  dplyr::case_when(
    !is.na(kod) & (kod == "01" | startsWith(kod, "01.")) ~ "Livsmedel",
    !is.na(kod) & (kod == "04.2" | startsWith(kod, "04.2.")) ~ "Ägda boendet",
    !is.na(kod) & (kod == "04.5" | startsWith(kod, "04.5.")) ~ "Energi",
    !is.na(kod) & (kod == "07.2.2" | startsWith(kod, "07.2.2.")) ~ "Energi",
    suffix == "IV" ~ "Övriga icke-varaktiga varor",
    suffix == "DV" ~ "Delvis varaktiga varor",
    suffix == "V" ~ "Varaktiga varor",
    suffix == "T" ~ "Tjänster exkl ägda boendet",
    TRUE ~ "Övrigt"
  )
}

special_aggregate_order_key <- function(labels) {
  labels_clean <- gsub("<br>", " ", labels, fixed = TRUE)
  order_levels <- special_aggregate_levels[special_aggregate_levels != "Övrigt"]
  idx <- match(labels_clean, order_levels)
  ifelse(
    !is.na(idx),
    idx,
    ifelse(labels_clean == "Övrigt", 9999L, 1000L)
  )
}

# ------------------------------------------------------------
# Plotly style helpers for contribution bars
# ------------------------------------------------------------
# Contribution charts can contain many direct subgroups. The default
# styling is deliberately calmer than a full pattern-per-series setup:
# - labels are ordered by their CP/COICOP code;
# - colours come from a curated qualitative palette;
# - subtle grey patterns are used only when two or more visible series
#   share the same broad hue family.
contribution_palette <- tibble::tibble(
  contribution_color = c(
    "#009E73", "#D55E00", "#7570B3", "#E7298A",
    "#66A61E", "#B79F00", "#A6761D", "#5F6368",
    "#0072B2", "#8DD3C7", "#FB8072", "#FDB462",
    "#B3A2C7", "#D4C35A", "#4E79A7", "#8C564B"
  ),
  contribution_hue_family = c(
    "green", "orange", "purple", "pink",
    "green", "yellow", "brown", "gray",
    "blue", "green", "red", "orange",
    "purple", "yellow", "blue", "brown"
  )
)

contribution_pattern_shapes <- c("/", "\\", ".", "-", "|", "+", "x")

component_sort_key <- function(labels) {
  out <- suppressWarnings(as.integer(stringr::str_extract(labels, "(?<=^CP)\\d+")))
  ifelse(is.na(out), 9999L, out)
}

hex_luminance <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055) ^ 2.4)
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}

pattern_fgcolor_for <- function(color) {
  if (hex_luminance(color) < 0.50) {
    "rgba(245, 245, 245, 0.42)"  # light grey on dark/saturated colours
  } else {
    "rgba(70, 70, 70, 0.30)"     # dark grey on light colours
  }
}

contribution_style_map <- function(labels) {
  labels <- unique(labels[!is.na(labels) & nzchar(labels)])

  if (length(labels) == 0) {
    return(tibble(
      component_label_wrapped = character(),
      contribution_color = character(),
      contribution_pattern = character()
    ))
  }

  label_order <- tibble(
    component_label_wrapped = labels
  ) %>%
    mutate(
      special_num = special_aggregate_order_key(component_label_wrapped),
      code_num = component_sort_key(component_label_wrapped)
    ) %>%
    arrange(special_num, code_num, component_label_wrapped)

  idx <- ((seq_len(nrow(label_order)) - 1L) %% nrow(contribution_palette)) + 1L

  label_order %>%
    bind_cols(contribution_palette[idx, ]) %>%
    group_by(contribution_hue_family) %>%
    mutate(
      hue_rank = row_number(),
      hue_n = n(),
      contribution_pattern = if_else(
        hue_n > 1L & hue_rank > 1L,
        contribution_pattern_shapes[((hue_rank - 2L) %% length(contribution_pattern_shapes)) + 1L],
        ""
      )
    ) %>%
    ungroup() %>%
    arrange(special_num, code_num, component_label_wrapped) %>%
    select(component_label_wrapped, contribution_color, contribution_pattern)
}

contribution_marker <- function(color, pattern_shape = "") {
  marker <- list(
    color = color,
    line = list(color = "rgba(38, 50, 56, 0.32)", width = 0.35)
  )

  if (!is.na(pattern_shape) && nzchar(pattern_shape)) {
    marker$pattern <- list(
      shape = pattern_shape,
      fillmode = "overlay",
      bgcolor = color,
      fgcolor = pattern_fgcolor_for(color),
      fgopacity = 0.70,
      size = 8,
      solidity = 0.18
    )
  }

  marker
}


month_names_sv <- c(
  "januari", "februari", "mars", "april", "maj", "juni",
  "juli", "augusti", "september", "oktober", "november", "december"
)

ym_value <- function(ar, period) sprintf("%04d-%02d", as.integer(ar), as.integer(period))
ym_label <- function(ar, period) paste(as.integer(ar), month_names_sv[as.integer(period)])

# ------------------------------------------------------------
# Read and reshape KPI data from SCB files
# ------------------------------------------------------------
data <- read.csv2(monthly_csv, fileEncoding = "Latin1", sep = ",")
data_ar <- read.csv2(annual_csv, fileEncoding = "Latin1", sep = ",")

data_wide <- data %>%
  rename(
    aggregat = `varu..tjänstegrupp`,
    värde = `Konsumentprisindex.2020.100`
  ) %>%
  pivot_wider(
    names_from  = tabellinnehåll,
    values_from = värde
  )

names(data_wide) <- clean_names_swe(names(data_wide))

data_wide <- data_wide %>%
  mutate(
    KOD = str_extract(aggregat, "^\\d+(?:\\.\\d+)*"),
    BESKR = str_squish(str_remove(aggregat, "^\\d+(?:\\.\\d+)*\\s*"))
  ) %>%
  mutate(
    EUKOD = to_eukod(KOD)
  ) %>%
  mutate(
    ar = as.integer(str_extract(manad, "^\\d{4}")),
    period = as.integer(str_extract(manad, "(?<=M)\\d{2}"))
  )

data_ar_wide <- data_ar %>%
  rename(
    aggregat = `varu..tjänstegrupp`,
    ar = år
  ) %>%
  mutate(
    varde = na_if(as.character(Konsumentprisindex), ".."),
    varde = as.numeric(varde)
  ) %>%
  pivot_wider(
    id_cols = c(aggregat, ar),
    names_from = tabellinnehåll,
    values_from = varde
  )

data_wide2 <- data_wide %>%
  left_join(data_ar_wide, by = c("aggregat", "ar"))

data_kpi <- data_wide2 %>%
  select(-any_of(c("arsforandring", "Manadsforandring", "aggregat", "manad")))

# ------------------------------------------------------------
# Standardised KPI calculation table
# ------------------------------------------------------------
id_cols <- c("KOD")

kpi0 <- data_kpi %>%
  mutate(
    ar = as.integer(ar),
    period = as.integer(period),
    is_total =
      str_to_upper(coalesce(as.character(EUKOD), "")) %in% c("TOTAL", "CP00") |
      str_to_upper(coalesce(as.character(KOD), "")) %in% c("TOTAL", "00", "00.0.0.0", "0") |
      str_detect(str_to_lower(coalesce(as.character(BESKR), "")), "^(total|totalt)")
  )

# Convert and standardise the columns used in the SCB effect calculation.
kpi0$Index         <- get_col_num(kpi0, c("Index"))
kpi0$Effekt_ar     <- get_col_num(kpi0, c("Effekt_ar", "Effekt_Ar"))
kpi0$Effekt_manad  <- get_col_num(kpi0, c("Effekt_manad", "Effekt_månad", "Effekt_man", "Effekt_mån"))
kpi0$M             <- get_col_num(kpi0, c("arManIndex", "ArManIndex", "årMånIndex", "ÅrMånIndex"))
kpi0$wM            <- get_col_num(kpi0, c("arManVikt", "ArManVikt", "årMånVikt", "ÅrMånVikt"))
kpi0$K             <- get_col_num(kpi0, c("Korttidsindex", "KorttidsIndex"))
kpi0$wK            <- get_col_num(kpi0, c("Korttidsvikt", "KorttidsVikt"))
kpi0$A             <- get_col_num(kpi0, c("ÅrÅrIndex", "ArArIndex", "arArIndex", "ararIndex", "År-år-index", "Ar-ar-index"))
kpi0$wA            <- get_col_num(kpi0, c("ÅrÅrVikt", "ArArVikt", "arArVikt", "ararVikt", "År-år-vikt", "Ar-ar-vikt"))
kpi0$L             <- get_col_num(kpi0, c("Långtidsindex", "Langtidsindex", "langtidsindex"))
kpi0$wL            <- get_col_num(kpi0, c("Långtidsvikt", "Langtidsvikt", "langtidsvikt"))

kpi0 <- kpi0 %>%
  mutate(
    time = zoo::as.yearmon(sprintf("%04d-%02d", ar, period)),
    time_date = as.Date(time),
    ym = ym_value(ar, period),
    ym_label = ym_label(ar, period)
  )

# Product-group links per month and year.
prod_m <- kpi0 %>%
  select(all_of(id_cols), ar, period, M, wM, K, wK)

prod_a <- kpi0 %>%
  group_by(across(all_of(id_cols)), ar) %>%
  summarise(
    A  = first_non_na(A),
    wA = first_non_na(wA),
    L  = first_non_na(L),
    wL = first_non_na(wL),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Hierarchy based on SCB's dotted KOD values
# ------------------------------------------------------------
total_kod <- "00"

kpi_hierarchy <- kpi0 %>%
  distinct(KOD, BESKR, EUKOD) %>%
  filter(!is.na(KOD), KOD != "") %>%
  mutate(
    parent_KOD = case_when(
      KOD == total_kod ~ NA_character_,
      str_detect(KOD, "\\.") ~ str_replace(KOD, "\\.\\d+$", ""),
      TRUE ~ total_kod
    ),
    level = case_when(
      KOD == total_kod ~ 0L,
      TRUE ~ str_count(KOD, "\\.") + 1L
    ),
    code_label = paste(EUKOD, BESKR),
    code_label_short = paste(KOD, BESKR)
  ) %>%
  arrange(level, KOD)

label_for_code <- function(kod) {
  out <- kpi_hierarchy$code_label[kpi_hierarchy$KOD == kod]
  if (length(out) == 0 || is.na(out[1])) kod else out[1]
}

component_codes_for_parent <- function(parent_kod) {
  parent_kod <- as.character(parent_kod)

  children <- kpi_hierarchy %>%
    filter(parent_KOD == parent_kod) %>%
    arrange(KOD) %>%
    pull(KOD)

  if (length(children) == 0) parent_kod else children
}

# Leaf-level descendants are used in the alternative contribution display.
# "Subklass" is interpreted as the most detailed available COICOP level
# below the selected aggregate. If the selected aggregate has no children,
# it is used as its own component.
descendant_codes_for_parent <- function(parent_kod) {
  parent_kod <- as.character(parent_kod)

  descendants <- kpi_hierarchy %>%
    filter(KOD != parent_kod) %>%
    filter(
      if (identical(parent_kod, total_kod)) {
        KOD != total_kod
      } else {
        startsWith(KOD, paste0(parent_kod, "."))
      }
    ) %>%
    arrange(level, KOD) %>%
    pull(KOD)

  if (length(descendants) == 0) parent_kod else descendants
}

subclass_codes_for_parent <- function(parent_kod) {
  descendants <- descendant_codes_for_parent(parent_kod)

  if (length(descendants) == 1 && identical(descendants[1], parent_kod)) {
    return(parent_kod)
  }

  parent_codes <- kpi_hierarchy %>%
    filter(!is.na(parent_KOD)) %>%
    pull(parent_KOD) %>%
    unique()

  subclasses <- descendants[!(descendants %in% parent_codes)]
  if (length(subclasses) == 0) descendants else subclasses
}

level_for_code <- function(kod) {
  out <- kpi_hierarchy$level[kpi_hierarchy$KOD == as.character(kod)]
  if (length(out) == 0 || is.na(out[1])) NA_integer_ else as.integer(out[1])
}

descendant_codes_at_relative_level <- function(parent_kod, relative_level) {
  parent_kod <- as.character(parent_kod)
  parent_level <- level_for_code(parent_kod)

  if (is.na(parent_level)) {
    return(character(0))
  }

  target_level <- parent_level + as.integer(relative_level)

  kpi_hierarchy %>%
    filter(KOD != parent_kod) %>%
    filter(level == target_level) %>%
    filter(
      if (identical(parent_kod, total_kod)) {
        KOD != total_kod
      } else {
        startsWith(KOD, paste0(parent_kod, "."))
      }
    ) %>%
    arrange(KOD) %>%
    pull(KOD)
}

is_important_component_mode <- function(component_mode) {
  component_mode %in% c("important_groups", "important_classes", "important_subclasses")
}

presentation_component_plural <- function(component_mode) {
  dplyr::case_when(
    component_mode == "important_groups" ~ "grupper",
    component_mode == "important_classes" ~ "klasser",
    component_mode == "important_subclasses" ~ "subklasser",
    component_mode == "special_aggregates" ~ "specialaggregat",
    TRUE ~ "komponenter"
  )
}

presentation_mode_label <- function(component_mode) {
  dplyr::case_when(
    component_mode == "important_groups" ~ "Viktigaste grupperna",
    component_mode == "important_classes" ~ "Viktigaste klasserna",
    component_mode == "important_subclasses" ~ "Viktigaste subklasserna",
    component_mode == "special_aggregates" ~ "Specialaggregat",
    TRUE ~ "Standard COICOP"
  )
}

presentation_choices_for_aggregate <- function(parent_kod) {
  parent_level <- level_for_code(parent_kod)

  choices <- c(
    "Standard COICOP" = "standard_coicop",
    "Specialaggregat" = "special_aggregates"
  )

  # Grupp- och klasslägena ska bara vara valbara på totalnivå
  # eller på COICOP-avdelningar, t.ex. 01, 02 och 03.
  if (!is.na(parent_level) && parent_level %in% c(0L, 1L)) {
    group_codes <- descendant_codes_at_relative_level(parent_kod, 1L)
    class_codes <- descendant_codes_at_relative_level(parent_kod, 2L)

    if (length(group_codes) > 0) {
      choices <- c(choices, "Viktigaste grupperna" = "important_groups")
    }

    if (length(class_codes) > 0) {
      choices <- c(choices, "Viktigaste klasserna" = "important_classes")
    }
  }

  choices <- c(choices, "Viktigaste subklasserna" = "important_subclasses")
  choices
}

component_codes_for_display <- function(parent_kod,
                                        component_mode = c(
                                          "standard_coicop",
                                          "special_aggregates",
                                          "important_groups",
                                          "important_classes",
                                          "important_subclasses"
                                        )) {
  component_mode <- match.arg(component_mode)

  if (component_mode == "standard_coicop") {
    return(component_codes_for_parent(parent_kod))
  }

  if (component_mode == "special_aggregates") {
    return(subclass_codes_for_parent(parent_kod))
  }

  if (component_mode == "important_groups") {
    return(descendant_codes_at_relative_level(parent_kod, 1L))
  }

  if (component_mode == "important_classes") {
    return(descendant_codes_at_relative_level(parent_kod, 2L))
  }

  subclass_codes_for_parent(parent_kod)
}

special_subclass_codes_for_group <- function(special_group) {
  special_group <- as.character(special_group)[1]
  subclasses <- subclass_codes_for_parent(total_kod)

  kpi_hierarchy %>%
    filter(KOD %in% subclasses) %>%
    mutate(special_group_calc = special_aggregate_group(KOD, BESKR)) %>%
    filter(special_group_calc == special_group) %>%
    arrange(KOD) %>%
    pull(KOD)
}

special_target_tables_for_codes <- function(component_codes) {
  target_m <- prod_m %>%
    filter(KOD %in% component_codes) %>%
    group_by(ar, period) %>%
    summarise(
      T_M = weighted_mean_or_na(M, wM),
      T_wM = sum_or_na(wM),
      T_K = weighted_mean_or_na(K, wK),
      T_wK = sum_or_na(wK),
      .groups = "drop"
    )

  target_a <- prod_a %>%
    filter(KOD %in% component_codes) %>%
    group_by(ar) %>%
    summarise(
      T_A = weighted_mean_or_na(A, wA),
      T_wA = sum_or_na(wA),
      T_L = weighted_mean_or_na(L, wL),
      T_wL = sum_or_na(wL),
      .groups = "drop"
    )

  list(target_m = target_m, target_a = target_a)
}

# ------------------------------------------------------------
# Rate-of-change table from the published index series
# ------------------------------------------------------------
rate_table <- kpi0 %>%
  select(KOD, ar, period, time, time_date, ym, ym_label, Index) %>%
  mutate(
    ar_prev_year = ar - 1L,
    ar_prev_month = if_else(period == 1L, ar - 1L, ar),
    period_prev_month = if_else(period == 1L, 12L, period - 1L)
  ) %>%
  left_join(
    kpi0 %>% select(KOD, ar, period, Index_prev_year = Index),
    by = c("KOD", "ar_prev_year" = "ar", "period" = "period")
  ) %>%
  left_join(
    kpi0 %>% select(KOD, ar, period, Index_prev_month = Index),
    by = c("KOD", "ar_prev_month" = "ar", "period_prev_month" = "period")
  ) %>%
  mutate(
    ann_rate = if_else(!is.na(Index) & !is.na(Index_prev_year) & Index_prev_year != 0,
                       Index / Index_prev_year * 100 - 100, NA_real_),
    m_rate = if_else(!is.na(Index) & !is.na(Index_prev_month) & Index_prev_month != 0,
                     Index / Index_prev_month * 100 - 100, NA_real_)
  ) %>%
  select(KOD, ar, period, time, time_date, ym, ym_label, ann_rate, m_rate)

# ------------------------------------------------------------
# Generic SCB-style effect calculation with selectable target
# ------------------------------------------------------------
# contribution_type:
# - "selected higher aggregate": components contribute to the selected aggregate.
# - "all-items KPI": components contribute to total Swedish KPI.
#
# For "selected higher aggregate", the same link-structure as in Testdata_prep.R
# is used, but target links and target-relative weights are used instead of
# all-items links and all-items weights.

calc_effects_for_target <- function(selected_kod,
                                    contribution_type = c("selected higher aggregate", "all-items KPI"),
                                    component_mode = c("standard_coicop", "special_aggregates", "important_groups", "important_classes", "important_subclasses"),
                                    component_codes_override = NULL,
                                    target_m_override = NULL,
                                    target_a_override = NULL,
                                    target_rates_override = NULL,
                                    target_kod_override = NULL,
                                    selected_kod_override = NULL,
                                    selected_label_override = NULL,
                                    target_label_override = NULL) {
  contribution_type <- match.arg(contribution_type)
  component_mode <- match.arg(component_mode)

  component_codes <- if (!is.null(component_codes_override)) {
    as.character(component_codes_override)
  } else {
    component_codes_for_display(selected_kod, component_mode)
  }

  if (length(component_codes) == 0) {
    return(tibble())
  }

  target_kod <- if (!is.null(target_kod_override)) {
    as.character(target_kod_override)[1]
  } else if (contribution_type == "selected higher aggregate") {
    selected_kod
  } else {
    total_kod
  }

  selected_kod_value <- if (!is.null(selected_kod_override)) {
    as.character(selected_kod_override)[1]
  } else {
    selected_kod
  }

  selected_label_value <- if (!is.null(selected_label_override)) {
    as.character(selected_label_override)[1]
  } else {
    label_for_code(selected_kod_value)
  }

  target_label_value <- if (!is.null(target_label_override)) {
    as.character(target_label_override)[1]
  } else {
    label_for_code(target_kod)
  }

  target_m <- if (!is.null(target_m_override)) {
    target_m_override %>% select(ar, period, T_M, T_wM, T_K, T_wK)
  } else {
    prod_m %>%
      filter(KOD == target_kod) %>%
      select(ar, period, T_M = M, T_wM = wM, T_K = K, T_wK = wK)
  }

  target_a <- if (!is.null(target_a_override)) {
    target_a_override %>% select(ar, T_A, T_wA, T_L, T_wL)
  } else {
    prod_a %>%
      filter(KOD == target_kod) %>%
      select(ar, T_A = A, T_wA = wA, T_L = L, T_wL = wL)
  }

  if (nrow(target_m) == 0 || nrow(target_a) == 0) {
    return(tibble())
  }

  # Constants for the 2005 method-change adjustment, now target-specific.
  sum_T_M_2004 <- sum_or_na(target_m$T_M[target_m$ar == 2004])
  sum_T_K_2004 <- sum_or_na(target_m$T_K[target_m$ar == 2004])

  T_A_2003 <- first_non_na(target_a$T_A[target_a$ar == 2003])

  T_M_2005_1  <- first_non_na(target_m$T_M[target_m$ar == 2005 & target_m$period == 1])
  T_M_2004_12 <- first_non_na(target_m$T_M[target_m$ar == 2004 & target_m$period == 12])
  T_K_2004_12 <- first_non_na(target_m$T_K[target_m$ar == 2004 & target_m$period == 12])

  target_2004_period <- target_m %>%
    filter(ar == 2004) %>%
    select(period, T_M_2004_m = T_M, T_K_2004_m = T_K)

  target_2005_period <- target_m %>%
    filter(ar == 2005) %>%
    select(period, T_M_2005_m = T_M)

  out <- kpi0 %>%
    filter(KOD %in% component_codes) %>%
    mutate(
      ar_prev = ar - 1L,
      ar_prev2 = ar - 2L,
      period_prev = period - 1L,
      period_dec = 12L
    ) %>%

    # Same product group, previous year, same month.
    left_join(
      prod_m %>% select(KOD, ar, period,
                        M_yminus1_m = M, wM_yminus1_m = wM,
                        K_yminus1_m = K, wK_yminus1_m = wK),
      by = c("KOD", "ar_prev" = "ar", "period" = "period")
    ) %>%

    # Same product group, previous year, December.
    left_join(
      prod_m %>% select(KOD, ar, period,
                        M_yminus1_dec = M, wM_yminus1_dec = wM,
                        K_yminus1_dec = K, wK_yminus1_dec = wK),
      by = c("KOD", "ar_prev" = "ar", "period_dec" = "period")
    ) %>%

    # Same product group, current year, previous month.
    left_join(
      prod_m %>% select(KOD, ar, period,
                        M_prev_month = M, K_prev_month = K),
      by = c("KOD", "ar" = "ar", "period_prev" = "period")
    ) %>%

    # Same product group, annual link y-2.
    left_join(
      prod_a %>% select(KOD, ar,
                        A_yminus2 = A, wA_yminus2 = wA),
      by = c("KOD", "ar_prev2" = "ar")
    ) %>%

    # Same product group, long-term link y-1.
    left_join(
      prod_a %>% select(KOD, ar,
                        L_yminus1 = L, wL_yminus1 = wL),
      by = c("KOD", "ar_prev" = "ar")
    ) %>%

    # Target links and target weights.
    left_join(
      target_m %>% rename(T_M_current = T_M, T_wM_current = T_wM,
                          T_K_current = T_K, T_wK_current = T_wK),
      by = c("ar", "period")
    ) %>%
    left_join(
      target_m %>% rename(T_M_yminus1_m = T_M, T_wM_yminus1_m = T_wM,
                          T_K_yminus1_m = T_K, T_wK_yminus1_m = T_wK),
      by = c("ar_prev" = "ar", "period" = "period")
    ) %>%
    left_join(
      target_m %>% rename(T_M_yminus1_dec = T_M, T_wM_yminus1_dec = T_wM,
                          T_K_yminus1_dec = T_K, T_wK_yminus1_dec = T_wK),
      by = c("ar_prev" = "ar", "period_dec" = "period")
    ) %>%
    left_join(
      target_m %>% rename(T_M_prev_month = T_M, T_wM_prev_month = T_wM,
                          T_K_prev_month = T_K, T_wK_prev_month = T_wK),
      by = c("ar" = "ar", "period_prev" = "period")
    ) %>%
    left_join(
      target_a %>% rename(T_A_yminus2 = T_A, T_wA_yminus2 = T_wA),
      by = c("ar_prev2" = "ar")
    ) %>%
    left_join(
      target_a %>% rename(T_L_yminus1 = T_L, T_wL_yminus1 = T_wL),
      by = c("ar_prev" = "ar")
    ) %>%

    # Target-specific values for the 2005 adjustment.
    left_join(target_2004_period, by = "period") %>%
    left_join(target_2005_period, by = "period") %>%

    mutate(
      # Rescale component weights to the chosen contribution target.
      wM_s = if_else(!is.na(wM) & !is.na(T_wM_current) & T_wM_current != 0,
                     wM / T_wM_current * 1000, NA_real_),
      wK_s = if_else(!is.na(wK) & !is.na(T_wK_current) & T_wK_current != 0,
                     wK / T_wK_current * 1000, NA_real_),

      wM_yminus1_m_s = if_else(!is.na(wM_yminus1_m) & !is.na(T_wM_yminus1_m) & T_wM_yminus1_m != 0,
                               wM_yminus1_m / T_wM_yminus1_m * 1000, NA_real_),
      wK_yminus1_m_s = if_else(!is.na(wK_yminus1_m) & !is.na(T_wK_yminus1_m) & T_wK_yminus1_m != 0,
                               wK_yminus1_m / T_wK_yminus1_m * 1000, NA_real_),

      wM_yminus1_dec_s = if_else(!is.na(wM_yminus1_dec) & !is.na(T_wM_yminus1_dec) & T_wM_yminus1_dec != 0,
                                 wM_yminus1_dec / T_wM_yminus1_dec * 1000, NA_real_),
      wK_yminus1_dec_s = if_else(!is.na(wK_yminus1_dec) & !is.na(T_wK_yminus1_dec) & T_wK_yminus1_dec != 0,
                                 wK_yminus1_dec / T_wK_yminus1_dec * 1000, NA_real_),

      wA_yminus2_s = if_else(!is.na(wA_yminus2) & !is.na(T_wA_yminus2) & T_wA_yminus2 != 0,
                             wA_yminus2 / T_wA_yminus2 * 1000, NA_real_),
      wL_yminus1_s = if_else(!is.na(wL_yminus1) & !is.na(T_wL_yminus1) & T_wL_yminus1 != 0,
                             wL_yminus1 / T_wL_yminus1 * 1000, NA_real_),

      # 2005 adjustment, target-specific version.
      R_ar_2005 = if_else(
        ar == 2005,
        wM_s / 1000 *
          (T_M_2005_m * T_A_2003 / sum_T_M_2004) *
          (sum_T_K_2004 / T_K_2004_m - sum_T_M_2004 / T_M_2004_m),
        0
      ),

      R_manad_2005 = if_else(
        ar == 2005 & period == 1,
        wM_s / 1000 *
          (T_M_2005_1 * T_A_2003 / sum_T_M_2004) *
          (sum_T_K_2004 / T_K_2004_12 - sum_T_M_2004 / T_M_2004_12),
        0
      ),

      Effekt_ar_target = case_when(
        ar <= 2004 ~
          T_L_yminus1 / T_K_yminus1_m *
          (wK_s * (K - 100) / 1000) -
          100 / T_K_yminus1_m *
          (wK_yminus1_m_s * (K_yminus1_m - 100) / 1000) +
          100 / T_K_yminus1_m *
          (wL_yminus1_s * (L_yminus1 - 100) / 1000),

        ar == 2005 ~
          T_A_yminus2 / T_M_yminus1_m *
          (wM_s * (M - 100) / 1000) -
          100 / T_M_yminus1_m *
          (wM_yminus1_m_s * (M_yminus1_m - 100) / 1000) +
          100 / T_M_yminus1_m *
          (wA_yminus2_s * (A_yminus2 - 100) / 1000) +
          R_ar_2005,

        ar >= 2006 ~
          T_A_yminus2 / T_M_yminus1_m *
          (wM_s * (M - 100) / 1000) -
          100 / T_M_yminus1_m *
          (wM_yminus1_m_s * (M_yminus1_m - 100) / 1000) +
          100 / T_M_yminus1_m *
          (wA_yminus2_s * (A_yminus2 - 100) / 1000),

        TRUE ~ NA_real_
      ),

      Effekt_manad_target = case_when(
        ar <= 2004 & period == 1 ~
          T_L_yminus1 / T_K_yminus1_dec *
          (wK_s * (K - 100) / 1000) -
          100 / T_K_yminus1_dec *
          (wK_yminus1_dec_s * (K_yminus1_dec - 100) / 1000) +
          100 / T_K_yminus1_dec *
          (wL_yminus1_s * (L_yminus1 - 100) / 1000),

        ar <= 2004 & period >= 2 ~
          100 / T_K_prev_month *
          (wK_s * (K - K_prev_month) / 1000),

        ar == 2005 & period == 1 ~
          T_A_yminus2 / T_M_yminus1_dec *
          (wM_s * (M - 100) / 1000) -
          100 / T_M_yminus1_dec *
          (wM_yminus1_dec_s * (M_yminus1_dec - 100) / 1000) +
          100 / T_M_yminus1_dec *
          (wA_yminus2_s * (A_yminus2 - 100) / 1000) +
          R_manad_2005,

        ar == 2005 & period >= 2 ~
          100 / T_M_prev_month *
          (wM_s * (M - M_prev_month) / 1000),

        ar >= 2006 & period == 1 ~
          T_A_yminus2 / T_M_yminus1_dec *
          (wM_s * (M - 100) / 1000) -
          100 / T_M_yminus1_dec *
          (wM_yminus1_dec_s * (M_yminus1_dec - 100) / 1000) +
          100 / T_M_yminus1_dec *
          (wA_yminus2_s * (A_yminus2 - 100) / 1000),

        ar >= 2006 & period >= 2 ~
          100 / T_M_prev_month *
          (wM_s * (M - M_prev_month) / 1000),

        TRUE ~ NA_real_
      )
    )

  target_rates <- if (!is.null(target_rates_override)) {
    target_rates_override %>% select(ar, period, target_ann_rate, target_m_rate)
  } else {
    rate_table %>%
      filter(KOD == target_kod) %>%
      select(ar, period, target_ann_rate = ann_rate, target_m_rate = m_rate)
  }

  out_final <- out %>%
    left_join(target_rates, by = c("ar", "period")) %>%
    left_join(
      kpi_hierarchy %>% select(KOD, component_label = code_label, component_label_short = code_label_short),
      by = "KOD"
    ) %>%
    mutate(
      target_kod = target_kod,
      target_label = target_label_value,
      selected_kod = selected_kod_value,
      selected_label = selected_label_value,
      contribution_type = contribution_type,
      component_label_wrapped = wrap_legend(component_label, width = 42)
    )

  if (component_mode == "special_aggregates") {
    out_final <- collapse_to_special_aggregates(out_final)
  }

  out_final
}

calc_effects_for_special_aggregate <- function(special_group) {
  special_group <- as.character(special_group)[1]
  component_codes <- special_subclass_codes_for_group(special_group)

  if (length(component_codes) == 0) {
    return(tibble())
  }

  target_tables <- special_target_tables_for_codes(component_codes)
  special_code <- special_aggregate_code_map$special_code[
    match(special_group, special_aggregate_code_map$special_group)
  ]
  if (length(special_code) == 0 || is.na(special_code)) {
    special_code <- "SP99"
  }

  out <- calc_effects_for_target(
    selected_kod = total_kod,
    contribution_type = "selected higher aggregate",
    component_mode = "important_subclasses",
    component_codes_override = component_codes,
    target_m_override = target_tables$target_m,
    target_a_override = target_tables$target_a,
    target_kod_override = special_code,
    selected_kod_override = special_code,
    selected_label_override = special_group,
    target_label_override = special_group
  )

  if (nrow(out) == 0) {
    return(out)
  }

  # Specialaggregaten saknar publicerad månads- och årsförändring.
  # Linjeserien sätts därför till summan av alla underliggande
  # subklassbidrag innan eventuell topp-X-kollaps till Övrigt.
  special_rates <- out %>%
    group_by(ar, period) %>%
    summarise(
      target_ann_rate = if (all(is.na(Effekt_ar_target))) NA_real_ else sum(Effekt_ar_target, na.rm = TRUE),
      target_m_rate = if (all(is.na(Effekt_manad_target))) NA_real_ else sum(Effekt_manad_target, na.rm = TRUE),
      .groups = "drop"
    )

  out %>%
    select(-target_ann_rate, -target_m_rate) %>%
    left_join(special_rates, by = c("ar", "period"))
}


# ------------------------------------------------------------
# Collapse contribution data to special aggregates
# ------------------------------------------------------------
collapse_to_special_aggregates <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  data %>%
    mutate(special_group = special_aggregate_group(KOD, BESKR)) %>%
    group_by(
      ar, period, time, time_date, ym, ym_label,
      target_ann_rate, target_m_rate, target_label,
      target_kod, selected_kod, selected_label, contribution_type,
      special_group
    ) %>%
    summarise(
      Effekt_ar_target = if (all(is.na(Effekt_ar_target))) NA_real_ else sum(Effekt_ar_target, na.rm = TRUE),
      Effekt_manad_target = if (all(is.na(Effekt_manad_target))) NA_real_ else sum(Effekt_manad_target, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(special_aggregate_code_map, by = "special_group") %>%
    mutate(
      KOD = coalesce(special_code, "SP99"),
      BESKR = special_group,
      EUKOD = NA_character_,
      component_label = special_group,
      component_label_short = special_group,
      component_label_wrapped = wrap_legend(special_group, width = 42)
    ) %>%
    select(
      KOD, BESKR, EUKOD, ar, period, time, time_date, ym, ym_label,
      Effekt_ar_target, Effekt_manad_target,
      target_ann_rate, target_m_rate, target_label,
      target_kod, selected_kod, selected_label, contribution_type,
      component_label, component_label_short, component_label_wrapped
    ) %>%
    arrange(time, KOD)
}

# ------------------------------------------------------------
# Collapse contribution data to top-X components plus Other
# ------------------------------------------------------------
collapse_to_top_contributors <- function(data,
                                         effect_col,
                                         top_n = 10,
                                         rank_months = 3,
                                         other_label = "Övrigt") {
  if (nrow(data) == 0 || !(effect_col %in% names(data))) {
    return(data)
  }

  top_n <- if (is.null(top_n) || length(top_n) == 0 || is.na(top_n)) 10L else max(1L, as.integer(top_n))
  rank_months <- if (is.null(rank_months) || length(rank_months) == 0 || is.na(rank_months)) 3L else max(1L, as.integer(rank_months))

  rank_periods <- data %>%
    distinct(time) %>%
    arrange(desc(time)) %>%
    slice_head(n = rank_months) %>%
    pull(time)

  top_codes <- data %>%
    filter(time %in% rank_periods) %>%
    group_by(KOD, component_label, component_label_short, component_label_wrapped) %>%
    summarise(
      rank_score = if (all(is.na(.data[[effect_col]]))) NA_real_ else mean(abs(.data[[effect_col]]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(rank_score)) %>%
    arrange(desc(rank_score), component_label_wrapped) %>%
    slice_head(n = top_n) %>%
    pull(KOD)

  if (length(top_codes) == 0) {
    return(data)
  }

  top_data <- data %>%
    filter(KOD %in% top_codes)

  other_source <- data %>%
    filter(!(KOD %in% top_codes))

  if (nrow(other_source) == 0) {
    return(top_data)
  }

  other_data <- other_source %>%
    group_by(
      ar, period, time, time_date, ym, ym_label,
      target_ann_rate, target_m_rate, target_label,
      target_kod, selected_kod, selected_label, contribution_type
    ) %>%
    summarise(
      effect_value = if (all(is.na(.data[[effect_col]]))) NA_real_ else sum(.data[[effect_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      KOD = "__OTHER__",
      BESKR = other_label,
      EUKOD = NA_character_,
      component_label = other_label,
      component_label_short = other_label,
      component_label_wrapped = other_label
    )

  other_data[[effect_col]] <- other_data$effect_value
  other_data$effect_value <- NULL

  bind_rows(top_data, other_data) %>%
    arrange(time, KOD)
}

# ------------------------------------------------------------
# Convenience datasets for plots
# ------------------------------------------------------------
index_series_for_code <- function(kod) {
  kpi0 %>%
    filter(KOD == kod) %>%
    arrange(time) %>%
    select(KOD, BESKR, EUKOD, ar, period, time, time_date, ym, ym_label, Index)
}

seasonality_series_for_code <- function(kod) {
  base <- index_series_for_code(kod) %>%
    filter(!is.na(Index))

  dec_prev <- base %>%
    filter(period == 12) %>%
    transmute(
      KOD,
      ar = ar + 1L,
      dec_yminus1 = Index
    )

  base %>%
    left_join(dec_prev, by = c("KOD", "ar")) %>%
    mutate(index_dec_yminus1 = Index / dec_yminus1 * 100)
}

weights_series_for_code <- function(kod) {
  component_codes <- component_codes_for_parent(kod)

  kpi0 %>%
    filter(KOD %in% component_codes) %>%
    group_by(KOD, BESKR, EUKOD, ar) %>%
    summarise(
      # The SCB data uses Korttidsvikt for the years before 2004.
      # From 2004 onward, the ordinary year-month weight is available.
      weight = if_else(
        first(ar) < 2004L,
        first_non_na(wK),
        first_non_na(wM)
      ),
      .groups = "drop"
    ) %>%
    left_join(
      kpi_hierarchy %>% select(KOD, component_label = code_label),
      by = "KOD"
    ) %>%
    mutate(component_label_wrapped = wrap_legend(component_label, width = 42))
}

# Optional check: when the target is total KPI, the calculation should match
# the published Effekt_ar/Effekt_manad columns apart from rounding.
kpi_effect_check <- calc_effects_for_target(total_kod, "all-items KPI") %>%
  mutate(
    diff_Effekt_ar = Effekt_ar_target - Effekt_ar,
    diff_Effekt_manad = Effekt_manad_target - Effekt_manad
  ) %>%
  summarise(
    max_abs_diff_ar = max(abs(diff_Effekt_ar), na.rm = TRUE),
    max_abs_diff_manad = max(abs(diff_Effekt_manad), na.rm = TRUE)
  )

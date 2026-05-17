
 library(shiny)
library(dplyr)
library(stringr)
library(zoo)
library(lubridate)
library(plotly)

function(input, output, session) {

  # ----------------------------------------------------------
  # Helpers inside server
  # ----------------------------------------------------------
  plot_title <- function(prefix, aggregate_label, extra = NULL) {
    txt <- paste(prefix, aggregate_label)
    if (!is.null(extra) && nzchar(extra)) txt <- paste0(txt, "<br><span style='font-size:12px;color:#526064;'>", extra, "</span>")
    txt
  }

  common_plot_layout <- function(p, title_text, y_title, height = 560) {
    p %>%
      layout(
        title = list(text = title_text, x = 0.02, y = 0.98, font = list(size = 14)),
        xaxis = list(title = ""),
        yaxis = list(title = y_title, zeroline = TRUE),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        font = list(size = 11),
        legend = list(
          traceorder = "normal",
          font = list(size = 11),
          x = 1.02,
          y = 1
        ),
        margin = list(r = 190, t = 70),
        height = height
      )
  }

  parse_ym <- function(x) {
    zoo::as.yearmon(x, format = "%Y-%m")
  }

  rate_line <- function() {
    list(
      color = "#111111",
      dash = "solid",
      width = 2.2
    )
  }

  rate_marker <- function() {
    list(
      color = "#111111",
      size = 5.8
    )
  }

  update_presentation_controls <- function(input_id, current_value) {
    req(input$aggregate)

    choices <- presentation_choices_for_aggregate(input$aggregate)
    selected <- if (!is.null(current_value) && current_value %in% choices) {
      current_value
    } else {
      "standard_coicop"
    }

    updateRadioButtons(
      session,
      input_id,
      choices = choices,
      selected = selected
    )
  }

  update_important_slider_label <- function(slider_id, presentation_value, force_subclasses = FALSE) {
    component_label <- if (isTRUE(force_subclasses)) {
      "subklasser"
    } else {
      presentation_component_plural(presentation_value)
    }

    updateSliderInput(
      session,
      slider_id,
      label = paste0("Antal viktiga ", component_label, " som ska visas separat")
    )
  }

  is_special_aggregate_mode <- function() {
    identical(input$aggregate_kind, "special_aggregate")
  }

  selected_dashboard_label <- function() {
    if (is_special_aggregate_mode()) {
      input$special_aggregate
    } else {
      label_for_code(input$aggregate)
    }
  }

  monthly_effect_data <- function() {
    if (is_special_aggregate_mode()) {
      req(input$special_aggregate)
      calc_effects_for_special_aggregate(input$special_aggregate)
    } else {
      req(input$aggregate, input$contribution_type_mr, input$presentation_mr)
      calc_effects_for_target(
        input$aggregate,
        input$contribution_type_mr,
        component_mode = input$presentation_mr
      )
    }
  }

  annual_effect_data <- function() {
    if (is_special_aggregate_mode()) {
      req(input$special_aggregate)
      calc_effects_for_special_aggregate(input$special_aggregate)
    } else {
      req(input$aggregate, input$contribution_type_ar, input$presentation_ar)
      calc_effects_for_target(
        input$aggregate,
        input$contribution_type_ar,
        component_mode = input$presentation_ar
      )
    }
  }

  # ----------------------------------------------------------
  # Dynamic controls
  # ----------------------------------------------------------
  observeEvent(list(input$aggregate_kind, input$aggregate), {
    if (!is_special_aggregate_mode()) {
      update_presentation_controls("presentation_mr", input$presentation_mr)
      update_presentation_controls("presentation_ar", input$presentation_ar)
    }
  }, ignoreInit = FALSE)

  observeEvent(list(input$aggregate_kind, input$presentation_mr), {
    update_important_slider_label(
      "important_n_mr",
      input$presentation_mr,
      force_subclasses = is_special_aggregate_mode()
    )
  }, ignoreInit = FALSE)

  observeEvent(list(input$aggregate_kind, input$presentation_ar), {
    update_important_slider_label(
      "important_n_ar",
      input$presentation_ar,
      force_subclasses = is_special_aggregate_mode()
    )
  }, ignoreInit = FALSE)

  observeEvent(list(input$aggregate_kind, input$aggregate, input$index_ref_type), {
    req(input$aggregate_kind == "coicop", input$aggregate, input$index_ref_type)

    data <- index_series_for_code(input$aggregate) %>%
      filter(!is.na(Index)) %>%
      arrange(time)

    if (nrow(data) == 0) return(NULL)

    if (input$index_ref_type == "year") {
      years <- sort(unique(data$ar))
      choices <- setNames(as.character(years), as.character(years))
      default <- if (2020 %in% years) "2020" else as.character(min(years))
    } else {
      ym <- data %>%
        distinct(ar, period, ym, ym_label) %>%
        arrange(ar, period)
      choices <- setNames(ym$ym, ym$ym_label)
      default <- if ("2020-01" %in% ym$ym) "2020-01" else ym$ym[1]
    }

    updateSelectInput(session, "index_ref", choices = choices, selected = default)
  }, ignoreInit = FALSE)

  update_month_choices <- function() {
    req(input$aggregate_kind)

    data <- monthly_effect_data() %>%
      filter(!is.na(Effekt_manad_target) | !is.na(target_m_rate)) %>%
      distinct(ar, period, ym, ym_label) %>%
      arrange(desc(ar), desc(period))

    if (nrow(data) == 0) return(NULL)

    latest <- data %>% slice(1)

    # Default selection: the latest available month and the same month
    # for the preceding 14 years, e.g. March 2026 plus March 2012-2025.
    selected <- data %>%
      filter(
        period == latest$period[1],
        ar >= latest$ar[1] - 14L,
        ar <= latest$ar[1]
      ) %>%
      arrange(desc(ar)) %>%
      pull(ym)

    if (length(selected) == 0) selected <- data$ym[1]

    updateSelectInput(
      session,
      "months_mr",
      choices = setNames(data$ym, data$ym_label),
      selected = selected
    )
  }

  observeEvent(list(input$aggregate_kind, input$aggregate, input$special_aggregate, input$contribution_type_mr, input$presentation_mr), {
    update_month_choices()
  }, ignoreInit = FALSE)

  observeEvent(list(input$aggregate_kind, input$aggregate, input$special_aggregate, input$contribution_type_ar, input$presentation_ar), {
    req(input$aggregate_kind)

    data <- annual_effect_data() %>%
      filter(!is.na(Effekt_ar_target) | !is.na(target_ann_rate))

    if (nrow(data) == 0) return(NULL)

    years <- sort(unique(data$ar))
    min_year <- min(years, na.rm = TRUE)
    max_year <- max(years, na.rm = TRUE)

    updateSliderInput(
      session,
      "range_ar",
      min = min_year,
      max = max_year,
      value = c(max(min_year, max_year - 5), max_year),
      step = 1
    )
  }, ignoreInit = FALSE)

  observeEvent(list(input$aggregate_kind, input$aggregate), {
    req(input$aggregate_kind == "coicop", input$aggregate)

    data <- seasonality_series_for_code(input$aggregate) %>%
      filter(!is.na(index_dec_yminus1)) %>%
      distinct(ar) %>%
      arrange(desc(ar))

    if (nrow(data) == 0) return(NULL)

    selected <- data$ar[1:min(5, nrow(data))]

    updateSelectInput(
      session,
      "years_seasonality",
      choices = setNames(as.character(data$ar), as.character(data$ar)),
      selected = as.character(selected)
    )
  }, ignoreInit = FALSE)

  observeEvent(list(input$aggregate_kind, input$aggregate), {
    req(input$aggregate_kind == "coicop", input$aggregate)

    data <- weights_series_for_code(input$aggregate) %>%
      filter(!is.na(weight))

    if (nrow(data) == 0) return(NULL)

    years <- sort(unique(data$ar))
    min_year <- min(years, na.rm = TRUE)
    max_year <- max(years, na.rm = TRUE)

    updateSliderInput(
      session,
      "range_weights",
      min = min_year,
      max = max_year,
      value = c(max(min_year, max_year - 5), max_year),
      step = 1
    )
  }, ignoreInit = FALSE)

  # ----------------------------------------------------------
  # Index plot
  # ----------------------------------------------------------
  output$plot_index <- renderPlotly({
    req(input$aggregate_kind == "coicop", input$aggregate, input$index_ref, input$index_ref_type)

    data <- index_series_for_code(input$aggregate) %>%
      filter(!is.na(Index)) %>%
      arrange(time)

    validate(need(nrow(data) > 0, "Ingen indexdata finns för valt aggregat."))

    if (input$index_ref_type == "year") {
      ref_year <- as.integer(input$index_ref)
      ref_value <- data %>%
        filter(ar == ref_year) %>%
        summarise(ref = mean(Index, na.rm = TRUE), .groups = "drop") %>%
        pull(ref)

      data <- data %>% filter(ar >= ref_year)
      ref_label <- as.character(ref_year)
    } else {
      ref_time <- parse_ym(input$index_ref)
      ref_value <- data %>%
        filter(time == ref_time) %>%
        summarise(ref = first_non_na(Index), .groups = "drop") %>%
        pull(ref)

      data <- data %>% filter(time >= ref_time)
      ref_label <- names(setNames(input$index_ref, input$index_ref))
      ref_label <- data$ym_label[data$ym == input$index_ref][1]
      if (is.na(ref_label)) ref_label <- input$index_ref
    }

    validate(need(!is.na(ref_value) && ref_value != 0, "Referensperioden saknar indexvärde."))

    data <- data %>% mutate(index_rebased = Index / ref_value * 100)

    p <- plot_ly(
      data,
      x = ~time_date,
      y = ~index_rebased,
      type = "scatter",
      mode = "lines",
      name = "KPI-index",
      line = list(width = 2),
      hovertemplate = paste(
        "%{x|%Y-%m}<br>",
        "Index = %{y:.2f}<extra></extra>"
      )
    )

    title_text <- plot_title(
      "Indexutveckling för",
      label_for_code(input$aggregate),
      paste0("Referensperiod: ", ref_label, " = 100")
    )

    common_plot_layout(p, title_text, paste0("Index, ", ref_label, " = 100"), height = 560)
  })

  # ----------------------------------------------------------
  # Monthly contributions plot
  # ----------------------------------------------------------
  output$plot_mr <- renderPlotly({
    req(input$aggregate_kind, input$months_mr)

    data <- monthly_effect_data() %>%
      filter(ym %in% input$months_mr)

    validate(need(nrow(data) > 0, "Ingen månadsdata finns för vald kombination."))

    if (is_special_aggregate_mode() || is_important_component_mode(input$presentation_mr)) {
      data <- collapse_to_top_contributors(
        data,
        effect_col = "Effekt_manad_target",
        top_n = input$important_n_mr,
        rank_months = input$important_rank_months_mr
      )
    }

    data <- data %>%
      arrange(time, KOD) %>%
      mutate(
        period_label = factor(ym_label, levels = unique(ym_label[order(time)])),
        Effekt_manad_target = if_else(is.na(Effekt_manad_target), 0, Effekt_manad_target)
      )

    bar_data <- data %>% filter(!is.na(component_label_wrapped))

    rate_data <- data %>%
      distinct(period_label, time, target_m_rate, target_label) %>%
      arrange(time)

    contribution_styles <- contribution_style_map(bar_data$component_label_wrapped)

    p <- plot_ly()

    for (i in seq_len(nrow(contribution_styles))) {
      style_row <- contribution_styles[i, ]
      component_data <- bar_data %>%
        filter(component_label_wrapped == style_row$component_label_wrapped)

      p <- p %>%
        add_bars(
          data = component_data,
          x = ~period_label,
          y = ~Effekt_manad_target,
          name = style_row$component_label_wrapped,
          legendgroup = style_row$component_label_wrapped,
          marker = contribution_marker(
            style_row$contribution_color,
            style_row$contribution_pattern
          ),
          hovertemplate = paste(
            "%{x}<br>",
            "%{fullData.name}<br>",
            "Bidrag = %{y:.3f} procentenheter<extra></extra>"
          )
        )
    }

    p <- p %>%
      add_trace(
        data = rate_data,
        x = ~period_label,
        y = ~target_m_rate,
        type = "scatter",
        mode = "markers+lines",
        inherit = FALSE,
        name = if (is_special_aggregate_mode()) {
          "Månadsförändring för valt specialaggregat"
        } else if (input$contribution_type_mr == "selected higher aggregate") {
          "Månadsförändring för valt aggregat"
        } else {
          "Månadsförändring för hela KPI"
        },
        marker = rate_marker(),
        line = rate_line(),
        hovertemplate = paste(
          "%{x}<br>",
          "Månadsförändring = %{y:.3f}%<extra></extra>"
        )
      ) %>%
      layout(barmode = "relative")

    y_title <- if (is_special_aggregate_mode()) {
      "Månadsförändring, % / bidrag, procentenheter"
    } else if (input$contribution_type_mr == "selected higher aggregate") {
      "Månadsförändring, % / bidrag, procentenheter"
    } else {
      "Bidrag till hela KPI, procentenheter"
    }

    extra <- if (is_special_aggregate_mode()) {
      paste0(
        "Visar topp ", input$important_n_mr, " subklasser baserat på de ",
        input$important_rank_months_mr,
        " senaste valda månaderna; övriga summeras till Övrigt. ",
        "Månadsförändringen sätts till summan av de underliggande subklassbidragen."
      )
    } else if (input$contribution_type_mr == "selected higher aggregate") {
      "Bidragen räknas mot valt aggregat."
    } else {
      "Bidragen räknas mot hela KPI."
    }

    if (!is_special_aggregate_mode() && input$presentation_mr == "special_aggregates") {
      extra <- paste0(
        extra,
        " Subklasser summeras till specialaggregat utifrån suffix och särskilda undantag."
      )
    }

    if (!is_special_aggregate_mode() && is_important_component_mode(input$presentation_mr)) {
      component_label <- presentation_component_plural(input$presentation_mr)

      extra <- paste0(
        extra,
        " Visar topp ", input$important_n_mr, " ", component_label,
        " baserat på de ", input$important_rank_months_mr,
        " senaste valda månaderna; övriga summeras till Övrigt."
      )
    }

    title_text <- plot_title(
      "Bidrag till M/M-1 för",
      selected_dashboard_label(),
      extra
    )

    common_plot_layout(p, title_text, y_title, height = 560)
  })

  # ----------------------------------------------------------
  # Annual contributions plot
  # ----------------------------------------------------------
  output$plot_ar <- renderPlotly({
    req(input$aggregate_kind, input$range_ar)

    data <- annual_effect_data() %>%
      filter(ar >= input$range_ar[1], ar <= input$range_ar[2])

    validate(need(nrow(data) > 0, "Ingen årsdata finns för vald kombination."))

    if (is_special_aggregate_mode() || is_important_component_mode(input$presentation_ar)) {
      data <- collapse_to_top_contributors(
        data,
        effect_col = "Effekt_ar_target",
        top_n = input$important_n_ar,
        rank_months = input$important_rank_months_ar
      )
    }

    data <- data %>%
      arrange(time, KOD) %>%
      mutate(Effekt_ar_target = if_else(is.na(Effekt_ar_target), 0, Effekt_ar_target))

    bar_data <- data %>% filter(!is.na(component_label_wrapped))

    rate_data <- data %>%
      distinct(time_date, target_ann_rate, target_label) %>%
      arrange(time_date)

    contribution_styles <- contribution_style_map(bar_data$component_label_wrapped)

    p <- plot_ly()

    for (i in seq_len(nrow(contribution_styles))) {
      style_row <- contribution_styles[i, ]
      component_data <- bar_data %>%
        filter(component_label_wrapped == style_row$component_label_wrapped)

      p <- p %>%
        add_bars(
          data = component_data,
          x = ~time_date,
          y = ~Effekt_ar_target,
          name = style_row$component_label_wrapped,
          legendgroup = style_row$component_label_wrapped,
          marker = contribution_marker(
            style_row$contribution_color,
            style_row$contribution_pattern
          ),
          hovertemplate = paste(
            "%{x|%Y-%m}<br>",
            "%{fullData.name}<br>",
            "Bidrag = %{y:.3f} procentenheter<extra></extra>"
          )
        )
    }

    p <- p %>%
      add_trace(
        data = rate_data,
        x = ~time_date,
        y = ~target_ann_rate,
        type = "scatter",
        mode = "lines",
        inherit = FALSE,
        name = if (is_special_aggregate_mode()) {
          "Årsförändring för valt specialaggregat"
        } else if (input$contribution_type_ar == "selected higher aggregate") {
          "Årsförändring för valt aggregat"
        } else {
          "Årsförändring för hela KPI"
        },
        line = rate_line(),
        hovertemplate = paste(
          "%{x|%Y-%m}<br>",
          "Årsförändring = %{y:.3f}%<extra></extra>"
        )
      ) %>%
      layout(barmode = "relative")

    y_title <- if (is_special_aggregate_mode()) {
      "Årsförändring, % / bidrag, procentenheter"
    } else if (input$contribution_type_ar == "selected higher aggregate") {
      "Årsförändring, % / bidrag, procentenheter"
    } else {
      "Bidrag till hela KPI, procentenheter"
    }

    extra <- if (is_special_aggregate_mode()) {
      paste0(
        "Visar topp ", input$important_n_ar, " subklasser baserat på de ",
        input$important_rank_months_ar,
        " senaste månaderna inom valt intervall; övriga summeras till Övrigt. ",
        "Årsförändringen sätts till summan av de underliggande subklassbidragen."
      )
    } else if (input$contribution_type_ar == "selected higher aggregate") {
      "Bidragen räknas mot valt aggregat."
    } else {
      "Bidragen räknas mot hela KPI."
    }

    if (!is_special_aggregate_mode() && input$presentation_ar == "special_aggregates") {
      extra <- paste0(
        extra,
        " Subklasser summeras till specialaggregat utifrån suffix och särskilda undantag."
      )
    }

    if (!is_special_aggregate_mode() && is_important_component_mode(input$presentation_ar)) {
      component_label <- presentation_component_plural(input$presentation_ar)

      extra <- paste0(
        extra,
        " Visar topp ", input$important_n_ar, " ", component_label,
        " baserat på de ", input$important_rank_months_ar,
        " senaste månaderna inom valt intervall; övriga summeras till Övrigt."
      )
    }

    title_text <- plot_title(
      "Bidrag till M/M-12 för",
      selected_dashboard_label(),
      extra
    )

    common_plot_layout(p, title_text, y_title, height = 620)
  })

  # ----------------------------------------------------------
  # Seasonality plot
  # ----------------------------------------------------------
  output$plot_seasonality <- renderPlotly({
    req(input$aggregate_kind == "coicop", input$aggregate, input$years_seasonality)

    base <- seasonality_series_for_code(input$aggregate) %>%
      filter(ar %in% as.integer(input$years_seasonality)) %>%
      filter(!is.na(index_dec_yminus1))

    # Add December Y-1 as month 0 for each selected year.
    dec_rows <- base %>%
      filter(period == 1) %>%
      mutate(
        period_plot = 0L,
        month_label_plot = "Dec Y-1",
        index_dec_yminus1 = 100,
        time_date = as.Date(zoo::as.yearmon(sprintf("%04d-12", ar - 1L)))
      )

    base <- base %>%
      mutate(
        period_plot = period,
        month_label_plot = month_names_sv[period]
      )

    data <- bind_rows(dec_rows, base) %>%
      arrange(ar, period_plot) %>%
      mutate(
        month_label_plot = factor(
          month_label_plot,
          levels = c("Dec Y-1", month_names_sv)
        ),
        ar_factor = as.factor(ar)
      )

    validate(need(nrow(data) > 0, "Ingen säsongsdata finns för valt aggregat och valda år."))

    p <- plot_ly(
      data,
      x = ~month_label_plot,
      y = ~index_dec_yminus1,
      color = ~ar_factor,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = paste(
        "År %{fullData.name}<br>",
        "%{x}<br>",
        "Index = %{y:.2f}<extra></extra>"
      )
    )

    title_text <- plot_title(
      "Säsongsmönster för",
      label_for_code(input$aggregate),
      "December föregående år = 100"
    )

    common_plot_layout(p, title_text, "Index, Dec Y-1 = 100", height = 560) %>%
      layout(xaxis = list(title = "", categoryorder = "array", categoryarray = c("Dec Y-1", month_names_sv)))
  })

  # ----------------------------------------------------------
  # Weights plot
  # ----------------------------------------------------------
  output$plot_weights <- renderPlotly({
    req(input$aggregate_kind == "coicop", input$aggregate, input$range_weights)

    data <- weights_series_for_code(input$aggregate) %>%
      filter(ar >= input$range_weights[1], ar <= input$range_weights[2]) %>%
      filter(!is.na(weight)) %>%
      mutate(ar_factor = factor(ar, levels = sort(unique(ar))))

    validate(need(nrow(data) > 0, "Ingen viktdata finns för valt aggregat och vald tidsperiod."))

    p <- plot_ly(
      data,
      x = ~ar_factor,
      y = ~weight,
      color = ~component_label_wrapped,
      type = "bar",
      legendgroup = ~component_label_wrapped,
      hovertemplate = paste(
        "%{x}<br>",
        "%{fullData.name}<br>",
        "Vikt = %{y:.2f}<extra></extra>"
      )
    ) %>%
      layout(barmode = "stack")

    title_text <- plot_title(
      "Vikter för direkta undergrupper till",
      label_for_code(input$aggregate),
      "Vikt i per mille av hela KPI, om inte valt aggregat själv visas."
    )

    common_plot_layout(p, title_text, "Vikt, per mille", height = 560)
  })
}


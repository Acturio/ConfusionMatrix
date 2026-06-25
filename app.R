library(shiny)
library(tidyverse)
library(yardstick)
library(plotly)

theme_colors <- list(
  navy = "#072146",
  blue = "#004481",
  bright_blue = "#1973B8",
  sky = "#49A5E6",
  aqua = "#2DCCCD",
  light = "#F4F7F8"
)

metric_value <- function(metric, truth, estimate = NULL, ...) {
  value <- if (is.null(estimate)) {
    metric(truth = truth, ...)
  } else {
    metric(truth = truth, estimate = estimate, ...)
  }

  as.numeric(value)
}

format_metric <- function(x) {
  if_else(is.na(x), "-", scales::number(x, accuracy = 0.001))
}

binary_metric_table <- function(data) {
  tibble(
    Metric = c(
      "Accuracy", "Sensitivity", "Specificity", "Precision",
      "Recall", "F1 Score", "ROC AUC", "PR AUC"
    ),
    Value = c(
      metric_value(accuracy_vec, data$truth, data$estimate),
      metric_value(sens_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(spec_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(ppv_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(recall_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(f_meas_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(roc_auc_vec, data$truth, data$score, event_level = "first"),
      metric_value(pr_auc_vec, data$truth, data$score, event_level = "first")
    )
  ) %>%
    mutate(Value = format_metric(Value))
}

one_vs_rest_data <- function(data, class_name, score) {
  data %>%
    transmute(
      truth = factor(if_else(truth == class_name, class_name, "Rest"),
        levels = c(class_name, "Rest")
      ),
      estimate = factor(if_else(estimate == class_name, class_name, "Rest"),
        levels = c(class_name, "Rest")
      ),
      score = score
    )
}

multiclass_metric_values <- function(data, classes) {
  map_dfr(classes, function(class_name) {
    class_data <- one_vs_rest_data(data, class_name, data[[class_name]])

    tibble(
      Class = class_name,
      Sensitivity = metric_value(sens_vec, class_data$truth, class_data$estimate,
        event_level = "first"
      ),
      Specificity = metric_value(spec_vec, class_data$truth, class_data$estimate,
        event_level = "first"
      ),
      Precision = metric_value(ppv_vec, class_data$truth, class_data$estimate,
        event_level = "first"
      ),
      `F1 Score` = metric_value(f_meas_vec, class_data$truth, class_data$estimate,
        event_level = "first"
      ),
      `ROC AUC` = metric_value(roc_auc_vec, class_data$truth, class_data$score,
        event_level = "first"
      ),
      `PR AUC` = metric_value(pr_auc_vec, class_data$truth, class_data$score,
        event_level = "first"
      )
    )
  })
}

multiclass_metric_table <- function(data, classes) {
  multiclass_metric_values(data, classes) %>%
    mutate(across(-Class, format_metric))
}

curve_data <- function(data, classes, curve_function) {
  map_dfr(classes, function(class_name) {
    class_data <- one_vs_rest_data(data, class_name, data[[class_name]])
    curve_function(class_data, truth, score, event_level = "first") %>%
      mutate(Class = class_name)
  })
}

curve_colors <- function(classes) {
  set_names(
    rep(c(theme_colors$bright_blue, theme_colors$aqua, "#F7893B", "#7C53A5", "#5BBF7A"),
      length.out = length(classes)
    ),
    classes
  )
}

binary_current_tooltip <- function(data, threshold) {
  metrics <- binary_metric_table(data) %>%
    deframe()

  paste0(
    "<b>Selected Threshold: ", scales::number(threshold, accuracy = 0.001), "</b>",
    "<br>Accuracy: ", metrics[["Accuracy"]],
    "<br>Sensitivity: ", metrics[["Sensitivity"]],
    "<br>Specificity: ", metrics[["Specificity"]],
    "<br>Precision: ", metrics[["Precision"]],
    "<br>Recall: ", metrics[["Recall"]],
    "<br>F1 Score: ", metrics[["F1 Score"]],
    "<br>ROC AUC: ", metrics[["ROC AUC"]],
    "<br>PR AUC: ", metrics[["PR AUC"]]
  )
}

binomial_confidence_interval <- function(positive_count, total_count, confidence_level) {
  if (total_count == 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }

  alpha <- 1 - confidence_level
  alpha_half <- alpha / 2
  lower <- if (positive_count == 0) {
    0
  } else {
    stats::qbeta(alpha_half, positive_count, total_count - positive_count + 1)
  }
  upper <- if (positive_count == total_count) {
    1
  } else {
    stats::qbeta(1 - alpha_half, positive_count + 1, total_count - positive_count)
  }

  c(lower = lower, upper = upper)
}

probability_bin_table <- function(data, confidence_level_percent = 95) {
  confidence_level <- pmin(pmax(confidence_level_percent / 100, 0.80), 0.95)
  alpha <- 1 - confidence_level
  alpha_half <- alpha / 2
  breaks <- seq(0, 1, by = 0.1)
  labels <- paste0(
    "[", scales::percent(head(breaks, -1), accuracy = 1),
    ", ", scales::percent(tail(breaks, -1), accuracy = 1),
    if_else(tail(breaks, -1) == 1, "]", ")")
  )

  data %>%
    mutate(
      score_bin = cut(pmin(score, 1 - 1e-12),
        breaks = breaks, labels = labels,
        include.lowest = TRUE, right = FALSE
      ),
      positive = truth == levels(truth)[[1]]
    ) %>%
    count(score_bin, wt = positive, name = "Positive Count") %>%
    right_join(
      data %>%
        mutate(score_bin = cut(pmin(score, 1 - 1e-12),
          breaks = breaks, labels = labels,
          include.lowest = TRUE, right = FALSE
        )) %>%
        count(score_bin, name = "Total Count"),
      by = "score_bin"
    ) %>%
    complete(
      score_bin = factor(labels, levels = labels),
      fill = list(`Positive Count` = 0, `Total Count` = 0)
    ) %>%
    mutate(
      `Positive Count` = replace_na(`Positive Count`, 0),
      `Total Count` = replace_na(`Total Count`, 0)
    ) %>%
    mutate(
      `Positive Rate` = if_else(`Total Count` > 0, `Positive Count` / `Total Count`, NA_real_),
      ci = map2(
        `Positive Count`, `Total Count`,
        ~ binomial_confidence_interval(.x, .y, confidence_level)
      ),
      `CI Lower` = map_dbl(ci, 1),
      `CI Upper` = map_dbl(ci, 2),
      Tooltip = paste0(
        "<b>Probability Bin: ", score_bin, "</b>",
        "<br>Total Count: ", `Total Count`,
        "<br>Positive Count: ", `Positive Count`,
        "<br>Positive Rate: ", scales::percent(`Positive Rate`, accuracy = 0.1),
        "<br>", scales::percent(confidence_level, accuracy = 1), " CI: [",
        scales::percent(`CI Lower`, accuracy = 0.1),
        ", ", scales::percent(`CI Upper`, accuracy = 0.1), "]",
        "<br>Alpha: ", scales::number(alpha, accuracy = 0.001),
        "<br>Alpha / 2: ", scales::number(alpha_half, accuracy = 0.001)
      )
    ) %>%
    select(-ci)
}

confusion_plot <- function(data) {
  counts <- data %>%
    count(truth, estimate, name = "Count") %>%
    complete(truth, estimate, fill = list(Count = 0)) %>%
    mutate(
      Tooltip = paste0(
        "<b>Confusion Matrix</b>",
        "<br>Actual Class: ", truth,
        "<br>Predicted Class: ", estimate,
        "<br>Count: ", Count
      )
    )

  ggplot(counts, aes(x = estimate, y = truth, fill = Count, text = Tooltip)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = Count), color = theme_colors$navy, fontface = "bold", size = 5) +
    scale_fill_gradient(low = "#EAF4FB", high = theme_colors$sky) +
    labs(title = "Confusion Matrix", x = "Predicted Class", y = "Actual Class", fill = "Count") +
    coord_equal() +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), legend.position = "bottom", plot.title = element_text(face = "bold"))
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$title("Model Performance Studio")
  ),
  div(
    class = "hero",
    div(
      class = "hero__content",
      tags$p(class = "eyebrow", "MODEL PERFORMANCE STUDIO"),
      tags$h1("Predictive Model Evaluation"),
      tags$p(
        class = "hero__subtitle",
        "Confusion Matrix, ROC Curve, and Precision-Recall Curve for binary and multiclass models."
      )
    ),
    div(
      class = "hero__mark",
      div(class = "hero__bar hero__bar--one"),
      div(class = "hero__bar hero__bar--two"),
      div(class = "hero__bar hero__bar--three")
    )
  ),
  fluidRow(
    column(
      width = 3,
      div(
        class = "panel configuration",
        tags$h2("Analysis Settings"),
        fileInput(
          "file", "Results file (.csv)",
          accept = c(".csv", "text/csv")
        ),
        radioButtons(
          "model_type",
          "Response type",
          choices = c("Binary" = "binary", "Multiclass" = "multiclass"),
          selected = "binary",
          inline = TRUE
        ),
        uiOutput("column_selectors"),
        conditionalPanel(
          condition = "input.model_type == 'binary'",
          sliderInput(
            "threshold", "Threshold probability",
            min = 0, max = 1, value = 0.5, step = 0.01
          ),
          numericInput(
            "confidence_level", "Confidence level (%)",
            value = 95, min = 80, max = 95, step = 5
          )
        ),
        actionButton("calculate", "Calculate results", class = "btn-calculate"),
        uiOutput("data_notice")
      )
    ),
    column(
      width = 9,
      uiOutput("summary_cards"),
      div(
        class = "panel results",
        tags$h2(class = "results__title", "Model Results"),
        fluidRow(
          column(6, div(class = "chart-card", plotlyOutput("roc_plot", height = "330px"))),
          column(6, div(class = "chart-card", plotlyOutput("pr_plot", height = "330px")))
        ),
        uiOutput("binary_extra_plots"),
        fluidRow(
          column(5, div(class = "chart-card", plotlyOutput("matrix_plot", height = "365px"))),
          column(
            7,
            div(
              class = "metrics-card",
              tags$h3("Performance Metrics"),
              div(class = "metrics-table", tableOutput("metrics_table"))
            )
          )
        ),
        tags$details(
          class = "data-details",
          tags$summary("Uploaded Data Preview"),
          div(class = "preview-table", tableOutput("preview_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$file)

    tryCatch(
      readr::read_csv(
        input$file$datapath,
        show_col_types = FALSE,
        name_repair = "unique"
      ),
      error = function(error) {
        validate(need(FALSE, paste("The file could not be read:", error$message)))
      }
    )
  })

  output$column_selectors <- renderUI({
    req(input$file)
    data <- uploaded_data()
    columns <- names(data)
    numeric_columns <- data %>%
      select(where(is.numeric)) %>%
      names()
    suggested_truth <- columns[str_detect(str_to_lower(columns), "truth|actual|real|response|target|clase")]
    truth_selected <- if (!is.null(input$truth_col) && input$truth_col %in% columns) {
      input$truth_col
    } else {
      dplyr::first(suggested_truth, default = dplyr::first(columns, default = ""))
    }

    if (input$model_type == "binary") {
      score_columns <- setdiff(numeric_columns, truth_selected)
      suggested_score <- score_columns[str_detect(str_to_lower(score_columns), "prob|score|pred")]
      score_selected <- if (!is.null(input$score_col) && input$score_col %in% score_columns) {
        input$score_col
      } else {
        dplyr::first(suggested_score, default = dplyr::first(score_columns, default = ""))
      }

      tagList(
        selectInput("truth_col", "Actual class column", choices = columns, selected = truth_selected),
        selectInput(
          "score_col", "Probability column",
          choices = score_columns,
          selected = score_selected
        ),
        uiOutput("positive_selector")
      )
    } else {
      detected_classes <- data %>%
        pull(all_of(truth_selected)) %>%
        discard(is.na) %>%
        as.character() %>%
        unique()
      default_count <- min(max(length(detected_classes), 3), 20)
      class_count <- if (!is.null(input$class_count)) input$class_count else default_count

      tagList(
        selectInput("truth_col", "Actual class column", choices = columns, selected = truth_selected),
        numericInput(
          "class_count", "Number of classes",
          value = class_count, min = 3, max = 20, step = 1
        ),
        uiOutput("multiclass_score_selectors"),
        tags$p(
          class = "form-hint",
          "Select one probability column for each class. Predicted Class is determined by the highest probability."
        )
      )
    }
  })

  output$positive_selector <- renderUI({
    req(input$model_type == "binary", input$truth_col)
    classes <- uploaded_data() %>%
      pull(all_of(input$truth_col)) %>%
      discard(is.na) %>%
      as.character() %>%
      unique()

    selectInput("positive_class", "Positive class", choices = classes)
  })

  output$multiclass_score_selectors <- renderUI({
    req(input$model_type == "multiclass", input$truth_col, input$class_count)
    data <- uploaded_data()
    classes <- data %>%
      pull(all_of(input$truth_col)) %>%
      discard(is.na) %>%
      as.character() %>%
      unique()
    numeric_columns <- data %>%
      select(where(is.numeric)) %>%
      names() %>%
      setdiff(input$truth_col)
    suggested_probabilities <- numeric_columns[
      str_detect(str_to_lower(numeric_columns), "prob|score|pred")
    ]
    ordered_columns <- c(suggested_probabilities, setdiff(numeric_columns, suggested_probabilities))
    class_count <- as.integer(input$class_count)

    tagList(
      tags$label(class = "control-label mapping-label", "Probability by class"),
      tags$p(
        class = "form-hint",
        paste("Detected Class order:", paste(classes, collapse = ", "))
      ),
      map(seq_len(class_count), function(index) {
        prior_selection <- input[[paste0("class_score_", index)]]
        selected_column <- if (!is.null(prior_selection) && prior_selection %in% numeric_columns) {
          prior_selection
        } else if (length(ordered_columns) >= index) {
          ordered_columns[[index]]
        } else {
          ""
        }

        selectInput(
          paste0("class_score_", index),
          paste("Class", index),
          choices = ordered_columns,
          selected = selected_column
        )
      })
    )
  })

  analysis_config <- eventReactive(input$calculate, {
    data <- uploaded_data()
    validate(need(input$truth_col %in% names(data), "Select the Actual Class column."))
    truth_raw <- data[[input$truth_col]]
    classes <- truth_raw %>%
      discard(is.na) %>%
      as.character() %>%
      unique()

    if (input$model_type == "binary") {
      validate(need(length(classes) == 2, "Binary response must contain exactly two classes."))
      validate(need(length(input$score_col) == 1, "A numeric Probability column is required."))
      validate(need(input$score_col %in% names(data), "Select a Probability column."))
      validate(need(input$positive_class %in% classes, "Select the Positive Class."))

      negative_class <- setdiff(classes, input$positive_class)
      prepared <- data %>%
        transmute(
          truth = factor(as.character(.data[[input$truth_col]]),
            levels = c(input$positive_class, negative_class)
          ),
          score = as.numeric(.data[[input$score_col]])
        ) %>%
        drop_na()

      validate(need(nrow(prepared) > 0, "No complete rows are available for analysis."))
      validate(need(all(between(prepared$score, 0, 1)), "Probability must be between 0 and 1."))

      list(
        type = "binary",
        data = prepared,
        classes = c(input$positive_class, negative_class),
        omitted = nrow(data) - nrow(prepared)
      )
    } else {
      validate(need(length(classes) >= 3, "Multiclass response must contain at least three classes."))
      validate(need(input$class_count >= 3 && input$class_count <= 20, "Number of classes must be between 3 and 20."))
      validate(need(length(classes) == input$class_count, "Number of classes must match the distinct values in Actual Class."))
      score_columns <- map_chr(seq_len(input$class_count), function(index) {
        selected <- input[[paste0("class_score_", index)]]
        if (is.null(selected)) "" else selected
      })
      validate(need(all(score_columns %in% names(data)), "Select Probability columns for every Class."))
      validate(need(n_distinct(score_columns) == length(classes), "Each Class must use a different Probability column."))

      score_data <- data %>%
        select(all_of(score_columns)) %>%
        set_names(classes)
      prepared <- bind_cols(
        tibble(truth = factor(as.character(truth_raw), levels = classes)),
        score_data
      ) %>%
        drop_na()

      validate(need(nrow(prepared) > 0, "No complete rows are available for analysis."))
      validate(need(all(map_lgl(prepared[classes], is.numeric)), "Probabilities must be numeric."))
      validate(need(all(map_lgl(prepared[classes], ~ all(between(.x, 0, 1)))), "All probabilities must be between 0 and 1."))

      score_matrix <- as.matrix(prepared[, classes])
      prepared <- prepared %>%
        mutate(estimate = factor(classes[max.col(score_matrix, ties.method = "first")],
          levels = classes
        ))

      list(
        type = "multiclass",
        data = prepared,
        classes = classes,
        omitted = nrow(data) - nrow(prepared)
      )
    }
  })

  analysis_data <- reactive({
    config <- analysis_config()

    if (config$type == "binary") {
      config$data %>%
        mutate(
          estimate = factor(
            if_else(score >= input$threshold, config$classes[[1]], config$classes[[2]]),
            levels = config$classes
          )
        )
    } else {
      config$data
    }
  })

  output$data_notice <- renderUI({
    req(input$calculate > 0)
    config <- analysis_config()
    if (config$omitted == 0) {
      tags$p(class = "notice success", "Data ready for analysis.")
    } else {
      tags$p(class = "notice", paste(config$omitted, "incomplete rows were excluded."))
    }
  })

  output$summary_cards <- renderUI({
    config <- analysis_config()
    data <- analysis_data()
    accuracy <- metric_value(accuracy_vec, data$truth, data$estimate)

    auc_values <- if (config$type == "binary") {
      c(
        metric_value(roc_auc_vec, data$truth, data$score, event_level = "first"),
        metric_value(pr_auc_vec, data$truth, data$score, event_level = "first")
      )
    } else {
      metrics <- multiclass_metric_values(data, config$classes)
      c(mean(metrics$`ROC AUC`, na.rm = TRUE), mean(metrics$`PR AUC`, na.rm = TRUE))
    }
    precision_recall_f1 <- if (config$type == "binary") {
      c(
        metric_value(ppv_vec, data$truth, data$estimate, event_level = "first"),
        metric_value(recall_vec, data$truth, data$estimate, event_level = "first"),
        metric_value(f_meas_vec, data$truth, data$estimate, event_level = "first")
      )
    } else {
      metrics <- multiclass_metric_values(data, config$classes)
      c(
        mean(metrics$Precision, na.rm = TRUE),
        mean(metrics$Sensitivity, na.rm = TRUE),
        mean(metrics$`F1 Score`, na.rm = TRUE)
      )
    }

    cards <- tibble(
      label = c("Observations", "Accuracy", "Precision", "Recall", "F1 Score", "ROC AUC", "PR AUC"),
      value = c(
        scales::comma(nrow(data)),
        format_metric(accuracy),
        format_metric(precision_recall_f1[[1]]),
        format_metric(precision_recall_f1[[2]]),
        format_metric(precision_recall_f1[[3]]),
        format_metric(auc_values[[1]]),
        format_metric(auc_values[[2]])
      )
    )

    div(
      class = "cards",
      map2(cards$label, cards$value, ~ div(
        class = "kpi-card",
        tags$p(.x),
        tags$strong(.y)
      ))
    )
  })

  output$binary_extra_plots <- renderUI({
    config <- analysis_config()
    if (config$type != "binary") {
      return(NULL)
    }

    fluidRow(
      column(
        6,
        div(
          class = "chart-card chart-card--titled",
          tags$h3(class = "chart-title", "Probability Distribution"),
          plotlyOutput("probability_distribution_plot", height = "310px")
        )
      ),
      column(
        6,
        div(
          class = "chart-card chart-card--titled",
          tags$h3(class = "chart-title", "Positive Rate by Probability Bin"),
          plotlyOutput("positive_rate_bin_plot", height = "310px")
        )
      )
    )
  })

  output$roc_plot <- renderPlotly({
    config <- analysis_config()
    data <- analysis_data()
    classes <- if (config$type == "binary") config$classes[[1]] else config$classes
    colors <- curve_colors(classes)
    plot_data <- (if (config$type == "binary") {
      roc_curve(data, truth, score, event_level = "first") %>% mutate(Class = classes)
    } else {
      curve_data(data, classes, roc_curve)
    }) %>%
      mutate(
        x = 1 - specificity,
        y = sensitivity,
        Tooltip = paste0(
          "Class: ", Class,
          "<br>Threshold: ", scales::number(.threshold, accuracy = 0.001),
          "<br>False Positive Rate: ", scales::number(x, accuracy = 0.001),
          "<br>Sensitivity: ", scales::number(y, accuracy = 0.001)
        )
      )

    plot <- plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
        line = list(color = "#D3DCE6", dash = "dash"), hoverinfo = "skip",
        showlegend = FALSE
      )

    for (class_name in classes) {
      current_curve <- plot_data %>% filter(Class == class_name)
      plot <- plot %>%
        add_trace(
          data = current_curve, x = ~x, y = ~y, text = ~Tooltip,
          type = "scatter", mode = "lines+markers",
          name = class_name, hoverinfo = "text",
          line = list(color = colors[[class_name]], width = 2),
          marker = list(color = colors[[class_name]], size = 5, opacity = 0.6)
        )
    }

    if (config$type == "binary") {
      selected <- tibble(
        x = 1 - metric_value(spec_vec, data$truth, data$estimate, event_level = "first"),
        y = metric_value(sens_vec, data$truth, data$estimate, event_level = "first"),
        Tooltip = binary_current_tooltip(data, input$threshold)
      )
      plot <- plot %>%
        add_trace(
          data = selected, x = ~x, y = ~y, text = ~Tooltip,
          type = "scatter", mode = "markers", name = "Selected Threshold",
          hoverinfo = "text",
          marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
        )
    }

    plot %>%
      layout(
        title = list(text = "ROC Curve"),
        xaxis = list(title = "False Positive Rate", range = c(0, 1)),
        yaxis = list(title = "Sensitivity", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$pr_plot <- renderPlotly({
    config <- analysis_config()
    data <- analysis_data()
    classes <- if (config$type == "binary") config$classes[[1]] else config$classes
    colors <- curve_colors(classes)
    plot_data <- (if (config$type == "binary") {
      pr_curve(data, truth, score, event_level = "first") %>% mutate(Class = classes)
    } else {
      curve_data(data, classes, pr_curve)
    }) %>%
      mutate(
        x = recall,
        y = precision,
        Tooltip = paste0(
          "Class: ", Class,
          "<br>Threshold: ", scales::number(.threshold, accuracy = 0.001),
          "<br>Recall: ", scales::number(x, accuracy = 0.001),
          "<br>Precision: ", scales::number(y, accuracy = 0.001)
        )
      )

    plot <- plot_ly()
    for (class_name in classes) {
      current_curve <- plot_data %>% filter(Class == class_name)
      plot <- plot %>%
        add_trace(
          data = current_curve, x = ~x, y = ~y, text = ~Tooltip,
          type = "scatter", mode = "lines+markers",
          name = class_name, hoverinfo = "text",
          line = list(color = colors[[class_name]], width = 2),
          marker = list(color = colors[[class_name]], size = 5, opacity = 0.6)
        )
    }

    if (config$type == "binary") {
      selected <- tibble(
        x = metric_value(recall_vec, data$truth, data$estimate, event_level = "first"),
        y = metric_value(ppv_vec, data$truth, data$estimate, event_level = "first"),
        Tooltip = binary_current_tooltip(data, input$threshold)
      )
      plot <- plot %>%
        add_trace(
          data = selected, x = ~x, y = ~y, text = ~Tooltip,
          type = "scatter", mode = "markers", name = "Selected Threshold",
          hoverinfo = "text",
          marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
        )
    }

    plot %>%
      layout(
        title = list(text = "Precision-Recall Curve"),
        xaxis = list(title = "Recall", range = c(0, 1)),
        yaxis = list(title = "Precision", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$probability_distribution_plot <- renderPlotly({
    config <- analysis_config()
    req(config$type == "binary")
    data <- analysis_data() %>%
      mutate(
        Tooltip = paste0(
          "Actual Class: ", truth,
          "<br>Predicted Probability: ", scales::number(score, accuracy = 0.001)
        )
      )

    plot <- plot_ly() %>%
      add_histogram(
        data = data, x = ~score, name = "Overall",
        histnorm = "probability density", nbinsx = 30,
        marker = list(color = "#9AA6B2", line = list(color = "white", width = 1)),
        opacity = 0.55, hovertemplate = paste(
          "Distribution: Overall",
          "<br>Probability bin: %{x}",
          "<br>Density: %{y:.3f}<extra></extra>"
        )
      )

    distribution_colors <- set_names(c("#0050A4", "#C43131"), levels(data$truth))
    for (truth_level in levels(data$truth)) {
      current_data <- data %>% filter(truth == truth_level)
      plot <- plot %>%
        add_histogram(
          data = current_data, x = ~score, name = paste("Actual Class:", truth_level),
          histnorm = "probability density", nbinsx = 30,
          marker = list(color = distribution_colors[[truth_level]]),
          opacity = 0.55, hovertemplate = paste(
            "Actual Class: ", truth_level,
            "<br>Probability bin: %{x}",
            "<br>Density: %{y:.3f}<extra></extra>"
          )
        )
    }

    plot %>%
      layout(
        barmode = "overlay",
        xaxis = list(title = "Predicted Probability", range = c(0, 1), tickformat = ".0%"),
        yaxis = list(title = "Density"),
        legend = list(orientation = "h", x = 0, y = 1.16, xanchor = "left", yanchor = "bottom"),
        margin = list(t = 36, b = 58, l = 58, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$positive_rate_bin_plot <- renderPlotly({
    config <- analysis_config()
    req(config$type == "binary")
    confidence_level_percent <- if (is.null(input$confidence_level)) {
      95
    } else {
      as.numeric(input$confidence_level)
    }
    confidence_level_percent <- pmin(pmax(confidence_level_percent, 80), 95)
    confidence_label <- scales::percent(confidence_level_percent / 100, accuracy = 1)
    bin_data <- probability_bin_table(analysis_data(), confidence_level_percent)

    plot_ly(bin_data, x = ~score_bin) %>%
      add_bars(
        y = ~`Positive Rate`, customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Positive Rate",
        marker = list(color = theme_colors$bright_blue, opacity = 0.72),
        textposition = "none"
      ) %>%
      add_markers(
        y = ~`Positive Rate`, customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = paste(confidence_label, "CI"),
        marker = list(color = theme_colors$navy, size = 7),
        error_y = list(
          type = "data",
          symmetric = FALSE,
          array = ~`CI Upper` - `Positive Rate`,
          arrayminus = ~`Positive Rate` - `CI Lower`,
          color = theme_colors$navy,
          thickness = 1.4,
          width = 4
        )
      ) %>%
      layout(
        xaxis = list(
          title = "Predicted Probability Bin",
          tickangle = -30,
          tickfont = list(size = 10)
        ),
        yaxis = list(
          title = paste("Positive Rate with", confidence_label, "CI"),
          range = c(0, 1), tickformat = ".0%",
          tickfont = list(size = 10)
        ),
        legend = list(orientation = "h", x = 0, y = 1.16, xanchor = "left", yanchor = "bottom"),
        margin = list(t = 36, b = 88, l = 68, r = 24)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$matrix_plot <- renderPlotly({
    suppressWarnings(ggplotly(confusion_plot(analysis_data()), tooltip = "text")) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$metrics_table <- renderTable({
    config <- analysis_config()
    data <- analysis_data()

    if (config$type == "binary") {
      binary_metric_table(data)
    } else {
      multiclass_metric_table(data, config$classes)
    }
  }, striped = TRUE, bordered = FALSE, spacing = "m", align = "l")

  output$preview_table <- renderTable({
    req(input$file)
    uploaded_data() %>% slice_head(n = 10)
  }, striped = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)

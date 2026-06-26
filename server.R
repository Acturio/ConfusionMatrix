server <- function(input, output, session) {
  analysis_ready <- reactiveVal(FALSE)
  llm_report <- reactiveVal(NULL)
  llm_report_error <- reactiveVal(NULL)

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
    req(input$analysis_type == "classification")
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
    req(input$analysis_type == "classification", input$model_type == "binary", input$truth_col)
    classes <- uploaded_data() %>%
      pull(all_of(input$truth_col)) %>%
      discard(is.na) %>%
      as.character() %>%
      unique()

    selectInput("positive_class", "Positive class", choices = classes, selected = 1)
  })

  output$multiclass_score_selectors <- renderUI({
    req(input$analysis_type == "classification", input$model_type == "multiclass", input$truth_col, input$class_count)
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

  output$multiclass_focus_controls <- renderUI({
    req(input$analysis_type == "classification", input$model_type == "multiclass", input$truth_col)
    data <- uploaded_data()
    classes <- data %>%
      pull(all_of(input$truth_col)) %>%
      discard(is.na) %>%
      as.character() %>%
      unique()
    selected_class <- if (!is.null(input$multiclass_focus_class) &&
      input$multiclass_focus_class %in% classes) {
      input$multiclass_focus_class
    } else {
      dplyr::first(classes, default = "")
    }

    tagList(
      selectInput(
        "multiclass_focus_class",
        "Class to analyze",
        choices = classes,
        selected = selected_class
      ),
      sliderInput(
        "multiclass_threshold",
        "One-vs-rest threshold",
        min = 0, max = 1, value = 0.5, step = 0.01
      ),
      numericInput(
        "multiclass_confidence_level",
        "Confidence level (%)",
        value = 95, min = 80, max = 95, step = 5
      ),
      tags$p(
        class = "form-hint",
        "These controls affect selected-class one-vs-rest diagnostics. Multiclass prediction still uses the highest probability."
      )
    )
  })

  output$regression_column_selectors <- renderUI({
    req(input$analysis_type == "regression", input$file)
    data <- uploaded_data()
    columns <- names(data)
    numeric_columns <- data %>%
      select(where(is.numeric)) %>%
      names()
    validate(need(length(numeric_columns) >= 2, "Regression requires at least two numeric columns."))

    suggested_actual <- numeric_columns[
      str_detect(str_to_lower(numeric_columns), "actual|truth|real|target|y_true|observed")
    ]
    actual_selected <- if (!is.null(input$regression_actual_col) &&
      input$regression_actual_col %in% numeric_columns) {
      input$regression_actual_col
    } else {
      dplyr::first(suggested_actual, default = dplyr::first(numeric_columns, default = ""))
    }

    predicted_choices <- setdiff(numeric_columns, actual_selected)
    suggested_predicted <- predicted_choices[
      str_detect(str_to_lower(predicted_choices), "pred|prediction|score|estimate|y_pred|fitted")
    ]
    predicted_selected <- if (!is.null(input$regression_predicted_col) &&
      input$regression_predicted_col %in% predicted_choices) {
      input$regression_predicted_col
    } else {
      dplyr::first(suggested_predicted, default = dplyr::first(predicted_choices, default = ""))
    }

    tagList(
      selectInput(
        "regression_actual_col",
        "Actual / y_true column",
        choices = numeric_columns,
        selected = actual_selected
      ),
      selectInput(
        "regression_predicted_col",
        "Predicted / y_pred column",
        choices = predicted_choices,
        selected = predicted_selected
      )
    )
  })

  output$regression_tolerance_input <- renderUI({
    req(input$analysis_type == "regression", input$file)
    data <- uploaded_data()
    numeric_columns <- data %>%
      select(where(is.numeric)) %>%
      names()
    req(input$regression_actual_col %in% numeric_columns, input$regression_predicted_col %in% numeric_columns)

    errors <- data %>%
      transmute(
        actual = as.numeric(.data[[input$regression_actual_col]]),
        predicted = as.numeric(.data[[input$regression_predicted_col]])
      ) %>%
      drop_na() %>%
      mutate(absolute_error = abs(predicted - actual)) %>%
      pull(absolute_error)

    max_error <- if (length(errors) == 0 || all(is.na(errors))) {
      1
    } else {
      max(errors, na.rm = TRUE)
    }
    slider_max <- max(1, signif(max_error * 1.1, 3))
    default_value <- if (length(errors) == 0 || all(is.na(errors))) {
      0
    } else {
      stats::median(errors, na.rm = TRUE)
    }
    selected_value <- if (!is.null(input$regression_tolerance)) {
      pmin(pmax(input$regression_tolerance, 0), slider_max)
    } else {
      min(default_value, slider_max)
    }

    sliderInput(
      "regression_tolerance",
      "Absolute error tolerance",
      min = 0,
      max = slider_max,
      value = selected_value,
      step = slider_max / 100
    )
  })

  analysis_config <- eventReactive(input$calculate, {
    req(input$analysis_type == "classification")
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

  regression_config <- eventReactive(input$calculate, {
    req(input$analysis_type == "regression")
    data <- uploaded_data()
    validate(need(input$regression_actual_col %in% names(data), "Select the Actual / y_true column."))
    validate(need(input$regression_predicted_col %in% names(data), "Select the Predicted / y_pred column."))
    validate(need(input$regression_actual_col != input$regression_predicted_col, "Actual and Predicted columns must be different."))
    validate(need(is.numeric(data[[input$regression_actual_col]]), "Actual / y_true column must be numeric."))
    validate(need(is.numeric(data[[input$regression_predicted_col]]), "Predicted / y_pred column must be numeric."))

    prepared <- data %>%
      transmute(
        actual = as.numeric(.data[[input$regression_actual_col]]),
        predicted = as.numeric(.data[[input$regression_predicted_col]])
      ) %>%
      drop_na()
    validate(need(nrow(prepared) > 0, "No complete numeric rows are available for regression analysis."))

    tolerance <- if (is.null(input$regression_tolerance)) {
      0
    } else {
      as.numeric(input$regression_tolerance)
    }
    tolerance <- max(tolerance, 0, na.rm = TRUE)
    prepared <- regression_add_diagnostics(prepared, tolerance)
    bins <- if (is.null(input$regression_bins)) 10 else as.integer(input$regression_bins)
    bins <- min(max(bins, 1), nrow(prepared))
    log_requested <- isTRUE(input$regression_log_scale)
    log_available <- regression_log_available(prepared)

    list(
      type = "regression",
      data = prepared,
      omitted = nrow(data) - nrow(prepared),
      tolerance = tolerance,
      bins = bins,
      main_metric = if (is.null(input$regression_main_metric)) "RMSE" else input$regression_main_metric,
      quantile_sort = if (is.null(input$regression_quantile_sort)) "actual" else input$regression_quantile_sort,
      show_outliers = isTRUE(input$regression_show_outliers),
      log_requested = log_requested,
      log_available = log_available,
      log_scale = log_requested && log_available
    )
  })

  observeEvent(input$file, {
    analysis_ready(FALSE)
    llm_report(NULL)
    llm_report_error(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$analysis_type, {
    analysis_ready(FALSE)
    llm_report(NULL)
    llm_report_error(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$model_type, {
    analysis_ready(FALSE)
    llm_report(NULL)
    llm_report_error(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$calculate, {
    llm_report(NULL)
    llm_report_error(NULL)

    tryCatch(
      {
        if (input$analysis_type == "regression") {
          regression_config()
        } else {
          analysis_config()
        }
        analysis_ready(TRUE)
      },
      error = function(error) {
        analysis_ready(FALSE)
      }
    )
  }, ignoreInit = TRUE)

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

  selected_multiclass_class <- reactive({
    config <- analysis_config()
    req(config$type == "multiclass")
    multiclass_focus_class(config, input$multiclass_focus_class)
  })

  selected_multiclass_threshold <- reactive({
    threshold <- if (is.null(input$multiclass_threshold)) {
      0.5
    } else {
      as.numeric(input$multiclass_threshold)
    }

    pmin(pmax(threshold, 0), 1)
  })

  selected_multiclass_confidence_level <- reactive({
    confidence_level <- if (is.null(input$multiclass_confidence_level)) {
      95
    } else {
      as.numeric(input$multiclass_confidence_level)
    }

    pmin(pmax(confidence_level, 80), 95)
  })

  selected_multiclass_data <- reactive({
    config <- analysis_config()
    req(config$type == "multiclass")
    multiclass_focus_data(
      config,
      selected_multiclass_class(),
      selected_multiclass_threshold()
    )
  })

  output$data_notice <- renderUI({
    req(input$calculate > 0)
    config <- if (input$analysis_type == "regression") {
      regression_config()
    } else {
      analysis_config()
    }

    if (config$omitted == 0) {
      tags$p(class = "notice success", "Data ready for analysis.")
    } else {
      tags$p(class = "notice", paste(config$omitted, "incomplete rows were excluded."))
    }
  })

  output$llm_controls <- renderUI({
    if (!isTRUE(analysis_ready())) {
      return(tags$p(
        class = "notice",
        "Run Calculate results successfully to enable the local AI report."
      ))
    }

    div(
      class = "ai-controls",
      tags$h3("AI report"),
      radioButtons(
        "llm_report_language",
        "Report language",
        choices = c("Spanish" = "Spanish", "English" = "English"),
        selected = "Spanish"
      ),
      radioButtons(
        "llm_model",
        "Local Ollama model",
        choices = c("qwen2.5:7b", "llama3.1:8b", "mistral:7b"),
        selected = "qwen2.5:7b"
      ),
      actionButton("generate_report", "Generate report", class = "btn-calculate")
    )
  })

  current_llm_payload <- reactive({
    req(analysis_ready())

    if (input$analysis_type == "regression") {
      config <- regression_config()
      return(build_llm_report_payload(
        analysis_type = "regression",
        config = config,
        data = config$data,
        inputs = list(
          target = input$regression_actual_col,
          prediction = input$regression_predicted_col
        )
      ))
    }

    config <- analysis_config()
    data <- analysis_data()
    build_llm_report_payload(
      analysis_type = "classification",
      config = config,
      data = data,
      inputs = list(
        target = input$truth_col,
        prediction = if (config$type == "binary") input$score_col else "highest_probability_class",
        threshold = if (config$type == "binary") input$threshold else NULL
      )
    )
  })

  observeEvent(input$generate_report, {
    req(analysis_ready())
    payload <- current_llm_payload()
    model <- if (is.null(input$llm_model)) "qwen2.5:7b" else input$llm_model
    language <- if (is.null(input$llm_report_language)) "Spanish" else input$llm_report_language

    llm_report(NULL)
    llm_report_error(NULL)
    updateTabsetPanel(session, "results_tabs", selected = "AI Report")

    tryCatch(
      {
        report <- withProgress(message = "Generating local AI report with Ollama...", value = 0, {
          incProgress(0.25, detail = "Building compact model summary")
          incProgress(0.35, detail = paste("Calling", model, "locally"))
          generate_llm_report(payload, model = model, language = language)
        })
        llm_report(report)
        showNotification("Local AI report generated with Ollama.", type = "message")
      },
      error = function(error) {
        friendly_message <- paste0(
          "I could not generate the local AI report. Make sure Ollama is running and the selected model is downloaded. ",
          "If this app is deployed, Ollama must be running on the deployment server; a deployed app cannot use Ollama from your laptop. ",
          "On macOS with Homebrew, try: brew services start ollama. ",
          "For a one-session server, try: ollama serve. ",
          "Then download the model with: ollama pull ", model, ". ",
          "Details: ", conditionMessage(error)
        )
        llm_report_error(friendly_message)
        showNotification(friendly_message, type = "error", duration = 12)
      }
    )
  })

  output$llm_report_panel <- renderUI({
    if (!isTRUE(analysis_ready())) {
      return(div(
        class = "ai-report-empty",
        tags$h3("Interpretive model report"),
        tags$p("Run Calculate results successfully before generating an AI report.")
      ))
    }

    error_message <- llm_report_error()
    if (!is.null(error_message)) {
      return(div(
        class = "ai-report-empty",
        tags$h3("Interpretive model report"),
        tags$p(class = "notice", error_message)
      ))
    }

    report <- llm_report()
    if (is.null(report)) {
      return(div(
        class = "ai-report-empty",
        tags$h3("Interpretive model report"),
        tags$p(
          "Use Generate report after updating the analysis. The app will send only compact metrics, summaries, and chart descriptions to your local Ollama model."
        )
      ))
    }

    div(
      class = "ai-report-body",
      HTML(markdown_report_to_html(report))
    )
  })

  output$summary_cards <- renderUI({
    req(input$calculate > 0)
    if (input$analysis_type == "regression") {
      config <- regression_config()
      metrics <- regression_metric_values(config$data, config$tolerance) %>%
        mutate(
          Label = recode(
            Metric,
            `Median Absolute Error` = "MedAE",
            `Pearson Correlation` = "Pearson Corr.",
            `Spearman Correlation` = "Spearman Corr.",
            `% Within Tolerance` = "Within Tolerance",
            .default = Metric
          ),
          Display = case_when(
            Metric %in% c("WAPE", "MAPE", "sMAPE", "% Within Tolerance") ~ format_percent_metric(Value),
            TRUE ~ format_metric(Value)
          )
        )

      return(
        div(
          class = "cards",
          map2(metrics$Label, metrics$Display, ~ div(
            class = "kpi-card",
            tags$p(.x),
            tags$strong(.y)
          ))
        )
      )
    }

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
    req(input$analysis_type == "classification")
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
          plotlyOutput("probability_distribution_plot", height = "310px"),
          tags$p(
            class = "chart-description",
            paste(
              "Use this to compare predicted probability distributions overall and by Actual Class.",
              "A useful model usually pushes positives toward higher scores and negatives toward lower scores; heavy overlap indicates weaker separation.",
              "The Overall layer helps you understand the full score population."
            )
          )
        )
      ),
      column(
        6,
        div(
          class = "chart-card chart-card--titled",
          tags$h3(class = "chart-title", "Calibration & Positive Rate by Probability Bin"),
          uiOutput("calibration_summary"),
          plotlyOutput("calibration_probability_bin_plot", height = "310px"),
          tags$p(
            class = "chart-description",
            paste(
              "Use this to evaluate calibration and observed risk by probability bin.",
              "The diagonal means perfect calibration, the Calibration Curve compares mean predicted probability with observed positive rate, CI markers show uncertainty, and Bin Volume shows how much data supports each bin.",
              "Brier Score measures probability error; lower is better and 0 is perfect."
            )
          )
        )
      )
    )
  })

  output$multiclass_extra_plots <- renderUI({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    if (config$type != "multiclass") {
      return(NULL)
    }

    div(
      class = "insights-section",
      tags$h3("Multiclass Overview"),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Class Performance Overview"),
            plotlyOutput("multiclass_metric_overview_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to compare one-vs-rest performance across all target classes.",
                "Precision, Recall, F1 Score, ROC AUC, and PR AUC are calculated per class so weak categories are easier to spot."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Average Probability by Actual Class"),
            plotlyOutput("multiclass_probability_heatmap_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this heatmap to inspect how probability mass is distributed for each Actual Class.",
                "A strong model tends to show higher values on the diagonal, meaning each true class receives its own highest average probability."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Actual vs Predicted Class Volume"),
            plotlyOutput("multiclass_class_volume_plot", height = "310px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to compare the true class mix against the model's predicted class mix.",
                "Large gaps can reveal systematic overprediction or underprediction of specific classes."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Prediction Confidence by Result"),
            plotlyOutput("multiclass_confidence_distribution_plot", height = "310px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to inspect the maximum predicted probability from the winning class.",
                "Correct predictions should generally have higher confidence than incorrect predictions; high-confidence errors deserve review."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Selected Class Probability Distribution"),
            plotlyOutput("multiclass_probability_distribution_plot", height = "310px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to analyze the selected class as one-vs-rest.",
                "The selected class should concentrate toward higher probabilities, while Rest should concentrate toward lower probabilities."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Selected Class Calibration & Positive Rate"),
            uiOutput("multiclass_calibration_summary"),
            plotlyOutput("multiclass_calibration_probability_bin_plot", height = "310px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to evaluate whether the selected class probabilities are calibrated.",
                "The Calibration Curve should follow the diagonal; CI markers show observed positive rate by probability bin and Bin Volume shows support."
              )
            )
          )
        )
      )
    )
  })

  output$business_validation_insights <- renderUI({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    if (config$type != "binary") {
      return(NULL)
    }

    div(
      class = "insights-section",
      tags$h3("Business Prioritization"),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Cumulative Gains"),
            plotlyOutput("cumulative_gains_plot", height = "320px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to understand business capture when cases are prioritized by descending score.",
                "Cumulative Gains shows what percentage of all positives you capture after reviewing the top X% of the population.",
                "A steep early curve means the model concentrates positives near the top; the marker shows the current threshold."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Lift Curve"),
            plotlyOutput("lift_curve_plot", height = "320px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to quantify how much better prioritized targeting is than random selection.",
                "Cumulative Lift is the precision within the prioritized population divided by the overall positive rate.",
                "Lift above 1 means the selected population is richer in positives than the average population; the marker follows the current threshold."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Decile Analysis"),
            plotlyOutput("decile_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to inspect score-ranked groups of similar size.",
                "Decile 1 contains the highest predicted probabilities and Decile 10 the lowest; bars show Positive Rate and the line shows Lift.",
                "The contrasting orange bar marks the decile where the current threshold falls, so you can see which ranked segment is being selected."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "metrics-card insights-table-card",
            tags$h4(class = "chart-title", "Decile Summary"),
            DT::DTOutput("decile_table"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this table to audit each decile in detail: observation count, average score, positives, Positive Rate, Lift, and cumulative captured positives.",
                "Cumulative Recall tells how many actual positives have been captured after moving from the highest-score deciles downward."
              )
            )
          )
        )
      ),
      tags$h3("Validation Diagnostics"),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "KS Curve"),
            plotlyOutput("ks_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to evaluate separation between positives and negatives over the score scale.",
                "The KS statistic is the maximum vertical distance between the cumulative positive rate (TPR) and cumulative negative rate (FPR).",
                "Higher KS means stronger separation; the plot marks both the maximum-KS threshold and the current threshold."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "MCC vs Threshold"),
            uiOutput("mcc_summary"),
            plotlyOutput("mcc_threshold_plot", height = "300px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to choose a threshold with balanced classification quality.",
                "Matthews Correlation Coefficient (MCC) ranges from -1 to 1 and uses TP, TN, FP, and FN together, making it useful when classes are imbalanced.",
                "The compact cards show current MCC and the best MCC found across thresholds."
              )
            )
          )
        )
      )
    )
  })

  output$multiclass_business_validation_insights <- renderUI({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    if (config$type != "multiclass") {
      return(NULL)
    }

    div(
      class = "insights-section",
      tags$h3("Multiclass Business Prioritization"),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Cumulative Gains by Class"),
            plotlyOutput("multiclass_cumulative_gains_plot", height = "320px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to compare how quickly each class can be captured when records are sorted by that class probability.",
                "The selected-class marker shows the population share and recall implied by the one-vs-rest threshold."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Lift Curve by Class"),
            plotlyOutput("multiclass_lift_curve_plot", height = "320px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to compare enrichment across classes.",
                "Lift above 1 means a score-ranked segment contains more true cases of that class than random selection."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Selected Class Decile Analysis"),
            plotlyOutput("multiclass_decile_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to audit score-ranked deciles for the selected class.",
                "The orange bar marks the decile where the selected one-vs-rest threshold falls."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "metrics-card insights-table-card",
            tags$h4(class = "chart-title", "Selected Class Decile Summary"),
            DT::DTOutput("multiclass_decile_table"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this table to inspect the selected class decile counts, average score, positives, Positive Rate, Lift, and Cumulative Recall.",
                "It shows how much of the class is captured as you move from high-score to low-score records."
              )
            )
          )
        )
      ),
      tags$h3("Selected Class Validation Diagnostics"),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Selected Class KS Curve"),
            plotlyOutput("multiclass_ks_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this one-vs-rest KS Curve to assess separation for the selected class.",
                "The maximum KS threshold highlights where positives and Rest are most separated by score."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Selected Class MCC vs Threshold"),
            uiOutput("multiclass_mcc_summary"),
            plotlyOutput("multiclass_mcc_threshold_plot", height = "300px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to tune the selected class one-vs-rest threshold.",
                "MCC balances true positives, true negatives, false positives, and false negatives, which is useful when class frequencies differ."
              )
            )
          )
        )
      )
    )
  })

  output$regression_results <- renderUI({
    req(input$analysis_type == "regression")
    config <- regression_config()

    tagList(
      uiOutput("regression_log_notice"),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Actual vs Predicted"),
            plotlyOutput("regression_actual_predicted_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to compare predicted values against actual values.",
                "The diagonal line is perfect prediction; points near the line are better, and the color shows whether absolute error is within the selected tolerance."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Residual Plot"),
            plotlyOutput("regression_residual_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to inspect residuals, where error equals predicted minus actual.",
                "A healthy model usually has residuals centered around zero without strong patterns across predicted values."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Residual Distribution"),
            plotlyOutput("regression_residual_distribution_plot", height = "320px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this histogram to see whether residuals are centered and symmetric.",
                "Bias is the mean residual; values above zero mean overprediction on average, while values below zero mean underprediction."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Error CDF / Tolerance Curve"),
            plotlyOutput("regression_error_cdf_plot", height = "320px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to understand what share of observations falls below any absolute-error level.",
                "The vertical line is the selected tolerance; the intersection shows the percentage within tolerance."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Regression Calibration Plot"),
            plotlyOutput("regression_calibration_plot", height = "330px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to compare average prediction and average actual value by prediction bins.",
                "If the model is calibrated, bin points should follow the diagonal; systematic gaps reveal overprediction or underprediction."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Absolute Error vs Actual"),
            plotlyOutput("regression_absolute_error_plot", height = "330px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to see whether errors grow with the size of the actual value.",
                "Outlier highlighting helps identify observations with unusually large absolute error."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "chart-card chart-card--titled",
            tags$h4(class = "chart-title", "Quantile Performance"),
            plotlyOutput("regression_quantile_plot", height = "340px"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this to compare performance across ordered value segments.",
                "Quantiles are sorted by Actual or Predicted values, and the selected Main metric controls the bar height."
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "metrics-card insights-table-card",
            tags$h4(class = "chart-title", "Quantile Summary"),
            DT::DTOutput("regression_quantile_table"),
            tags$p(
              class = "chart-description",
              paste(
                "Use this table to audit n, mean actual, mean predicted, MAE, RMSE, Bias, WAPE, and MAPE by quantile.",
                "MAE is average absolute error, RMSE penalizes large errors more, and WAPE/MAPE express error relative to actual values."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "metrics-card",
            tags$h3("Regression Metrics"),
            div(class = "metrics-table", tableOutput("regression_metrics_table")),
            tags$p(
              class = "chart-description",
              paste(
                "MAE and Median Absolute Error summarize typical error magnitude; RMSE emphasizes large misses.",
                "R2 measures explained variance, correlations measure association, WAPE/MAPE are scale-relative errors, and Within Tolerance uses the selected absolute-error tolerance."
              )
            )
          )
        )
      )
    )
  })

  output$roc_plot <- renderPlotly({
    req(input$analysis_type == "classification")
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
        xaxis = continuous_x_axis("False Positive Rate"),
        yaxis = list(title = "Sensitivity", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$pr_plot <- renderPlotly({
    req(input$analysis_type == "classification")
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

    plot <- plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(1, 0), type = "scatter", mode = "lines",
        name = "Baseline", hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash"),
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
        xaxis = continuous_x_axis("Recall"),
        yaxis = list(title = "Precision", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$probability_distribution_plot <- renderPlotly({
    req(input$analysis_type == "classification")
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
        xaxis = continuous_x_axis("Predicted Probability", percent = TRUE),
        yaxis = list(title = "Density"),
        legend = list(orientation = "h", x = 0, y = 1.16, xanchor = "left", yanchor = "bottom"),
        margin = list(t = 36, b = 58, l = 58, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$calibration_probability_bin_plot <- renderPlotly({
    req(input$analysis_type == "classification")
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
    calibration <- calibration_table(analysis_data())
    observed <- calibration %>% filter(Observations > 0)

    plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
        name = "Perfect Calibration", hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash", width = 2)
      ) %>%
      add_trace(
        data = observed,
        x = ~`Mean Predicted Probability`,
        y = ~`Observed Positive Rate`,
        text = ~Tooltip,
        type = "scatter", mode = "lines+markers",
        name = "Calibration Curve",
        hoverinfo = "text",
        line = list(color = theme_colors$bright_blue, width = 2.3),
        marker = list(color = theme_colors$bright_blue, size = 8)
      ) %>%
      add_markers(
        data = bin_data,
        x = ~bin_midpoint,
        y = ~`Positive Rate`,
        customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = paste("Positive Rate", confidence_label, "CI"),
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
      add_bars(
        data = bin_data,
        x = ~bin_midpoint,
        y = ~`Total Count`,
        customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Bin Volume",
        yaxis = "y2",
        width = 0.075,
        marker = list(color = "#9AA6B2", opacity = 0.35),
        textposition = "none"
      ) %>%
      layout(
        xaxis = continuous_x_axis("Predicted Probability", percent = TRUE),
        yaxis = list(
          title = "Observed Positive Rate",
          range = c(0, 1),
          tickformat = ".0%",
          tickfont = list(size = 10)
        ),
        yaxis2 = list(
          title = "Bin Volume",
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          rangemode = "tozero"
        ),
        legend = list(orientation = "h", x = 0, y = 1.16, xanchor = "left", yanchor = "bottom"),
        margin = list(t = 42, b = 62, l = 68, r = 68),
        barmode = "overlay"
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$cumulative_gains_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    data <- analysis_data()
    gains <- lift_gains_table(data)
    selected <- threshold_capture_summary(data, input$threshold)

    plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
        name = "Random Baseline", hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash")
      ) %>%
      add_trace(
        data = gains, x = ~cumulative_population, y = ~cumulative_recall,
        text = ~Tooltip, type = "scatter", mode = "lines",
        name = "Cumulative Gains", hoverinfo = "text",
        line = list(color = theme_colors$bright_blue, width = 2.3)
      ) %>%
      add_trace(
        data = selected, x = ~population_pct, y = ~cumulative_recall,
        text = ~Tooltip, type = "scatter", mode = "markers",
        name = "Selected Threshold", hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      ) %>%
      layout(
        xaxis = continuous_x_axis("Cumulative Population", percent = TRUE),
        yaxis = list(title = "Cumulative Positives Captured", tickformat = ".0%", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.2),
        margin = list(t = 24, b = 72, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$lift_curve_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    data <- analysis_data()
    gains <- lift_gains_table(data) %>% filter(!is.na(cumulative_lift))
    selected <- threshold_capture_summary(data, input$threshold)

    plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(1, 1), type = "scatter", mode = "lines",
        name = "No Lift Baseline", hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash")
      ) %>%
      add_trace(
        data = gains, x = ~cumulative_population, y = ~cumulative_lift,
        text = ~Tooltip, type = "scatter", mode = "lines",
        name = "Cumulative Lift", hoverinfo = "text",
        line = list(color = theme_colors$aqua, width = 2.3)
      ) %>%
      add_trace(
        data = selected, x = ~population_pct, y = ~lift,
        text = ~Tooltip, type = "scatter", mode = "markers",
        name = "Selected Threshold", hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      ) %>%
      layout(
        xaxis = continuous_x_axis("Cumulative Population", percent = TRUE),
        yaxis = list(title = "Cumulative Lift", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.2),
        margin = list(t = 24, b = 72, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$ks_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    data <- analysis_data()
    ks_data <- ks_table(data)
    selected <- ks_at_threshold(data, input$threshold)
    max_ks <- ks_data %>%
      filter(!is.na(KS)) %>%
      slice_max(KS, n = 1, with_ties = FALSE)

    plot <- plot_ly() %>%
      add_trace(
        data = ks_data, x = ~score, y = ~TPR, text = ~Tooltip,
        type = "scatter", mode = "lines", name = "TPR",
        hoverinfo = "text", line = list(color = theme_colors$bright_blue, width = 2.3)
      ) %>%
      add_trace(
        data = ks_data, x = ~score, y = ~FPR, text = ~Tooltip,
        type = "scatter", mode = "lines", name = "FPR",
        hoverinfo = "text", line = list(color = "#C43131", width = 2.3)
      ) %>%
      add_trace(
        data = selected, x = ~score, y = ~TPR, text = ~Tooltip,
        type = "scatter", mode = "markers", name = "Selected Threshold",
        hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      )

    if (nrow(max_ks) > 0) {
      plot <- plot %>%
        add_trace(
          x = c(max_ks$score, max_ks$score), y = c(0, 1),
          type = "scatter", mode = "lines", name = "Max KS Threshold",
          hoverinfo = "text", text = max_ks$Tooltip,
          line = list(color = "#F7893B", dash = "dash", width = 2)
        ) %>%
        add_annotations(
          x = max_ks$score, y = 1,
          text = paste0("Max KS: ", scales::number(max_ks$KS, accuracy = 0.001)),
          showarrow = TRUE, arrowhead = 2, ax = 20, ay = -28
        )
    }

    plot %>%
      layout(
        xaxis = continuous_x_axis("Threshold"),
        yaxis = list(title = "Cumulative Distribution", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(t = 36, b = 80, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$decile_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    deciles <- decile_analysis_table(analysis_data(), input$threshold)
    bar_colors <- if_else(deciles$`Current Threshold Decile`, "#F7893B", theme_colors$bright_blue)

    plot_ly(deciles, x = ~factor(decile, levels = 1:10)) %>%
      add_bars(
        y = ~`Positive Rate`, customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Positive Rate",
        marker = list(color = bar_colors, opacity = 0.78),
        textposition = "none"
      ) %>%
      add_trace(
        y = ~Lift, customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        type = "scatter", mode = "lines+markers",
        name = "Decile Lift", yaxis = "y2",
        line = list(color = theme_colors$navy, width = 2),
        marker = list(color = theme_colors$navy, size = 7)
      ) %>%
      layout(
        xaxis = list(title = "Decile (1 = Highest Scores)"),
        yaxis = list(title = "Positive Rate", tickformat = ".0%", range = c(0, 1)),
        yaxis2 = list(title = "Lift", overlaying = "y", side = "right", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 64)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$decile_table <- DT::renderDT({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    deciles <- decile_analysis_table(analysis_data(), input$threshold)
    current_decile <- current_threshold_decile(analysis_data(), input$threshold)
    display <- deciles %>%
      transmute(
        Decile = decile,
        Observations,
        `Average Score`,
        Positives,
        `Positive Rate`,
        Lift,
        `Cumulative Positives`,
        `Cumulative Recall`
      )

    DT::datatable(
      display,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 10, scrollX = TRUE)
    ) %>%
      DT::formatRound(c("Average Score", "Lift"), digits = 3) %>%
      DT::formatPercentage(c("Positive Rate", "Cumulative Recall"), digits = 1) %>%
      DT::formatStyle(
        "Decile",
        target = "row",
        backgroundColor = DT::styleEqual(current_decile, "#EAF4FB")
      )
  })

  output$calibration_summary <- renderUI({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    div(
      class = "insight-metric",
      tags$span("Brier Score (lower is better)"),
      tags$strong(format_metric(brier_score_value(analysis_data())))
    )
  })

  output$mcc_threshold_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    data <- analysis_data()
    mcc_values <- mcc_threshold_table(data)
    current <- tibble(
      threshold = input$threshold,
      MCC = mcc_value(data, input$threshold),
      Tooltip = paste0(
        "<b>Selected Threshold: ", scales::number(input$threshold, accuracy = 0.001), "</b>",
        "<br>MCC: ", format_metric(mcc_value(data, input$threshold))
      )
    )
    best <- mcc_values %>%
      filter(!is.na(MCC)) %>%
      slice_max(MCC, n = 1, with_ties = FALSE)

    plot <- plot_ly(mcc_values, x = ~threshold, y = ~MCC, text = ~Tooltip) %>%
      add_trace(
        type = "scatter", mode = "lines",
        name = "MCC", hoverinfo = "text",
        line = list(color = theme_colors$bright_blue, width = 2.3)
      ) %>%
      add_trace(
        data = current, x = ~threshold, y = ~MCC, text = ~Tooltip,
        type = "scatter", mode = "markers",
        name = "Selected Threshold", hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      )

    if (nrow(best) > 0) {
      plot <- plot %>%
        add_trace(
          data = best, x = ~threshold, y = ~MCC, text = ~Tooltip,
          type = "scatter", mode = "markers",
          name = "Best MCC Threshold", hoverinfo = "text",
          marker = list(color = "#F7893B", line = list(color = theme_colors$navy, width = 1), size = 11)
        )
    }

    plot %>%
      layout(
        xaxis = continuous_x_axis("Threshold"),
        yaxis = list(title = "MCC", range = c(-1, 1)),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(t = 26, b = 80, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$mcc_summary <- renderUI({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "binary")
    data <- analysis_data()
    mcc_values <- mcc_threshold_table(data)
    current_mcc <- mcc_value(data, input$threshold)
    best <- mcc_values %>%
      filter(!is.na(MCC)) %>%
      slice_max(MCC, n = 1, with_ties = FALSE)

    best_mcc <- if (nrow(best) == 0) NA_real_ else best$MCC[[1]]
    best_threshold <- if (nrow(best) == 0) NA_real_ else best$threshold[[1]]

    div(
      class = "mini-kpi-grid",
      div(
        class = "mini-kpi",
        tags$p("Current MCC"),
        tags$strong(format_metric(current_mcc)),
        tags$span(paste("Thr.", scales::number(input$threshold, accuracy = 0.001)))
      ),
      div(
        class = "mini-kpi",
        tags$p("Best MCC"),
        tags$strong(format_metric(best_mcc)),
        tags$span(paste("Thr.", format_metric(best_threshold)))
      )
    )
  })

  output$multiclass_metric_overview_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    metric_data <- multiclass_metric_values(analysis_data(), config$classes) %>%
      pivot_longer(-Class, names_to = "Metric", values_to = "Value") %>%
      filter(Metric %in% c("Precision", "Sensitivity", "F1 Score", "ROC AUC", "PR AUC")) %>%
      mutate(
        Metric = recode(Metric, Sensitivity = "Recall"),
        Tooltip = paste0(
          "<b>Class: ", Class, "</b>",
          "<br>Metric: ", Metric,
          "<br>Value: ", scales::number(Value, accuracy = 0.001)
        )
      )

    plot_ly(
      metric_data,
      x = ~Class,
      y = ~Value,
      color = ~Metric,
      colors = c(theme_colors$bright_blue, theme_colors$aqua, "#F7893B", "#7C53A5", "#5BBF7A"),
      type = "bar",
      text = ~Tooltip,
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "group",
        yaxis = list(title = "Metric Value", range = c(0, 1), tickformat = ".0%"),
        xaxis = list(title = "Class"),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(t = 28, b = 90, l = 58, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_probability_heatmap_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    heatmap_data <- multiclass_probability_heatmap_table(analysis_data(), config$classes)
    heatmap_wide <- heatmap_data %>%
      select(`Actual Class`, `Probability Class`, `Mean Probability`) %>%
      pivot_wider(names_from = `Probability Class`, values_from = `Mean Probability`) %>%
      arrange(factor(`Actual Class`, levels = config$classes))
    z_matrix <- as.matrix(heatmap_wide[, config$classes])
    rownames(z_matrix) <- heatmap_wide$`Actual Class`

    plot_ly(
      x = colnames(z_matrix),
      y = rownames(z_matrix),
      z = z_matrix,
      type = "heatmap",
      zmin = 0,
      zmax = 1,
      colors = c("#EAF4FB", theme_colors$bright_blue),
      hovertemplate = paste(
        "Actual Class: %{y}",
        "<br>Probability Class: %{x}",
        "<br>Mean Probability: %{z:.1%}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Probability Class"),
        yaxis = list(title = "Actual Class"),
        margin = list(t = 28, b = 72, l = 82, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_class_volume_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    data <- analysis_data()
    volume_data <- bind_rows(
      data %>%
        count(Class = truth, name = "Count") %>%
        mutate(Type = "Actual"),
      data %>%
        count(Class = estimate, name = "Count") %>%
        mutate(Type = "Predicted")
    ) %>%
      complete(
        Class = factor(config$classes, levels = config$classes),
        Type = c("Actual", "Predicted"),
        fill = list(Count = 0)
      ) %>%
      mutate(
        Class = factor(Class, levels = config$classes),
        Tooltip = paste0(
          "<b>", Type, " Class Volume</b>",
          "<br>Class: ", Class,
          "<br>Count: ", scales::comma(Count)
        )
      )

    plot_ly(
      volume_data,
      x = ~Class,
      y = ~Count,
      color = ~Type,
      colors = c(theme_colors$bright_blue, theme_colors$aqua),
      type = "bar",
      text = ~Tooltip,
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Observations", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_confidence_distribution_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    confidence_data <- multiclass_confidence_table(analysis_data(), config$classes)

    plot_ly() %>%
      add_histogram(
        data = confidence_data,
        x = ~`Max Probability`,
        color = ~Result,
        colors = c(Correct = theme_colors$bright_blue, Incorrect = "#C43131"),
        histnorm = "probability density",
        nbinsx = 30,
        opacity = 0.62,
        hovertemplate = paste(
          "Prediction Result: %{fullData.name}",
          "<br>Max Probability bin: %{x}",
          "<br>Density: %{y:.3f}<extra></extra>"
        )
      ) %>%
      layout(
        barmode = "overlay",
        xaxis = continuous_x_axis("Max Predicted Probability", percent = TRUE),
        yaxis = list(title = "Density"),
        legend = list(orientation = "h", x = 0, y = 1.16, xanchor = "left", yanchor = "bottom"),
        margin = list(t = 36, b = 58, l = 58, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_probability_distribution_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    focus_class <- selected_multiclass_class()
    data <- selected_multiclass_data()

    plot <- plot_ly() %>%
      add_histogram(
        data = data, x = ~score, name = "Overall",
        histnorm = "probability density", nbinsx = 30,
        marker = list(color = "#9AA6B2", line = list(color = "white", width = 1)),
        opacity = 0.55,
        hovertemplate = paste(
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
          data = current_data, x = ~score, name = paste("Actual:", truth_level),
          histnorm = "probability density", nbinsx = 30,
          marker = list(color = distribution_colors[[truth_level]]),
          opacity = 0.55,
          hovertemplate = paste(
            "Selected Class: ", focus_class,
            "<br>Actual Group: ", truth_level,
            "<br>Probability bin: %{x}",
            "<br>Density: %{y:.3f}<extra></extra>"
          )
        )
    }

    plot %>%
      layout(
        barmode = "overlay",
        xaxis = continuous_x_axis(paste("Predicted Probability for", focus_class), percent = TRUE),
        yaxis = list(title = "Density"),
        legend = list(orientation = "h", x = 0, y = 1.16, xanchor = "left", yanchor = "bottom"),
        margin = list(t = 36, b = 58, l = 58, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_calibration_summary <- renderUI({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    div(
      class = "insight-metric",
      tags$span(paste("Class", selected_multiclass_class(), "| Brier Score (lower is better)")),
      tags$strong(format_metric(brier_score_value(selected_multiclass_data())))
    )
  })

  output$multiclass_calibration_probability_bin_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    focus_class <- selected_multiclass_class()
    confidence_level_percent <- selected_multiclass_confidence_level()
    confidence_label <- scales::percent(confidence_level_percent / 100, accuracy = 1)
    data <- selected_multiclass_data()
    bin_data <- probability_bin_table(data, confidence_level_percent)
    calibration <- calibration_table(data)
    observed <- calibration %>% filter(Observations > 0)

    plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
        name = "Perfect Calibration", hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash", width = 2)
      ) %>%
      add_trace(
        data = observed,
        x = ~`Mean Predicted Probability`,
        y = ~`Observed Positive Rate`,
        text = ~Tooltip,
        type = "scatter", mode = "lines+markers",
        name = "Calibration Curve",
        hoverinfo = "text",
        line = list(color = theme_colors$bright_blue, width = 2.3),
        marker = list(color = theme_colors$bright_blue, size = 8)
      ) %>%
      add_markers(
        data = bin_data,
        x = ~bin_midpoint,
        y = ~`Positive Rate`,
        customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = paste("Positive Rate", confidence_label, "CI"),
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
      add_bars(
        data = bin_data,
        x = ~bin_midpoint,
        y = ~`Total Count`,
        customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Bin Volume",
        yaxis = "y2",
        width = 0.075,
        marker = list(color = "#9AA6B2", opacity = 0.35),
        textposition = "none"
      ) %>%
      layout(
        xaxis = continuous_x_axis(paste("Predicted Probability for", focus_class), percent = TRUE),
        yaxis = list(
          title = "Observed Positive Rate",
          range = c(0, 1),
          tickformat = ".0%",
          tickfont = list(size = 10)
        ),
        yaxis2 = list(
          title = "Bin Volume",
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          rangemode = "tozero"
        ),
        legend = list(orientation = "h", x = 0, y = 1.16, xanchor = "left", yanchor = "bottom"),
        margin = list(t = 42, b = 62, l = 68, r = 68),
        barmode = "overlay"
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_cumulative_gains_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    data <- analysis_data()
    gains <- multiclass_lift_gains_table(data, config$classes)
    colors <- curve_colors(config$classes)
    focus_class <- selected_multiclass_class()
    threshold <- selected_multiclass_threshold()
    selected <- threshold_capture_summary(selected_multiclass_data(), threshold) %>%
      mutate(Tooltip = paste0("<b>Selected Class: ", focus_class, "</b><br>", Tooltip))

    plot <- plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
        name = "Random Baseline", hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash"),
        showlegend = FALSE
      )

    for (class_name in config$classes) {
      current_curve <- gains %>% filter(Class == class_name)
      plot <- plot %>%
        add_trace(
          data = current_curve,
          x = ~cumulative_population,
          y = ~cumulative_recall,
          text = ~Tooltip,
          type = "scatter",
          mode = "lines",
          name = class_name,
          hoverinfo = "text",
          line = list(color = colors[[class_name]], width = 2.3)
        )
    }

    plot %>%
      add_trace(
        data = selected, x = ~population_pct, y = ~cumulative_recall,
        text = ~Tooltip, type = "scatter", mode = "markers",
        name = "Selected Threshold", hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      ) %>%
      layout(
        xaxis = continuous_x_axis("Cumulative Population", percent = TRUE),
        yaxis = list(title = "Cumulative Positives Captured", tickformat = ".0%", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(t = 24, b = 80, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_lift_curve_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    data <- analysis_data()
    gains <- multiclass_lift_gains_table(data, config$classes) %>%
      filter(!is.na(cumulative_lift))
    colors <- curve_colors(config$classes)
    focus_class <- selected_multiclass_class()
    threshold <- selected_multiclass_threshold()
    selected <- threshold_capture_summary(selected_multiclass_data(), threshold) %>%
      mutate(Tooltip = paste0("<b>Selected Class: ", focus_class, "</b><br>", Tooltip))

    plot <- plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(1, 1), type = "scatter", mode = "lines",
        name = "No Lift Baseline", hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash"),
        showlegend = FALSE
      )

    for (class_name in config$classes) {
      current_curve <- gains %>% filter(Class == class_name)
      plot <- plot %>%
        add_trace(
          data = current_curve,
          x = ~cumulative_population,
          y = ~cumulative_lift,
          text = ~Tooltip,
          type = "scatter",
          mode = "lines",
          name = class_name,
          hoverinfo = "text",
          line = list(color = colors[[class_name]], width = 2.3)
        )
    }

    plot %>%
      add_trace(
        data = selected, x = ~population_pct, y = ~lift,
        text = ~Tooltip, type = "scatter", mode = "markers",
        name = "Selected Threshold", hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      ) %>%
      layout(
        xaxis = continuous_x_axis("Cumulative Population", percent = TRUE),
        yaxis = list(title = "Cumulative Lift", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(t = 24, b = 80, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_decile_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    deciles <- decile_analysis_table(selected_multiclass_data(), selected_multiclass_threshold())
    bar_colors <- if_else(deciles$`Current Threshold Decile`, "#F7893B", theme_colors$bright_blue)

    plot_ly(deciles, x = ~factor(decile, levels = 1:10)) %>%
      add_bars(
        y = ~`Positive Rate`, customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Positive Rate",
        marker = list(color = bar_colors, opacity = 0.78),
        textposition = "none"
      ) %>%
      add_trace(
        y = ~Lift, customdata = ~Tooltip,
        hovertemplate = "%{customdata}<extra></extra>",
        type = "scatter", mode = "lines+markers",
        name = "Decile Lift", yaxis = "y2",
        line = list(color = theme_colors$navy, width = 2),
        marker = list(color = theme_colors$navy, size = 7)
      ) %>%
      layout(
        xaxis = list(title = paste("Decile for", selected_multiclass_class(), "(1 = Highest Scores)")),
        yaxis = list(title = "Positive Rate", tickformat = ".0%", range = c(0, 1)),
        yaxis2 = list(title = "Lift", overlaying = "y", side = "right", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 64)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_decile_table <- DT::renderDT({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    deciles <- decile_analysis_table(selected_multiclass_data(), selected_multiclass_threshold())
    current_decile <- current_threshold_decile(selected_multiclass_data(), selected_multiclass_threshold())
    display <- deciles %>%
      transmute(
        Decile = decile,
        Observations,
        `Average Score`,
        Positives,
        `Positive Rate`,
        Lift,
        `Cumulative Positives`,
        `Cumulative Recall`
      )

    DT::datatable(
      display,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 10, scrollX = TRUE)
    ) %>%
      DT::formatRound(c("Average Score", "Lift"), digits = 3) %>%
      DT::formatPercentage(c("Positive Rate", "Cumulative Recall"), digits = 1) %>%
      DT::formatStyle(
        "Decile",
        target = "row",
        backgroundColor = DT::styleEqual(current_decile, "#EAF4FB")
      )
  })

  output$multiclass_ks_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    data <- selected_multiclass_data()
    ks_data <- ks_table(data)
    selected <- ks_at_threshold(data, selected_multiclass_threshold())
    max_ks <- ks_data %>%
      filter(!is.na(KS)) %>%
      slice_max(KS, n = 1, with_ties = FALSE)

    plot <- plot_ly() %>%
      add_trace(
        data = ks_data, x = ~score, y = ~TPR, text = ~Tooltip,
        type = "scatter", mode = "lines", name = "TPR",
        hoverinfo = "text", line = list(color = theme_colors$bright_blue, width = 2.3)
      ) %>%
      add_trace(
        data = ks_data, x = ~score, y = ~FPR, text = ~Tooltip,
        type = "scatter", mode = "lines", name = "FPR",
        hoverinfo = "text", line = list(color = "#C43131", width = 2.3)
      ) %>%
      add_trace(
        data = selected, x = ~score, y = ~TPR, text = ~Tooltip,
        type = "scatter", mode = "markers", name = "Selected Threshold",
        hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      )

    if (nrow(max_ks) > 0) {
      plot <- plot %>%
        add_trace(
          x = c(max_ks$score, max_ks$score), y = c(0, 1),
          type = "scatter", mode = "lines", name = "Max KS Threshold",
          hoverinfo = "text", text = max_ks$Tooltip,
          line = list(color = "#F7893B", dash = "dash", width = 2)
        ) %>%
        add_annotations(
          x = max_ks$score, y = 1,
          text = paste0("Max KS: ", scales::number(max_ks$KS, accuracy = 0.001)),
          showarrow = TRUE, arrowhead = 2, ax = 20, ay = -28
        )
    }

    plot %>%
      layout(
        xaxis = continuous_x_axis(paste("Threshold for", selected_multiclass_class())),
        yaxis = list(title = "Cumulative Distribution", range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(t = 36, b = 80, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_mcc_threshold_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    data <- selected_multiclass_data()
    threshold <- selected_multiclass_threshold()
    mcc_values <- mcc_threshold_table(data)
    current <- tibble(
      threshold = threshold,
      MCC = mcc_value(data, threshold),
      Tooltip = paste0(
        "<b>Selected Class: ", selected_multiclass_class(), "</b>",
        "<br>Selected Threshold: ", scales::number(threshold, accuracy = 0.001),
        "<br>MCC: ", format_metric(mcc_value(data, threshold))
      )
    )
    best <- mcc_values %>%
      filter(!is.na(MCC)) %>%
      slice_max(MCC, n = 1, with_ties = FALSE)

    plot <- plot_ly(mcc_values, x = ~threshold, y = ~MCC, text = ~Tooltip) %>%
      add_trace(
        type = "scatter", mode = "lines",
        name = "MCC", hoverinfo = "text",
        line = list(color = theme_colors$bright_blue, width = 2.3)
      ) %>%
      add_trace(
        data = current, x = ~threshold, y = ~MCC, text = ~Tooltip,
        type = "scatter", mode = "markers",
        name = "Selected Threshold", hoverinfo = "text",
        marker = list(color = "white", line = list(color = theme_colors$navy, width = 3), size = 12)
      )

    if (nrow(best) > 0) {
      plot <- plot %>%
        add_trace(
          data = best, x = ~threshold, y = ~MCC, text = ~Tooltip,
          type = "scatter", mode = "markers",
          name = "Best MCC Threshold", hoverinfo = "text",
          marker = list(color = "#F7893B", line = list(color = theme_colors$navy, width = 1), size = 11)
        )
    }

    plot %>%
      layout(
        xaxis = continuous_x_axis(paste("Threshold for", selected_multiclass_class())),
        yaxis = list(title = "MCC", range = c(-1, 1)),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(t = 26, b = 80, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$multiclass_mcc_summary <- renderUI({
    req(input$analysis_type == "classification")
    config <- analysis_config()
    req(config$type == "multiclass")
    data <- selected_multiclass_data()
    threshold <- selected_multiclass_threshold()
    mcc_values <- mcc_threshold_table(data)
    current_mcc <- mcc_value(data, threshold)
    best <- mcc_values %>%
      filter(!is.na(MCC)) %>%
      slice_max(MCC, n = 1, with_ties = FALSE)

    best_mcc <- if (nrow(best) == 0) NA_real_ else best$MCC[[1]]
    best_threshold <- if (nrow(best) == 0) NA_real_ else best$threshold[[1]]

    div(
      class = "mini-kpi-grid",
      div(
        class = "mini-kpi",
        tags$p("Current MCC"),
        tags$strong(format_metric(current_mcc)),
        tags$span(paste("Thr.", scales::number(threshold, accuracy = 0.001)))
      ),
      div(
        class = "mini-kpi",
        tags$p("Best MCC"),
        tags$strong(format_metric(best_mcc)),
        tags$span(paste("Thr.", format_metric(best_threshold)))
      )
    )
  })

  regression_plot_data <- reactive({
    config <- regression_config()
    config$data %>%
      mutate(
        PointStatus = case_when(
          config$show_outliers & is_outlier ~ "Outlier",
          within_tolerance ~ "Within Tolerance",
          TRUE ~ "Outside Tolerance"
        ),
        PointStatus = factor(PointStatus, levels = c("Within Tolerance", "Outside Tolerance", "Outlier"))
      )
  })

  output$regression_log_notice <- renderUI({
    config <- regression_config()
    if (config$log_requested && !config$log_available) {
      tags$p(
        class = "notice",
        "Log scale was requested but ignored because Actual or Predicted contains values less than or equal to zero."
      )
    } else {
      NULL
    }
  })

  output$regression_actual_predicted_plot <- renderPlotly({
    config <- regression_config()
    data <- regression_plot_data()
    range_values <- regression_range(data)
    line_values <- if (config$log_scale) {
      positive_values <- c(data$actual, data$predicted)
      positive_values <- positive_values[positive_values > 0]
      range(positive_values, na.rm = TRUE)
    } else {
      range_values
    }
    axis_type <- if (config$log_scale) "log" else "linear"

    plot_ly() %>%
      add_trace(
        data = data,
        x = ~actual,
        y = ~predicted,
        color = ~PointStatus,
        colors = c(
          "Within Tolerance" = theme_colors$bright_blue,
          "Outside Tolerance" = "#F7893B",
          "Outlier" = "#C43131"
        ),
        type = "scatter",
        mode = "markers",
        text = ~Tooltip,
        hoverinfo = "text",
        marker = list(size = 7, opacity = 0.72)
      ) %>%
      add_trace(
        x = line_values,
        y = line_values,
        type = "scatter",
        mode = "lines",
        name = "Perfect Prediction",
        hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash", width = 2),
        inherit = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Actual", type = axis_type, range = if (config$log_scale) NULL else range_values),
        yaxis = list(title = "Predicted", type = axis_type, range = if (config$log_scale) NULL else range_values),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$regression_residual_plot <- renderPlotly({
    config <- regression_config()
    data <- regression_plot_data()
    xaxis <- list(title = "Predicted", type = if (config$log_scale) "log" else "linear")
    status_colors <- c(
      "Within Tolerance" = theme_colors$bright_blue,
      "Outside Tolerance" = "#F7893B",
      "Outlier" = "#C43131"
    )
    segment_data <- data %>%
      filter(is.finite(predicted), is.finite(error)) %>%
      mutate(segment_id = row_number()) %>%
      select(segment_id, predicted, error, PointStatus, Tooltip) %>%
      tidyr::uncount(3, .id = "segment_point") %>%
      mutate(
        x = if_else(segment_point == 3, NA_real_, predicted),
        y = case_when(
          segment_point == 1 ~ 0,
          segment_point == 2 ~ error,
          TRUE ~ NA_real_
        ),
        Tooltip = if_else(segment_point == 3, NA_character_, Tooltip)
      )

    plot_ly() %>%
      add_trace(
        data = segment_data,
        x = ~x,
        y = ~y,
        color = ~PointStatus,
        colors = status_colors,
        type = "scatter",
        mode = "lines",
        text = ~Tooltip,
        hoverinfo = "text",
        line = list(width = 1.6),
        opacity = 0.72
      ) %>%
      add_trace(
        x = range(data$predicted, na.rm = TRUE),
        y = c(0, 0),
        type = "scatter",
        mode = "lines",
        name = "Zero Error",
        hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash", width = 2),
        inherit = FALSE
      ) %>%
      layout(
        xaxis = xaxis,
        yaxis = list(title = "Residual (Predicted - Actual)", zeroline = TRUE),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$regression_residual_distribution_plot <- renderPlotly({
    data <- regression_config()$data
    residual_mean <- mean(data$error, na.rm = TRUE)
    residual_median <- median(data$error, na.rm = TRUE)
    x_range <- range(data$error, na.rm = TRUE)

    plot_ly() %>%
      add_histogram(
        data = data,
        x = ~error,
        name = "Residuals",
        marker = list(color = theme_colors$bright_blue, line = list(color = "white", width = 1)),
        opacity = 0.72,
        nbinsx = 35,
        hovertemplate = "Residual bin: %{x}<br>Count: %{y}<extra></extra>"
      ) %>%
      add_trace(
        x = c(0, 0),
        y = c(0, 1),
        yaxis = "y2",
        type = "scatter",
        mode = "lines",
        name = "Zero Error",
        hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash", width = 2)
      ) %>%
      add_trace(
        x = c(residual_mean, residual_mean),
        y = c(0, 1),
        yaxis = "y2",
        type = "scatter",
        mode = "lines",
        name = "Mean Error",
        hovertemplate = paste0("Mean Error: ", scales::number(residual_mean, accuracy = 0.001), "<extra></extra>"),
        line = list(color = "#F7893B", width = 2)
      ) %>%
      add_trace(
        x = c(residual_median, residual_median),
        y = c(0, 1),
        yaxis = "y2",
        type = "scatter",
        mode = "lines",
        name = "Median Error",
        hovertemplate = paste0("Median Error: ", scales::number(residual_median, accuracy = 0.001), "<extra></extra>"),
        line = list(color = theme_colors$navy, width = 2, dash = "dot")
      ) %>%
      layout(
        xaxis = list(title = "Residual", range = x_range),
        yaxis = list(title = "Count"),
        yaxis2 = list(overlaying = "y", side = "right", visible = FALSE, range = c(0, 1)),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 20),
        barmode = "overlay"
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$regression_error_cdf_plot <- renderPlotly({
    config <- regression_config()
    cdf_data <- regression_error_cdf_table(config$data)
    within_share <- mean(config$data$within_tolerance, na.rm = TRUE)

    plot_ly() %>%
      add_trace(
        data = cdf_data,
        x = ~absolute_error,
        y = ~cumulative_share,
        text = ~Tooltip,
        type = "scatter",
        mode = "lines",
        name = "Error CDF",
        hoverinfo = "text",
        line = list(color = theme_colors$bright_blue, width = 2.3)
      ) %>%
      add_trace(
        x = c(config$tolerance, config$tolerance),
        y = c(0, within_share),
        type = "scatter",
        mode = "lines",
        name = "Selected Tolerance",
        hovertemplate = paste0(
          "Tolerance: ", scales::number(config$tolerance, accuracy = 0.001),
          "<br>Within Tolerance: ", scales::percent(within_share, accuracy = 0.1),
          "<extra></extra>"
        ),
        line = list(color = "#F7893B", dash = "dash", width = 2)
      ) %>%
      layout(
        xaxis = list(title = "Absolute Error", rangemode = "tozero"),
        yaxis = list(title = "Cumulative Share", range = c(0, 1), tickformat = ".0%"),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$regression_calibration_plot <- renderPlotly({
    config <- regression_config()
    data <- regression_calibration_table(config$data, config$bins)
    range_values <- regression_range(tibble(actual = data$`Mean Actual`, predicted = data$`Mean Predicted`))
    line_values <- if (config$log_scale) {
      positive_values <- c(data$`Mean Actual`, data$`Mean Predicted`)
      positive_values <- positive_values[positive_values > 0]
      range(positive_values, na.rm = TRUE)
    } else {
      range_values
    }
    axis_type <- if (config$log_scale) "log" else "linear"

    plot_ly() %>%
      add_trace(
        data = data,
        x = ~`Mean Predicted`,
        y = ~`Mean Actual`,
        text = ~Tooltip,
        type = "scatter",
        mode = "lines+markers",
        name = "Calibration",
        hoverinfo = "text",
        line = list(color = theme_colors$bright_blue, width = 2.3),
        marker = list(color = theme_colors$bright_blue, size = 8)
      ) %>%
      add_trace(
        x = line_values,
        y = line_values,
        type = "scatter",
        mode = "lines",
        name = "Perfect Calibration",
        hoverinfo = "skip",
        line = list(color = "#D3DCE6", dash = "dash", width = 2),
        inherit = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Mean Predicted", type = axis_type, range = if (config$log_scale) NULL else range_values),
        yaxis = list(title = "Mean Actual", type = axis_type, range = if (config$log_scale) NULL else range_values),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$regression_quantile_plot <- renderPlotly({
    config <- regression_config()
    quantiles <- regression_quantile_table(config$data, config$bins, config$quantile_sort)
    metric_column <- regression_metric_column(config$main_metric)
    if (!metric_column %in% names(quantiles)) {
      metric_column <- "RMSE"
    }
    metric_values <- quantiles[[metric_column]]
    is_percent_metric <- metric_column %in% c("WAPE", "MAPE", "sMAPE")
    quantiles <- quantiles %>%
      mutate(
        SelectedMetric = metric_values,
        Tooltip = paste0(
          Tooltip,
          "<br>Selected Metric (", metric_column, "): ",
          if (is_percent_metric) {
            scales::percent(SelectedMetric, accuracy = 0.1)
          } else {
            scales::number(SelectedMetric, accuracy = 0.001)
          }
        )
      )

    plot_ly(
      quantiles,
      x = ~factor(Quantile),
      y = ~SelectedMetric,
      customdata = ~Tooltip,
      hovertemplate = "%{customdata}<extra></extra>",
      type = "bar",
      name = metric_column,
      marker = list(color = theme_colors$bright_blue, opacity = 0.78)
    ) %>%
      layout(
        xaxis = list(title = paste("Quantile sorted by", str_to_title(config$quantile_sort))),
        yaxis = list(
          title = metric_column,
          tickformat = if (is_percent_metric) ".0%" else NULL,
          rangemode = "tozero"
        ),
        margin = list(t = 28, b = 78, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$regression_absolute_error_plot <- renderPlotly({
    config <- regression_config()
    data <- regression_plot_data()
    xaxis <- list(title = "Actual", type = if (config$log_scale) "log" else "linear")

    plot_ly(
      data,
      x = ~actual,
      y = ~absolute_error,
      color = ~PointStatus,
      colors = c(
        "Within Tolerance" = theme_colors$bright_blue,
        "Outside Tolerance" = "#F7893B",
        "Outlier" = "#C43131"
      ),
      type = "scatter",
      mode = "markers",
      text = ~Tooltip,
      hoverinfo = "text",
      marker = list(size = 7, opacity = 0.72)
    ) %>%
      layout(
        xaxis = xaxis,
        yaxis = list(title = "Absolute Error", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 28, b = 78, l = 64, r = 20)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$regression_metrics_table <- renderTable({
    config <- regression_config()
    regression_metric_table(config$data, config$tolerance)
  }, striped = TRUE, bordered = FALSE, spacing = "m", align = "l")

  output$regression_quantile_table <- DT::renderDT({
    config <- regression_config()
    display <- regression_quantile_table(config$data, config$bins, config$quantile_sort) %>%
      select(
        Quantile,
        Observations,
        `Mean Actual`,
        `Mean Predicted`,
        MAE,
        RMSE,
        MedAE,
        Bias,
        R2,
        WAPE,
        MAPE,
        sMAPE
      )

    DT::datatable(
      display,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(display), scrollX = TRUE)
    ) %>%
      DT::formatRound(c("Mean Actual", "Mean Predicted", "MAE", "RMSE", "MedAE", "Bias", "R2"), digits = 3) %>%
      DT::formatPercentage(c("WAPE", "MAPE", "sMAPE"), digits = 1)
  })

  output$matrix_plot <- renderPlotly({
    req(input$analysis_type == "classification")
    suppressWarnings(ggplotly(confusion_plot(analysis_data()), tooltip = "text")) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  output$metrics_table <- renderTable({
    req(input$analysis_type == "classification")
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

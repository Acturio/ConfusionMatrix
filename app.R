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

continuous_x_axis <- function(title, percent = FALSE) {
  axis <- list(
    title = title,
    range = c(0, 1),
    tick0 = 0,
    dtick = 0.1
  )

  if (percent) {
    axis$tickformat <- ".0%"
  }

  axis
}

binary_metric_table <- function(data) {
  tibble(
    Metric = c(
      "Accuracy", "Sensitivity", "Specificity", "Precision",
      "Recall", "F1 Score", "MCC", "ROC AUC", "PR AUC"
    ),
    Value = c(
      metric_value(accuracy_vec, data$truth, data$estimate),
      metric_value(sens_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(spec_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(ppv_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(recall_vec, data$truth, data$estimate, event_level = "first"),
      metric_value(f_meas_vec, data$truth, data$estimate, event_level = "first"),
      mcc_value(data),
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

multiclass_focus_class <- function(config, selected_class = NULL) {
  if (!is.null(selected_class) && selected_class %in% config$classes) {
    return(selected_class)
  }

  config$classes[[1]]
}

multiclass_focus_data <- function(config, selected_class = NULL, threshold = NULL) {
  class_name <- multiclass_focus_class(config, selected_class)
  score <- as.numeric(config$data[[class_name]])
  predicted_focus <- if (is.null(threshold)) {
    config$data$estimate == class_name
  } else {
    score >= threshold
  }

  config$data %>%
    transmute(
      truth = factor(if_else(truth == class_name, class_name, "Rest"),
        levels = c(class_name, "Rest")
      ),
      estimate = factor(if_else(predicted_focus, class_name, "Rest"),
        levels = c(class_name, "Rest")
      ),
      score = score
    )
}

multiclass_lift_gains_table <- function(data, classes) {
  map_dfr(classes, function(class_name) {
    one_vs_rest_data(data, class_name, data[[class_name]]) %>%
      lift_gains_table() %>%
      mutate(Class = class_name)
  })
}

multiclass_probability_heatmap_table <- function(data, classes) {
  data %>%
    select(truth, all_of(classes)) %>%
    group_by(`Actual Class` = truth) %>%
    summarise(across(all_of(classes), mean), .groups = "drop") %>%
    pivot_longer(
      cols = all_of(classes),
      names_to = "Probability Class",
      values_to = "Mean Probability"
    ) %>%
    mutate(
      Tooltip = paste0(
        "<b>Actual Class: ", `Actual Class`, "</b>",
        "<br>Probability Class: ", `Probability Class`,
        "<br>Mean Probability: ", scales::percent(`Mean Probability`, accuracy = 0.1)
      )
    )
}

multiclass_confidence_table <- function(data, classes) {
  max_probability <- do.call(pmax, c(as.list(data[classes]), list(na.rm = TRUE)))

  data %>%
    mutate(
      `Max Probability` = max_probability,
      Result = if_else(truth == estimate, "Correct", "Incorrect"),
      Tooltip = paste0(
        "<b>Prediction Result: ", Result, "</b>",
        "<br>Actual Class: ", truth,
        "<br>Predicted Class: ", estimate,
        "<br>Max Probability: ", scales::percent(`Max Probability`, accuracy = 0.1)
      )
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
    "<br>MCC: ", metrics[["MCC"]],
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
  bin_starts <- head(breaks, -1)
  bin_ends <- tail(breaks, -1)
  labels <- paste0(
    "[", scales::percent(bin_starts, accuracy = 1),
    ", ", scales::percent(bin_ends, accuracy = 1),
    if_else(bin_ends == 1, "]", ")")
  )
  bin_lookup <- tibble(
    score_bin = factor(labels, levels = labels),
    bin_start = bin_starts,
    bin_end = bin_ends,
    bin_midpoint = (bin_starts + bin_ends) / 2
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
    left_join(bin_lookup, by = "score_bin") %>%
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

safe_divide <- function(numerator, denominator, default = NA_real_) {
  output_length <- max(length(numerator), length(denominator), length(default))
  numerator <- rep(numerator, length.out = output_length)
  denominator <- rep(denominator, length.out = output_length)
  default <- rep(default, length.out = output_length)
  invalid <- denominator == 0 | is.na(denominator)
  result <- numerator / denominator
  result[invalid] <- default[invalid]
  result
}

binary_positive <- function(data) {
  data$truth == levels(data$truth)[[1]]
}

binary_confusion_counts <- function(data, threshold = NULL) {
  positive <- binary_positive(data)
  predicted_positive <- if (is.null(threshold)) {
    data$estimate == levels(data$truth)[[1]]
  } else {
    data$score >= threshold
  }

  tibble(
    TP = sum(predicted_positive & positive, na.rm = TRUE),
    TN = sum(!predicted_positive & !positive, na.rm = TRUE),
    FP = sum(predicted_positive & !positive, na.rm = TRUE),
    FN = sum(!predicted_positive & positive, na.rm = TRUE)
  )
}

mcc_from_counts <- function(tp, tn, fp, fn) {
  tp <- as.numeric(tp)
  tn <- as.numeric(tn)
  fp <- as.numeric(fp)
  fn <- as.numeric(fn)
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if_else(denominator == 0 | is.na(denominator), 0, (tp * tn - fp * fn) / denominator)
}

mcc_value <- function(data, threshold = NULL) {
  counts <- binary_confusion_counts(data, threshold)
  mcc_from_counts(counts$TP, counts$TN, counts$FP, counts$FN)
}

binary_sorted_data <- function(data) {
  data %>%
    mutate(
      row_id = row_number(),
      positive = binary_positive(data)
    ) %>%
    arrange(desc(score), row_id)
}

threshold_capture_summary <- function(data, threshold) {
  positive <- binary_positive(data)
  selected <- data$score >= threshold
  total_count <- nrow(data)
  total_positive <- sum(positive, na.rm = TRUE)
  selected_count <- sum(selected, na.rm = TRUE)
  captured_positive <- sum(selected & positive, na.rm = TRUE)
  prevalence <- safe_divide(total_positive, total_count)
  precision <- safe_divide(captured_positive, selected_count)

  tibble(
    threshold = threshold,
    population_count = selected_count,
    population_pct = safe_divide(selected_count, total_count, 0),
    captured_positive = captured_positive,
    cumulative_recall = safe_divide(captured_positive, total_positive, 0),
    cumulative_precision = precision,
    lift = safe_divide(precision, prevalence),
    Tooltip = paste0(
      "<b>Selected Threshold: ", scales::number(threshold, accuracy = 0.001), "</b>",
      "<br>Cumulative Population: ", scales::percent(safe_divide(selected_count, total_count, 0), accuracy = 0.1),
      "<br>Captured Positives: ", captured_positive, " / ", total_positive,
      "<br>Cumulative Recall: ", scales::percent(safe_divide(captured_positive, total_positive, 0), accuracy = 0.1),
      "<br>Cumulative Precision: ", scales::percent(precision, accuracy = 0.1),
      "<br>Lift: ", scales::number(safe_divide(precision, prevalence), accuracy = 0.001)
    )
  )
}

lift_gains_table <- function(data) {
  sorted <- binary_sorted_data(data)
  total_count <- nrow(sorted)
  total_positive <- sum(sorted$positive, na.rm = TRUE)
  prevalence <- safe_divide(total_positive, total_count)

  gains <- sorted %>%
    mutate(
      rank = row_number(),
      cumulative_population = safe_divide(rank, total_count, 0),
      cumulative_positive = cumsum(positive),
      cumulative_recall = safe_divide(cumulative_positive, total_positive, 0),
      cumulative_precision = safe_divide(cumulative_positive, rank),
      cumulative_lift = safe_divide(cumulative_precision, prevalence),
      approx_threshold = score,
      Tooltip = paste0(
        "<b>Cumulative Population: ", scales::percent(cumulative_population, accuracy = 0.1), "</b>",
        "<br>Approx Threshold: ", scales::number(approx_threshold, accuracy = 0.001),
        "<br>Captured Positives: ", cumulative_positive, " / ", total_positive,
        "<br>Cumulative Recall: ", scales::percent(cumulative_recall, accuracy = 0.1),
        "<br>Cumulative Precision: ", scales::percent(cumulative_precision, accuracy = 0.1),
        "<br>Lift: ", scales::number(cumulative_lift, accuracy = 0.001)
      )
    )

  bind_rows(
    tibble(
      rank = 0,
      cumulative_population = 0,
      cumulative_positive = 0,
      cumulative_recall = 0,
      cumulative_precision = NA_real_,
      cumulative_lift = NA_real_,
      approx_threshold = NA_real_,
      Tooltip = "<b>Cumulative Population: 0%</b>"
    ),
    gains %>% select(rank, cumulative_population, cumulative_positive, cumulative_recall,
      cumulative_precision, cumulative_lift, approx_threshold, Tooltip
    )
  )
}

ks_table <- function(data) {
  positive <- binary_positive(data)
  total_positive <- sum(positive, na.rm = TRUE)
  total_negative <- sum(!positive, na.rm = TRUE)

  data %>%
    transmute(score, positive) %>%
    group_by(score) %>%
    summarise(
      positive_count = sum(positive, na.rm = TRUE),
      negative_count = sum(!positive, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(score)) %>%
    mutate(
      TPR = safe_divide(cumsum(positive_count), total_positive, 0),
      FPR = safe_divide(cumsum(negative_count), total_negative, 0),
      KS = abs(TPR - FPR),
      Tooltip = paste0(
        "<b>Threshold: ", scales::number(score, accuracy = 0.001), "</b>",
        "<br>TPR: ", scales::number(TPR, accuracy = 0.001),
        "<br>FPR: ", scales::number(FPR, accuracy = 0.001),
        "<br>KS: ", scales::number(KS, accuracy = 0.001)
      )
    ) %>%
    arrange(score)
}

ks_at_threshold <- function(data, threshold) {
  positive <- binary_positive(data)
  selected <- data$score >= threshold
  total_positive <- sum(positive, na.rm = TRUE)
  total_negative <- sum(!positive, na.rm = TRUE)
  tpr <- safe_divide(sum(selected & positive, na.rm = TRUE), total_positive, 0)
  fpr <- safe_divide(sum(selected & !positive, na.rm = TRUE), total_negative, 0)

  tibble(
    score = threshold,
    TPR = tpr,
    FPR = fpr,
    KS = abs(tpr - fpr),
    Tooltip = paste0(
      "<b>Selected Threshold: ", scales::number(threshold, accuracy = 0.001), "</b>",
      "<br>TPR: ", scales::number(tpr, accuracy = 0.001),
      "<br>FPR: ", scales::number(fpr, accuracy = 0.001),
      "<br>KS: ", scales::number(abs(tpr - fpr), accuracy = 0.001)
    )
  )
}

current_threshold_decile <- function(data, threshold) {
  total_count <- nrow(data)
  selected_count <- sum(data$score >= threshold, na.rm = TRUE)
  if (total_count == 0) {
    return(NA_integer_)
  }

  pmin(10L, pmax(1L, ceiling(pmax(selected_count, 1) / total_count * 10)))
}

decile_analysis_table <- function(data, threshold) {
  sorted <- binary_sorted_data(data)
  total_count <- nrow(sorted)
  total_positive <- sum(sorted$positive, na.rm = TRUE)
  prevalence <- safe_divide(total_positive, total_count)
  highlighted_decile <- current_threshold_decile(data, threshold)

  sorted %>%
    mutate(decile = pmin(10L, ceiling(row_number() * 10 / total_count))) %>%
    group_by(decile) %>%
    summarise(
      Observations = n(),
      `Average Score` = mean(score),
      Positives = sum(positive, na.rm = TRUE),
      `Positive Rate` = safe_divide(Positives, Observations),
      Lift = safe_divide(`Positive Rate`, prevalence),
      .groups = "drop"
    ) %>%
    complete(
      decile = 1:10,
      fill = list(Observations = 0, Positives = 0)
    ) %>%
    arrange(decile) %>%
    mutate(
      `Average Score` = replace(`Average Score`, Observations == 0, NA_real_),
      `Positive Rate` = replace(`Positive Rate`, Observations == 0, NA_real_),
      Lift = replace(Lift, Observations == 0, NA_real_),
      `Cumulative Positives` = cumsum(Positives),
      `Cumulative Recall` = safe_divide(`Cumulative Positives`, total_positive, 0),
      `Current Threshold Decile` = decile == highlighted_decile,
      Tooltip = paste0(
        "<b>Decile ", decile, "</b>",
        "<br>Observations: ", Observations,
        "<br>Average Score: ", scales::number(`Average Score`, accuracy = 0.001),
        "<br>Positives: ", Positives,
        "<br>Positive Rate: ", scales::percent(`Positive Rate`, accuracy = 0.1),
        "<br>Lift: ", scales::number(Lift, accuracy = 0.001),
        "<br>Cumulative Positives: ", `Cumulative Positives`,
        "<br>Cumulative Recall: ", scales::percent(`Cumulative Recall`, accuracy = 0.1)
      )
    )
}

calibration_table <- function(data) {
  breaks <- seq(0, 1, by = 0.1)
  bin_starts <- head(breaks, -1)
  bin_ends <- tail(breaks, -1)
  labels <- paste0(
    "[", scales::percent(bin_starts, accuracy = 1),
    ", ", scales::percent(bin_ends, accuracy = 1),
    if_else(bin_ends == 1, "]", ")")
  )
  bin_lookup <- tibble(
    score_bin = factor(labels, levels = labels),
    bin_start = bin_starts,
    bin_end = bin_ends,
    bin_midpoint = (bin_starts + bin_ends) / 2
  )

  data %>%
    mutate(
      score_bin = cut(pmin(score, 1 - 1e-12),
        breaks = breaks, labels = labels,
        include.lowest = TRUE, right = FALSE
      ),
      positive = binary_positive(data)
    ) %>%
    group_by(score_bin) %>%
    summarise(
      Observations = n(),
      `Mean Predicted Probability` = mean(score),
      `Observed Positive Rate` = mean(positive),
      .groups = "drop"
    ) %>%
    complete(
      score_bin = factor(labels, levels = labels),
      fill = list(Observations = 0)
    ) %>%
    left_join(bin_lookup, by = "score_bin") %>%
    mutate(
      `Mean Predicted Probability` = replace(`Mean Predicted Probability`, Observations == 0, NA_real_),
      `Observed Positive Rate` = replace(`Observed Positive Rate`, Observations == 0, NA_real_),
      `Calibration Error` = abs(`Observed Positive Rate` - `Mean Predicted Probability`),
      Tooltip = paste0(
        "<b>Bin: ", score_bin, "</b>",
        "<br>Mean Prediction: ", scales::percent(`Mean Predicted Probability`, accuracy = 0.1),
        "<br>Observed Rate: ", scales::percent(`Observed Positive Rate`, accuracy = 0.1),
        "<br>Calibration Error: ", scales::number(`Calibration Error`, accuracy = 0.001),
        "<br>Observations: ", Observations
      )
    )
}

brier_score_value <- function(data) {
  positive <- as.numeric(binary_positive(data))
  if (nrow(data) == 0) {
    return(NA_real_)
  }

  mean((data$score - positive)^2, na.rm = TRUE)
}

mcc_threshold_table <- function(data, thresholds = seq(0, 1, by = 0.01)) {
  map_dfr(thresholds, function(threshold) {
    counts <- binary_confusion_counts(data, threshold)
    value <- mcc_from_counts(counts$TP, counts$TN, counts$FP, counts$FN)
    tibble(
      threshold = threshold,
      MCC = value,
      Tooltip = paste0(
        "<b>Threshold: ", scales::number(threshold, accuracy = 0.001), "</b>",
        "<br>MCC: ", scales::number(value, accuracy = 0.001),
        "<br>TP: ", counts$TP,
        "<br>TN: ", counts$TN,
        "<br>FP: ", counts$FP,
        "<br>FN: ", counts$FN
      )
    )
  })
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
  div(
    class = "dashboard-layout",
    div(
      class = "control-sidebar",
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
        conditionalPanel(
          condition = "input.model_type == 'multiclass'",
          uiOutput("multiclass_focus_controls")
        ),
        actionButton("calculate", "Calculate results", class = "btn-calculate"),
        uiOutput("data_notice")
      )
    ),
    div(
      class = "main-scroll",
      uiOutput("summary_cards"),
      div(
        class = "panel results",
        tags$h2(class = "results__title", "Model Results"),
        fluidRow(
          column(
            6,
            div(
              class = "chart-card",
              plotlyOutput("roc_plot", height = "330px"),
              tags$p(
                class = "chart-description",
                paste(
                  "Use this to evaluate rank separation across thresholds.",
                  "The ROC Curve plots Sensitivity against False Positive Rate; stronger models bend toward the upper-left corner.",
                  "The highlighted marker shows the operating point produced by the current threshold."
                )
              )
            )
          ),
          column(
            6,
            div(
              class = "chart-card",
              plotlyOutput("pr_plot", height = "330px"),
              tags$p(
                class = "chart-description",
                paste(
                  "Use this to inspect the Precision-Recall trade-off, especially when positives are scarce.",
                  "Precision answers how many selected cases are truly positive; Recall answers how many actual positives are captured.",
                  "The dashed diagonal is a visual baseline, and the highlighted marker follows the selected threshold."
                )
              )
            )
          )
        ),
        uiOutput("binary_extra_plots"),
        uiOutput("multiclass_extra_plots"),
        uiOutput("business_validation_insights"),
        uiOutput("multiclass_business_validation_insights"),
        fluidRow(
          column(
            5,
            div(
              class = "chart-card",
              plotlyOutput("matrix_plot", height = "365px"),
              tags$p(
                class = "chart-description",
                paste(
                  "Use this to see the classification counts at the selected threshold.",
                  "The diagonal cells are correct predictions; off-diagonal cells are errors and help identify which classes are confused."
                )
              )
            )
          ),
          column(
            7,
            div(
              class = "metrics-card",
              tags$h3("Performance Metrics"),
              div(class = "metrics-table", tableOutput("metrics_table")),
              tags$p(
                class = "chart-description",
                paste(
                  "Use these metrics to summarize model quality.",
                  "Threshold-based metrics such as Accuracy, Precision, Recall, F1 Score, and MCC update with the slider; ROC AUC and PR AUC summarize ranking performance across thresholds."
                )
              )
            )
          )
        ),
        tags$details(
          class = "data-details",
          tags$summary("Uploaded Data Preview"),
          div(class = "preview-table", tableOutput("preview_table")),
          tags$p(
            class = "chart-description",
            "Use this table to quickly verify that the uploaded file, Actual Class column, and probability columns were mapped as expected."
          )
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

    selectInput("positive_class", "Positive class", choices = classes, selected = 1)
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

  output$multiclass_focus_controls <- renderUI({
    req(input$model_type == "multiclass", input$truth_col)
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
        xaxis = continuous_x_axis("False Positive Rate"),
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
    config <- analysis_config()
    req(config$type == "binary")
    div(
      class = "insight-metric",
      tags$span("Brier Score (lower is better)"),
      tags$strong(format_metric(brier_score_value(analysis_data())))
    )
  })

  output$mcc_threshold_plot <- renderPlotly({
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
    config <- analysis_config()
    req(config$type == "multiclass")
    div(
      class = "insight-metric",
      tags$span(paste("Class", selected_multiclass_class(), "| Brier Score (lower is better)")),
      tags$strong(format_metric(brier_score_value(selected_multiclass_data())))
    )
  })

  output$multiclass_calibration_probability_bin_plot <- renderPlotly({
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

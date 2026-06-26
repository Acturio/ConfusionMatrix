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

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  median(x, na.rm = TRUE)
}

safe_correlation <- function(x, y, method = "pearson") {
  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]

  if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  suppressWarnings(stats::cor(x, y, method = method))
}

format_percent_metric <- function(x) {
  if_else(is.na(x), "-", scales::percent(x, accuracy = 0.1))
}

regression_range <- function(data) {
  values <- c(data$actual, data$predicted)
  values <- values[is.finite(values)]

  if (length(values) == 0) {
    return(c(0, 1))
  }

  range_values <- range(values, na.rm = TRUE)
  padding <- diff(range_values) * 0.05
  if (is.na(padding) || padding == 0) {
    padding <- max(abs(range_values), 1) * 0.05
  }

  range_values + c(-padding, padding)
}

regression_log_available <- function(data) {
  all(data$actual > 0, data$predicted > 0, na.rm = TRUE)
}

regression_outlier_limit <- function(absolute_error) {
  absolute_error <- absolute_error[is.finite(absolute_error)]

  if (length(absolute_error) < 4) {
    return(Inf)
  }

  q1 <- stats::quantile(absolute_error, 0.25, na.rm = TRUE, names = FALSE)
  q3 <- stats::quantile(absolute_error, 0.75, na.rm = TRUE, names = FALSE)
  iqr <- q3 - q1

  if (is.na(iqr) || iqr == 0) {
    return(Inf)
  }

  q3 + 1.5 * iqr
}

regression_add_diagnostics <- function(data, tolerance = 0) {
  outlier_limit <- regression_outlier_limit(abs(data$predicted - data$actual))

  data %>%
    mutate(
      error = predicted - actual,
      absolute_error = abs(error),
      percentage_error = if_else(actual == 0, NA_real_, error / actual),
      APE = abs(percentage_error),
      sAPE = if_else(
        abs(actual) + abs(predicted) == 0,
        NA_real_,
        2 * absolute_error / (abs(actual) + abs(predicted))
      ),
      within_tolerance = absolute_error <= tolerance,
      is_outlier = absolute_error > outlier_limit,
      Tooltip = paste0(
        "<b>Regression Prediction</b>",
        "<br>Actual: ", scales::number(actual, accuracy = 0.001),
        "<br>Predicted: ", scales::number(predicted, accuracy = 0.001),
        "<br>Error: ", scales::number(error, accuracy = 0.001),
        "<br>Absolute Error: ", scales::number(absolute_error, accuracy = 0.001)
      )
    )
}

regression_metric_values <- function(data, tolerance = 0) {
  total_ss <- sum((data$actual - mean(data$actual, na.rm = TRUE))^2, na.rm = TRUE)
  residual_ss <- sum((data$actual - data$predicted)^2, na.rm = TRUE)
  absolute_actual_sum <- sum(abs(data$actual), na.rm = TRUE)

  tibble(
    Metric = c(
      "MAE", "RMSE", "Median Absolute Error", "Bias",
      "R2", "Pearson Correlation", "Spearman Correlation",
      "WAPE", "MAPE", "sMAPE", "% Within Tolerance"
    ),
    Value = c(
      safe_mean(data$absolute_error),
      sqrt(safe_mean(data$error^2)),
      safe_median(data$absolute_error),
      safe_mean(data$error),
      if_else(total_ss == 0, NA_real_, 1 - residual_ss / total_ss),
      safe_correlation(data$actual, data$predicted, method = "pearson"),
      safe_correlation(data$actual, data$predicted, method = "spearman"),
      if_else(absolute_actual_sum == 0, NA_real_, sum(data$absolute_error, na.rm = TRUE) / absolute_actual_sum),
      safe_mean(data$APE),
      safe_mean(data$sAPE),
      safe_mean(data$within_tolerance)
    )
  )
}

regression_metric_table <- function(data, tolerance = 0) {
  regression_metric_values(data, tolerance) %>%
    mutate(
      Value = case_when(
        Metric %in% c("WAPE", "MAPE", "sMAPE", "% Within Tolerance") ~ format_percent_metric(Value),
        TRUE ~ format_metric(Value)
      )
    )
}

regression_error_cdf_table <- function(data) {
  data %>%
    filter(!is.na(absolute_error)) %>%
    arrange(absolute_error) %>%
    mutate(
      cumulative_share = row_number() / n(),
      Tooltip = paste0(
        "<b>Absolute Error: ", scales::number(absolute_error, accuracy = 0.001), "</b>",
        "<br>Cumulative Share: ", scales::percent(cumulative_share, accuracy = 0.1)
      )
    )
}

regression_calibration_table <- function(data, bins = 10) {
  bin_count <- min(max(as.integer(bins), 1), nrow(data))
  if (bin_count < 1) {
    return(tibble())
  }

  data %>%
    arrange(predicted) %>%
    mutate(bin = pmin(bin_count, ceiling(row_number() * bin_count / n()))) %>%
    group_by(bin) %>%
    summarise(
      Observations = n(),
      `Mean Predicted` = mean(predicted),
      `Mean Actual` = mean(actual),
      `Mean Error` = mean(error),
      .groups = "drop"
    ) %>%
    mutate(
      Tooltip = paste0(
        "<b>Bin: ", bin, "</b>",
        "<br>Observations: ", Observations,
        "<br>Mean Prediction: ", scales::number(`Mean Predicted`, accuracy = 0.001),
        "<br>Mean Actual: ", scales::number(`Mean Actual`, accuracy = 0.001),
        "<br>Mean Error: ", scales::number(`Mean Error`, accuracy = 0.001)
      )
    )
}

regression_quantile_table <- function(data, bins = 10, sort_by = "actual") {
  bin_count <- min(max(as.integer(bins), 1), nrow(data))
  sort_column <- if_else(sort_by == "predicted", "predicted", "actual")

  if (bin_count < 1) {
    return(tibble())
  }

  data %>%
    arrange(.data[[sort_column]]) %>%
    mutate(Quantile = pmin(bin_count, ceiling(row_number() * bin_count / n()))) %>%
    group_by(Quantile) %>%
    summarise(
      Observations = n(),
      `Mean Actual` = mean(actual),
      `Mean Predicted` = mean(predicted),
      MAE = mean(absolute_error),
      RMSE = sqrt(mean(error^2)),
      MedAE = median(absolute_error),
      Bias = mean(error),
      R2 = {
        total_ss <- sum((actual - mean(actual))^2)
        residual_ss <- sum((actual - predicted)^2)
        if (total_ss == 0) NA_real_ else 1 - residual_ss / total_ss
      },
      WAPE = if_else(sum(abs(actual)) == 0, NA_real_, sum(absolute_error) / sum(abs(actual))),
      MAPE = safe_mean(APE),
      sMAPE = safe_mean(sAPE),
      .groups = "drop"
    ) %>%
    mutate(
      Tooltip = paste0(
        "<b>Quantile: ", Quantile, "</b>",
        "<br>Observations: ", Observations,
        "<br>Mean Actual: ", scales::number(`Mean Actual`, accuracy = 0.001),
        "<br>Mean Predicted: ", scales::number(`Mean Predicted`, accuracy = 0.001),
        "<br>MAE: ", scales::number(MAE, accuracy = 0.001),
        "<br>RMSE: ", scales::number(RMSE, accuracy = 0.001),
        "<br>Bias: ", scales::number(Bias, accuracy = 0.001),
        "<br>R2: ", scales::number(R2, accuracy = 0.001),
        "<br>WAPE: ", scales::percent(WAPE, accuracy = 0.1),
        "<br>MAPE: ", scales::percent(MAPE, accuracy = 0.1)
      )
    )
}

regression_metric_column <- function(metric) {
  recode(
    metric,
    "Median Absolute Error" = "MedAE",
    "MedAE" = "MedAE",
    .default = metric
  )
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

compact_numeric <- function(x, digits = 5) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  round(as.numeric(x), digits)
}

compact_named_vector <- function(x, digits = 5) {
  x <- as.list(x)
  map(x, ~ if (is.numeric(.x)) compact_numeric(.x, digits) else .x)
}

confusion_matrix_payload <- function(data) {
  data %>%
    count(actual = truth, predicted = estimate, name = "count") %>%
    arrange(actual, predicted) %>%
    mutate(
      actual = as.character(actual),
      predicted = as.character(predicted)
    )
}

class_distribution_payload <- function(data) {
  data %>%
    count(class = truth, name = "count") %>%
    mutate(
      class = as.character(class),
      share = compact_numeric(count / sum(count))
    )
}

binary_log_loss_value <- function(data) {
  score <- pmin(pmax(data$score, 1e-15), 1 - 1e-15)
  positive <- as.numeric(binary_positive(data))

  compact_numeric(-mean(positive * log(score) + (1 - positive) * log(1 - score), na.rm = TRUE))
}

binary_report_metrics <- function(data) {
  sensitivity <- metric_value(sens_vec, data$truth, data$estimate, event_level = "first")
  specificity <- metric_value(spec_vec, data$truth, data$estimate, event_level = "first")

  list(
    accuracy = compact_numeric(metric_value(accuracy_vec, data$truth, data$estimate)),
    balanced_accuracy = compact_numeric(mean(c(sensitivity, specificity), na.rm = TRUE)),
    precision = compact_numeric(metric_value(ppv_vec, data$truth, data$estimate, event_level = "first")),
    recall = compact_numeric(metric_value(recall_vec, data$truth, data$estimate, event_level = "first")),
    specificity = compact_numeric(specificity),
    f1 = compact_numeric(metric_value(f_meas_vec, data$truth, data$estimate, event_level = "first")),
    mcc = compact_numeric(mcc_value(data)),
    auc = compact_numeric(metric_value(roc_auc_vec, data$truth, data$score, event_level = "first")),
    pr_auc = compact_numeric(metric_value(pr_auc_vec, data$truth, data$score, event_level = "first")),
    log_loss = binary_log_loss_value(data)
  )
}

multiclass_report_metrics <- function(data, classes) {
  per_class <- multiclass_metric_values(data, classes)
  weights <- data %>%
    count(Class = truth, name = "support") %>%
    mutate(Class = as.character(Class))
  per_class_weighted <- per_class %>%
    left_join(weights, by = "Class")

  list(
    accuracy = compact_numeric(metric_value(accuracy_vec, data$truth, data$estimate)),
    balanced_accuracy = compact_numeric(mean(per_class$Sensitivity, na.rm = TRUE)),
    macro_precision = compact_numeric(mean(per_class$Precision, na.rm = TRUE)),
    macro_recall = compact_numeric(mean(per_class$Sensitivity, na.rm = TRUE)),
    macro_f1 = compact_numeric(mean(per_class$`F1 Score`, na.rm = TRUE)),
    weighted_precision = compact_numeric(weighted.mean(per_class_weighted$Precision, per_class_weighted$support, na.rm = TRUE)),
    weighted_recall = compact_numeric(weighted.mean(per_class_weighted$Sensitivity, per_class_weighted$support, na.rm = TRUE)),
    weighted_f1 = compact_numeric(weighted.mean(per_class_weighted$`F1 Score`, per_class_weighted$support, na.rm = TRUE)),
    per_class = per_class_weighted %>%
      transmute(
        class = Class,
        support,
        precision = compact_numeric(Precision),
        recall = compact_numeric(Sensitivity),
        specificity = compact_numeric(Specificity),
        f1 = compact_numeric(`F1 Score`),
        roc_auc = compact_numeric(`ROC AUC`),
        pr_auc = compact_numeric(`PR AUC`)
      )
  )
}

regression_report_metrics <- function(data, tolerance = 0, bins = 10, quantile_sort = "predicted") {
  metric_values <- regression_metric_values(data, tolerance) %>%
    mutate(Value = compact_numeric(Value)) %>%
    deframe()
  residuals <- data$error
  absolute_errors <- data$absolute_error
  bin_count <- min(max(as.integer(bins), 1), nrow(data))
  range_diagnostics <- regression_quantile_table(data, bin_count, quantile_sort) %>%
    transmute(
      quantile = Quantile,
      observations = Observations,
      mean_actual = compact_numeric(`Mean Actual`),
      mean_predicted = compact_numeric(`Mean Predicted`),
      mae = compact_numeric(MAE),
      rmse = compact_numeric(RMSE),
      bias = compact_numeric(Bias),
      wape = compact_numeric(WAPE),
      mape = compact_numeric(MAPE)
    )

  list(
    RMSE = metric_values[["RMSE"]],
    MAE = metric_values[["MAE"]],
    R2 = metric_values[["R2"]],
    MAPE = metric_values[["MAPE"]],
    WAPE = metric_values[["WAPE"]],
    mean_error = metric_values[["Bias"]],
    median_absolute_error = metric_values[["Median Absolute Error"]],
    within_tolerance = metric_values[["% Within Tolerance"]],
    residual_distribution = list(
      mean = compact_numeric(mean(residuals, na.rm = TRUE)),
      median = compact_numeric(median(residuals, na.rm = TRUE)),
      sd = compact_numeric(stats::sd(residuals, na.rm = TRUE)),
      min = compact_numeric(min(residuals, na.rm = TRUE)),
      max = compact_numeric(max(residuals, na.rm = TRUE)),
      q05 = compact_numeric(stats::quantile(residuals, 0.05, na.rm = TRUE, names = FALSE)),
      q25 = compact_numeric(stats::quantile(residuals, 0.25, na.rm = TRUE, names = FALSE)),
      q75 = compact_numeric(stats::quantile(residuals, 0.75, na.rm = TRUE, names = FALSE)),
      q95 = compact_numeric(stats::quantile(residuals, 0.95, na.rm = TRUE, names = FALSE))
    ),
    absolute_error_percentiles = list(
      p50 = compact_numeric(stats::quantile(absolute_errors, 0.50, na.rm = TRUE, names = FALSE)),
      p75 = compact_numeric(stats::quantile(absolute_errors, 0.75, na.rm = TRUE, names = FALSE)),
      p90 = compact_numeric(stats::quantile(absolute_errors, 0.90, na.rm = TRUE, names = FALSE)),
      p95 = compact_numeric(stats::quantile(absolute_errors, 0.95, na.rm = TRUE, names = FALSE)),
      p99 = compact_numeric(stats::quantile(absolute_errors, 0.99, na.rm = TRUE, names = FALSE))
    ),
    performance_by_range = range_diagnostics
  )
}

automatic_report_observations <- function(payload) {
  observations <- character()

  if (payload$tipo_modelo == "binary_classification") {
    observations <- c(
      observations,
      paste("Metrics are computed at the selected probability threshold:", payload$threshold_actual),
      paste("Positive class:", payload$positive_class)
    )
    if (!is.na(payload$metrics$balanced_accuracy) && payload$metrics$balanced_accuracy < 0.65) {
      observations <- c(observations, "Balanced accuracy is relatively low; review class separation and threshold choice.")
    }
    if (!is.na(payload$metrics$precision) && !is.na(payload$metrics$recall) &&
      abs(payload$metrics$precision - payload$metrics$recall) > 0.2) {
      observations <- c(observations, "Precision and recall differ materially, so the operating threshold should be tied to business cost.")
    }
  }

  if (payload$tipo_modelo == "multiclass_classification") {
    weak_classes <- payload$metrics$per_class %>%
      filter(is.na(f1) | f1 == min(f1, na.rm = TRUE)) %>%
      pull(class)
    observations <- c(observations, paste("Weakest class by F1:", paste(weak_classes, collapse = ", ")))
  }

  if (payload$tipo_modelo == "regression") {
    mean_error <- payload$metrics$mean_error
    observations <- c(
      observations,
      paste("Mean error is", mean_error, "where positive values indicate overprediction and negative values indicate underprediction.")
    )
    if (!is.na(payload$metrics$R2) && payload$metrics$R2 < 0.3) {
      observations <- c(observations, "R2 is low; the model may explain limited variance in the target.")
    }
  }

  observations
}

classification_graph_summaries <- function(model_type) {
  base <- c(
    "Confusion matrix shows the count of correct and incorrect predictions.",
    "ROC and Precision-Recall curves summarize rank quality across thresholds."
  )

  if (model_type == "binary") {
    c(
      base,
      "Probability distribution, calibration, gains, lift, decile, KS, and MCC-vs-threshold diagnostics are available for the selected threshold."
    )
  } else {
    c(
      base,
      "Class performance, probability heatmap, class volume, confidence distribution, selected-class calibration, lift, decile, KS, and MCC diagnostics are available."
    )
  }
}

regression_graph_summaries <- function() {
  c(
    "Actual-vs-predicted plot checks calibration around the diagonal.",
    "Residual plot uses vertical error segments from zero to the residual across predicted values.",
    "Residual distribution, error CDF, regression calibration, absolute error, and quantile performance diagnostics are available."
  )
}

build_llm_report_payload <- function(analysis_type, config, data, inputs = list()) {
  if (analysis_type == "classification" && config$type == "binary") {
    payload <- list(
      tipo_modelo = "binary_classification",
      n_observaciones = nrow(data),
      target = inputs$target,
      prediccion = inputs$prediction,
      threshold_actual = compact_numeric(inputs$threshold),
      positive_class = as.character(config$classes[[1]]),
      negative_class = as.character(config$classes[[2]]),
      distribucion_clases = class_distribution_payload(data),
      matriz_confusion = confusion_matrix_payload(data),
      metrics = binary_report_metrics(data),
      resumen_graficas_disponibles = classification_graph_summaries("binary")
    )
    payload$observaciones_automaticas <- automatic_report_observations(payload)
    return(payload)
  }

  if (analysis_type == "classification" && config$type == "multiclass") {
    payload <- list(
      tipo_modelo = "multiclass_classification",
      n_observaciones = nrow(data),
      target = inputs$target,
      prediccion = "highest_probability_class",
      numero_clases = length(config$classes),
      clases = as.character(config$classes),
      distribucion_clases = class_distribution_payload(data),
      matriz_confusion = confusion_matrix_payload(data),
      metrics = multiclass_report_metrics(data, config$classes),
      resumen_graficas_disponibles = classification_graph_summaries("multiclass")
    )
    payload$observaciones_automaticas <- automatic_report_observations(payload)
    return(payload)
  }

  if (analysis_type == "regression") {
    payload <- list(
      tipo_modelo = "regression",
      n_observaciones = nrow(config$data),
      target = inputs$target,
      prediccion = inputs$prediction,
      tolerance = compact_numeric(config$tolerance),
      bins = config$bins,
      quantile_sort = config$quantile_sort,
      metrics = regression_report_metrics(config$data, config$tolerance, config$bins, config$quantile_sort),
      resumen_graficas_disponibles = regression_graph_summaries()
    )
    payload$observaciones_automaticas <- automatic_report_observations(payload)
    return(payload)
  }

  stop("Unsupported analysis configuration for LLM report.")
}

llm_report_system_prompt <- function(language = "Spanish") {
  paste(
    "You are a senior machine learning model validation analyst.",
    "Write a brief, concrete, interpretive, and actionable model performance report.",
    "Use only the structured payload provided by the application.",
    "Do not invent metrics. Do not assume data that is not present in the payload.",
    "If important information is missing, say so explicitly.",
    "Do not ask follow-up questions.",
    "For binary classification, interpret metrics considering the selected threshold.",
    "For multiclass classification, pay attention to per-class performance, weak classes, macro averages, weighted averages, and confusion matrix if available.",
    "For regression, interpret RMSE, MAE, R2, MAPE, residuals, bias, extreme errors, and performance by ranges if available.",
    "Distinguish technical recommendations from business communication recommendations.",
    "Avoid generic or overly long language.",
    paste("Write the final report in", language, "."),
    "Use exactly this markdown structure:",
    "# Interpretive model report",
    "## 1. Executive summary",
    "## 2. Technical reading of results",
    "## 3. Positive findings",
    "## 4. Negative findings or warning signs",
    "## 5. Improvement areas",
    "## 6. Communication recommendations",
    "## 7. What to review before production",
    "Omit subsection 7 only if there are truly no additional production checks to recommend.",
    sep = "\n"
  )
}

generate_llm_report <- function(payload, model = "qwen2.5:7b", language = "Spanish") {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The R package 'ellmer' is not installed. Install it and make sure Ollama is running locally.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The R package 'jsonlite' is required to build the compact LLM payload.")
  }

  system_prompt <- llm_report_system_prompt(language)
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 5)
  user_prompt <- paste(
    "Analyze this compact model-evaluation payload. It intentionally excludes raw rows and full CSV data.",
    "Return only the markdown report.",
    "",
    payload_json,
    sep = "\n"
  )

  chat <- ellmer::chat_ollama(model = model, system_prompt = system_prompt)

  response <- chat$chat(user_prompt)
  paste(as.character(response), collapse = "\n")
}

markdown_report_to_html <- function(markdown_text) {
  if (is.null(markdown_text) || !nzchar(markdown_text)) {
    return("")
  }

  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(commonmark::markdown_html(markdown_text))
  }

  if (requireNamespace("markdown", quietly = TRUE)) {
    return(markdown::markdownToHTML(text = markdown_text, fragment.only = TRUE))
  }

  paste0("<pre>", htmltools::htmlEscape(markdown_text), "</pre>")
}

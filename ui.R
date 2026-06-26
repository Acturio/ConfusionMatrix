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
        radioButtons(
          "analysis_type",
          "Model task",
          choices = c("Classification" = "classification", "Regression" = "regression"),
          selected = "classification",
          inline = TRUE
        ),
        fileInput(
          "file", "Results file (.csv)",
          accept = c(".csv", "text/csv")
        ),
        conditionalPanel(
          condition = "input.analysis_type == 'classification'",
          radioButtons(
            "model_type",
            "Response type",
            choices = c("Binary" = "binary", "Multiclass" = "multiclass"),
            selected = "binary",
            inline = TRUE
          ),
          uiOutput("column_selectors"),
          conditionalPanel(
            condition = "input.analysis_type == 'classification' && input.model_type == 'binary'",
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
            condition = "input.analysis_type == 'classification' && input.model_type == 'multiclass'",
            uiOutput("multiclass_focus_controls")
          )
        ),
        conditionalPanel(
          condition = "input.analysis_type == 'regression'",
          uiOutput("regression_column_selectors"),
          uiOutput("regression_tolerance_input"),
          selectInput(
            "regression_bins",
            "Bins / quantiles",
            choices = c("5" = 5, "10" = 10, "20" = 20),
            selected = 10
          ),
          selectInput(
            "regression_main_metric",
            "Main metric",
            choices = c("MAE", "RMSE", "MedAE", "Bias", "R2", "WAPE", "MAPE", "sMAPE"),
            selected = "RMSE"
          ),
          selectInput(
            "regression_quantile_sort",
            "Quantile sort",
            choices = c("Actual" = "actual", "Predicted" = "predicted"),
            selected = "actual"
          ),
          checkboxInput("regression_show_outliers", "Show outlier highlight", value = TRUE),
          checkboxInput("regression_log_scale", "Use log scale when valid", value = FALSE)
        ),
        actionButton("calculate", "Calculate results", class = "btn-calculate"),
        uiOutput("data_notice"),
        uiOutput("llm_controls")
      )
    ),
    div(
      class = "main-scroll",
      uiOutput("summary_cards"),
      div(
        class = "panel results",
        tags$h2(class = "results__title", "Model Results"),
        tabsetPanel(
          id = "results_tabs",
          type = "tabs",
          tabPanel(
            "Model Results",
            conditionalPanel(
              condition = "input.analysis_type == 'classification'",
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
              )
            ),
            conditionalPanel(
              condition = "input.analysis_type == 'regression'",
              uiOutput("regression_results")
            ),
            tags$details(
              class = "data-details",
              tags$summary("Uploaded Data Preview"),
              div(class = "preview-table", tableOutput("preview_table")),
              conditionalPanel(
                condition = "input.analysis_type == 'classification'",
                tags$p(
                  class = "chart-description",
                  "Use this table to quickly verify that the uploaded file, Actual Class column, and probability columns were mapped as expected."
                )
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'regression'",
                tags$p(
                  class = "chart-description",
                  "Use this table to quickly verify that the uploaded file, Actual / y_true column, and Predicted / y_pred column were mapped as expected."
                )
              )
            )
          ),
          tabPanel(
            "AI Report",
            uiOutput("llm_report_panel")
          )
        )
      )
    )
  )
)

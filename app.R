library(shiny)
library(tidyverse)
library(caret)
library(yardstick)
library(ggplot2)
library(MLmetrics)

# data <- read.csv("probas.csv") %>% select(prob, response) %>% as_tibble()
# 
# data <- data %>% dplyr::select(prob, response) %>%
#     mutate(response = factor(ifelse(response == 0, "No", "Yes"),
#                           levels = c("Yes","No"), labels = c("Yes","No")))
# 
# roc_tbl <- data %>% roc_curve(truth = response, estimate = prob)
# pr_auc_tbl <- data %>% pr_curve(truth = response, estimate = prob)

ui <- fluidPage(

    titlePanel("Confusion Matrix"),

    sidebarLayout(
        sidebarPanel(
            
            shiny::fileInput("file", "Ingrese un archivo", accept = ".csv"),
            
            sliderInput("tresh",
                        "Treshold Probability:",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.01),
            shiny::verbatimTextOutput("matrix_table")
        ),

        mainPanel(
            plotOutput("PR"),
            plotOutput("ROC")
        )
    )
)

server <- function(input, output) {
    
    data2 <- reactive({

        if(is.null(input$file)) return(NULL)

        archivo <- input$file
        archivo <- read.csv(archivo$datapath, stringsAsFactors = F, fileEncoding = "utf-8")

        archivo %>% dplyr::select(prob, response) %>%
            mutate(
                response = factor(ifelse(
                    response == 0, "No", "Yes"),
                    levels = c("Yes","No"),
                    labels = c("Yes","No")
                ),
                Pred = factor(ifelse(
                    prob > input$tresh, "Yes", "No"),
                    levels = c("Yes","No"),
                    labels = c("Yes","No"))
            )
    })

    roc_tbl <- reactive({
        data2() %>% roc_curve(truth = response, estimate = prob)
    })

    pr_auc_tbl <- reactive({
        data2() %>% pr_curve(truth = response, estimate = prob)
    })
    

    # data2 <- reactive({
    # 
    #     table <- data %>%
    #         mutate(Pred = factor(ifelse(prob > input$tresh, "Yes", "No"),
    #                              levels = c("Yes","No"), labels = c("Yes","No")))
    #     table
    # })

    output$matrix_table <- renderPrint({

        confusionMatrix(data2()$Pred, data2()$response,
                        mode = "prec_recall",
                        positive = "Yes")

    })

    output$ROC <- renderPlot({

        #sens <- MLmetrics::Sensitivity(data2()$Pred, data2()$response, positive = "Yes")
        sens <- yardstick::sens(data2(), truth = response, estimate = Pred)$.estimate
        #tfp <- 1 - MLmetrics::Specificity(data2()$Pred, data2()$response, positive = "Yes")
        tfp <- 1 - yardstick::spec(data2(), truth = response, estimate = Pred)$.estimate

        ggplot(
            #roc_tbl,
            roc_tbl(), 
            aes(x = 1 - specificity, y = sensitivity)) +
            geom_path(aes(colour = .threshold), size = 1.2) +
            geom_abline(colour = "gray") +
            geom_hline(yintercept = sens, linetype = "dashed", color = "red") +
            geom_vline(xintercept = tfp, linetype = "dashed", color = "red") +
            xlab("Tasa de falsos positivos") +
            ylab("Sensibilidad") +
            ggtitle("Curva ROC")

    })

    output$PR <- renderPlot({

        #recall <- MLmetrics::Recall(data2()$Pred, data2()$response, positive = "Yes")
        recall <- yardstick::recall(data2(), Pred, response)$.estimate
        #pre <- MLmetrics::Precision(data2()$Pred, data2()$response, positive = "Yes")
        pre <- yardstick::precision(data2(), Pred, response)$.estimate
        

        ggplot(
            #pr_auc_tbl, 
            pr_auc_tbl(),
            aes(x = recall , y = precision)) +
            geom_path(aes(colour = .threshold), size = 1.2) +
            geom_abline(colour = "gray") +
            geom_hline(yintercept = recall, linetype = "dashed", color = "red") +
            geom_vline(xintercept = pre, linetype = "dashed", color = "red") +
            ylim(c(0, 1)) +
            xlab("Recall (Sensibilidad)") +
            ylab("Precisión") +
            ggtitle("Curva Precisión vs Recall")
    })
}

# Run the application
shinyApp(ui = ui, server = server)

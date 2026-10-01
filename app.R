library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(readxl)

# Função para carregar os dados
load_data <- function(file_input, sample_name) {
  if (!is.null(file_input)) {
    ext <- tolower(tools::file_ext(file_input$name))

    if (ext %in% c("csv")) {
      return(read.csv(file_input$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE))
    }

    if (ext %in% c("xls", "xlsx")) {
      return(read_excel(file_input$datapath))
    }

    stop("Formato de arquivo não suportado. Use CSV, XLS ou XLSX.")
  }

  if (!is.null(sample_name) && sample_name %in% c("iris", "mtcars", "airquality")) {
    return(get(sample_name))
  }

  iris
}

ui <- fluidPage(
  titlePanel("Shiny App para Análise Exploratória de Dados"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput(
        "file1",
        "Carregar arquivo CSV/XLS/XLSX",
        accept = c(
          ".csv",
          ".xls",
          ".xlsx"
        )
      ),

      selectInput(
        "sample_dataset",
        "Ou usar exemplo:",
        choices = c("iris", "mtcars", "airquality"),
        selected = "iris"
      ),

      hr(),

      uiOutput("var_numeric_ui"),
      uiOutput("var_x_ui"),
      uiOutput("var_y_ui")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Dados",
          DTOutput("table_head")
        ),

        tabPanel(
          "Resumo",
          verbatimTextOutput("summary")
        ),

        tabPanel(
          "Valores ausentes",
          plotOutput("missing_plot", height = "500px")
        ),

        tabPanel(
          "Distribuição",
          plotOutput("distribution_plot", height = "500px")
        ),

        tabPanel(
          "Correlação",
          plotOutput("correlation_plot", height = "500px")
        ),

        tabPanel(
          "Dispersão",
          plotOutput("scatter_plot", height = "500px")
        ),

        tabPanel(
          "Boxplot",
          plotOutput("boxplot_plot", height = "500px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_reactive <- reactive({
    req(input$sample_dataset)
    load_data(input$file1, input$sample_dataset)
  })

  numeric_vars <- reactive({
    df <- data_reactive()
    if (is.null(df)) return(character(0))
    names(df)[vapply(df, is.numeric, logical(1))]
  })

  output$var_numeric_ui <- renderUI({
    choices <- numeric_vars()
    if (length(choices) == 0) {
      return(helpText("Não há variáveis numéricas no dataset."))
    }

    selectInput(
      "var_dist",
      "Variável numérica para distribuição:",
      choices = choices,
      selected = choices[1]
    )
  })

  output$var_x_ui <- renderUI({
    choices <- numeric_vars()
    if (length(choices) < 2) {
      return(helpText("Selecione pelo menos 2 variáveis numéricas para dispersão/correlação."))
    }

    selectInput(
      "var_x",
      "Eixo X:",
      choices = choices,
      selected = choices[1]
    )
  })

  output$var_y_ui <- renderUI({
    choices <- numeric_vars()
    if (length(choices) < 2) {
      return(NULL)
    }

    selectInput(
      "var_y",
      "Eixo Y:",
      choices = choices,
      selected = choices[2]
    )
  })

  output$table_head <- renderDT({
    df <- data_reactive()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$summary <- renderPrint({
    summary(data_reactive())
  })

  output$missing_plot <- renderPlot({
    df <- data_reactive()
    missing_df <- df %>%
      summarise(across(everything(), ~ sum(is.na(.x)))) %>%
      pivot_longer(everything(), names_to = "variavel", values_to = "faltantes")

    ggplot(missing_df, aes(x = reorder(variavel, faltantes), y = faltantes)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Valores ausentes por variável",
        x = "Variável",
        y = "Quantidade de ausências"
      ) +
      theme_minimal()
  })

  output$distribution_plot <- renderPlot({
    req(input$var_dist)
    df <- data_reactive()

    ggplot(df, aes(x = .data[[input$var_dist]])) +
      geom_histogram(bins = 30, fill = "cornflowerblue", color = "white") +
      labs(
        title = paste("Distribuição de", input$var_dist),
        x = input$var_dist,
        y = "Frequência"
      ) +
      theme_minimal()
  })

  output$correlation_plot <- renderPlot({
    df <- data_reactive()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

    if (length(numeric_cols) < 2) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Não há suficientes variáveis numéricas para calcular correlação.", cex = 1)
      return()
    }

    corr <- cor(df[, numeric_cols, drop = FALSE], use = "pairwise.complete.obs")
    corr_long <- as.data.frame(as.table(corr))
    names(corr_long) <- c("Var1", "Var2", "correlacao")

    ggplot(corr_long, aes(x = Var1, y = Var2, fill = correlacao)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Mapa de correlação", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$scatter_plot <- renderPlot({
    req(input$var_x, input$var_y)
    df <- data_reactive()

    ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
      geom_point(color = "darkorange", alpha = 0.8, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
      labs(
        title = paste("Relação entre", input$var_x, "e", input$var_y),
        x = input$var_x,
        y = input$var_y
      ) +
      theme_minimal()
  })

  output$boxplot_plot <- renderPlot({
    req(input$var_dist)
    df <- data_reactive()

    if (ncol(df) < 2) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Dados insuficientes para boxplot.", cex = 1)
      return()
    }

    if (is.factor(df[[1]]) || is.character(df[[1]])) {
      ggplot(df, aes_string(x = names(df)[1], y = input$var_dist)) +
        geom_boxplot(width = 0.5, fill = "seagreen") +
        labs(title = paste("Boxplot de", input$var_dist, "por", names(df)[1]), x = names(df)[1], y = input$var_dist) +
        theme_minimal()
    } else {
      ggplot(df, aes(y = .data[[input$var_dist]], x = "")) +
        geom_boxplot(width = 0.5, fill = "seagreen") +
        labs(title = paste("Boxplot de", input$var_dist), x = "", y = input$var_dist) +
        theme_minimal() +
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    }
  })
}

shinyApp(ui = ui, server = server)

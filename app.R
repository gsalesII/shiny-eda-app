library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(readxl)
library(data.table)
library(plotly)

# Função para carregar os dados
load_data <- function(file_input, sample_name) {
  if (!is.null(file_input)) {
    ext <- tolower(tools::file_ext(file_input$name))
    
    if (ext %in% c("csv")) {
      # Use fread for faster loading
      return(as.data.frame(fread(file_input$datapath, header = TRUE, sep = ",")))
    }
    
    if (ext %in% c("xls", "xlsx")) {
      return(as.data.frame(read_excel(file_input$datapath)))
    }
    
    stop("Formato de arquivo não suportado. Use CSV, XLS ou XLSX.")
  }
  
  if (!is.null(sample_name) && sample_name %in% c("iris", "mtcars", "airquality")) {
    return(get(sample_name))
  }
  
  iris
}

# Função para detectar outliers usando IQR
detect_outliers_iqr <- function(df, numeric_cols) {
  outliers <- data.frame(row_index = integer(), variable = character(), value = numeric(), 
                         lower_bound = numeric(), upper_bound = numeric(), stringsAsFactors = FALSE)
  
  for (col in numeric_cols) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    
    idx <- which(df[[col]] < lower | df[[col]] > upper)
    if (length(idx) > 0) {
      outliers <- rbind(outliers, data.frame(
        row_index = idx,
        variable = col,
        value = df[idx, col],
        lower_bound = lower,
        upper_bound = upper,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  outliers
}

# Função para detectar outliers usando Z-score
detect_outliers_zscore <- function(df, numeric_cols, threshold = 3) {
  outliers <- data.frame(row_index = integer(), variable = character(), value = numeric(),
                         z_score = numeric(), stringsAsFactors = FALSE)
  
  for (col in numeric_cols) {
    z_scores <- abs(scale(df[[col]]))[,1]
    idx <- which(z_scores > threshold)
    if (length(idx) > 0) {
      outliers <- rbind(outliers, data.frame(
        row_index = idx,
        variable = col,
        value = df[idx, col],
        z_score = z_scores[idx],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  outliers
}

ui <- fluidPage(
  titlePanel("Análise Exploratória com Detecção de Anomalias"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput(
        "file1",
        "Carregar arquivo CSV/XLS/XLSX",
        accept = c(".csv", ".xls", ".xlsx")
      ),
      
      selectInput(
        "sample_dataset",
        "Ou usar exemplo:",
        choices = c("iris", "mtcars", "airquality"),
        selected = "iris"
      ),
      
      hr(),
      
      h4("Detecção de Anomalias"),
      selectInput(
        "outlier_method",
        "Método de detecção:",
        choices = c("IQR (Interquartile Range)" = "iqr", 
                    "Z-Score (Desvio Padrão)" = "zscore"),
        selected = "iqr"
      ),
      
      conditionalPanel(
        condition = "input.outlier_method == 'zscore'",
        sliderInput("zscore_threshold", "Z-Score Threshold:", min = 2, max = 5, value = 3, step = 0.5)
      ),
      
      hr(),
      
      uiOutput("var_numeric_ui"),
      uiOutput("var_x_ui"),
      uiOutput("var_y_ui"),
      
      hr(),
      
      h4("Informações dos Dados"),
      textOutput("data_info")
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
          "Valores Ausentes",
          plotOutput("missing_plot", height = "500px")
        ),
        
        tabPanel(
          "Distribuição",
          plotOutput("distribution_plot", height = "500px")
        ),
        
        tabPanel(
          "Correlação",
          div(
            style = "overflow-x: auto;",
            plotOutput("correlation_plot", height = "600px", width = "100%")
          ),
          h4("Correlações Fortes (|r| > 0.7)"),
          DTOutput("strong_corr_table")
        ),
        
        tabPanel(
          "Anomalias - IQR",
          h4("Outliers detectados pelo método IQR"),
          textOutput("outliers_iqr_count"),
          DTOutput("outliers_iqr_table"),
          plotOutput("outliers_iqr_plot", height = "500px")
        ),
        
        tabPanel(
          "Anomalias - Z-Score",
          h4("Outliers detectados pelo método Z-Score"),
          textOutput("outliers_zscore_count"),
          DTOutput("outliers_zscore_table"),
          plotOutput("outliers_zscore_plot", height = "500px")
        ),
        
        tabPanel(
          "Dispersão com Anomalias",
          plotlyOutput("interactive_scatter", height = "600px")
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
    df <- load_data(input$file1, input$sample_dataset)
    df
  })
  
  numeric_vars <- reactive({
    df <- data_reactive()
    if (is.null(df)) return(character(0))
    names(df)[vapply(df, is.numeric, logical(1))]
  })
  
  outliers_iqr <- reactive({
    df <- data_reactive()
    numeric_cols <- numeric_vars()
    if (length(numeric_cols) == 0) return(NULL)
    detect_outliers_iqr(df, numeric_cols)
  })
  
  outliers_zscore <- reactive({
    df <- data_reactive()
    numeric_cols <- numeric_vars()
    if (length(numeric_cols) == 0) return(NULL)
    detect_outliers_zscore(df, numeric_cols, threshold = input$zscore_threshold)
  })
  
  output$data_info <- renderText({
    df <- data_reactive()
    paste0(
      "Linhas: ", nrow(df), "\n",
      "Colunas: ", ncol(df), "\n",
      "Variáveis numéricas: ", length(numeric_vars())
    )
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
    numeric_cols <- numeric_vars()
    
    if (length(numeric_cols) < 2) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Não há suficientes variáveis numéricas para correlação.", cex = 1)
      return()
    }
    
    corr <- cor(df[, numeric_cols, drop = FALSE], use = "pairwise.complete.obs")
    corr_long <- as.data.frame(as.table(corr))
    names(corr_long) <- c("Var1", "Var2", "correlacao")
    
    ggplot(corr_long, aes(x = Var1, y = Var2, fill = correlacao)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1)) +
      geom_text(aes(label = round(correlacao, 2)), size = 3) +
      labs(title = "Mapa de Correlação", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$strong_corr_table <- renderDT({
    df <- data_reactive()
    numeric_cols <- numeric_vars()
    
    if (length(numeric_cols) < 2) {
      return(NULL)
    }
    
    corr <- cor(df[, numeric_cols, drop = FALSE], use = "pairwise.complete.obs")
    corr_long <- as.data.frame(as.table(corr))
    names(corr_long) <- c("Var1", "Var2", "Correlacao")
    
    # Filtrar correlações fortes (excluindo auto-correlações)
    strong_corr <- corr_long %>%
      filter(Var1 != Var2 & abs(Correlacao) > 0.7) %>%
      mutate(Correlacao = round(Correlacao, 4)) %>%
      arrange(desc(abs(Correlacao))) %>%
      distinct(paste(pmin(Var1, Var2), pmax(Var1, Var2)), .keep_all = TRUE)
    
    datatable(strong_corr, options = list(pageLength = 10))
  })
  
  output$outliers_iqr_count <- renderText({
    outliers <- outliers_iqr()
    if (is.null(outliers) || nrow(outliers) == 0) {
      return("Nenhum outlier detectado pelo método IQR.")
    }
    paste("Total de outliers detectados (IQR):", nrow(outliers))
  })
  
  output$outliers_iqr_table <- renderDT({
    outliers <- outliers_iqr()
    if (is.null(outliers) || nrow(outliers) == 0) {
      return(datatable(data.frame(message = "Nenhum outlier detectado")))
    }
    datatable(
      outliers %>% mutate(
        value = round(value, 4),
        lower_bound = round(lower_bound, 4),
        upper_bound = round(upper_bound, 4)
      ),
      options = list(pageLength = 10)
    )
  })
  
  output$outliers_iqr_plot <- renderPlot({
    outliers <- outliers_iqr()
    if (is.null(outliers) || nrow(outliers) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Nenhum outlier detectado.", cex = 1)
      return()
    }
    
    ggplot(outliers, aes(x = reorder(variable, value), y = value, fill = variable)) +
      geom_boxplot(alpha = 0.7) +
      geom_point(color = "red", size = 3, shape = 21) +
      coord_flip() +
      labs(title = "Outliers por Variável (IQR)", x = "Variável", y = "Valor") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$outliers_zscore_count <- renderText({
    outliers <- outliers_zscore()
    if (is.null(outliers) || nrow(outliers) == 0) {
      return("Nenhum outlier detectado pelo método Z-Score.")
    }
    paste("Total de outliers detectados (Z-Score):", nrow(outliers))
  })
  
  output$outliers_zscore_table <- renderDT({
    outliers <- outliers_zscore()
    if (is.null(outliers) || nrow(outliers) == 0) {
      return(datatable(data.frame(message = "Nenhum outlier detectado")))
    }
    datatable(
      outliers %>% mutate(
        value = round(value, 4),
        z_score = round(z_score, 4)
      ),
      options = list(pageLength = 10)
    )
  })
  
  output$outliers_zscore_plot <- renderPlot({
    outliers <- outliers_zscore()
    if (is.null(outliers) || nrow(outliers) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Nenhum outlier detectado.", cex = 1)
      return()
    }
    
    ggplot(outliers, aes(x = reorder(variable, z_score), y = z_score, fill = variable)) +
      geom_col(alpha = 0.7) +
      geom_hline(yintercept = input$zscore_threshold, linetype = "dashed", color = "red") +
      coord_flip() +
      labs(title = paste("Outliers por Variável (Z-Score > ", input$zscore_threshold, ")"),
           x = "Variável", y = "Z-Score") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$interactive_scatter <- renderPlotly({
    req(input$var_x, input$var_y)
    df <- data_reactive()
    outliers <- outliers_iqr()
    
    # Marcar pontos que são outliers
    df$is_outlier <- FALSE
    if (!is.null(outliers)) {
      outlier_rows <- unique(outliers$row_index)
      df$is_outlier[outlier_rows] <- TRUE
    }
    
    p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], 
                        color = is_outlier, label = row.names(df))) +
      geom_point(size = 2.5, alpha = 0.6) +
      scale_color_manual(values = c("FALSE" = "darkblue", "TRUE" = "red"), 
                         labels = c("FALSE" = "Normal", "TRUE" = "Anomalia")) +
      geom_smooth(method = "lm", se = FALSE, color = "darkgray", inherit.aes = FALSE,
                  aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
      labs(
        title = paste("Relação entre", input$var_x, "e", input$var_y),
        x = input$var_x,
        y = input$var_y,
        color = "Tipo de Ponto"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "label", "color"))
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
        labs(title = paste("Boxplot de", input$var_dist, "por", names(df)[1]), 
             x = names(df)[1], y = input$var_dist) +
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

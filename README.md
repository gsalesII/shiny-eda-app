# Shiny EDA App

Aplicação em R/Shiny para análise exploratória de dados (EDA), com:

- upload de arquivos CSV / XLS / XLSX
- uso de dados de exemplo (`iris`, `mtcars`, `airquality`)
- visualização dos dados em tabela
- resumo estatístico
- análise de valores ausentes
- histogramas e boxplots
- correlação entre variáveis numéricas
- gráfico de dispersão

## Requisitos

Instale os pacotes abaixo no R:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "DT", "readxl"))
```

## Como executar

No terminal do R ou RStudio:

```r
shiny::runApp("app.R")
```

Se estiver em um diretório com o arquivo `app.R`, pode rodar também:

```r
source("app.R")
```

## Estrutura do projeto

- `app.R` — aplicação principal

## Sugestões de extensão

- adicionar filtros por categoria
- criar gráficos interativos com `plotly`
- incluir dashboard com `shinydashboard`
- exportar relatórios em HTML/PDF


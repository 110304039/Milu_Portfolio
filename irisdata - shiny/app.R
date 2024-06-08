library(shiny)
library(ggplot2)
library(dplyr)
library(FactoMineR)  # 加入 FactoMineR 套件進行 CA 分析
library(factoextra)  # 加入 factoextra 套件來繪製 CA 圖

# 載入 iris 資料集
data(iris)

# UI 部分
ui <- fluidPage(
  titlePanel("Interactive PCA and CA Analysis"),
  sidebarLayout(
    sidebarPanel(
      # PCA panel
      tabsetPanel(
        tabPanel("PCA",
                 sliderInput("pcx_pca", "Select pcx (1-4):", min = 1, max = 4, value = 1),
                 sliderInput("pcy_pca", "Select pcy (1-4):", min = 1, max = 4, value = 2)
        ),
        # CA panel
        tabPanel("CA",
                 sliderInput("pcx_ca", "Select pcx (1-3):", min = 1, max = 3, value = 1),
                 sliderInput("pcy_ca", "Select pcy (1-3):", min = 1, max = 3, value = 2)
        )
      ),
      # 輸入要分析的資料數量
      numericInput("num_data", "Number of data points to analyze:", value = 50, min = 1, max = nrow(iris))
    ),
    mainPanel(
      tabsetPanel(
        # 第一個 panel: input_data
        tabPanel("Input Data", tableOutput("input_table")),
        # 第二個 panel: pca
        tabPanel("PCA", plotOutput("pca_plot")),
        # 第三個 panel: ca
        tabPanel("CA", plotOutput("ca_plot"))
      )
    )
  )
)

# Server 部分
server <- function(input, output) {
  # 產生要顯示的 iris 資料
  output$input_table <- renderTable({
    head(iris, input$num_data)
  })
  
  # 產生 PCA plot
  output$pca_plot <- renderPlot({
    # 執行 PCA
    pca_result <- prcomp(iris[, -5], scale. = TRUE)
    # 提取 pcax 和 pcay 的結果
    x_var <- paste0("PC", input$pcx_pca)
    y_var <- paste0("PC", input$pcy_pca)
    pca_data <- data.frame(pca_result$x)
    # 加上 iris 的 Species 作為顏色分組
    pca_data$Species <- iris$Species
    # 繪製 PCA plot
    ggplot(data = pca_data, aes_string(x = x_var, y = y_var, color = "Species")) +
      geom_point() +
      labs(x = x_var, y = y_var, title = "PCA Plot")
  })
  
  # 產生 CA plot
  output$ca_plot <- renderPlot({
    # 執行 CA
    ca_result <- CA(iris[, -5], graph = FALSE)
    # 繪製 CA plot
    fviz_ca(ca_result, axes = c(input$pcx_ca, input$pcy_ca))
  })
}

# 執行 Shiny app
shinyApp(ui = ui, server = server)

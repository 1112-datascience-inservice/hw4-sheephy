library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# 加载iris数据集
data(iris)

# 创建Shiny应用程序
ui <- fluidPage(
  headerPanel("110971025")
  , sidebarLayout(
      sidebarPanel(
        selectInput(
          "component1"
          , "選擇主成分x: "
          , choices = c("PC1", "PC2", "PC3", "PC4")
          , selected = "PC1"
        )
        , selectInput(
          "component2"
          , "選擇主成分y: "
          , choices = c("PC1", "PC2", "PC3", "PC4")
          , selected = "PC2"
        )
        , sliderInput(
          "numData"
          , "選擇數據數量:"
          , min = 10,
          , max = nrow(iris)
          , value = 100
        )
      )
      , mainPanel(
        tabsetPanel(
          tabPanel(
            "PCA"
            , plotOutput("pcaPlot")
          )
          , tabPanel(
            "CA"
            , plotOutput("CAPlot")
          )
        )
      )
    )
)

server <- function(input, output){
  output$pcaPlot <- renderPlot(
    {
      row_index <- sample(nrow(iris), input$numData)
      sampled_data <- iris[row_index, ]

      pca <- PCA(sampled_data[, 1:4], scale.unit = TRUE, graph = FALSE)
      selected_component1 <- switch(
        input$component1
        , "PC1" = 1
        , "PC2" = 2
        , "PC3" = 3
        , "PC4" = 4
      )
      selected_component2 <- switch(
        input$component2
        , "PC1" = 1
        , "PC2" = 2
        , "PC3" = 3
        , "PC4" = 4
      )
      biplot <- fviz_pca_biplot(
        pca, axes = c(selected_component1, selected_component2)
        , lable = "var"
        , habillage = sampled_data[, 5]
        , addEllipses = TRUE
        , ellipse.level = 0.95
      )
      print(biplot)
    }
  )

  output$CAPlot <- renderPlot(
    {
      row_index <- sample(nrow(iris), input$numData)
      sampled_data <- iris[row_index, ]

      ca <- CA(sampled_data[, 1:4], graph = FALSE)
      selected_component1 <- switch(
        input$component1
        , "PC1" = 1
        , "PC2" = 2
        , "PC3" = 3
        , "PC4" = 4
        )
      selected_component2 <- switch(
          input$component2
          , "PC1" = 1
          , "PC2" = 2
          , "PC3" = 3
          , "PC4" = 4
        )
      CAPlot <- fviz_ca_biplot(
        ca, axes = c(selected_component1, selected_component2)
        , col.row = sampled_data[, 5]
      )
      print(CAPlot)
    }
  )
}

# 运行Shiny应用程序
shinyApp(ui = ui, server = server)



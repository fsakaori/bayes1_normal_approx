library(shiny)

shinyUI(fluidPage(
  pageWithSidebar(
    headerPanel("Posterior distribution ~ Normal distribution "), # タイトル
    
    sidebarPanel(
      sliderInput("size", "Please Select Size: ",
                  min=10, max=200, value=10, step=1), # サンプルサイズ n のスライダー
      sliderInput("mu", "Please Select Mean mu : ",
                  min=-1, max=1, value=0, step=0.01),   # 真の値 mu のスライダー
      sliderInput("sigma", "Please Select Variance sigma : ",
                  min=0.1, max=3, value=1, step=0.01),  # 真の値 sigma のスライダー
      sliderInput("seed", "Please Select seed: ",
                  min=1, max=100, value=1, step=1),      # シードの値のスライダー
      sliderInput("xlim", "Please Select xlim: ",
                  min = 0.2, max=2, value=0.5, step=0.01), # xlim の値のスライダー
      sliderInput("ylim", "Please Select ylim: ",
                  min = 0.2, max=2, value=0.5, step=0.01)  # ylim の値のスライダー
    ),
    mainPanel(
      tableOutput("Data"), plotOutput("graph_1"), plotOutput("graph_2"), plotOutput("graph_3")
    )
  )
))
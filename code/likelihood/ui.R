library('shiny')

shinyUI(
  ui = shinyUI(fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("p1", label = "p1",
                    min = 0, max = 1, value = 0.7, step = 0.01),
        sliderInput("p2", label = "p2",
                    min = 0, max = 1, value = 0.5, step = 0.01),
        sliderInput("k", label = "k",
                    min = 1, max = 50, value = 7, step = 1),
        sliderInput("N", label = "N",
                    min = 1, max = 50, value = 10, step = 1),
        br(),
        br(),
        textOutput("LRatio1"),
        br(),
        textOutput("LRatio2"),
        tags$head(tags$style("#LRatio1, #LRatio2 { font-size: 25px; }"))
        ),
      mainPanel(
        plotOutput("LRplot", height="500px")
        )
      )
    )
  )
)
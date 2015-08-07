library('shiny')

shinyUI(
  ui = shinyUI(fluidPage(
    sidebarLayout(
      sidebarPanel(
        div(style = 'display: inline-block;', numericInput('a', label = h4('a'), value = 1)),
        div(style = 'display: inline-block;', numericInput('b', label = h4('b'), value = 1)),
        div(style = 'display: inline-block;', numericInput('k', label = h4('k'), value = 1)),
        div(style = 'display: inline-block;', numericInput('N', label = h4('N'), value = 1)),
        br(),
        br(),
        textOutput("BF_01"),
        br(),
        textOutput("BF_10"),
        tags$head(tags$style("#BF_01, #BF_10 { font-size: 25px; }"))
        ),
      mainPanel(
        plotOutput('update_plot', height='500px')
        )
      )
    )
  )
)
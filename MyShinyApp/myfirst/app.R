library(shiny)

ui <- fluidPage(
  tags$h1("안녕, 난 주니온이라고 해!"),
  tags$img(src = "https://imgnews.pstatic.net/image/421/2022/08/19/0006286193_001_20220819100106028.jpg?type=w647")
)

server <- function (input, output) {
  
  
}

shinyApp(ui, server = server)

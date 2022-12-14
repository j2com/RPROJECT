library(shiny)
library(gapminder)
library(ggplot2)

ui <- pageWithSidebar(
    headerPanel(
        tags$h1("여기가 헤더임")
    ),
    sidebarPanel(
        selectInput("year", "몇 년도를 보여드릴까요?",
                    seq(1952, 2007, 5))
    ),
    mainPanel(
      h3("해당 연도의 GDP 대비 기대수명 그래프입니다."),
      plotOutput('distPlot')
    )
)

server <- function(input, output){
  output$distPlot <- renderPlot({
    ggplot(subset(gapminder, year == input$year), aes(x = gdpPercap, y = lifeExp)) + 
      geom_point(aes(color = continent)) +
      scale_x_log10() + 
      geom_smooth()
  })
}


shinyApp(ui = ui, server = server)

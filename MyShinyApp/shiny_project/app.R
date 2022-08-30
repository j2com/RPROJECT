library(shiny)
library(tidyverse)
library(gganimate)
library(nord)
library(viridis)
library(av)
library(ggplot2)
library(hrbrthemes)
library(babynames)
library(tsibble)
library(plotly)
library(data.table)
library(MASS)
getwd()
part <- read.csv("C:\\RPROJECT\\MyProject\\학교급별.csv", fileEncoding = "euc-kr")
part2 <- part[part["항목"] != "일반고 (만원)",]

part3 <- part2[part2["항목"]=="고등학교 (만원)", c(1:2, 6:11)]
part4 <- part2[part2["항목"]=="고등학교 (만원)", c(1:2, 18:22)]

# pivot_longer
part_sub <- pivot_longer(
  part3,
  cols = c("국..어","영..어","수..학","사회.과학","논..술","제2외국어.한문.컴퓨터.등"),
  names_to = "과목",
  values_to = "사교육비",
  names_sep = NULL,
  values_drop_na = F
)

part_cate <- pivot_longer(
  part4,
  cols = c("개인과외","그룹과외","학원수강","방문학습지","유료인터넷.및.통신강좌.등"),
  names_to = "유형",
  values_to = "사교육비",
  names_sep = NULL,
  values_drop_na = F
)
part_cate
# shiny code
ui <- pageWithSidebar(
  # 1. 헤더 패널
  headerPanel(h1("학급별 사교육비 데이터셋")),
  # 2. 사이드바 패널
  sidebarPanel(
    sliderInput("count", 
                "Number of values: ",
                min = 1, 
                max = 10000,
                value = 5000)),
  # 3. 메인 패널
  mainPanel(
    plotOutput("distPlot"))
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
      part_cate |>
        filter(유형!="방문학습지") |>
        ggplot( aes(fill=유형, y=사교육비, x=시점)) +
        ggtitle("2007 ~ 2021 고등학교 유형별 사교육비") +
        geom_bar(position="dodge", stat="identity", width = 0.7) +
        geom_text(aes(label = 사교육비), vjust = 0, color = "white") +
        facet_wrap(scales = 'free', . ~ 유형) +
        theme_ft_rc()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

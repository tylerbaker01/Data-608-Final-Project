# Data 608 Final
# The Effects Education plays on a Country

# load libraries
library(shiny)
library(tidyverse)
library(plotly)

#load data
pisa_vs_gdp <- read.csv("https://raw.githubusercontent.com/tylerbaker01/Data-608-Final-Project/main/pisa_vs_gdp.csv")
pisa_vs_homicide <- read.csv("https://raw.githubusercontent.com/tylerbaker01/Data-608-Final-Project/main/pisa_vs_homicide.csv")
pisa_vs_lifeexp <- read.csv("https://raw.githubusercontent.com/tylerbaker01/Data-608-Final-Project/main/pisa_vs_lifeexp.csv")

# Start App
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("PISA Scores and GDP",
             selectInput(inputId = "subject_gdp", label = "Test Subject", choices= pisa_vs_gdp$test_type),
             selectInput(inputId = "country_gdp" , label = "Select a Country", choices = pisa_vs_gdp$country),
             plotOutput("gdp_plot_one"),
             plotOutput("gdp_plot_two")),
    tabPanel("PISA Scores and Life Expectancy",
             selectInput(inputId = "subject_lifeexp", label = "Test Subject", choices = pisa_vs_lifeexp$test_type),
             selectInput(inputId = "country_life", label = "Select a Country", choices = pisa_vs_lifeexp$country),
             plotOutput("lifeexp_plot_one"),
             plotOutput("lifeexp_plot_two")),
    tabPanel("PISA Scores and Homicide Rate",
             selectInput(inputId = "subject_homicide", label = "Test Subject", choices = pisa_vs_homicide$test_type),
             selectInput(inputId = "country_homicide", label = "Select a Country", choices = pisa_vs_homicide$country),
             plotOutput("homicide_plot_one"),
             plotOutput("homicide_plot_two"))
  )
)

server <- function(input,output) {
  output$gdp_plot_one <- renderPlot({
    pisa_gdp <- pisa_vs_gdp %>%
      filter(test_type==input$subject_gdp)%>%
      filter(country==input$country_gdp)%>%
      group_by(test_type, country)
    ggplot()+
      geom_line(data = pisa_gdp, aes(x=year, y=score))
  })
  output$gdp_plot_two <- renderPlot({
    pisa_gdp <- pisa_vs_gdp %>%
      filter(test_type==input$subject_gdp)%>%
      filter(country==input$country_gdp)%>%
      group_by(test_type, country)
    ggplot()+
      geom_line(data = pisa_gdp, aes(x=year, y=gdp))
  })
  output$lifeexp_plot_one <- renderPlot({
    pisa_lifeexp <- pisa_vs_lifeexp %>%
      filter(test_type==input$subject_lifeexp)%>%
      filter(country==input$country_life)%>%
      group_by(test_type, country)
    ggplot()+
      geom_line(data = pisa_lifeexp, aes(x=year, y=score))
  })
  output$lifeexp_plot_two <- renderPlot({
    pisa_lifeexp <- pisa_vs_lifeexp %>%
      filter(test_type==input$subject_lifeexp)%>%
      filter(country==input$country_life)%>%
      group_by(test_type, country)
    ggplot()+
      geom_line(data = pisa_lifeexp, aes(x=year, y=life_expectancy))
  })
  output$homicide_plot_one <- renderPlot({
    pisa_homicide <- pisa_vs_homicide %>%
      filter(test_type==input$subject_homicide)%>%
      filter(country==input$country_homicide)%>%
      group_by(test_type, country)
    ggplot()+
      geom_line(data = pisa_homicide, aes(x=year, y=score))
  })
  output$homicide_plot_two <- renderPlot({
    pisa_homicide <- pisa_vs_homicide %>%
      filter(test_type==input$subject_homicide)%>%
      filter(country==input$country_homicide)%>%
      group_by(test_type, country)
    ggplot()+
      geom_line(data = pisa_homicide, aes(x=year, y=homicide_rate))
  })
}

shinyApp(ui=ui, server=server)

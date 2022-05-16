# Data 608 Final
# The Effects Education plays on a Country

# load libraries
library(shiny)
library(tidyverse)
library(plotly)

#load data
pisa <- read.csv("https://raw.githubusercontent.com/tylerbaker01/Data-608-Final-Project/main/pisa.csv")
pisa$gdp <- as.numeric(pisa$gdp)

# Start App
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Main",
             titlePanel("Does Education Effect a Countries Success?"),
             br(),
             p("Tyler Baker"),
             br(),
             p("I have spent many years of my life as a tutor. I desperately want to feel like I am making the world
               a better place. I have always been a firm believer that most of the world's problems stim from a degree of ignorance.
               By teaching, or tutoring students, I believed that I was making the world a safer, richer, and more enjoyable place.
               I decided to see if I was right in my belief."),
             br(),
             p("I used data from PISA, and WorldBank. PISA is an international testing company that will test 15 year olds every 3 years.
               While not every country contributes, it does provide a lot of information still. Specifically, PISA tests for math, science,
               and reading. The WorldBank is a data collection company that gathers tons of data on countries. Specifically, I wanted to see
               if education played an impact on GDP, life expectancy, and homicide rate. I figured that by looking at these I would be able to
               say that education makes a country worth more money, education makes it so a country can provide its citizens better health care, and 
               that education makes a country's citizens happier(or at least better copers)."),
             br(),
             p("In the end, my data analyst has not definitively proven myself correct. However, it shows that their might be some truth to my belief.
               When looking at countries that scored higher than the national average, it seems they are more likely to have a higher GDP, and life expectancy, while having a lower
               homicide rate."),
             br(),
             p('A few things to note. PISA has not been established very long. The first year of their international testing that could be collected was from 2006.
               In the future, as long as PISA stays around, we will have more data which we can use to look at how countries changed over time. I also am sure there will be 
               a few out there that say, "This does not show that education makes the difference. This simply shows that money has an impact on education, life expectancy, and homicide rate."
               To those few, you might be correct. The best way we will know for certain is when we have more data in the future. If we are able to study the rise and fall of nations, then we will be able to state which impacts the other.')),
    tabPanel("International Averages",
             selectInput(inputId = "subject_avg", label= "Test Subject", choices = pisa$subject),
             plotOutput("avg_scores"),
             plotOutput("avg_gdp"),
             plotOutput("avg_lifeexp"),
             plotOutput("avg_homicide")),
    tabPanel("Country Compared to International Average",
             selectInput(inputId = "subject", label = "Test Subject", choices = pisa$subject),
             selectInput(inputId = "country", label = "Select Country", choices = pisa$country),
             plotOutput("scores"),
             plotOutput("gdp"),
             plotOutput("lifeexp"),
             plotOutput("homicide"))
  )
)

server <- function(input,output) {
  output$avg_scores <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject_avg)%>%
      filter(country=="International Average")
    ggplot()+
      geom_line(data = pisa_avg, aes(x=year, y =score))+
      labs(title = "International Test Score Average")
  })
  output$avg_gdp <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject_avg)%>%
      filter(country=="International Average")
    ggplot()+
      geom_line(data=pisa_avg, aes(x=year, y=gdp))+
      labs(title = "International GDP")
  })
  output$avg_lifeexp <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject_avg)%>%
      filter(country=="International Average")
    ggplot()+
      geom_line(dat=pisa_avg, aes(x=year, y=life_expectancy))+
      labs(title = "International Life Expectancy")
  })
  output$avg_homicide <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject_avg)%>%
      filter(country=="International Average")
    ggplot()+
      geom_line(data=pisa_avg, aes(x=year, y=homicide_rate))+
      labs(title = "International Homicides (Per 100,000 People)")
  })
  output$scores <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country=="International Average")
    pisa_country <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country==input$country)
    colors <- c("Selected Country"="blue", "International Average"="red")
    ggplot()+
      geom_line(data=pisa_country, aes(x=year , y=score, color="Selected Country"))+
      geom_line(data=pisa_avg, aes(x=year, y=score, color ="International Average"))+
      labs(title = "Test Score Compared to International Average")
  })
  output$gdp <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country=="International Average")
    pisa_country <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country==input$country)
    colors <- c("Selected Country"="blue", "International Average"="red")
    ggplot()+
      geom_line(data=pisa_country, aes(x=year , y=gdp, color="Selected Country"))+
      geom_line(data=pisa_avg, aes(x=year, y=gdp, color ="International Average"))+
      labs(title = "GDP Compared to International GDP")
  })
  output$lifeexp <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country=="International Average")
    pisa_country <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country==input$country)
    colors <- c("Selected Country"="blue", "International Average"="red")
    ggplot()+
      geom_line(data=pisa_country, aes(x=year , y=life_expectancy, color="Selected Country"))+
      geom_line(data=pisa_avg, aes(x=year, y=life_expectancy, color ="International Average"))+
      labs(title = "Life Expectancy Compared to International Life Expectancy")
  })
  output$homicide <- renderPlot({
    pisa_avg <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country=="International Average")
    pisa_country <- pisa %>%
      filter(subject==input$subject)%>%
      filter(country==input$country)
    colors <- c("Selected Country"="blue", "International Average"="red")
    ggplot()+
      geom_line(data=pisa_country, aes(x=year , y=homicide_rate, color="Selected Country"))+
      geom_line(data=pisa_avg, aes(x=year, y=homicide_rate, color ="International Average"))+
      labs(title = "Homicides Compared to International Homicides(Per 100,000 people)")
  })
  
  
 
}

shinyApp(ui=ui, server=server)

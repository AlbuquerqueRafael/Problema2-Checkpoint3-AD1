library(shiny)
library(plotly)
library(ggplot2movies)  # Needed for the 'movies' data set

shinyUI(fluidPage(
  titlePanel("Deputados Cearenses"),
  sidebarPanel(
      helpText("Veja algumas maravilhas que podem ser feitas com o shiny + plotly"),
      
      selectInput("var", 
                  label = "Escolha um gráfico",
                  choices = c("Inicio", "Passagens", "Gastos Gerais", "Fornecedores"),
                  selected = "Inicio")
      
    ),
  

  mainPanel(
    h4(textOutput("text1"), align="justify"),
    plotlyOutput("trendPlot")
  )
))
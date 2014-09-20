require(XML)
require(ggplot2)
library(ggmap)
library(scales)
library(gridExtra)
library(markdown)
options(encoding = "UTF-8")

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  headerPanel("Datos meteorológicos"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    wellPanel(
      helpText(HTML("<b>Extraer datos</b>")),
      HTML("Introduce una localización y pulsa el botón actualizar"),
      submitButton("Actualizar")
    ),
    wellPanel(
      textInput("text", "Location:", "Madrid")
    )
  ),

  mainPanel(
    tabsetPanel(     
      tabPanel("Gráfico mosaico", plotOutput("plot")),
      tabPanel("Gráfico de viento", plotOutput("plot2")),
      tabPanel("Gráfico de precipitación", plotOutput("plot3")),
      tabPanel("Mapa", plotOutput("map")),
      tabPanel("Sobre la aplicación", includeMarkdown("intro.md"))
    )
  )
))
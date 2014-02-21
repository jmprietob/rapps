library(shiny)
library(raster)
library(ncdf)
library(rgdal)
library(scales)
library(ggmap)
library(ggplot2)
library(gridExtra)


# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Datos climáticos horarios"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    wellPanel(
      helpText(HTML("<b>Extrae datos</b>")),
      HTML("Introduzca las coordenadas que desee. Pulse el botón para actualizar los datos."),
      submitButton("Actualiza")
    ),
    wellPanel(
      helpText(HTML("<b>Introduzca coordenadas</b>")),
      numericInput("lat", "Latitud:", -0.285628),   
      numericInput("lon", "Longitud:", 42.7244749)
    )
    ),

  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    tabsetPanel(
      tabPanel("Gráfico", plotOutput("plot")),
      tabPanel("Mapa", plotOutput("map")),
      tabPanel("Resumen", verbatimTextOutput("summary")), 
      tabPanel("Datos", tableOutput("table"))
    )
  )
))

##shinyapps::setAccountInfo(name="jmprietob", token="D5A33911727E73B34255FA055EB3018F", secret="VGCvm3nPWs6gEE/WAehs7Nb01HyMfJ9GJGUN/ldf")
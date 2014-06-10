library(shiny)
library(scales)
library(ggmap)
library(ggplot2)
library(gridExtra)
library(raster)
library(ncdf)
library(rgdal)
library(markdown)


# Define UI for dataset viewer application
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  headerPanel("Climatic hourly data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    wellPanel(
      helpText(HTML("<b>Extract data</b>")),
      HTML("Insert Location and press update button to refresh the data."),
      submitButton("Update")
    ),
    wellPanel(
      helpText(HTML("<b>Insert Location</b>")),
      textInput("text", "Location:", "Madrid")
    )
    ),

  mainPanel(
    tabsetPanel(     
      tabPanel("Mosaic plot", plotOutput("plot")),
      tabPanel("Compact plot", plotOutput("plot2")),
      tabPanel("Wind plot", plotOutput("plot3")),
      tabPanel("Map", plotOutput("map")),
      tabPanel("Summary", verbatimTextOutput("summary")), 
      tabPanel("Data", tableOutput("table")),
      tabPanel("About", includeMarkdown("datos/intro.md"))      
    )
  )
))
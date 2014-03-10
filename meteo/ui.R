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
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Climatic hourly data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    wellPanel(
      helpText(HTML("<b>Extract data</b>")),
      HTML("Insert the coordinates. Press update button to refresh the data."),
      submitButton("Update")
    ),
    wellPanel(
      helpText(HTML("<b>Insert Lat - Lon</b>")),
      numericInput("lat", "Latitude:", -0.285628),   
      numericInput("lon", "Longitude:", 42.7244749)
    )
    ),

  mainPanel(
    tabsetPanel(     
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Map", plotOutput("map")),
      tabPanel("Summary", verbatimTextOutput("summary")), 
      tabPanel("Data", tableOutput("table")),
      tabPanel("About", includeMarkdown("datos/intro.md"))      
    )
  )
))
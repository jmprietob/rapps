library(shiny)
library(ggplot2)
library(ggmap)
library(markdown)

shinyUI(pageWithSidebar(
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Application title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  headerPanel("Demo shiny application"),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sidebar Panel
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sidebarPanel(
    
    wellPanel(
      helpText(HTML("<b>R, Maps, ggplot</b>")),
      HTML("Select the data you want to display"),
      submitButton("Refresh")
    ),
    
    wellPanel(
      helpText(HTML("<b>BASIC SETTINGS</b>")),     
      helpText("Data from year 2002 to 2012"),
      sliderInput("year", "Year of analysis:",min = 2002, max = 2011, step = 1, value = 2011),
      helpText(HTML("<b>Selection of tree species</b>")),
      selectInput("especie", "Select one:",choice = c('Quercus ilex', 'Quercus pyrenaica', 'Pinus silvestris', 'Pinus pinea'))
    )   
  ),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main Panel
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  mainPanel(
    tabsetPanel(
      tabPanel("Intro", includeMarkdown("docs/introduction.md")),     
      tabPanel("Grafs", plotOutput("plot")),
      tabPanel("Map", plotOutput("map")),
      tabPanel("About", includeMarkdown("docs/acercade.md"))        
    )
  )
  
))

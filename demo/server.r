library(shiny)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(markdown)

load("datos/madrid.RData")

shinyServer(function(input, output) {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reactive Functions
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  especie <- reactive({
    subset(defoliacion, Especie==input$especie) 
  })
  
  espano <- reactive({
    subset(defoliacion, Especie==input$especie & Temporada==input$year)
  })
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 1 - plot
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$plot <- renderPlot({
    
    datosEsp <- especie()
    
     graficoCaja <- ggplot(datosEsp, aes(factor(Temporada), defo))+
       geom_boxplot(fill="grey80",colour="#56B4E9",size = 1,outlier.colour="#56B4E9")+
       labs(x = "temporada", y = "%")
     graficoLinea <- ggplot(datosEsp, aes(Temporada, defo))+
       geom_point(colour="#56B4E9",size = 1)+stat_smooth(method="loess")+
       stat_smooth(method="lm",size = 1,fill="blue",colour="darkblue")+
       labs(x = "temporada", y = "%")
     
    #output
    multiplot<-grid.arrange(graficoCaja,graficoLinea, ncol=2, main = input$especie)
    
    print(multiplot)
    
  })
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 2 - Map
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$map <- renderPlot({
    
    datosEspAno <- espano()
    
    map.base <- get_googlemap(
      center= cbind(-3.75,40.55),
      maptype = 'terrain', ## Map type as defined above (roadmap, terrain, satellite, hybrid)
      zoom = 9, ## 14 is just about right for a 1-mile radius
      scale = 2, ## Set it to 2 for high resolution output
    )    

    map.base <- ggmap(map.base, extend = "panel") + coord_cartesian() + coord_fixed(ratio = 1.5)
    
    ## Main ggplot
    map.final <- map.base +
      geom_point(data=datosEspAno, aes(x=x, y=y),colour='white', size=9)+
      geom_point(data=datosEspAno, aes(x=x, y=y, colour = defo),size=8) +
      geom_text(data=datosEspAno,aes(x=x, y=y,label=round(defo, digits = 1)),colour="white",size=6, fontface=2,hjust=0, vjust=0)+
      geom_text(data=datosEspAno,aes(x=x, y=y,label=round(defo, digits = 1)),colour="black",size=6, hjust=0, vjust=0)+
      scale_colour_gradient(low = "forestgreen",high = "greenyellow")      
    
    print(map.final)
    
  }, width = 640, height = 640)

})

#  gsr <- function(Source, Search, Replace)
#  {
#    if (length(Search) != length(Replace))
#      stop("Search and Replace Must Have Equal Number of Items\n")
#    
#    Changed <- as.character(Source)
#    
#    for (i in 1:length(Search))
#    {
#      cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
#      Changed <- replace(Changed, Changed == Search[i], Replace[i])
#   }
#   
#   cat("\n")
#   
#    Changed
#  } 
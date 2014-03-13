library(shiny)
library(scales)
library(ggmap)
library(ggplot2)
library(gridExtra)
library(raster)
library(ncdf)
library(rgdal)
library(markdown)

archivo <- "datos/datosnetcdf.nc"
idx <- seq(as.POSIXct('2014-03-13 01:00:00', tz="UTC"), as.POSIXct('2014-03-17 00:00:00', tz="UTC"), 'hour')
idx <- as.POSIXct(idx)

## Leemos la variable temperatura y generamos su leyenda
t <- stack(archivo, varname = "temp")
t <- t-273# pasar a grados C
proj4string(t) <- CRS("+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84")
s <- stack(archivo, varname = "snowlevel")
proj4string(s) <- CRS("+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84")
p <- stack(archivo, varname = "prec")
proj4string(p) <- CRS("+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84")
snow <- stack(archivo, varname = "snow_prec")
proj4string(snow) <- CRS("+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84")

#Panticosa -0.285628,42.7244749
#Formigal -0.37, 42.775
#Zapardiel -5.3292159, 40.3588136
#La morcuera (-3.830237, 40.828359)
#Madrid (,-3.6817174,40.4352117)

shinyServer(function(input, output) {
  
  punto <- reactive({
    cbind(input$lat,input$lon) 
  })
  puntoTRANS <- reactive({
    ptoWGS <- SpatialPoints(punto(), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projLCC <- projection(t)
    spTransform(ptoWGS, CRS(projLCC)) 
  })  
  
  plotFunc = function() {
    
    puntoLCC <- puntoTRANS()
    
    vals <- extract(t, puntoLCC,ncol = 2)
    datos <- data.frame(t(vals))
    vsl <- extract(s, puntoLCC,ncol = 2)
    datos$snow_level <- t(vsl)
    vpreci <- extract(p, puntoLCC,ncol = 2)
    datos$lluvia <- t(vpreci)
    spreci <- extract(snow, puntoLCC,ncol = 2)
    datos$nieve <- t(spreci)
    datos$time<-idx
    
    t_plot <- ggplot(datos, aes(x=time, y=t.vals.))+ geom_line(colour = "red", size = 1)+ylab("Temperatura ºC") + xlab("Fecha")+ ggtitle("Temperatura")+scale_x_datetime(breaks = date_breaks("4 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))
    s_plot <- ggplot(datos, aes(x=time, y=nieve))+geom_bar(stat="identity",fill="orange", colour="orange") +ylab("mm/h") + xlab("Fecha") + ggtitle("Nieve")+scale_x_datetime(breaks = date_breaks("4 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))
    sl_plot <- ggplot(datos, aes(x=time, y=snow_level)) + geom_line(colour = "green", size = 1)+ylab("Altura (m)") + xlab("Fecha") + ggtitle("Snow level")+scale_x_datetime(breaks = date_breaks("4 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))
    p_plot <- ggplot(datos, aes(x=time, y=lluvia))+geom_bar(stat="identity",fill="blue", colour="blue")+ylab("mm/h") + xlab("Fecha") + ggtitle("Lluvia")+scale_x_datetime(breaks = date_breaks("4 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))        
    multiplot<-grid.arrange(t_plot, sl_plot, s_plot, p_plot, ncol=2, main = paste('Coordenadas ', input$lat, ', ', input$lon))
    
    print(multiplot)
    
  }
  
  output$plot <- renderPlot(plotFunc(), height=500)
  
  # Generate a summary of the data
  
  output$summary <- renderPrint({
    puntoLCC <- puntoTRANS() 
    
    vals <- extract(t, puntoLCC,ncol = 2)
    datos <- data.frame(t(vals))
    vsl <- extract(s, puntoLCC,ncol = 2)
    datos$snow_level <- t(vsl)
    vpreci <- extract(p, puntoLCC,ncol = 2)
    datos$lluvia <- t(vpreci)
    spreci <- extract(snow, puntoLCC,ncol = 2)
    datos$nieve <- t(spreci)
    datos$time<-idx
    
    summary(datos)
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    
    puntoLCC <- puntoTRANS() 
    
    vals <- extract(t, puntoLCC,ncol = 2)
    datos <- data.frame(t(vals))
    vsl <- extract(s, puntoLCC,ncol = 2)
    datos$snow_level <- t(vsl)
    vpreci <- extract(p, puntoLCC,ncol = 2)
    datos$lluvia <- t(vpreci)
    spreci <- extract(snow, puntoLCC,ncol = 2)
    datos$nieve <- t(spreci)
    datos$time<-idx
    
    datos
  })
  
  output$map <- renderPlot({
    map.base <- get_googlemap(
      center= punto(),
      maptype = 'hybrid', ## Map type as defined above (roadmap, terrain, satellite, hybrid)
      markers = data.frame(punto()),
      zoom = 10, ## 14 is just about right for a 1-mile radius
      scale = 1, ## Set it to 2 for high resolution output
    )
    #map.base = get_map(location = c(input$lat,input$lon),zoom=14, source = "osm")
    
  print(ggmap(map.base))
  }, width = 600, height = 600)

})
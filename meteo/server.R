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
idx <- seq(as.POSIXct('2014-06-10 01:00:00', tz="UTC"), as.POSIXct('2014-06-14 00:00:00', tz="UTC"), 'hour')
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
vd <- stack(archivo, varname = "dir")
proj4string(vd) <- CRS("+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84")
vm <- stack(archivo, varname = "mod")
proj4string(vm) <- CRS("+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84")



shinyServer(function(input, output) {
  
  punto <- reactive({
    geocode(input$text) 
  })
  puntoTRANS <- reactive({
    ptoWGS <- SpatialPoints(punto(), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projLCC <- projection(t)
    spTransform(ptoWGS, CRS(projLCC)) 
  }) 
  
  
  plot1 = function() {
    
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
    
    t_plot <- ggplot(datos, aes(x=time, y=t.vals.))+ geom_line(colour = "red", size = 1)+ylab("Temperatura ºC") + xlab("Fecha")+ ggtitle("Temperatura")+scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))
    s_plot <- ggplot(datos, aes(x=time, y=nieve))+geom_bar(stat="identity",fill="orange", colour="orange") +ylab("mm/h") + xlab("Fecha") + ggtitle("Nieve")+scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))
    sl_plot <- ggplot(datos, aes(x=time, y=snow_level)) + geom_line(colour = "green", size = 1)+ylab("Altura (m)") + xlab("Fecha") + ggtitle("Snow level")+scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))
    p_plot <- ggplot(datos, aes(x=time, y=lluvia))+geom_bar(stat="identity",fill="blue", colour="blue")+ylab("mm/h") + xlab("Fecha") + ggtitle("Lluvia")+scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+theme(axis.text.x = element_text(angle = 90))        
    multiplot<-grid.arrange(t_plot, sl_plot, s_plot, p_plot, ncol=2, main = input$text)
    
    print(multiplot)   
  }
  
  output$plot <- renderPlot(plot1(), height=500)
  
  plot2 = function() {
    
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
    
    #Define Margins. The trick is to use give as much space possible on the left margin (second value)
    par(mar=c(4, 14, 1, 1) + 0.1)
    #Plot the first time series. Notice that you don't have to draw the axis nor the labels
    
    plot(datos$time, datos$t.vals., axes=F, ylim=c(min(datos$t.vals.)-10,max(datos$t.vals.)+5), xlab="", ylab="",type="l",col="red", main="")
    points(datos$time, datos$t.vals.,pch=20,col="red")
    axis(2, ylim=c(min(datos$t.vals.)-10,max(datos$t.vals.)+5),col="red",lwd=2)
    mtext(2,text="Temperatura - ºC",line=2)
    
    #Plot the second time series. The command par(new=T) is handy here. If you just need to plot two timeseries, you could also use the right vertical axis as well. In that case you have to substitute "2" with "4" in the functions axis() and mtext(). Notice that in both functions lines is increased so that the new axis and its label is placed to the left of the first one. You don't need to increase the value if you use the right vertical axis.
    par(new=T)
    plot(datos$time, datos$snow_level, axes=F, ylim=c(0,max(datos$snow_level)+500), xlab="", ylab="", col="green",
         type="l",lty=2, main="",lwd=2)
    axis(2, ylim=c(0,max(datos$snow_level)+500),col="green",lwd=2,line=3.5)
    points(datos$time, datos$snow_level,col="green",pch=20)
    mtext(2,text="Snow level - msnm",line=5.5)
    
    #Plot the third time series. Again the line parameter are both further increased.
    
    par(new=T)
    plot(datos$time, datos$lluvia, axes=F, ylim=c(0,max(datos$lluvia)+2), xlab="", ylab="", 
         type="l",col="blue", lty=3, main="",lwd=2)
    axis(2, ylim=c(0,max(datos$lluvia)+2),col="blue", lwd=2,line=7)
    points(datos$time, datos$lluvia,col="blue",pch=20)
    mtext(2,text="Lluvia - mm/h",line=9)
    
    par(new=T)
    plot(datos$time, datos$nieve, axes=F, ylim=c(0,max(datos$nieve)+1), xlab="", ylab="", 
         type="l",col="darkgrey", lty=2, main="",lwd=2)
    axis(2, ylim=c(0,max(datos$nieve)+1),col="darkgrey", lwd=2,line=10.5)
    points(datos$nieve, datos$lluvia,col="darkgrey",pch=20)
    mtext(2,text="Nieve - mm/h",line=12.5)
    
    #We can now draw the X-axis, which is of course shared by all the three time-series.
    axis.POSIXct(1, datos$time, format="%m/%d %H:%M")
    mtext("Day",side=1,col="grey",line=2)
    
    #Leyenda
    legend("topright",legend=c("Temperatura","Snow level","Lluvia","Nieve"),
           cex = 0.9, ncol = 1,lty=c(1,2,3,2),col=c("red","green","blue","darkgrey"))  
  }
  
  output$plot2 <- renderPlot(plot2(), height=500)

  # Wind plot
  plot3 = function() {
    
    puntoLCC <- puntoTRANS()
    
    vals <- extract(vd, puntoLCC,ncol = 2)
    datos <- data.frame(t(vals))
    vsl <- extract(vm, puntoLCC,ncol = 2)
    datos$vm <- t(vsl)
    datos$time<-idx
    
    ### Plot de viento
    #Define Margins. The trick is to use give as much space possible on the left margin (second value)
    par(mar=c(4, 7, 1, 1) + 0.1)
    #Plot the first time series. Notice that you don't have to draw the axis nor the labels
    
    plot(datos$time, datos$t.vals., axes=F, ylim=c(0,360), xlab="", ylab="",type="l",col="darkgrey", main="")
    points(datos$time, datos$t.vals.,pch=20,col="darkgrey")
    axis(2, ylim=c(0,360),col="darkgrey",lwd=2)
    mtext(2,text="Direcci?n del viento",line=2)
    
    #Plot the second time series. The command par(new=T) is handy here. If you just need to plot two timeseries, you could also use the right vertical axis as well. In that case you have to substitute "2" with "4" in the functions axis() and mtext(). Notice that in both functions lines is increased so that the new axis and its label is placed to the left of the first one. You don't need to increase the value if you use the right vertical axis.
    par(new=T)
    plot(datos$time, datos$vm, axes=F, ylim=c(0,max(datos$vm)+2), xlab="", ylab="", col="blue",
         type="l",lty=2, main="",lwd=2)
    axis(2, ylim=c(0,max(datos$vm)+2),col="blue",lwd=2,line=3.5)
    points(datos$time, datos$vm,col="blue",pch=20)
    mtext(2,text="Velocidad del viento m/s",line=5.5)
    
    #We can now draw the X-axis, which is of course shared by all the three time-series.
    axis.POSIXct(1, datos$time, format="%m/%d %H:%M")
    mtext("Day",side=1,col="grey",line=2)
    
    #Leyenda
    legend("topright",legend=c("Dirección","Velocidad"),
           cex = 0.9, ncol = 1,lty=c(1,2,3,2),col=c("darkgrey","blue"))
    
    
  }
  
  output$plot3 <- renderPlot(plot3(), height=500)  
  
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
      center= cbind(punto()$lon,punto()$lat),
      maptype = 'hybrid', ## Map type as defined above (roadmap, terrain, satellite, hybrid)
      markers = punto(),
      zoom = 12, ## 14 is just about right for a 1-mile radius
      scale = 1, ## Set it to 2 for high resolution output
    )
    #map.base = get_map(location = c(input$lat,input$lon),zoom=14, source = "osm")
    
  print(ggmap(map.base))
  }, width = 600, height = 600)

})
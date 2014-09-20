require(XML)
require(ggplot2)
library(ggmap)
library(rgbif)
library(scales)
library(gridExtra)
library(markdown)
options(encoding = "UTF-8")

shinyServer(function(input, output) {
  
  punto <- reactive({
    geocode(input$text) 
  })
  
  dataXML <- reactive({
    inp <- geocode(input$text)
    pto<-elevation(latitude=inp$lat,longitude=inp$lon)
    climateDataXML <- paste("http://api.yr.no/weatherapi/locationforecast/1.9/?lat=",pto$latitude,";lon=",pto$longitude,";msl=",round(pto$elevation),sep="")
    xmlParse(climateDataXML)
  })
  
  dataP <- reactive({
    doc <- dataXML()
    ### datetime
    e <- lapply(doc["//product/time[@from!=@to]"], xmlAttrs) 
    r <- do.call(rbind, e) 
    u <- as.data.frame(r)
    ##precipitation
    pre = xpathSApply(doc,'//product/time/location/precipitation',xmlAttrs)
    pre = t(pre)
    pre[,2]<-as.numeric(pre[,2])
    
    preci <- cbind(u,pre[,2])
    names(preci)<-c('tipo','from','to','preci')
    
    preci[,3] <- as.POSIXct(strptime(preci[,3], "%Y-%m-%dT%H:%M:%S"),tz="UTC")
    preci[,2] <- as.POSIXct(strptime(preci[,2], "%Y-%m-%dT%H:%M:%S"),tz="UTC")
    preci[,4] <- as.numeric(as.character(preci[,4]))
    preci
  })
  
  dataT <- reactive({  
    doc <- dataXML()
    y <- lapply(doc["//product/time[@from = @to]"], xmlAttrs) 
    m <- do.call(rbind, y) 
    d <- as.data.frame(m)
    ##temperature
    tem = xpathSApply(doc,'//product/time/location/temperature',xmlAttrs)
    tem = t(tem)
    tem[,3]<-as.numeric(tem[,3])
    ##windDirection
    wd = xpathSApply(doc,'//product/time/location/windDirection',xmlAttrs)
    wd = t(wd)
    wd[,2]<-as.numeric(wd[,2])
    ##windSpeed ms
    ws = xpathSApply(doc,'//product/time/location/windSpeed',xmlAttrs)
    ws = t(ws)
    ws[,2]<-as.numeric(ws[,2])
    ##humidity %
    hu = xpathSApply(doc,'//product/time/location/humidity',xmlAttrs)
    hu = t(hu)
    hu[,1]<-as.numeric(hu[,1])
    ##pressure hPa
    pr = xpathSApply(doc,'//product/time/location/pressure',xmlAttrs)
    pr = t(pr)
    pr[,3]<-as.numeric(pr[,3])
    ##cloudiness %
    cl = xpathSApply(doc,'//product/time/location/cloudiness',xmlAttrs)
    cl = t(cl)
    cl[,2]<-as.numeric(cl[,2])
    
    datos <- cbind(d,tem[,3],wd[,2],wd[,3],ws[,2],hu[,1],pr[,3],cl[,2])
    names(datos)<-c('tipo','from','to','temp','wd','wdt','ws','hu','pr','cl')
    
    datos[,2] <- as.POSIXct(strptime(datos[,2], "%Y-%m-%dT%H:%M:%S"),tz="UTC")
    datos[,3] <- as.POSIXct(strptime(datos[,3], "%Y-%m-%dT%H:%M:%S"),tz="UTC")
    datos[,4] <- as.numeric(as.character(datos[,4]))
    datos[,5] <- as.numeric(as.character(datos[,5]))
    datos[,6] <- as.character(datos[,6])
    datos[,7] <- as.numeric(as.character(datos[,7]))
    datos[,8] <- as.numeric(as.character(datos[,8]))
    datos[,9] <- as.numeric(as.character(datos[,9]))
    datos[,10] <- as.numeric(as.character(datos[,10]))
    datos
  }) 
  
  
  plot1 = function() {
    datosT<-dataT()
    t_plot <- ggplot(datosT, aes(x=to, y=temp))+
      geom_line(colour = "red", size = 1)+
      ylab("Temperatura ºC") + xlab("Fecha")+
      ggtitle("Temperatura")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    p_plot <- ggplot(datosT, aes(x=to, y=pr))+
      geom_line(colour = "blue", size = 1)+
      ylab("Presión hPa") + xlab("Fecha")+
      ggtitle("Presión atmosférica")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    h_plot <- ggplot(datosT, aes(x=to, y=hu))+
      geom_line(colour = "green", size = 1)+
      ylab("Humedad %") + xlab("Fecha")+
      ggtitle("Humedad")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    cl_plot <- ggplot(datosT, aes(x=to, y=cl))+
      geom_line(colour = "grey", size = 1)+
      ylab("Nubosidad %") + xlab("Fecha")+
      ggtitle("Nubosidad")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    multiplot<-grid.arrange(t_plot, p_plot, h_plot, cl_plot, ncol=1, main = paste("Predicción para: ",input$text,sep=''))
    
    print(multiplot)   
  }
  
  output$plot <- renderPlot(plot1(), height=800)
  
  plot2 = function() {
    datosT <- dataT()
    wd_plot <- ggplot(datosT, aes(x=to, y=wd,label=wdt))+
      geom_line(colour = "red", size = 1)+
      geom_text(fontface="bold",size=4)+
      ylab("Dirección del viento º") + xlab("Fecha")+
      ggtitle("Dirección del viento")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    ws_plot <- ggplot(datosT, aes(x=to, y=ws))+
      geom_line(colour = "orange", size = 1)+
      ylab("Velocidad del viento ms") + xlab("Fecha")+
      ggtitle("Velocidad del viento")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    multiplot<-grid.arrange(wd_plot, ws_plot, ncol=1, main = paste("Predicción para: ",input$text,sep=''))
    print(multiplot)
  }
  output$plot2 <- renderPlot(plot2(), height=500)

  plot3 = function() {
    datosT <- dataT()
    datosP <- dataP() 
    cl_plot <- ggplot(datosT, aes(x=to, y=cl))+
      geom_line(colour = "grey", size = 1)+
      ylab("Nubosidad %") + xlab("Fecha")+
      ggtitle("Nubosidad")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    preci_plot <- ggplot(datosP, aes(x=from, y=preci))+
      geom_bar(stat="identity",fill="blue", colour="blue") +
      ylab("mm") + xlab("Fecha") + ggtitle("Precipitación")+
      scale_x_datetime(breaks = date_breaks("6 hours"),labels = date_format("%d-%m %H h"))+
      theme(axis.text.x = element_text(angle = 90))
    
    multiplot<-grid.arrange(preci_plot, cl_plot, ncol=1, main = paste("Predicción para: ",input$text,sep=''))
    print(multiplot)
  }
  
  output$plot3 <- renderPlot(plot3(), height=500)
  
  output$map <- renderPlot({
    map.base <- get_googlemap(
      center= cbind(punto()$lon,punto()$lat),
      maptype = 'hybrid', ## Map type as defined above (roadmap, terrain, satellite, hybrid)
      markers = data.frame(punto()$lon,punto()$lat),
      zoom = 11, ## 14 is just about right for a 1-mile radius
      scale = 1, ## Set it to 2 for high resolution output
    )
    #map.base = get_map(location = c(input$lat,input$lon),zoom=14, source = "osm")
    
  print(ggmap(map.base))
  }, width = 500, height = 500)

  
})
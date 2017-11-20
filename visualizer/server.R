# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 20*1024^2)

# Define server logic to read selected file ----

# Create the map




server <- function(input, output,session) {
  
  
  
  
  output$map1 <- renderLeaflet(env=parent.frame(sys.nframe()), {
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% clearBounds()
    
  })
  
  outputOptions(output,"map1",suspendWhenHidden=FALSE)
  
  colorpal <- reactive({
    colorNumeric(input$colors, df$monitor)
  })
  
 data <- reactive({ 
    
   req(input$mergeButton)
    #req(input$file1) ## ?req #  require that the input is available
    #input$mergeButton
   
   gps.data = NULL
   langan.data = NULL
   ptrak.data = NULL
   ptrakscreen.data =NULL
   
    if(!is.null(input$gpsfile)){
      gps.data.list <- lapply(1:nrow(input$gpsfile), FUN = function(fileind) {
        read.gps(datafile=input$gpsfile[[fileind, "datapath"]], 
                 runname = getrunname(input$gpsfile[[fileind, "name"]]),
                 timeaverage = as.numeric(input$usertimav)/60)
      })
      
      gps.data = rbindlist(gps.data.list, fill=T)
      setkey(gps.data, timeint, runname)
    }
      
      if(!is.null(input$LanganCO)){
        langan.data.list <- lapply(1:nrow(input$LanganCO), FUN = function(fileind) {
          read.langan(datafile=input$LanganCO[[fileind, "datapath"]], 
                   runname = getrunname(input$LanganCO[[fileind, "name"]]),
                   timeaverage = as.numeric(input$usertimav)/60)
        })
        
       langan.data = rbindlist(langan.data.list, fill=T)
       setkey(langan.data, timeint, runname)
      }
   
   if(!is.null(input$ptrak)){
     ptrak.data.list <- lapply(1:nrow(input$ptrak), FUN = function(fileind) {
       read.ptrak(datafile=input$ptrak[[fileind, "datapath"]], 
                runname = getrunname(input$ptrak[[fileind, "name"]]),
                timeaverage = as.numeric(input$usertimav)/60,
                screen = F)
     })
     
     ptrak.data = rbindlist(ptrak.data.list, fill=T)
     setkey(ptrak.data, timeint, runname)
   }
   
   if(!is.null(input$ptrakscreen)){
     ptrak.screen.data.list <- lapply(1:nrow(input$ptrakscreen), FUN = function(fileind) {
       read.ptrak(datafile=input$ptrakscreen[[fileind, "datapath"]], 
                  runname = getrunname(input$ptrakscreen[[fileind, "name"]]),
                  timeaverage = as.numeric(input$usertimav)/60,
                  screen=T)
     })
     
     ptrakscreen.data = rbindlist(ptrak.screen.data.list, fill=T)
     setkey(ptrakscreen.data, timeint, runname)
   }
   
   if(!is.null(input$filelog)){
     filelog.data.list <- lapply(1:nrow(input$filelog), FUN = function(fileind) {
       read.ptrak(datafile=input$filelog[[fileind, "datapath"]], 
                  runname = getrunname(input$filelog[[fileind, "name"]]),
                  timeaverage = as.numeric(input$usertimav)/60,
                  screen=T)
     })
     
     filelog.data = rbindlist(filelog.data.list, fill=T)
     setkey(filelog.data, timeint, runname)
   }
   
   indexval = c(!is.null(gps.data),!is.null(langan.data), !is.null(ptrak.data), !is.null(ptrak.data))
   
   merge.all <- function(x, y) {
     merge(x, y, all=TRUE)
   }
   
   output <- Reduce(merge.all, list(gps.data, langan.data, ptrak.data, ptrakscreen.data)[indexval])
   try(
   output[,pnc_diff := pnc_noscreen - pnc_screen],
   silent=T)
   
   return(output)
   
  })
  
  
  output$contents <- renderTable({
    
    req(input$mergeButton)
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    mydata<<-data()
    
    return( apply(mydata[1:max(input$rowsn),],2, as.character))
    
  })
  
  
  # output$tsplot <- renderPlot({
  #   
  #   
  #   if (is.null(df)) {
  #     
  #     ggplot() + 
  #       annotate("text", 
  #                x = 4, y = 25, size=8, color="darkgrey",
  #                label = "Please Load Data.") + 
  #       theme_bw() +
  #       theme(panel.grid.major=element_blank(),
  #             panel.grid.minor=element_blank())+
  #       theme(line = element_blank(),
  #             text = element_blank(),
  #             title = element_blank())
  #     
  #   } else  {
  #     
  #     plotdata=data()
  #     
  #     p1 <- ggplot(plotdata, 
  #                  aes(as.POSIXct(localdatetime), as.numeric(as.character(pm25m)), color=monitor)) + 
  #       ylab( expression(paste("PM"[2.5], " (", mu, "g/", m^3, ")")) ) + xlab("Time") +
  #       theme_light(12)+ ylim(0, as.numeric(as.character(input$ylimpm)) )+
  #       xlim(as.POSIXct(input$Dates[1],origin="1970-01-01"), as.POSIXct(input$Dates[2],origin="1970-01-01"))
  #     
  #     p1 + geom_point(alpha =0.3, cex=.75)  + 
  #       #scale_x_datetime(date_breaks ="2 day", date_labels = "%m/%d") +
  #       #geom_point(data=df, aes(as.POSIXct(datetime), outdoorPM2.5, color="Outdoor"), size=.75) + 
  #       
  #       guides(colour = guide_legend(override.aes = list(size=6)))
  #   }
  #   
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mydata", ".csv", sep = "")
    },
    content = function(con) {
      write.csv(data(), con, row.names = FALSE)
    }
  )
  
  
  
  
  observe( {
    req(input$mergeButton)
    pollutant = input$pollmap
    mapdata<-data.table(mydata)
    mapdata$poll <- as.numeric(as.character(mapdata[[pollutant]]))
    if(!(input$pollmap %in% colnames(mapdata))) {
      leafletMap
    } else {
    
    mapdata <- mapdata[!is.na(poll),]
    mapdata<-mapdata[,lon:=as.numeric(as.character(Longitude))]
    mapdata<-mapdata[,lat:=as.numeric(as.character(Latitude))]
    mapdata<-mapdata[!lon==0,]
    mapdata<-mapdata[!lat==0,]
    leafletmap <-leafletProxy("map1", data=mapdata)

    leafletmap  %>% clearControls()
    
    leafletmap %>% clearHeatmap()


    palette_rev2 <- c("#0066b2","#00addd","#00e9a2","#f2cd00","#c69522","#9c5600","#62452c")
    pal2 <- colorBin(palette_rev2, pretty=T, na.color ="lightgrey",
                    c(min(mapdata$poll), max(mapdata$poll)))

    leafletmap  %>%
      addHeatmap(~lon, ~lat, radius = 3, intensity = mapdata$poll, gradient = pal2(mapdata$poll), blur = 2,
                 data=mapdata, minOpacity=.1)  %>% 
      setView(median(mapdata$lon, na.rm=T),median(mapdata$lat, na.rm=T), zoom=11)  %>%
    addLegend("topleft", pal=pal2, values=mapdata$poll, title=pollutant,
              layerId="colorLegend",na.label = "No Data", opacity=1)
      # 
      # addCircleMarkers(~lon, ~lat , radius=7,
      #                  stroke=T, weight=1.2, opacity=.6,
      #                  #fillColor = pal2(mapdata[pollutant]),
      #                  fillOpacity=.3, group=~runname)#%>%
    }

    
    
  })
  
  
  
}
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
   ae51.data= NULL
   nanoScan.data = NULL
   nanoSingle.data = NULL
   Labview.data=NULL
   filelog.data =NULL
   weatherdata = NULL
   
    if(!is.null(input$gpsfile)){
      gps.data.list <- lapply(1:nrow(input$gpsfile), FUN = function(fileind) {
        read.gps(datafile=input$gpsfile[[fileind, "datapath"]], 
                 runname = getrunname(input$gpsfile[[fileind, "name"]]),
                 timeaverage = as.numeric(input$usertimav)/60,
                 splineval= "missing" %in% input$dataoptions)
      })
      
      gps.data = rbindlist(gps.data.list, fill=T)
      setkey(gps.data, timeint, runname)
    }
      
      if(!is.null(input$LanganCO)){
        langan.data.list <- lapply(1:nrow(input$LanganCO), FUN = function(fileind) {
          read.langan(datafile=input$LanganCO[[fileind, "datapath"]], 
                   runname = getrunname(input$LanganCO[[fileind, "name"]]),
                   timeaverage = as.numeric(input$usertimav)/60,
                   splineval= "missing" %in% input$dataoptions )
        })
        
       langan.data = rbindlist(langan.data.list, fill=T)
       setkey(langan.data, timeint, runname)
      }
   
   if(!is.null(input$ptrak)){
     ptrak.data.list <- lapply(1:nrow(input$ptrak), FUN = function(fileind) {
       read.ptrak(datafile=input$ptrak[[fileind, "datapath"]], 
                runname = getrunname(input$ptrak[[fileind, "name"]]),
                timeaverage = as.numeric(input$usertimav)/60,
                screen = F,
                splineval= "missing" %in% input$dataoptions )
     })
     
     ptrak.data = rbindlist(ptrak.data.list, fill=T)
     
     ptrak.data = ptrak.data[, lapply(.SD, mean) , by = c("timeint","runname")]
     
     setkey(ptrak.data, timeint, runname)
     
     
  
   }
   
   if(!is.null(input$ptrakscreen)){
     ptrak.screen.data.list <- lapply(1:nrow(input$ptrakscreen), FUN = function(fileind) {
       read.ptrak(datafile=input$ptrakscreen[[fileind, "datapath"]], 
                  runname = getrunname(input$ptrakscreen[[fileind, "name"]]),
                  timeaverage = as.numeric(input$usertimav)/60,
                  screen=T,
                  splineval= "missing" %in% input$dataoptions )
     })
     
     ptrakscreen.data = rbindlist(ptrak.screen.data.list, fill=T)
     ptrakscreen.data = ptrakscreen.data[, lapply(.SD, mean) , by = c("timeint","runname")]
     setkey(ptrakscreen.data, timeint, runname)
   }
   
   if(!is.null(input$ae51)){
     ae51.data.list <- lapply(1:nrow(input$ae51), FUN = function(fileind) {
       read.ae51(datafile=input$ae51[[fileind, "datapath"]], 
                  runname = getrunname(input$ae51[[fileind, "name"]]),
                  timeaverage = as.numeric(input$usertimav)/60,
                 splineval= "missing" %in% input$dataoptions )
     })
     
     ae51.data = rbindlist(ae51.data.list, fill=T)
     setkey(ae51.data, timeint, runname)
   }
   
   if(!is.null(input$nanoScan)){
     nanoScan.data.list <- lapply(1:nrow(input$nanoScan), FUN = function(fileind) {
       read.nano.scan(datafile=input$nanoScan[[fileind, "datapath"]], 
                 runname = getrunname(input$nanoScan[[fileind, "name"]]),
                 timeaverage = as.numeric(input$usertimav)/60,
                 splineval= "missing" %in% input$dataoptions)
     })
     
     nanoScan.data = rbindlist(nanoScan.data.list, fill=T)
     setkey(nanoScan.data, timeint, runname)
   }
   
   if(!is.null(input$nanoSingle)){
     nanoSingle.data.list <- lapply(1:nrow(input$nanoSingle), FUN = function(fileind) {
       read.nano.single(datafile=input$nanoSingle[[fileind, "datapath"]], 
                 runname = getrunname(input$nanoSingle[[fileind, "name"]]),
                 timeaverage = as.numeric(input$usertimav)/60,
                 splineval= "missing" %in% input$dataoptions)
     })
     
     nanoSingle.data = rbindlist(nanoSingle.data.list, fill=T)
     setkey(nanoSingle.data, timeint, runname)
   }
   
   if(!is.null(input$Labview)){
     Labview.data.list <- lapply(1:nrow(input$Labview), FUN = function(fileind) {
       read.labview(datafile=input$Labview[[fileind, "datapath"]], 
                        runname = getrunname(input$Labview[[fileind, "name"]]),
                        timeaverage = as.numeric(input$usertimav)/60,
                    splineval= "missing" %in% input$dataoptions)
     })
     
     Labview.data = rbindlist(Labview.data.list, fill=T)
     setkey(Labview.data, timeint, runname)
   }
   
   if(!is.null(input$filelog)){
     filelog.data.list <- lapply(1:nrow(input$filelog), FUN = function(fileind) {
       read.ptrak(datafile=input$filelog[[fileind, "datapath"]], 
                  runname = getrunname(input$filelog[[fileind, "name"]]),
                  timeaverage = as.numeric(input$usertimav)/60,
                  splineval= "missing" %in% input$dataoptions)
     })
     
     filelog.data = rbindlist(filelog.data.list, fill=T)
     setkey(filelog.data, timeint, runname)
   }
   
   
   indexval = c(!is.null(gps.data),
                !is.null(langan.data), 
                !is.null(ptrak.data), 
                !is.null(ptrakscreen.data),
                !is.null(ae51.data),
                !is.null(nanoScan.data),
                !is.null(nanoSingle.data),
                !is.null(Labview.data),
                !is.null(filelog.data)
                )
   
   merge.all <- function(x, y) {
     merge(x, y, all=TRUE)
   }
   
   output <- Reduce(merge.all, list(gps.data, langan.data, ptrak.data, ptrakscreen.data,
                                    ae51.data,
                                    nanoScan.data, nanoSingle.data,
                                    Labview.data, filelog.data)[indexval])
   
   if("ksea" %in% input$dataoptions){
     weatherdata <- get_ASOS(date_start=format(min(output$timeint), "%Y-%m-%d"),
                             date_end = format(max(output$timeint), "%Y-%m-%d"))
     setkey(weatherdata, timeint)
     
     output = weatherdata[output]
     
   }
   
   if("missing" %in% input$dataoptions)
     
     output[, (which(sapply(output, is.numeric))) :=
            lapply(.SD, function(x)
       na.spline(x, na.rm=F, maxgap=10)),
                   , .SDcols = which(sapply(output, is.numeric))]
   
   try(
   output[,pnc_diff := pnc_noscreen - pnc_screen],
   silent=T)
   
   try(
     output[,ratio := pnc_diff / BC],
     silent=T)
   
   return(output)
   
   #add nano scan single mode (timestamp is the end of the interval) (DONE)
   #add nano scan scanning mode (timestamp is the beginning of the interval) (DONE)
   #add labview gps and CO2 (bu353) new GPS is (dg500) labview is end of the 10 second int (DONE)
   #import weather data from airport? 
   #AE51 time stamp end of interval (ng/m3) in BC column (DONE)
   #could you create an E/W concentration gradient, maybe compare to ?
   #ratio of pdif/bc add to file 
   #change color scale 
   #add boxcar values 
   #add interpolation
   #incude wind rose conditional on threshold pollutant value
   #using windspeed to triangulate source?
   #indentify longitude bands, derive summary statistics for each 
   #get 18th of august windrose
   #get october 3rd windrose
  })
  
  
  output$contents <- renderTable({
    
    req(input$mergeButton)
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    mydata <<- data()
    
    return( apply(mydata[1:max(input$rowsn),],2, as.character))
    
  })
  
  
  output$tsplot <- renderPlot({
    df = data()

    if (is.null(df)) {

      ggplot() +
        annotate("text",
                 x = 4, y = 25, size=8, color="darkgrey",
                 label = "Please Load Data.") +
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())+
        theme(line = element_blank(),
              text = element_blank(),
              title = element_blank())

    } else  {

      plotdata=data()

      p1 <- ggplot(plotdata,
                   aes(as.POSIXct(timeint), as.numeric(as.character(pnc_diff)), color=runname)) +
        ylab( "Particle Count Difference (#/cc)" ) + xlab("Time") +
        theme_light(12)+ ylim(0, as.numeric(as.character(input$ylimpm)) )+
        xlim(as.POSIXct(input$Dates[1],origin="1970-01-01"), as.POSIXct(input$Dates[2],origin="1970-01-01"))

      p1 + geom_point(alpha =0.3, cex=.75)  +
        #scale_x_datetime(date_breaks ="2 day", date_labels = "%m/%d") +
        #geom_point(data=df, aes(as.POSIXct(datetime), outdoorPM2.5, color="Outdoor"), size=.75) +

        guides(colour = guide_legend(override.aes = list(size=6)))
    }

  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("MOVUPdatamerge", ".csv", sep = "")
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
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
  
 data <- eventReactive(input$mergeButton, { 
    
   #req(input$mergeButton)
    #req(input$file1) ## ?req #  require that the input is available
    #input$mergeButton
   
   gps.data = NULL
   langan.data = NULL
   ptrak.data = NULL
   ptrakscreen.data =NULL
   ae51.data= NULL
   cpc.data=NULL
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
   
   if(!is.null(input$cpc)){
     cpc.data.list <- lapply(1:nrow(input$cpc), FUN = function(fileind) {
       read.cpc(datafile=input$ae51[[fileind, "datapath"]], 
                 runname = getrunname(input$cpc[[fileind, "name"]]),
                 timeaverage = as.numeric(input$usertimav)/60,
                 splineval= "missing" %in% input$dataoptions )
     })
     
     cpc.data = rbindlist(cpc.data.list, fill=T)
     setkey(cpc.data, timeint, runname)
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
                !is.null(cpc.data),
                !is.null(nanoScan.data),
                !is.null(nanoSingle.data),
                !is.null(Labview.data),
                !is.null(filelog.data)
                )
   
   merge.all <- function(x, y) {
     merge(x, y, all=TRUE)
   }
   
   output <- Reduce(merge.all, list(gps.data, langan.data, ptrak.data, ptrakscreen.data,
                                    ae51.data,cpc.data,
                                    nanoScan.data, nanoSingle.data,
                                    Labview.data, filelog.data)[indexval])
   
   if("ksea" %in% input$dataoptions){
     weatherdata <- get_ASOS(date_start=format(min(output$timeint)-60*60*2, "%Y-%m-%d"),
                             date_end = format(max(output$timeint), "%Y-%m-%d"))
     setkey(weatherdata, timeint)
     
     output = weatherdata[output]
     
   }
   
   if("missing" %in% input$dataoptions)
   {
     
     colnamesvals = names(which(sapply(output, is.numeric)))
     
     output[ ,(colnamesvals) :=
               lapply(.SD, as.double), .SDcols=colnamesvals]
     
     output[, (colnamesvals) := 
            lapply(.SD, FUN = function(x){
       tempval= rep(NA, length(x))
       if(is.finite(max(x,na.rm=T))) {
       tempval = na.spline(x, na.rm=F, maxgap= Inf)
       tempval[tempval>=max(x, na.rm=T)]= max(x, na.rm=T)
       tempval[tempval<=min(x, na.rm=T)]= min(x, na.rm=T)
       }
       tempval[is.na(tempval)] = (-9999999)
       tempval[!is.finite(tempval)] = (-9999999)
       tempval
            }),
       .SDcols = colnamesvals, by=runname]
     
     output[output==-9999999] = NA
     
     if("drct" %in% colnames(output))
     {
       maxgapval =  1/as.numeric(input$usertimav)*60*80
   weathervars = c("tmpf","relh","drct","sknt","alti","mslp","vsby")
   
   output[, (weathervars) :=
            lapply(.SD, function(x)
              na.spline(x, na.rm=F, maxgap= maxgapval)),
          , .SDcols = weathervars]
     }
   }
   
   try(
   output[,pnc_diff := pnc_noscreen - pnc_screen],
   silent=T)
   
   try(
     output[,ratio := pnc_diff / BC],
     silent=T)
   
   try({
     
     if(input$usertimav=="1"){
       output[,timeint := as.POSIXct(timeint)]
       # g <- data.table(timeint=seq(min(output$timeint), max(output$timeint), 1))
       # setkey(g, timeint)
       # setkey(output, timeint, runname)
       # 
       # output = output[g]
       
       output[, pnc_background := 
                rollapply(pnc_noscreen, width = 30, FUN = function(x){
                  quantile(x, 0.05, na.rm=T)
                }, align='right', partial=F, fill=NA),
              by=c("runname")]
       output = output[!is.na(runname), ]
     }
   },
   silent=T
   )
   
   plotdata <<- output
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

  
  output$contents <- renderDataTable({
    
    req(input$mergeButton)
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    mydata <- data()
    
    mydata <- as.data.table(mydata)
    
    transformcols = names(mydata)[unlist(mydata[,lapply(.SD, is.numeric)])]
    
    summdata <- mydata[,list(mean = lapply(.SD, FUN = function(x) round(mean(x, na.rm=T), input$sigs)),
                             min = lapply(.SD, FUN = function(x) round(min(x, na.rm=T),input$sigs)),
                             max = lapply(.SD, FUN = function(x) round(max(x, na.rm=T),input$sigs)),
                             missing = lapply(.SD, FUN = function (x){sum(is.na(x))}),
                             N = lapply(.SD, length)),
                             by=runname,
                       .SDcols = transformcols]
    
    
    summdata$variable = transformcols
    summdata = summdata[!variable %in% 
                          c("timeint",
                            "station",
                            "lon",
                            "lat",
                            "sknt",
                            "p01i",
                            "alti",
                            "mslp",
                            "vsby",
                            "skyc1","skyc2","skyc3","skyc4","skyl1",
                            "skyl2","skyl3","skyl4","wxcodes","metar")]
    
    summdata=data.table(summdata)
    
    return(summdata)
    
  })
  
  output$Dates <- renderUI({
    if(req(input$mergeButton)){
      dateRangeInput("Dates", "Date Range:",
                start=min(as.POSIXct(mydata$timeint), na.rm=T),
                end = max(as.POSIXct(mydata$timeint), na.rm=T))
    } else {
      dateRangeInput("Dates", "Date Range:",
                     min="2017-07-01")
    }
      
                })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # output$windRoseplot <- renderPlot({
  #   
  #   if("drct" %in% colnames(plotdata)){
  #     
  #     windRose(plotdata, 
  #              ws = "ws", wd = "drct", bias.corr=T, cols="hue", type="runname")
  #   } 
  #   
  # })
  
  output$tsplot <- renderPlot({
    df = plotdata

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
              title = element_blank()) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)

    } else  {

      plotdata=df
      
      
      rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
                                      space = "rgb")
      
      dateval= unlist(strsplit(unique(plotdata$runname),c("car1","car2","car3"))[1])
      datevalplot = format(as.POSIXct(dateval, format="%Y%b%d"), "%b %d, %Y")
      
      carnames = gsub(dateval, "", unique(plotdata$runname))
      
      
      p1 <- ggplot(plotdata,
                   aes_string("timeint", as.character(input$tspoll), color="runname")) +
        ylab( "Concentration" ) + xlab("Time") +
        theme_light(16) +  
        scale_color_manual(name = datevalplot,
          labels = tools::toTitleCase(carnames),
                          values = rgb.palette(length(unique(plotdata$runname)))) +
        guides(colour = guide_legend(override.aes = list(size=12))) +
        geom_line() + 
        geom_point()+
        scale_x_datetime(limits = (ranges$x))+
        scale_y_continuous(limits = (ranges$y)) 
     
     if("drct" %in% colnames(plotdata))
     {
       
       plotdata$windplottime = floor_date(plotdata$timeint, "15 minutes")
       plotdata[duplicated(windplottime), windplottime := NA]
       plotdata[!is.na(windplottime), windangleplot := drct]
       yrangelower= ggplot_build(p1)$layout$panel_ranges[[1]]$y.range[2]
       yrangeupper= yrangelower + mean(ggplot_build(p1)$layout$panel_ranges[[1]]$y.range)/3
       
       p1 = p1 + 
         geom_rect(ymin = yrangelower, 
                   ymax = yrangeupper, 
                   xmin = -Inf, xmax = Inf, fill = 'grey') +
         expand_limits(y = yrangeupper) +
       geom_text(data=plotdata, inherit.aes=TRUE, 
                      x= plotdata$windplottime,
                      y=mean(yrangelower:yrangeupper),
                      size=6,
                      color="black",
                      aes(angle=-plotdata$windangleplot + 90), label="←")
        }
     
      p1
    }

  })
  
  observeEvent(input$tsplot_dblclick, {
    brush <- input$tsplot_brush
    if (!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin="1970-01-01")
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("MOVUPdatamerge", ".csv", sep = "")
    },
    content = function(con) {
      write.csv(plotdata, con, row.names = FALSE)
    }
  )
  
  
  
  
  observe( {
    req(input$mergeButton)
    pollutant = input$pollmap
    
    mapdata<-data.table(plotdata)
    mapdata$poll <- as.numeric(as.character(mapdata[[pollutant]]))
    if(!(input$pollmap %in% colnames(mapdata))) {
      leafletMap
    } else {
    
    
    mapdata <- mapdata[!is.na(poll),]
    mapdata<-mapdata[,lon:=as.numeric(as.character(Longitude))]
    mapdata<-mapdata[,lat:=as.numeric(as.character(Latitude))]
    mapdata<-mapdata[!lon==0,]
    mapdata<-mapdata[!lat==0,]
    
    if(input$windangle == "north")
    mapdata <- mapdata[drct >= 315 & drct <= 45,]
    
    if(input$windangle == "east")
      mapdata <- mapdata[drct >= 45 & drct <= 135,]
    
    if(input$windangle == "south")
      mapdata <- mapdata[drct >= 135 & drct <= 225,]
    
    if(input$windangle == "west")
      mapdata <- mapdata[drct >= 225 & drct <= 315,]
    
    if(input$windangle == "all")
      mapdata <- mapdata
    
    mapdata[, Lon.r := round(lon, 3)]
    mapdata[, Lat.r := round(lat, 3)]
    mapdata[, smooth_plotvar :=
            lapply(.SD, function(x)
              mean(x, na.rm=F)),
          .SDcols = "poll",
          by = c("Lon.r", "Lat.r", "runname")]
    
    
    leafletmap <-leafletProxy("map1", data=mapdata)

    leafletmap  %>% clearControls()
    
    leafletmap %>% clearHeatmap()


    palette_rev1 <- c("#0066b2","#00addd","#00e9a2","#f2cd00","#c69522","#9c5600","#62452c")
    pal1 <- colorBin(palette_rev1, pretty=T, na.color ="lightgrey",
                    c(min(mapdata$poll), max(mapdata$poll)))
    
    
    
    leafletmap %>%
      addCircleMarkers(data=mapdata, ~Lon.r, ~Lat.r,
                       radius = 1, opacity = .5,
                       color = pal1(unlist(mapdata$poll)),
                       group="November 8th 2017") %>%
      addLegend("bottomleft", pal=pal1, 
                values=mapdata, 
                title="Concentration",
                layerId="colorLegend",na.label = "No Data", opacity=1)  %>% 
      setView(median(mapdata$lon, na.rm=T),median(mapdata$lat, na.rm=T), zoom=11) 
    

    }

    
    
  })
  
  
  
}
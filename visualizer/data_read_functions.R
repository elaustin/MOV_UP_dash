########################
#
#Functions to read data from MOV_UP drives
#
#####################

#ptrack file headers restart when power turns off?
#can you select multiple nanoscan files at the same time?
#for the ptrack, how do you select 
#CPC add multiple files at the same time


gpsfilename<-"C:\\Users\\Johnathan\\OneDrive\\Documents\\UW Postdoc\\MOVUP\\Oct 31\\Air monitor GPS 2017Oct31 airportN,Mgnsn.csv"


getrunname <- function (filenameval) {
  
  unlist(tstrsplit(filenameval, "_", fixed=T, keep=1))
}

#Value of average indicates the end of the sample interval
averageTime <- function (datetime, timeaverage) {
  as.POSIXct(ceiling(as.numeric(datetime)/(timeaverage*60))*(timeaverage*60),origin='1970-01-01')
}

fread2 <- function(filename) {
  tmp <- scan(file = filename, what = "character", sep="\t",quiet = TRUE,nlines=1)
  # remove empty lines caused by \r
  tmp <- tmp[tmp != ""]
  # paste lines back together together with \n character
  tmp <- paste(tmp, collapse = "\n")
  test = fread(tmp,fill=TRUE)
  test
}


read.gps <- function(datafile, runname, timeaverage, splineval = T) {
  
  dt <- fread(datafile)
  dt[, datetime.gps := as.POSIXct(paste0(Date,`Time (local)`), tz="America/Los_Angeles")]
  dt[, runname := runname]
  dt[,Date:= NULL]
  dt[,`Time (local)` := NULL]
  #get Correct time average
  
   dt[,timeint := averageTime(datetime.gps, timeaverage)]
  
  dt = 
    dt[,lapply(.SD, mean), by=c("timeint","runname"), 
       .SDcols=c("Latitude","Longitude","Altitude (m)","Speed (km/hr)")]
  
  if(splineval) {
    
    transcol = c("Latitude","Longitude","Altitude (m)","Speed (km/hr)")
    dt[, (transcol)  := 
         lapply(.SD, function(x)
           na.spline(x, na.rm=F)), .SDcols = transcol ]
  }
  

  return(dt)
}

read.langan <- function(datafile, runname, timeaverage, splineval = T) {
  
  dt <- fread(datafile, skip=1,na.string="")
  dt[, `#` := NULL]
  #ask Time about the TimeZONE
  dt[, datetime.langan := as.POSIXct(`Date Time, GMT-07:00`, format="%m/%d/%y %I:%M:%S %p", tz="America/Los_Angeles")]
  dt[, runname := as.character(runname)]
  dt[,`Date Time, GMT-07:00`:= NULL]
  #get Correct time average
  setnames(dt, grep("CO", colnames(dt), value=T),"CO")
  setnames(dt, grep("Temp", colnames(dt), value=T), "TempC" )
  setnames(dt, grep("Stopped", colnames(dt), value=T), "Stopped")
  
  
  
  dt[,timeint := averageTime(datetime.langan, timeaverage)]
  dt = 
    dt[,lapply(.SD, mean), by=c("timeint","runname"), .SDcols=c("TempC","CO")]
  
  if(splineval) {
    
    transcol = c("CO","TempC")
    dt[, (transcol)  := 
         lapply(.SD, function(x)
           na.spline(x, na.rm=F)), .SDcols = transcol ]
  }
  return(dt)
}

read.ptrak <- function(datafile, runname, timeaverage, screen=F,  splineval = T) {
  
  dt <- fread(datafile, skip=30)
  pncname = ifelse(screen, "pnc_screen","pnc_noscreen")
  colnames(dt) = c("Date", "Time", pncname)
  dt[, datetime.ptrak := as.POSIXct(paste0(Date,Time), format="%m/%d/%Y %T", tz="America/Los_Angeles")]
  dt[, runname := runname]
  dt[, Date:= NULL]
  dt[, Time := NULL]
  #get Correct time average
  
  
  dt[,timeint := averageTime(datetime.ptrak, timeaverage)]
  
  dt = 
    dt[,lapply(.SD, FUN = function(x)
      mean(as.numeric(as.character(x)))), 
      by=c("timeint","runname"), 
       .SDcols=c(pncname)]
  
  if(splineval) {
    
    transcol = c(pncname)
    dt[, (transcol)  := 
         lapply(.SD, function(x)
           na.spline(x, na.rm=F)), .SDcols = transcol ]
  }
  
  return(dt)
}

read.ae51 <- function(datafile, runname, timeaverage,  splineval = T) {
  
  dt <- data.table(read.table(datafile,sep=";", skip=15, header=T))
  dt[, datetime.ae51 := as.POSIXct(paste0(Date,`Time`), tz="America/Los_Angeles")]
  dt[, runname := runname]
  dt[,Date:= NULL]
  dt[,`Time` := NULL]
  #get Correct time average
  
  
  dt[,timeint := averageTime(datetime.ae51, timeaverage)]
  
  dt = 
    dt[,lapply(.SD, mean), by=c("timeint","runname"), 
       .SDcols=c("Ref","Sen","ATN","Flow","PCB.temp","Status","Battery",
                 "BC")]
  
  if(splineval) {
    
    transcol = c("Ref","Sen","ATN","Flow","PCB.temp","Status","Battery",
                 "BC")
    dt[, (transcol)  := 
         lapply(.SD, function(x)
           na.spline(x, na.rm=F )), .SDcols = transcol ]
  }
  
  return(dt)
}


read.labview <- function(datafile, runname, timeaverage) {
  
  dt <- data.table(read.table(datafile,sep="\t", header=T))
  
  dt = dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))),with=F]
  
  cols.labview=c("Computer Time Stamp",
                 "Marker",
                 "GPS  Mode, 1=nofix, 2=2D, 3=3D",
                 "GPS  No. of Active Satellites",
                 "GPS  Hor. Precision (HDOP)",
                 "GPS  Time Stamp",
                 "GPS  Latitude (deg)",
                 "GPS  Longitude (deg)",
                 "GPS  Speed (km/h)",
                 "GPS  Direction (deg)",
                 "Time NO",
                 "Time NOx",
                 "AE52 Time",
                 "AE52Flow","AE52Status",
                 "Precon HS-2000 Temp (°C)",
                 "Precon HS-2000 RH (%)",
                 "SenseAir CO2 conc. (ppm)")
  
  keep.labview=c("Computer Time Stamp",
                 "Marker",
                 "GPS  Mode, 1=nofix, 2=2D, 3=3D",
                 "GPS  No. of Active Satellites",
                 "GPS  Hor. Precision (HDOP)",
                 "GPS  Time Stamp",
                 "GPS  Latitude (deg)",
                 "GPS  Longitude (deg)",
                 "GPS  Speed (km/h)",
                 "GPS  Direction (deg)",
                 "Precon HS-2000 Temp (°C)",
                 "Precon HS-2000 RH (%)",
                 "SenseAir CO2 conc. (ppm)")
  
  setnames(dt, colnames(dt),cols.labview)
  
  num.labview=c( "GPS  No. of Active Satellites",
                 "GPS  Time Stamp",
                 "GPS  Latitude (deg)",
                 "GPS  Longitude (deg)",
                 "GPS  Speed (km/h)",
                 "GPS  Direction (deg)",
                 "Precon HS-2000 Temp (°C)",
                 "Precon HS-2000 RH (%)",
                 "SenseAir CO2 conc. (ppm)")
  
  
  dt[, datetime.labview := as.POSIXct(`Computer Time Stamp`,
                                      format="%d/%m/%Y %I:%M:%S %p",
                                      tz="America/Los_Angeles")]
  
  dt[, `GPS  Time Stamp` := as.POSIXct(`GPS  Time Stamp`,
                                      format="%d/%m/%Y %I:%M:%S %p",
                                      tz="America/Los_Angeles")]
  dt[, runname := runname]
  dt[,`Computer Time Stamp`:= NULL]

  #get Correct time average
  
  dt[,timeint := averageTime(datetime.labview, timeaverage)]
  
  dt = 
    dt[,lapply(.SD, mean), by=c("timeint","runname"), 
       .SDcols=num.labview]
  
  if(splineval) {
    
    transcol = num.labview
    dt[, (transcol)  := 
         lapply(.SD, function(x)
           na.spline(x, na.rm=F)), .SDcols = transcol ]
  }
  
  return(dt)
}

read.nano.scan <- function(datafile, runname, timeaverage, splineval=T) {
  
  dt <- fread(datafile,skip=2, header=T)
  dt[, datetime.nano.scan := as.POSIXct(`Date Time`, tz="America/Los_Angeles")]
  #correct so time is at the end of the interval
  dt[, datetime.nano.scan := datetime.nano.scan + 60]
  dt[, runname := runname]
  dt[,`Date Time`:= NULL]
  #get Correct time average
  
  
  dt[,timeint := averageTime(datetime.nano.scan, timeaverage, splineval=T)]
  
  dt = 
    dt[,lapply(.SD, mean), by=c("timeint","runname","Status")]
  
  if(splineval) {
    
    transcol = c("15.4","20.5","27.4","36.5","48.7","64.9",
                 "86.6","115.5","154.0","205.4","273.8","365.2" ,
                 "Total Conc", "Median (nm)","Mean (nm)",
                 "Geo Mean (nm)", "Mode (nm)", "GSD",
                 "Particle Density (g/cc)")
    dt[, (transcol)  := 
         lapply(.SD, function(x)
           na.spline(x, na.rm=F)), .SDcols = transcol ]
  }
  
  return(dt)
}

read.nano.single <- function(datafile, runname, timeaverage, splineval=T) {
  
  #get datetime
  startval= read.csv(datafile, skip=7, nrow=1,colClasses="character")[2]
  timeval = start= read.csv(datafile, skip=8, nrow=1,colClasses="character")[2]
  start.time = as.POSIXct(paste(startval, timeval), tz="America/Los_Angeles")
  dt <- read.csv(datafile,skip=14, header=T)
  dt = data.table(dt)
  setnames(dt,names(dt), c("Time elapsed","single channel number/cc","status"))
  
  dt[,iteration := -999]
  dt$iteration[1] = 1
  dt[iteration==-999]$iteration = NA
  
  timecorr = data.table(Date = c(startval,t(dt[`Time elapsed`=="Date"][,2])),
                        Time = c(timeval,t(dt[`Time elapsed`=="Time"][,2])))
  
  timecorr[,datetime := as.POSIXct(paste(as.character(timecorr$Date), 
                                         as.character(timecorr$Time)), tz="America/Los_Angeles")]
  
  if(ncol(timecorr)>1){
  dt[`Time elapsed`=="Date", iteration := (2:(ncol(timecorr)))]
  timecorr$iteration = 1:ncol(timecorr)
  }
  
  dt[,iteration := na.locf(iteration)]
  
  setkey(dt, iteration)
  setkey(timecorr, iteration)
  
  dt = dt[timecorr]
  
  dt[, runname := runname]
  
  dt[,`Time elapsed` := as.numeric(as.character(`Time elapsed`))]
  
  dt = dt[!is.na(`Time elapsed`),]
  
  dt[,datetime := datetime + `Time elapsed`]
  
  dt[,timeint := averageTime(datetime, timeaverage)]
  
  dt[,iteration := NULL]
  dt[, `Time elapsed` := NULL]
  dt[,Date := NULL]
  dt[,Time :=NULL]
  
  dt = 
    dt[,lapply(.SD, FUN = 
                 function(x) mean(as.numeric(as.character(x)))), 
                                  by=c("timeint", "status", "runname"),
       .SDcols = c("single channel number/cc")]
  
  if(splineval) {
    
    transcol = c("single channel number/cc")
    dt[, (transcol)  := 
         lapply(.SD, function(x)
           na.spline(x, na.rm=F)), .SDcols = transcol ]
  }
  
  
  return(dt)
}

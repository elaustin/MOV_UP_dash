########################
#
#Functions to read data from MOV_UP drives
#
#####################

gpsfilename<-"C:\\Users\\Johnathan\\OneDrive\\Documents\\UW Postdoc\\MOVUP\\Oct 31\\Air monitor GPS 2017Oct31 airportN,Mgnsn.csv"


getrunname <- function (filenameval) {
  
  unlist(tstrsplit(filenameval, "_", fixed=T, keep=1))
}

averageTime <- function (datetime, timeaverage) {
  as.POSIXct(floor(as.numeric(datetime)/(timeaverage*60))*(timeaverage*60),origin='1970-01-01')
}

read.gps <- function(datafile, runname, timeaverage) {
  
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
  
  return(dt)
}

read.langan <- function(datafile, runname, timeaverage) {
  
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
  
  return(dt)
}

read.ptrak <- function(datafile, runname, timeaverage, screen=F) {
  
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
    dt[,lapply(.SD, mean), by=c("timeint","runname"), 
       .SDcols=c(pncname)]
  
  return(dt)
}

###
#    CODE SNIPPETS FROM MANUSCRIPT WITH INLINE COMMENTS
###

###
# SNIPPET 1 - INSTALLING RNOMADS
###

install.packages("rNOMADS")

###
# SNIPPET 2 - ACCESSING NEAR REAL TIME DATA WITH GRIB
###

library(rNOMADS)
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 2)                    #Find latest model
model.parameters <- ParseModelPage(urls.out[1])                            #Get available predictions
pred.ind <- which(grepl("03$", model.parameters$pred))                     #Get 3 hour forecast
variables <- c("TMP")                                                      #Temperature
levels <- c("2 m above ground", "300 mb")                                  #Vertical location

                                                                           #Download grib file
grib.info <- GribGrab(urls.out[1], model.parameters$pred[pred.ind],
   levels, variables)

                                                                           #Read the model file into R (requires wgrib2 installation)
grib.data <- ReadGrib(grib.info$file.name, levels, variables)

###
# SNIPPET 3 - ACCESSING NEAR REAL TIME DATA WITH DODS
###	

library(rNOMADS)
model.urls <- GetDODSDates("gfs_0p50")                                     #Get available model runs
latest.model <- tail(model.urls$url, 1)                                    #Find most recent run
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

                                                                           #Get data
time <- c(1, 1)                                                            #3 hour forecast
lon <- c(0, 719)                                                           #All 720 longitude points
lat <- c(0, 360)                                                           #All 361 latitude points
var.lev <- "tmp2m"                                                         #Temperature at 2 m above ground
tmp2m.data <- DODSGrab(latest.model, latest.model.run,
    var.lev, time, lon, lat)
var <- "tmpprs"                                                            #Temperature
lev <- c(28, 28)                                                           #300 mb level
tmp300mb.data <- DODSGrab(latest.model, latest.model.run,
    var, time, lon, lat, level = lev)

###
# SNIPPET 4 - ACCESSING ARCHIVED DATA WITH GRIB ###   
###

library(rNOMADS)
model.date <- 20130921
model.hour <- 12
pred <- 3
grib.info <- ArchiveGribGrab("gfsanl", model.date, model.hour, pred)
variables <- c("TMP")
levels <- c("2 m above ground", "300 mb")
grib.data <- ReadGrib(grib.info$file.name, levels, variables)

###
# SNIPPET 5 - ACCESSING ARCHIVED DATA WITH DODS
# Note - the NOMADS NCDC server is currently throwing JavaScript errors when the GrADS-DODS interface is used,
# which means that this code snippet may not work (though real time DODS is fine).
# I have notified the server maintainers of the problem.
# Danny Bowman 2-17-2015
# Apparently gfs-avn-hi does not archive more than a few years before present, so I've modified this script to pull data
# three months before today's date.
###   

library(rNOMADS)
library(lubridate)
date.tmp <- Sys.Date() + duration(-3, units = "month")
model.date <- paste0(year(date.tmp), 
   sprintf("%02i", month(date.tmp)), 
   sprintf("%02i", day(date.tmp)))

model.urls <- GetDODSDates("gfs-avn-hi", archive = TRUE)                   #Get available model runs
m.ind <- which(model.urls$date == as.character(model.date))
model.runs <- GetDODSModelRuns(model.urls$url[m.ind])
model.run <- model.runs$model.run[grepl("1200", model.runs$model.run)]

                                                                           #Get data
time <- c(1, 1)                                                            #3 hour forecast
lon <- c(0, 359)                                                           #All 360 longitude points
lat <- c(0, 180)                                                           #All 180 latitude points
var.lev <- "tmp2m"                                                         #Temperature at 2 m above ground
tmp2m.data <- DODSGrab(model.urls$url[m.ind], model.run,
    var.lev, time, lon, lat)
var <- "tmp"                                                               #Temperature
lev <- c(16, 16)                                                           #300 mb level
tmp300mb <- DODSGrab(model.urls$url[m.ind], model.run,
    var, time, lon, lat, levels = lev)


#######################################################
### DOWNLOADING DATA FROM THE GLOBAL FORECAST SYSTEM USING GRIB 
#######################################################

#  This code shows how to generate global weather maps and atmospheric profiles
#  using rNOMADS and the NCEP GRIB filter.
#  Note that this utilizes the external program 'wgrib2.'
#  I have used this program in Ubuntu and others have had success compiling it for Mac OS.
#  For help, post questions to the rNOMADS mailing list:
#  https://lists.r-forge.r-project.org/mailman/listinfo/rnomads-user

#######################################################
### INSTALLING RNOMADS AND DEPENDENCIES FOR THESE EXAMPLES
#######################################################

#   Download wgrib2 and find compiliation instructions here:
#   http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/ 
#   For unix systems, you may need to install other dependencies as well
#   in Ubuntu, install (ie. via apt-get or synaptic):
#   libxml2-dev
#   libcurl4-openssl-dev
#   libboost-dev
#   prior to installing rNOMADS.

####
#  This document is based on rNOMADS examples here:
#  http://www.unc.edu/~haksaeng/rNOMADS/rNOMADS_grib_examples.pdf
###
###########################################################
### BEGIN EXAMPLES
### First, generate global maps
###########################################################

#Get model data:
#Planetary temperature, relative humidity, winds 
#at 2 m above ground and at 300 mb (jet stream level)

library(GEOmap)
library(rNOMADS)
library(fields)

#Get the latest 2 model instances
urls.out <- CrawlModels(abbrev = "gfs_0p50", 
    depth = 2, verbose = FALSE)

#Get the available predictions.
#If this throws an error, try urls.out[2]
#because sometimes the web page appears before the data does

model.parameters <- ParseModelPage(urls.out[1])
latest.pred <- model.parameters$pred[1]
levels <- c("2 m above ground", "300 mb")
variables <- c("TMP", "RH", "UGRD", "VGRD")
resolution <- c(0.5, 0.5)

#Download model file
grib.info <- GribGrab(urls.out[1], latest.pred, 
    levels, variables, verbose = FALSE)

#Read the model file
grb.data <- ReadGrib(grib.info$file.name, levels, variables)

#Make an array for quick indexing
atmos <- ModelGrid(grb.data, resolution)


#FIGURE 1
#Temperature at ground level


#Get variable and level indices
li <- which(atmos$levels == "2 m above ground")
vi <- which(atmos$variables == "TMP")


#Set up color scale
colormap <- rev(rainbow(100, start = 0 , end = 5/6))

#Make forecast image
image(x = atmos$x + 180, y = sort(atmos$y), z = atmos$z[li,vi,,], col = colormap,
    xlab = "Longitude", ylab = "Latitude",
    main = paste("World Temperature at Ground Level:", atmos$fcst.date))

#Plot coastlines
plotGEOmap(coastmap, border = "black", add = TRUE, 
    MAPcol = NA, shiftlon = 180)


#FIGURE 2
#Temperature at 300 mb

li <- which(atmos$levels == "300 mb")
vi <- which(atmos$variables == "TMP")
colormap <- rev(rainbow(100, start = 0 , end = 5/6))
image(x = atmos$x + 180, y = atmos$y, z = atmos$z[li,vi,,], col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Temperature at 300 mb:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE, 
    MAPcol = NA, shiftlon = 180)


#FIGURE 3
#Relative humidity at ground level

li <- which(atmos$levels == "2 m above ground")
vi <- which(atmos$variables == "RH")
colormap <- rev(cm.colors(100))
image(x = atmos$x + 180, y = atmos$y, z = atmos$z[li,vi,,], col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Relative Humidity at Ground Level:", 
    atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA, shiftlon = 180)


#FIGURE 4
#Winds at 300 mb (around jet stream level)

li <- which(atmos$levels == "300 mb")
vi <- which(atmos$variables == "UGRD")
ew.winds <- atmos$z[li,vi,,]
vi <- which(atmos$variables == "VGRD")
ns.winds <- atmos$z[li,vi,,]
winds.vel <- sqrt(ew.winds^2 + ns.winds^2)
colormap <- topo.colors(100)
image(x = atmos$x + 180, y = atmos$y, z = winds.vel, col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Wind Velocity at 300 mb:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA, shiftlon = 180)


#Get model data
#and plot surface wind gust for the 6 hour forecast

variables <- c("GUST")
levels <- c("surface")
resolution <- c(0.5, 0.5)

#Get the latest 2 model instances
urls.out <- CrawlModels(abbrev = "gfs_0p50",
    depth = 2, verbose = FALSE)

#Get the available predictions.
#If this throws an error, try urls.out[2]
#because sometimes the web page appears before the data does

model.parameters <- ParseModelPage(urls.out[1])

#Get 6 hr prediction
#This will be 6 to 12 hours from now
#depending on when the model was run

pred.6hr <- model.parameters$pred[grep("06$", model.parameters$pred)]


grib.info <- GribGrab(urls.out[1], pred.6hr, 
    levels, variables, verbose = FALSE)

grb.data <- ReadGrib(grib.info$file.name, levels, variables)

#Make an array for quick indexing
atmos <- ModelGrid(grb.data, resolution)

li <- which(atmos$levels == "surface")
vi <- which(atmos$variables == "GUST")

#FIGURE 5
image(x = atmos$x + 180, y = atmos$y, z = atmos$z[li, vi, ,], col = colormap,
    xlab = "Longitude", ylab = "Latitude",
    main = paste("World Wind Gust at Ground Surface:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA, shiftlon = 180)


#####################################################
### GENERATE ATMOSPHERIC PROFILES
######################################################


library(rNOMADS)

#Get the latest 2 model instances
urls.out <- CrawlModels(abbrev = "gfs_0p50",
    depth = 2, verbose = FALSE)

#Get the available predictions.
#If this throws an error, try urls.out[2]
#because sometimes the web page appears before the data does

model.parameters <- ParseModelPage(urls.out[1])

#Get latest prediction

pred.now <- model.parameters$pred[1]

#Get temperatures, winds, and elevations for whole atmosphere

pressure <- c(1, 2, 3, 5, 7, 
   10, 20, 30, 50, 70,
   seq(100, 1000, by = 25))

levels <- paste(pressure, " mb", sep = "")

#Height, temp, e-w wind, n-s wind
variables <- c("HGT", "TMP", "UGRD", "VGRD")

#Location to examine
lon <- -79.052104
lat <- 35.907553

#Get grib file
grib.info <- GribGrab(urls.out[1], pred.now, levels, variables,
   model.domain = c(lon - 2, lon + 2, lat + 2, lat - 2))
model.data <- ReadGrib(grib.info$file.name, levels, variables)

#Build atmospheric profile
profile <- BuildProfile(model.data, lon, lat, 
    spatial.average = TRUE, points = 8)
    
#Get height in meters
hgt <- profile[[1]]$profile.data[, which(variables == "HGT"),]

#FIGURE 6 - temperature

tmp <- profile[[1]]$profile.data[, which(variables == "TMP"),]

#Let's make a spline

tmp.spline <- splinefun(hgt, tmp, method = "natural")

synth.hgt <- seq(min(hgt), max(hgt), length.out = 1000)
synth.tmp <- tmp.spline(synth.hgt)

plot(tmp, hgt, pch = 19, col = "red", 
   xlab = "Temperature (C)", ylab = "Height (m)",
   main = paste("Temperature versus Geopotential Height:",  
   profile[[1]]$forecast.date))
lines(synth.tmp, synth.hgt, col = "blue")
legend("topright", col = c("red", "blue"), pch = c(19, NA),
   lty = c(NA, 1), legend = c("Model Values", "Spline Fit"),
   bg = "white")

#FIGURE 7 - Wind Speed and azimuth

wu <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "UGRD"),]
wv <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "VGRD"),]

#Let's make a spline 
wu.spline <- splinefun(hgt, wu, method = "natural")
wv.spline <- splinefun(hgt, wv, method = "natural")

synth.hgt <- seq(min(hgt), max(hgt), length.out = 1000)
synth.uvel <- wu.spline(synth.hgt)
synth.vvel <- wv.spline(synth.hgt)

PlotWindProfile(synth.uvel, synth.vvel, synth.hgt, lines = TRUE, 
    points = FALSE, elev.circles = c(0, 25000, 50000), 
    elev.labels = c("0", "25", "50 km asl"),
    radial.lines = seq(45, 360, by = 45), colorbar = TRUE, invert = FALSE, 
    point.cex = 2, pch = 19, lty = 1, lwd = 3, 
    height.range = c(0, 50000), colorbar.label = "Wind Speed (m/s)")


#FIGURE 8
#ATMOSPHERIC DENSITY (ASSUMING DRY AIR)

p <- as.numeric(gsub("\\D", "", profile[[1]]$levels)) * 100
R <- 287.058 #Specific gas constant, J/(kg * K)

rho <- p / (tmp * R) #Air density

rho.spline <- splinefun(hgt, rho, method = "natural")
synth.hgt <- seq(min(hgt), max(hgt), length.out = 1000)
synth.rho <- rho.spline(synth.hgt)

plot(rho, hgt, pch = 19, col = "red",
   xlab = "Density (kg/m3)", ylab = "Height (m)",
   main = paste("Dry Air Density versus Geopotential Height:",
   atmos$fcst.date))
lines(synth.rho, synth.hgt, col = "blue")
legend("topright", col = c("red", "blue"), pch = c(19, NA),
   lty = c(NA, 1), legend = c("Model Values", "Spline Fit"))

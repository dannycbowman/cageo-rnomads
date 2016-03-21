#######################################################
### DOWNLOADING DATA FROM THE GLOBAL FORECAST SYSTEM USING GRADS-DODS
#######################################################

#  This code shows how to generate global weather maps and atmospheric profiles
#  using rNOMADS and the cross-platform GrADS-DODS capability
#  For help, post questions to the rNOMADS mailing list:
#  https://lists.r-forge.r-project.org/mailman/listinfo/rnomads-user

#######################################################
### INSTALLING RNOMADS AND DEPENDENCIES FOR THESE EXAMPLES
#######################################################

#   for mac OS and windows, simply type 
#   install.packages("rNOMADS") 
#   install.packages("fields")
#   install.packages("GEOmap")
#   in the R interpreter
#   For unix systems, you may need to install other dependencies
#   in Ubuntu, install (ie. via apt-get or synaptic):
#   libxml2-dev
#   libcurl4-openssl-dev
#   libboost-dev
#   prior to installing rNOMADS.

####
#  This document is based on rNOMADS examples here:
#  http://www.unc.edu/~haksaeng/rNOMADS/rNOMADS_dods_examples.pdf
####

###########################################################
### BEGIN EXAMPLES
### First, generate global maps
###########################################################

# Generate global maps of temperature, relative humidity, and wind speed
# at the ground surface and the 300 mb pressure level

library(GEOmap)
library(rNOMADS)
library(fields)

#Get latest GFS model

model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

#Get data

time      <- c(0,0) #Analysis model
lon       <- c(0, 719) #All 720 longitude points
lat       <- c(0, 360) #All 361 latitude points
ground.variables <- c("tmp2m", "rh2m") #Temperature and relative humidity

ground.data <- DODSGrab(latest.model, latest.model.run,
   ground.variables, time, lon, lat)

mb300.variables <- c("tmpprs", "ugrdprs", "vgrdprs") #Temp, E-W wind, N-S wind
lev       <- c(28,28) #get 300 mb level

mb300.data <- DODSGrab(latest.model, latest.model.run,
   mb300.variables, time, lon, lat, levels = lev)

#FIGURE 1
#Temperature at ground level

#Make model grid

ground <- ModelGrid(ground.data, c(0.5, 0.5))

#Set up color scale
colormap <- rev(rainbow(100, start = 0 , end = 5/6))

#Make forecast image
image(x = ground$x, y = sort(ground$y), 
    z = ground$z[1,which(ground.variables == "tmp2m"),,], col = colormap,
    xlab = "Longitude", ylab = "Latitude",
    main = paste("World Temperature at Ground Level:", ground$fcst.date))

#Plot coastlines
plotGEOmap(coastmap, border = "black", add = TRUE, 
    MAPcol = NA)


#FIGURE 2
#Temperature at 300 mb
atmos <- ModelGrid(mb300.data, c(0.5, 0.5))
colormap <- rev(rainbow(100, start = 0 , end = 5/6))
image(x = atmos$x, y = atmos$y, z = atmos$z[1,which(mb300.variables == "tmpprs"),,], col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Temperature at 300 mb:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE, 
    MAPcol = NA)

#FIGURE 3
#Relative humidity at ground level

colormap <- rev(cm.colors(100))
image(x = ground$x, y = ground$y, 
    z = ground$z[1,which(ground.variables == "rh2m"),,], col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Relative Humidity at Ground Level:", 
    ground$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA)

#FIGURE 4
#Winds at 300 mb (around jet stream level)
winds.vel <- sqrt(atmos$z[1,which(mb300.variables == "ugrdprs"),,]^2 + 
    atmos$z[1,which(mb300.variables == "vgrdprs"),,]^2)
colormap <- topo.colors(100)
image(x = atmos$x, y = atmos$y, z = winds.vel, col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Wind Velocity at 300 mb:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA)

#Get model data
#and plot 10 m wind speed for 6 hr forecast

time <- c(2,2) #6 hr forecast
wind.variables <- c("ugrd10m", "vgrd10m")
wind.data <- DODSGrab(latest.model, latest.model.run,
   wind.variables, time, lon, lat)
 
#Make an array for quick indexing
wind <- ModelGrid(wind.data, c(0.5, 0.5))

#Wind magnitude
winds.vel <- sqrt(wind$z[1,1,,]^2 + wind$z[1,2,,]^2)

#FIGURE 5
image(x = atmos$x, y = atmos$y, z = winds.vel, col = colormap,
    xlab = "Longitude", ylab = "Latitude",
    main = paste("World Wind Speed at 10 m above ground:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA)


######################################################
### GENERATE ATMOSPHERIC PROFILES
######################################################
# Generate temperature and wind speed profiles above a given point

library(rNOMADS)

#Location to examine
lon <- -79.052104
lat <- 35.907553

#Figure out nearest model node to this point

lons <- seq(0, 359.5, by = 0.5)
lats <- seq(-90, 90, by = 0.5)

lon.diff <- abs(lon + 360 - lons)
lat.diff <- abs(lat - lats)

model.lon.ind <- which(lon.diff == min(lon.diff)) - 1
model.lat.ind <- which(lat.diff == min(lat.diff)) - 1

model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

#Get data

time      <- c(0,0) #Analysis model
lev       <- c(0, 46) #All levels in atmosphere
variables <- c("tmpprs", "hgtprs", "ugrdprs", "vgrdprs") #Temp, height, E-W wind, N-S wind

model.data <- DODSGrab(latest.model, latest.model.run, variables,
    time, c(model.lon.ind - 2, model.lon.ind + 2),
    c(model.lat.ind - 2, model.lat.ind + 2),
    levels = lev)
    
    

profile <- BuildProfile(model.data, lon, lat, spatial.average = TRUE, points = 4)

hgt <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "hgtprs"),]


#FIGURE 6 - temperature

tmp <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "tmpprs"),] - 272.15

#Let's make a spline

tmp.spline <- splinefun(hgt, tmp, method = "natural")

synth.hgt <- seq(min(hgt), max(hgt), length.out = 1000)
synth.tmp <- tmp.spline(synth.hgt)

plot(tmp, hgt, pch = 19, col = "red", 
   xlab = "Temperature (C)", ylab = "Height (m)",
   main = paste("Temperature versus Geopotential Height"))
lines(synth.tmp, synth.hgt, col = "blue")
legend("topright", col = c("red", "blue"), pch = c(19, NA),
   lty = c(NA, 1), legend = c("Model Values", "Spline Fit"),
   bg = "white")

#FIGURE 7 - Wind Speed and azimuth

wu <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),]
wv <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]

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

p <- profile[[1]]$levels * 100
R <- 287.058 #Specific gas constant, J/(kg * K)

rho <- p / ((tmp + 273.15) * R) #Air density

rho.spline <- splinefun(hgt, rho, method = "natural")
synth.hgt <- seq(min(hgt), max(hgt), length.out = 1000)
synth.rho <- rho.spline(synth.hgt)

plot(rho, hgt, pch = 19, col = "red",
   xlab = "Density (kg/m3)", ylab = "Height (m)",
   main = paste("Dry Air Density versus Geopotential Height"))
lines(synth.rho, synth.hgt, col = "blue")
legend("topright", col = c("red", "blue"), pch = c(19, NA),
   lty = c(NA, 1), legend = c("Model Values", "Spline Fit"))


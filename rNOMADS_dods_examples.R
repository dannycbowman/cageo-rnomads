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

time <- c(0,0) #Analysis model
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points

tmp2m.data <- DODSGrab(latest.model, latest.model.run,
   "tmp2m", time, lon, lat, display.url = FALSE)
rh2m.data <- DODSGrab(latest.model, latest.model.run,
   "rh2m", time, lon, lat, display.url = FALSE)

lev <- c(28,28) #get 300 mb level
tmp300.data <- DODSGrab(latest.model, latest.model.run,
   "tmpprs", time, lon, lat, levels = lev, display.url = FALSE)
ugrd300.data  <- DODSGrab(latest.model, latest.model.run,
   "ugrdprs", time, lon, lat, levels = lev, display.url = FALSE)
vgrd300.data  <- DODSGrab(latest.model, latest.model.run,
   "vgrdprs", time, lon, lat, levels = lev, display.url = FALSE)


#FIGURE 1
#Temperature at ground level

#Make model grid

atmos <- ModelGrid(tmp2m.data, c(0.5, 0.5), "latlon")

#Set up color scale
colormap <- rev(rainbow(500, start = 0 , end = 5/6))

#Make forecast image
image(x = atmos$x, y = sort(atmos$y), z = atmos$z[1,1,,], col = colormap,
    xlab = "Longitude", ylab = "Latitude",
    main = paste("World Temperature at Ground Level:", atmos$fcst.date))

#Plot coastlines
plotGEOmap(coastmap, border = "black", add = TRUE, 
    MAPcol = NA)


#FIGURE 2
#Temperature at 300 mb
atmos <- ModelGrid(tmp300.data, c(0.5, 0.5), "latlon")
colormap <- rev(rainbow(500, start = 0 , end = 5/6))
image(x = atmos$x, y = atmos$y, z = atmos$z[1,1,,], col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Temperature at 300 mb:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE, 
    MAPcol = NA)

#FIGURE 3
#Relative humidity at ground level

colormap <- rainbow(500, start = 0 , end = 5/6)
atmos <- ModelGrid(rh2m.data, c(0.5, 0.5), "latlon")
image(x = atmos$x, y = atmos$y, z = atmos$z[1,1,,], col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Relative Humidity at Ground Level:", 
    atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA)

#FIGURE 4
#Winds at 300 mb (around jet stream level)
atmos.ew <- ModelGrid(ugrd300.data, c(0.5, 0.5), "latlon")
atmos.ns <- ModelGrid(vgrd300.data, c(0.5, 0.5), "latlon")
winds.vel <- sqrt(atmos.ew$z[1,1,,]^2 + atmos.ns$z[1,1,,]^2)
colormap <- rainbow(500, start = 0 , end = 5/6)
image(x = atmos.ew$x, y = atmos.ew$y, z = winds.vel, col = colormap,
    xlab = "Longitude", ylab = "Latitude", 
    main = paste("World Wind Velocity at 300 mb:", atmos$fcst.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
    MAPcol = NA)

#Get model data
#and plot 10 m wind speed for 6 hr forecast

time <- c(2,2) #6 hr forecast

u10.data <- DODSGrab(latest.model, latest.model.run,
   "ugrd10m", time, lon, lat, display.url = FALSE)
v10.data <-DODSGrab(latest.model, latest.model.run,
   "vgrd10m", time, lon, lat, display.url = FALSE)
 
#Make an array for quick indexing
atmos.u10 <- ModelGrid(u10.data, c(0.5, 0.5))
atmos.v10 <- ModelGrid(v10.data, c(0.5, 0.5))

#Wind magnitude
winds.vel <- sqrt(atmos.u10$z[1,1,,]^2 + atmos.v10$z[1,1,,]^2)

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

time <- c(0,0) #Analysis model
lev <- c(0, 46) #All levels in atmosphere

#Temperature
tmp.data <- DODSGrab(latest.model, latest.model.run,
   "tmpprs", time, rep(model.lon.ind, 2), rep(model.lat.ind, 2), 
    levels = lev, display.url = FALSE)

#Geopotential height
hgt.data <- DODSGrab(latest.model, latest.model.run,
   "hgtprs", time, rep(model.lon.ind, 2), rep(model.lat.ind, 2), 
    levels = lev, display.url = FALSE)

#E-W wind
ugrd.data <- DODSGrab(latest.model, latest.model.run,
   "ugrdprs", time, rep(model.lon.ind, 2), rep(model.lat.ind, 2), 
    levels = lev, display.url = FALSE)

#N-S wind
vgrd.data <- DODSGrab(latest.model, latest.model.run,
   "vgrdprs", time, rep(model.lon.ind, 2), rep(model.lat.ind, 2),  
    levels = lev, display.url = FALSE)


hgt <- hgt.data$value


#FIGURE 6 - temperature


tmp <- tmp.data$value - 273.15

#Let's make a spline

tmp.spline <- splinefun(hgt, tmp, method = "natural")

synth.hgt <- seq(min(hgt), max(hgt), length.out = 1000)
synth.tmp <- tmp.spline(synth.hgt)

plot(tmp, hgt, pch = 19, col = "red", 
   xlab = "Temperature (C)", ylab = "Height (m)",
   main = paste("Temperature versus Geopotential Height"))
lines(synth.tmp, synth.hgt, col = "blue")
legend("topright", col = c("red", "blue"), pch = c(19, NA),
   lty = c(NA, 1), legend = c("Model Values", "Spline Fit"))

#FIGURE 7 - Wind Speed

wu <- ugrd.data$value
wv <- vgrd.data$value
wvel <- sqrt(wu^2 + wv^2) 

#Convert to km/hr
wvel <- wvel * 3.6

#Let's make a spline 

tmp.spline <- splinefun(hgt, wvel, method = "natural")

synth.hgt <- seq(min(hgt), max(hgt), length.out = 1000)
synth.wvel <- tmp.spline(synth.hgt)

plot(wvel, hgt, pch = 19, col = "red",
   xlab = "Wind Speed (km/hr)", ylab = "Height (m)",
   main = paste("Wind Speed versus Geopotential Height"))
lines(synth.wvel, synth.hgt, col = "blue")
legend("bottomright", col = c("red", "blue"), pch = c(19, NA),
   lty = c(NA, 1), legend = c("Model Values", "Spline Fit"))

#FIGURE 8
#ATMOSPHERIC DENSITY (ASSUMING DRY AIR)

p <- ugrd.data$levels * 100
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


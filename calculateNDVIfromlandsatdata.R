library(xlsx)
library(sp)
library(sf)
library(rgdal)
library(raster)
library(dplyr)
library(magrittr)
library(gstat)
library(tmap)
library(spatstat)
library(maptools)
library(tidyverse)
library(caret)
library(leaps)

# Load files
betdata <- read.xlsx("betuladata/betula data 2013-2018.xlsx", sheetIndex = 1)

# Rename columns
names(betdata) <- gsub(".","_", names(betdata), fixed=T)

# Convert relevant columns to numeric
betdata$X <- as.numeric(betdata$X)
betdata$Y <- as.numeric(betdata$Y)
betdata$Flowering_duration <- as.numeric(betdata$Flowering_duration)

# Get betula trees where flowering phenology data is available
betloc <- betdata[(!is.na(betdata$flowering_start_date)|!is.na(betdata$Peak_date)), c("year","X", "Y","flowering_start_date","flowering_end_date","Flowering_duration","Peak_date")]

# Convert to spatial points
betlocsp <- SpatialPointsDataFrame(betloc[,c("Y", "X")], data = betloc, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Load a shapefile of augsburg
augsburg <- readOGR("shapefiles/augsburg/", "augsburg_boundary")

# First overview of tree locations within augsburg
plot(augsburg)
plot(betlocsp, add =T)

# Function to calculate ndvi
ndvi <- function(x){
  return((x$B4 - x$B3)/(x$B4 + x$B3)) 
}

# Loop over all landsat 7 images, calculate NDVI and write to directory
for (i in list.files("L7")){
  
  # Stack bands for NDVI calculation
  rastndvi <- raster::stack(paste0(getwd(),"/L7/", i, "/", list.files(paste0("L7/", i, "/"))))
  names(rastndvi) <- c("B3", "B4")
  
  # Calculate NDVI
  ndvicalc <- ndvi(rastndvi)
  
  # write to directory
  writeRaster(ndvicalc, paste0("ndvioutputs/ndvi", i, ".tif"))
  
  show(i)
}

# Function to calculate NDVI for L8
ndvi8 <- function(x){
  return((x$B5 - x$B4)/(x$B5 + x$B4)) 
}

# Loop over all Landsat 8 images, calculate NDVI and write to directory
for (i in list.files("L8")){
  
  # Stack bands for NDVI calculation
  rastndvi <- raster::stack(paste0(getwd(),"/L8/", i, "/", list.files(paste0("L8/", i, "/"))))
  names(rastndvi) <- c("B4", "B5")
  
  # Calculate NDVI
  ndvicalc <- ndvi8(rastndvi)
  
  # write to directory
  writeRaster(ndvicalc, paste0("ndvioutputs/ndvi", i, "_L8.tif"))
  
  show(i)
}

library()

rastlist <- list.files(path = "ndvioutputs", pattern='.tif$', all.files=TRUE, full.names=T)
allrasters <- lapply(rastlist, raster)
crop_allrasters <-lapply(allrasters, FUN = resample, y=allrasters[[1]])

ndvil7<- do.call("stack", crop_allrasters)

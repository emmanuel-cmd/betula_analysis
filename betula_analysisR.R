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

# Load files
betdata <- read.xlsx("betuladata/betula data 2013-2018.xlsx", sheetIndex = 1)

# Rename columns
names(betdata) <- gsub(".","_", names(betdata), fixed=T)

# Convert relevant columns to numeric
betdata$X <- as.numeric(betdata$X)
betdata$Y <- as.numeric(betdata$Y)
betdata$Flowering_duration <- as.numeric(betdata$Flowering_duration)

# Get location of betula trees in augsburg
betloc <- betdata[, c("X", "Y")]

# Convert to spatial points
betlocsp <- SpatialPointsDataFrame(betloc[,c("Y", "X")], data = betdata[,-c(5,6)], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Load a shapefile of augsburg
augsburg <- readOGR("shapefiles/augsburg/", "augsburg_boundary")

# First overview of tree locations within augsburg
plot(augsburg)
plot(betlocsp, add =T)

# Get only locations within augsburg
sub <- sp::over(betlocsp, augsburg)
betlocspaug <- betlocsp[!is.na(sub$full_id), ]

# Plot
plot(augsburg)
plot(betlocspaug, add =T)

# Interpolate variables over the extent of Augsburg

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(betlocsp, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
#proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(betlocspaug)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(NO2 ~ 1, betlocspaug, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- crop(r, augsburg) 
r.m <- mask(r.m, augsburg)
plot(r.m)

plot(augsburg, add =T)
# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(augsburg)
# e <- e + 1000 # add this as all y's are the same

r <- raster(e, nrow = 100, ncol=100)
proj4string(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# or r <- raster(xmn=, xmx=,  ...

# you need to provide a function 'fun' for when there are multiple points per cell



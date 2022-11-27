library(gstat)
library(raster)
library(sp)
library(sf)
library(rgdal)
library(xlsx)
library(sf)
library(dplyr)
library(magrittr)
library(greenbrown)
library(tmap)
library(spatstat)
library(maptools)
library(tidyverse)
library(caret)
library(leaps)
library(tidyr)
library(ellipse)
library(automap)
library(stars)
library(terra)

# 1. Load augsburg boundary layer
augsburg <- readOGR("shapefiles/augsburg/", "augsburg_boundary")

## Transform augsburg boundary
aug <- spTransform(augsburg, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
plot(aug)

# 2. BETULA DATA - Load files
betdata <- read.xlsx("betuladata/betula data 2013-2018.xlsx", sheetIndex = 1)

## Rename columns
names(betdata) <- gsub(".","_", names(betdata), fixed=T)

## Convert relevant columns to numeric
betdata$X <- as.numeric(betdata$X)
betdata$Y <- as.numeric(betdata$Y)
betdata$Flowering_duration <- as.numeric(betdata$Flowering_duration)

## Get location of betula trees in augsburg
betloc <- betdata[, c("X", "Y")]

## Convert betdata to spatial points dataframe and transform
betdatasp <- SpatialPointsDataFrame(betdata[,c("UTM_X", "UTM_Y")], data = betdata, proj4string = CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"))
betdatasp <- spTransform(betdatasp, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))

# Get only locations within augsburg
# sub <- sp::over(betdatasp, aug)
# betdataspaug <- betdatasp[!is.na(sub$full_id), ]

# 3. Load raster layers
rasmod1 <- stack(paste0(getwd(),"/", list.files("VegetationData/germany/VI_16Days_250m_v6/", pattern='.tif$', all.files=TRUE, full.names=T)))
rasmod2 <- stack(paste0(getwd(),"/", list.files("VegetationData/germany/VI_16Days_250m_v6/NDVI/", pattern='.tif$', all.files=TRUE, full.names=T)))

# # 3. Load raster layers
# rasmod1 <- stack(paste0(getwd(),"/", list.files("VegetationData/", pattern='.tif$', all.files=TRUE, full.names=T)))
# rasmod2 <- stack(paste0(getwd(),"/", list.files("VegetationData/NDVI/", pattern='.tif$', all.files=TRUE, full.names=T)))

## Stack raster together
allrastersmod <- stack(rasmod1, rasmod2)

## Extract names
years <- do.call("rbind", strsplit(names(allrastersmod), "_", fixed = T))[,c(3:4)] %>% as.data.frame()
nameyears <- paste(years$V1, years$V2, sep="_")

## Append names to all raster layers
names(allrastersmod) <- nameyears

## Sort rasters based on newly created names
allrastersort <- allrastersmod[[order(nameyears)]]

## Crop to augsburg boundary
allrasteraug <- raster::crop(allrastersort, extent(betdatasp))

## Calculate monthly means from each 9-day interval
rastlist <- list()

## Loop and calculate means
for(j in seq(0, 229, 46)){
  for (i in 1:12){
    
    idx <- c(1, 5, 8, 12, 16, 20, 24, 28, 32, 35, 39, 43, 46)
    
    a <- (j + idx[i]):(idx[i+1] + j);
    
    rastlist[[paste0(j,"_", i)]] <- mean(allrasteraug[[a]], na.rm=T)
    
  }
}

## adjust names of layers
names(rastlist) <- paste0(rep(2014:2018, each=12), ".", 1:12)

## Stack layers together
meanrastlist <- stack(rastlist)

## Dividing values by 10000 to have NDVI values between -1 and 1
gain(meanrastlist) <- 0.0001

## Set dates for raster layers 
meanrastlist <- setZ(meanrastlist, as.POSIXct(paste0(rep(2014:2018, each=12), "-", 1:12,"-", 1:31)), "Date")

## Calculate mean within each buffered point
meanrastad <- list()

## Loop over all layers and calculate mean ndvi values
for (i in 1:length(names(meanrastlist))){
  meanrastad[[i]] <- values(meanrastlist[[i]]) %>% mean(.,na.rm=T)
} 

## Applying phenoRaster on the mean values
pheno_resuts <- PhenologyRaster(meanrastlist, start = c(2014, 1), freq = 12)

# 4. Comparison between measured and predicted flowering phenology data

# Convert to sf object for buffer
betdataspsf <- st_as_sf(betdatasp)
betdataspbuf <- st_buffer(betdataspsf, dist=500) %>% sf::st_sf() 

# Visualization
plot(pheno_resuts$SOS.2014)
plot(betdataspbuf, add=T)

# betdataspaugsp <- as_Spatial(betdataspaugbuf)
# Select columns
selcol <- c("SOS.2014", "SOS.2015", "SOS.2016","SOS.2017","SOS.2018","EOS.2014","EOS.2015",  
            "EOS.2016","EOS.2017","EOS.2018","LOS.2014","LOS.2015","LOS.2016","LOS.2017",   
            "LOS.2018","PEAK.2014","PEAK.2015","PEAK.2016","PEAK.2017","PEAK.2018")

# Calculate mean of parameters within buffer radius 
meanbetflow <- raster::extract(pheno_resuts[[selcol]], betdataspbuf, fun=mean, na.rm=T) %>% as.data.frame
names(meanbetflow) <- selcol

# Merge to existing data
betdatamepre <- cbind(st_drop_geometry(betdataspbuf), meanbetflow)

# Convert long data type
meanbetflowlong <- data.frame(betdatamepre[,c("X", "Y", names(data.frame(meanbetflow)))]) %>% pivot_longer(.,cols=selcol,names_to = "name", values_to = "value")

# Split column name 
varyear <- do.call(rbind, strsplit(meanbetflowlong$name, split=".", fixed = T))
meanbetflowsplt <- cbind(meanbetflowlong[,-3], varyear)

# Make dataframe wider so that variables are along the column
meanbetflowide <- pivot_wider(meanbetflowsplt, names_from = "1", values_fn = {mean})

# Fix incorrect names 
names(meanbetflowide)[3] <- "year"

# Drop columns
betdatamepre[, selcol] <- NULL

# Merge to original dataframe
betdatam <- merge(betdatamepre, meanbetflowide, by = c("X", "Y", "year"), all.x=T)

# Replace NAs 
#betdatam[is.na(betdatam)]<- 0

# Pearson correlation between measured flowering start date and start of season
cor(betdatam$flowering_end_date, betdatam$EOS)

# Variable selection and habitat modelling
cm <- cor(betdatam[,c(25:35, 40:46)], use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

# Linear regression model produces a model with satisfactory performance. 
model <- lm(log(Flowering_duration) ~ SOS + PEAK + LOS + EOS, betdatam)
summary(model)

# SPATIAL KRIGING WITH ENVIRONMENTAL VARIABLES
betdatamcopy <- betdatam
#betdatamcopy[betdatamcopy==0] <- NA
#betdatamcopy <- betdatamcopy[betdatamcopy$year==2015,]

# Convert to spatial object
coordinates(betdatamcopy)=~X+Y

# Set crs
proj4string(betdatamcopy) <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")

# Get extent of point
ext <- extent(betdatasp) %>% as.vector(.)

# Expand grid
A1.grd <- expand.grid(x=seq(from=ext[1], to=ext[2], by=50),
                      y=seq(from=ext[3], to=ext[4], by=50))
# Extract raster 
mat <- raster::extract(pheno_resuts[[which(str_detect(selcol,"2015"))]], A1.grd)

# Merge to A1.grd
A1.grd <- cbind(A1.grd, mat)

# Convert to spatial object
coordinates(A1.grd) <- ~x+y
gridded(A1.grd) <- TRUE

# Set CRS of grid
A1.grd@proj4string <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")

# Convert to stars object
A1grdstars <- stars::st_as_stars(A1.grd)

meuse_gridcv <- split(A1grdstars, "band")
meuse_gridcv

# Convert nas to 0
#A1grdstars[is.na(A1grdstars)] <- 0

samplebet <- betdatamcopy[!is.na(betdatamcopy@data$NO2),]

# GEOSPATIAL KRIGING
# create sample variogram
#betdatamcopy@data[is.na(betdatamcopy@data)] <- 0
flower.v <- gstat::variogram(sqrt(sqrt(NO2)) ~ 1, samplebet)

# fit variogram model
# plot(variogram(Flowering_duration ~ 1, betdatamcopy))
flower.vfit <- gstat::fit.variogram(flower.v, vgm(psill=1.2, "Per", range=0.05, nugget=0.0005))
plot(flower.v, flower.vfit)

#ordinary kriging
lz.ok <- gstat::krige(formula=NO2 ~ 1, locations = samplebet, newdata=A1grdstars, model=flower.vfit)

# Plot

a <- raster::mask(raster(terra::rast(lz.ok['var1.var'])), aug)
plot(a)

plot(terra::rast(lz.ok['var1.var']))
plot(samplebet["NO2"], col="blue", cex=0.5, type="p", add=T)


# IDW Interpolation
betidw <- gstat::idw(NO2 ~ 1, locations=samplebet, newdata=A1grdstars, idp = 2)

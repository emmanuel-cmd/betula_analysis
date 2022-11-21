library(raster)
library(sp)
library(sf)
library(rgdal)
library(greenbrown)
library(xlsx)
library(sp)
library(sf)
library(rgdal)
library(raster)
library(dplyr)
library(magrittr)
#library(gstat)
library(tmap)
library(spatstat)
library(maptools)
library(tidyverse)
library(caret)
library(leaps)
library(tidyr)
library(ellipse)
library(automap)

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
sub <- sp::over(betdatasp, aug)
betdataspaug <- betdatasp[!is.na(sub$full_id), ]

# 3. Load raster layers
rasmod1 <- stack(paste0(getwd(),"/", list.files("VegetationData/germany/VI_16Days_250m_v6/", pattern='.tif$', all.files=TRUE, full.names=T)))
rasmod2 <- stack(paste0(getwd(),"/", list.files("VegetationData/germany/VI_16Days_250m_v6/NDVI", pattern='.tif$', all.files=TRUE, full.names=T)))

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
allrasteraug <- raster::crop(allrastersort, aug)

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

# plot(meanrastlist[[12]])
# plot(a, cex=21)

# # Get location of UKA in augsburg
# a <- st_sfc(st_point(c(10.840414350002051, 48.384622412290014)), crs=4326)
# atr <- st_transform(a, crs = st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))
# 
# # Buffer point
# atr_poly <- st_buffer(atr, 1500)
# ab <- as(atr_poly, 'Spatial')

# # Visualize
# plot(meanrastlist[[1]])
# plot(ab, add=T)

# # Crop all raster layers to the extent of buffered point
# meanrastab <- stack(crop(meanrastlist, ab))

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
betdataspaugsf <- st_as_sf(betdataspaug)
betdataspaugbuf <- st_buffer(betdataspaugsf, dist=500) %>% sf::st_sf() 

# Visualization
plot(pheno_resuts$SOS.2014)
plot(betdataspaugbuf, add=T)

# betdataspaugsp <- as_Spatial(betdataspaugbuf)
# Select columns
selcol <- c("SOS.2014", "SOS.2015", "SOS.2016","SOS.2017","SOS.2018","EOS.2014","EOS.2015",  
            "EOS.2016","EOS.2017","EOS.2018","LOS.2014","LOS.2015","LOS.2016","LOS.2017",   
            "LOS.2018","PEAK.2014","PEAK.2015","PEAK.2016","PEAK.2017","PEAK.2018")

# Calculate mean of parameters within buffer radius 
meanbetflow <- raster::extract(pheno_resuts[[selcol]], betdataspaugbuf, fun=mean, na.rm=T)

# Merge to existing data
betdatamepre <- cbind(st_drop_geometry(betdataspaugsf), data.frame(meanbetflow))

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
betdatam[is.na(betdatam)]<- 0

# Pearson correlation between measured flowering start date and start of season
cor(betdatam$flowering_end_date, betdatam$EOS)

# Variable selection and habitat modelling
cm <- cor(betdatam[,c(25:35, 40:46)], use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

# Linear regression model produces a model with satisfactory performance. 
model <- lm(Flowering_duration ~ SOS + PEAK + Peak_date + LOS + flowering_start_date, betdatam)
summary(model)

# SPATIAL KRIGING WITH ENVIRONMENTAL VARIABLES
betdatamcopy <- betdatam
#betdatamcopy[betdatamcopy==0] <- NA
betdatamcopy <- betdatamcopy[betdatamcopy$year==2015,]

coordinates(betdatamcopy)=~X+Y

# Set crs
#proj4string(betdatamcopy) <- CRS("+init=epsg:4326")

# Extract x and y range 
x.range <- (range(betdataspaug@coords[,1]))
y.range <- (range(betdataspaug@coords[,2]))

# Expand grid
A1.grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5),  
                      y=seq(from=y.range[1], to=y.range[2], by=0.5))

# Append variables 
# cols <- raster::extract(pheno_resuts, A1.grd) %>% as.data.frame()
# 
# A1.grd <- cbind(A1.grd, cols)
coordinates(A1.grd) <- ~x+y
gridded(A1.grd) <- TRUE

# ---
A1.grd$PEAK <- sample(betdatamcopy$PEAK, 400, replace = T)
A1.grd$Peak_date <- sample(betdatamcopy$Peak_date, 400, replace = T)
A1.grd$LOS <- sample(betdatamcopy$LOS, 400, replace = T)
A1.grd$flowering_start_date <- sample(betdatamcopy$flowering_start_date, 400, replace = T)


# Set CRS of grid
A1.grd@proj4string <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")

# PEAK <- pheno_resuts[["PEAK.2014"]]
# LOS <- pheno_resuts[["LOS.2014"]]

betdatamcopy@proj4string <-  CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")

krigres <- automap::autoKrige(Flowering_duration ~ PEAK + Peak_date + LOS + flowering_start_date, betdatamcopy, A1.grd)

output <- raster(krigres$krige_output)
plot(output)
# https://gis.stackexchange.com/questions/239301/kriging-using-multiple-spatial-input-variables-and-one-spatial-output-response

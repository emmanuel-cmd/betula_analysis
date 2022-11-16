library(raster)
library(sp)
library(sf)
library(rgdal)

# Load augsburg boundary layer
augsburg <- readOGR("shapefiles/augsburg/", "augsburg_boundary")

# Load raster layers
rasmod1 <- stack(paste0(getwd(),"/", list.files("VegetationData/germany/VI_16Days_250m_v6/", pattern='.tif$', all.files=TRUE, full.names=T)))
rasmod2 <- stack(paste0(getwd(),"/", list.files("VegetationData/germany/VI_16Days_250m_v6/NDVI", pattern='.tif$', all.files=TRUE, full.names=T)))

# Stack raster together 
allrastersmod <- stack(rasmod1, rasmod2)

# Extract names
years <- do.call("rbind", strsplit(names(allrastersmod), "_", fixed = T))[,c(3:4)] %>% as.data.frame()
nameyears <- paste(years$V1, years$V2, sep="_")

# Append names to all raster layers
names(allrastersmod) <- nameyears

# Sort rasters based on newly created names
allrastersort <- allrastersmod[[order(nameyears)]]

# Transform augsburg boundary
aug <- spTransform(augsburg, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
plot(aug)

# Crop to augsburg boundary
allrasteraug <- raster::crop(allrastersort, aug)  

# Calculate monthly means from each 9-day interval
rastlist <- list()

# Loop and calculate means
for(j in seq(0, 229, 46)){
  for (i in 1:12){
    
    idx <- c(1, 5, 8, 12, 16, 20, 24, 28, 32, 35, 39, 43, 46)
    
    a <- (j + idx[i]):(idx[i+1] + j);
    
    rastlist[[paste0(j,"_", i)]] <- mean(allrasteraug[[a]], na.rm=T)
    
  }
}

# adjust names of layers
names(rastlist) <- paste0(rep(2014:2018, each=12), ".", 1:12)

# Stack layers together
meanrastlist <- stack(rastlist)

# Dividing values by 10000 to have NDVI values between -1 and 1
gain(meanrastlist) <- 0.0001

meanrastlist <- setZ(meanrastlist, as.POSIXct(paste0(rep(2014:2018, each=12), "-", 1:12,"-", 1:31)), "Date")

PhenologyRaster(meanrastlist[[1:12]], start = c(2014, 1), freq = 12)

# plot(meanrastlist[[12]])
# plot(a, cex=21)

# a <- st_sfc(st_point(c(10.840414350002051, 48.384622412290014)), crs=4326)
# atr <- st_transform(a, crs = st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))
# 
# atr_poly <- st_buffer(atr, 1500)
# ab <- as(atr_poly, 'Spatial')
# 
# plot(rastlist[[1]])
# plot(ab, add=T)


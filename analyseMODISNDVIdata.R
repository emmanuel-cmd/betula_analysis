library(raster)
library(sp)
library(sf)

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

rastlist <- list()

for (i in seq(1, 227, 4)){
  #rastlist[[j]] <- mean(allrasteraug[[i:i+5]])
  
  show(paste0(i," ", i+5));
}
  



do.call("stack", rastlist) %>% names()
mean(allrasteraug[[1:6]])



names(allrasteraug)
# plot(allrastersort[[1]])
# 
# plot(rasmod[[1]])


length(seq(1,365, 8))

seq(1, 230, 4)



plot(ras_sample)

SpatialPoints(cbind(48.384769285134816, 10.838529545678112))


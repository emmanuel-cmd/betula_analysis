library(xlsx)
library(mapview)

# Load data
allbirch <- read.xlsx("betuladata/Birkenkartierung.xlsx", sheetIndex = 1)

# Rename columns
names(allbirch) <- gsub(".","_", names(allbirch), fixed=T)

# Load a shapefile of augsburg
augsburg <- readOGR("shapefiles/augsburg/", "augsburg_boundary")

# Convert to spatialpoints dataframe
allbirchsp <- SpatialPointsDataFrame(allbirch[,c("lon", "lat")], data = allbirch[,-c(1:6)], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Visualize
mapview(list(augsburg, allbirchsp), color = c("blue", "grey"))

# Thin birch data
# Prepare data
datbirch <- allbirch[,2:3]
names(datbirch) <- c("LAT", "LONG")
datbirch$SPEC <- "birch"

# Apply spatial thinning of 0.5km
set.seed(1234)
datbirchthind <- spThin::thin(datbirch, thin.par = .5, write.files = F, reps = 5, locs.thinned.list.return = T, write.log.file = F)
thindbirch <- datbirchthind[[1]]

# Thinned data - Visualization
thindbirchsp <- SpatialPoints(thindbirch[, c("Longitude", "Latitude")], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
mapview(thindbirchsp)

# get other variables for thinned data
names(allbirch)[2:3] <- c("Latitude", "Longitude")

# Merge to get other variables from data
birchdat <- merge(allbirch, thindbirch, all=F)

# Convert to spatial points data
birchdatsp <- SpatialPoints(birchdat[,c("Longitude", "Latitude")], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Are all points within augsburg
ns <- sp::over(birchdatsp, augsburg)
birchdatspaug  <- birchdatsp[!is.na(ns$full_id), ]

# 
plot(augsburg)
plot(birchdatspaug, add =T)

mapview(list(birchdatspaug, augsburg), color=c("red", "blue"))

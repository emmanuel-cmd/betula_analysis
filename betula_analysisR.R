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

# # IDW - Inverse distance weigthing
# # Create an empty grid where n is the total number of cells
# grd              <- as.data.frame(spsample(betlocspaug, "regular", n=50000))
# names(grd)       <- c("X", "Y")
# coordinates(grd) <- c("X", "Y")
# gridded(grd)     <- TRUE  # Create SpatialPixel object
# fullgrid(grd)    <- TRUE  # Create SpatialGrid object
#
# # Add P's projection information to the empty grid
# #proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
# proj4string(grd) <- proj4string(betlocspaug)
#
# # Interpolate the grid cells using a power value of 2 (idp=2.0)
# P.idw <- gstat::idw(NO2 ~ 1, betlocspaug, newdata=grd, idp=2.0)
#
# # Convert to raster object then clip to Texas
# r       <- raster(P.idw)
# r.m     <- crop(r, augsburg)
# r.m <- mask(r.m, augsburg)
# plot(r.m)
#
# plot(augsburg, add =T)
# # Plot
# tm_shape(r.m) +
#   tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
#             title="Predicted precipitation \n(in inches)") +
#   tm_shape(P) + tm_dots(size=0.2) +
#   tm_legend(legend.outside=TRUE)
#
# # set up an 'empty' raster, here via an extent object derived from your data
# e <- extent(augsburg)
# # e <- e + 1000 # add this as all y's are the same
#
# r <- raster(e, nrow = 100, ncol=100)
# proj4string(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# # or r <- raster(xmn=, xmx=,  ...
#
# # you need to provide a function 'fun' for when there are multiple points per cell

# # Create a grid
# bbox <- st_bbox(betlocspaug)
# bbox
#
# cell_size <- 5
#
# x <- seq(as.vector(bbox$xmin), as.vector(bbox$xmax), by=cell_size)
# y <- seq(bbox$ymin, bbox$ymax, by=cell_size)
#
# meuse_grid <- expand.grid(x=x, y=y)
# plot(meuse_grid$x, meuse_grid$y, pch=19, cex=0.1)
#
# meuse_grid <- expand.grid(x=x, y=y)
# plot(meuse_grid$x, meuse_grid$y, pch=1)
#
# # KRIGING
#
# # create sample variogram
# betloc.v <- gstat::variogram(NO2 ~ 1, betlocspaug)
#
# # fit variogram model
# betloc.vfit <- gstat::fit.variogram(betloc.v, vgm(1, "Sph", 300, 1))
#
# # ordinary kriging
# lz.ok <- gstat::krige(NO2 ~ 1, betlocspaug, grd, betloc.vfit)


betlocspaugdf <- as.data.frame(betlocspaug)[,-c(41:42)]
str(betlocspaugdf)

# Write a function to scale columns btw 0 and 1
normalizevar <- function(x){
  (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T))
}

# 
newbetdf <- data.frame("pollenprod" = normalizevar(betlocspaugdf$PG_per_tree), 
                       "allergen" = normalizevar(betlocspaugdf$Bet_v_1), 
                       "floweringdur" = normalizevar(betlocspaugdf$Flowering_duration))
# Calculate VPA
newbetdf$VPA <- apply(newbetdf, 1, prod, na.rm=T)

# Assign NA to the value 1
newbetdf[newbetdf$VPA == 1, "VPA"] <- NA

# check which rows of pollen production, allergenicity or duration has the value 1
newbetdf[newbetdf$pollenprod == 1 | newbetdf$allergen == 1| newbetdf$floweringdur == 1, ]

# Take care of a case where allergenicity is 1
newbetdf[130, "VPA"] <- 1

# Check range 
range(newbetdf$VPA, na.rm=T)

# Append other variables 
newbetdf <- cbind(newbetdf, betlocspaugdf[,5:ncol(betlocspaugdf)])

# Set seed for reproducibility
set.seed(123)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

newbetdf[is.na(newbetdf)] <- 0

# Train the model
step.model <- train(VPA ~., data = newbetdf[,-c(1:3)],
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control, 
                    na.action = na.pass
)
step.model$results

# Best model 
step.model$bestTune
summary(step.model$finalModel)

# Get coefficient of the best model
coef(step.model$finalModel, 5)

# Linear regression model for all variables 
modelall <- lm(VPA ~ ., data = newbetdf[,-c(1:3)])
summary(modelall)

# Sort dataframe 
modelimp <- data.frame(varImp(modelall), "names"=rownames(varImp(modelall)))
modelimp[order(modelimp$Overall, decreasing = T),]

# The most important variables are Bet_v_1, flowering_end_date, Flowering_duration, flowering_start_date, LTB_4, g_pollen___catkin, tree_height, trunk_perimeter, 
# NO___NO2, avg_catkin_weight, NO2,  O3, NH3

modelgud <- lm(VPA ~ Bet_v_1 + flowering_end_date + Flowering_duration + 
     flowering_start_date + LTB_4 + g_pollen___catkin + tree_height + trunk_perimeter + 
     NO___NO2 + avg_catkin_weight + NO2 + O3 + NH3, data = newbetdf)
summary(modelgud)

# Get coefficients
modelgudcoeff <- coefficients(modelgud)

# A function to implement human exposure index 
HEX <- function(x){
  
  return((x$Bet_v_1 * 7.328213e-06) + (x$flowering_end_date * -3.185729e-03) + 
          (x$Flowering_duration * 1.135925e-02) + (x$flowering_start_date * 1.598094e-03) + 
           (x$LTB_4 * -1.335148e-04) + (x$g_pollen___catkin * -1.277308e+00) + (x$tree_height * 2.340924e-03) + 
           (x$trunk_perimeter * -3.142437e-04) + (x$NO___NO2 * 2.783331e-03) + (x$avg_catkin_weight*2.120750e-01) + 
           (x$NO2 * 1.786553e-03) + (x$O3 * 8.337465e-05) + (x$NH3 * 3.541691e-03))
} 

HEX(newbetdf[,-c(1:3)]) %>% range()

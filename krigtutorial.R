library("gstat")   # geostatistics
library("mapview") # map plot
library("sf")      # spatial vector data
library("stars")   # spatial-temporal data
library("terra")   # raster data handling 
library("ggplot2") # plotting
library(sp)
mapviewOptions(fgb = FALSE)

# Load data
data(meuse)
data("meuse.grid_ll")
coordinates(meuse) = ~x+y

head(meuse)

# Spatial exploratory data
mapview(meuse['zinc'])

# Load meuse grid
meuse_gridcv <- st_as_stars(meuse.grid)
st_as_stars(meuse.grid_ll) %>% plot()

# Create a grid
bbox <- st_bbox(meuse)
bbox

# Create a regularly spaced coordinates in the x and y direction
cell_size <- 40
x <- seq(bbox$xmin, bbox$xmax, by=cell_size)
y <- seq(bbox$ymin, bbox$ymax, by=cell_size)

# Use expand.grid() yields every combination of x and y 
meuse_grid <- expand.grid(x=x, y=y)
plot(meuse_grid$x, meuse_grid$y, pch=19, cex=0.1)

# Convert the grid dataframe to a stars object 
meuse_grid$tmp <- 1
meuse_grid <- st_as_stars(meuse_grid, crs=st_crs(meuse))
st_crs(meuse_grid) <- st_crs(meuse) # re-assign crs to be safe

# IDW interpolation
zn.idw <- gstat::idw(zinc ~ 1, locations=meuse, newdata=meuse_grid, idp = 2)
st_crs(zn.idw) <- st_crs(4326)

#
plot(rast(zn.idw["var1.pred"]))
plot(meuse["zinc"], col=1, cex=0.5, add=T, type="p")

# Map view 
mapview(zn.idw, zcol='var1.pred', layer.name = "Zinc ppm")

# STATIONARITY

# Sample variogram
meuse.v <- gstat::variogram(log(zinc) ~ 1, meuse)
plot(meuse.v, ylab=bquote(gamma), xlab=c("h (separation distance in m)"))

# Direction dependence 
plot(variogram(log(zinc) ~ 1, meuse, alpha = c(0, 45, + 90, 135)))

# Cutoff and lag width
plot(variogram(log(zinc) ~ 1, meuse, cutoff = 1000, width = 50))

# Theoretical variogram

# Fit a variogram
myVariogramModel <- vgm(psill=1, "Sph", range=100, nugget=0.5)
plot(myVariogramModel, cutoff=150)

# A visual overview of the basic variogram models in gstat
show.vgms()

# Fit a variogram

# 1. Calculate the sample variogram
meuse.v <- variogram(log(zinc) ~ 1, meuse)

# 2. Choose a suitable model
# 3. Choose suitable initial values for partial sill(s), range(s), and possibly nugget
meuse.sph <- vgm(psill=0.6, "Sph", range=800, nugget=0.1)
plot(meuse.v, meuse.sph, cutoff=1000)

# 4. Fit this model using one of the fitting criteria
meuse.vfit <- fit.variogram(meuse.v, meuse.sph)
meuse.vfit

# Plot the fitted variogram
plot(meuse.v, meuse.vfit)

# Model fitting may fail if we assign values that are unreasonable
fit.variogram(meuse.v, vgm(1, "Sph", 10, 1))

# Partial variogram model fitting
fit.variogram(meuse.v, vgm(0.6, "Sph", 800, 0.06), fit.sills = c(FALSE, TRUE))

# Visual variogram model fitting
library(geoR)
v.eye <- eyefit(variog(as.geodata(meuse["zinc"]), max.dist = 1500))
ve.fit <- as.vgm.variomodel(v.eye[[1]])

# ORDINARY KRIGING
# create sample variogram
meuse.v <- gstat::variogram(log(zinc) ~ 1, meuse)

# fit variogram model
meuse.vfit <- gstat::fit.variogram(meuse.v, vgm(1, "Sph", 800, 1))

# ordinary kriging
lz.ok <- gstat::krige(log(zinc) ~ 1, meuse, meuse_gridcv, meuse.vfit)

# Plot interpolation and points 
plot(rast(lz.ok['var1.pred']))
plot(meuse["zinc"], col="blue", cex=0.5, type="p", add=T)

# Universal kriging
# create sample variogram
meuse.rv <- variogram(log(zinc) ~ sqrt(dist), meuse)

# fit variogram model
meuse.rvfit <- fit.variogram(meuse.rv, vgm(1, "Sph", 300, 1))

# 
plot(meuse_gridcv["dist"])

# Universal kriging 
lz.uk <- krige(log(zinc) ~ sqrt(dist), locations=meuse, 
               newdata=meuse_gridcv, 
               model=meuse.rvfit)
#  
plot(rast(lz.uk["var1.pred"]))
plot(meuse["zinc"], col="red", cex=0.5, type="p", add=T)

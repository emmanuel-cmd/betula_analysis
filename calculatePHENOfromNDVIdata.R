library(greenbrown)
library(sp)
library(sf)
library(raster)


# Read NDVI raster with brick
ndvirasterstack_L8 <- stack(paste0(getwd(),"/", list.files("ndviL8cropped", pattern='.tif$', all.files=TRUE, full.names=T)))
ndvirasterstack_L7 <- stack(paste0(getwd(),"/", list.files("ndviL8cropped/L7", pattern='.tif$', all.files=TRUE, full.names=T)))


# Sort dates
dates <- c("2014-03-14", "2014-03-21","2014-03-30", "2014-06-09", "2015-06-05", "2015-06-12",
           "2015-08-31", "2015-10-02", "2016-04-11", "2016-05-06", "2016-05-22", "2016-06-23",
           "2016-08-26", "2016-09-11", "2017-02-25", "2017-04-30", "2017-07-19", "2017-08-29",
           "2017-10-16", "2018-05-28", "2018-07-31", "2018-09-17",
           "2014-03-13", "2014-03-29", "2014-06-10", "2014-07-19", "2015-03-09", "2015-04-10",
           "2015-06-04", "2015-08-07", "2015-09-01", "2016-01-14", "2016-03-18", "2016-06-06",
           "2016-08-25", "2016-09-10", "2016-11-29", "2017-05-17", "2017-08-28", "2017-10-15",
           "2018-04-18", "2018-07-30", "2018-09-16", "2018-10-11")

datesorted <- sort(dates)

# Merge all NDVI together into one
ndvirasterallyears <- stack(ndvirasterstack_L7, ndvirasterstack_L8)
ndvirasterallyears <- setZ(ndvirasterallyears, dates, "Date")

# Order raster
ndvirasterallyears <- ndvirasterallyears[[order(dates)]]

# Append dates Z values
ndviraster14 <- setZ(ndvirasterallyears[[1:8]], as.POSIXct(datesorted[1:8]), "Date")
ndviraster15 <- setZ(ndvirasterallyears[[9:17]], as.POSIXct(datesorted[9:17]), "Date")
ndviraster16 <- setZ(ndvirasterallyears[[18:29]], as.POSIXct(datesorted[18:29]), "Date")
ndviraster17 <- setZ(ndvirasterallyears[[30:37]], as.POSIXct(datesorted[30:37]), "Date")
ndviraster18 <- setZ(ndvirasterallyears[[38:44]], as.POSIXct(datesorted[38:44]), "Date")

# aa <- PhenologyRaster(ndviraster14)

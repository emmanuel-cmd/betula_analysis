library(greenbrown)
library(sp)
library(sf)

# Read NDVI raster with brick
ndvirasterstack_L7 <- readRDS("ndvirasterstack_L7.rds")
ndvirasterstack_L8 <- readRDS("ndvirasterstack_L8.rds")

# Append dates as names to the raster
names(ndvirasterstack_L7) <- c("2014-03-14", "2014-03-21","2014-03-30", "2014-06-09", "2015-06-05", "2015-06-12", 
                               "2015-08-31", "2015-10-02", "2016-04-11", "2016-05-06", "2016-05-22", "2016-06-23", 
                               "2016-08-26", "2016-09-11", "2017-02-25", "2017-04-30", "2017-07-19", "2017-08-29", 
                               "2017-10-16", "2018-05-28", "2018-07-31", "2018-09-17")
# Write out dates
dat <- c("2014-03-14", "2014-03-21","2014-03-30", "2014-06-09", "2015-06-05", "2015-06-12", 
         "2015-08-31", "2015-10-02", "2016-04-11", "2016-05-06", "2016-05-22", "2016-06-23", 
         "2016-08-26", "2016-09-11", "2017-02-25", "2017-04-30", "2017-07-19", "2017-08-29", 
         "2017-10-16", "2018-05-28", "2018-07-31", "2018-09-17")

dat_sorted <-  sort(c("2014-03-14", "2014-03-21","2014-03-30", "2014-06-09", "2015-06-05", "2015-06-12", 
                      "2015-08-31", "2015-10-02", "2016-04-11", "2016-05-06", "2016-05-22", "2016-06-23", 
                      "2016-08-26", "2016-09-11", "2017-02-25", "2017-04-30", "2017-07-19", "2017-08-29", 
                      "2017-10-16", "2018-05-28", "2018-07-31", "2018-09-17"))

# Order raster
ndvirasterstack_L7 <- ndvirasterstack_L7[[order(dat_sorted)]]

# Append dates Z values
ndvirasterstack_L7_dated <- setZ(ndvirasterstack_L7, as.POSIXct(dat_sorted), "Date")

# Append dates as names to the raster
names(ndvirasterstack_L8) <- c("2014-03-13", "2014-03-29", "2014-06-10", "2014-07-19", "2015-03-09", "2015-04-10", 
                               "2015-06-04", "2015-08-07", "2015-09-01", "2016-01-14", "2016-03-18", "2016-06-06", 
                               "2016-08-25", "2016-09-10", "2016-11-29", "2017-05-17", "2017-08-28", "2017-10-15", 
                               "2018-04-18", "2018-07-30", "2018-09-16", "2018-10-11")
# Sort dates 
sortedind <- sort(c("2014-03-13", "2014-03-29", "2014-06-10", "2014-07-19", "2015-03-09", "2015-04-10", 
                    "2015-06-04", "2015-08-07", "2015-09-01", "2016-01-14", "2016-03-18", "2016-06-06", 
                    "2016-08-25", "2016-09-10", "2016-11-29", "2017-05-17", "2017-08-28", "2017-10-15", 
                    "2018-04-18", "2018-07-30", "2018-09-16", "2018-10-11"))

# Order raster
ndvirasterstack_L8 <- ndvirasterstack_L8[[order(sortedind)]]

# Append dates Z values
ndvirasterstack_L8_dated <- setZ(ndvirasterstack_L8, as.POSIXct(sortedind), "Date")



PhenologyRaster(ndvirasterstack_L7_dated, freq = 1)
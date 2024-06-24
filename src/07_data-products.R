#
# Title: Creation of the data products for the ABMI website
# Created: June 24, 2024
# Last Updated: June 24, 2024
# Author: Brandon Allen
# Objectives: Create the raster files (tif) to be displayed on the ABMI website for download
# Keywords: Notes, Raster Creation
#
#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###################
# Raster Creation #
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(dismo)
library(gdalUtils)
library(raster)
library(sf)

# Load new kgrid without the climate information
load("results/kgrid/kgrid-no-climate.Rdata")

# Load the provincial mapping file to get the relevant CRS
load("data/base/mapping/provincial-boundary.Rdata")

##################################################
# Load the 1991-2020 climate normals and process #
##################################################

# Load the csv and merge with the kgrid so we have a single file
input.data <- read.csv("data/processed/kgrid_normals_1991_2020_yearly.csv")
colnames(input.data)[1] <- "LinkID"
input.data <- input.data[, -c(2:5)]

kgrid <- merge.data.frame(kgrid, input.data, by = "LinkID")
kgrid <- kgrid[ , colnames(kgrid) != "MAR"] # MAR is a voided attribute

# Add the monthly variables
input.data <- read.csv("data/processed/kgrid_normals_1991_2020_monthly.csv")
rownames(input.data) <- input.data$id1

tmax <- as.matrix(input.data[, 6:17])
tmin <- as.matrix(input.data[, 18:29])
precip <- as.matrix(input.data[, 42:53])

derived.climate <- as.data.frame(biovars(prec = precip, 
                                         tmin = tmin, 
                                         tmax = tmax))
derived.climate$LinkID <- input.data$id1

kgrid <- merge.data.frame(kgrid, derived.climate, by = "LinkID")

############################
# Create the spatial files #
############################

climate.variables <- colnames(kgrid)[23:ncol(kgrid)]

for(variable in climate.variables) {
  
  # Convert to raster
  cur.raster <- rasterFromXYZ(kgrid[, c("X", "Y", variable)], crs = crs(province.shapefile))
  
  writeRaster(x = cur.raster, 
              filename = paste0("results/data-product/", variable, 
                                ".tif"),
              overwrite = TRUE)

}

rm(list=ls())
gc()


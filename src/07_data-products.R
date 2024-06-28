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
library(raster)
library(reticulate)
library(sf)

# Initialize arcpy so we can update the metadata for the rasters with a template
py_discover_config() # We need version 3.9
py_config() # Double check it is version 3.9

# Set python 
use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

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
  
  # # Define the functions for updating the metadata
  # # We can add this metadata at a later date
  # metadata.function <- arcpy$metadata
  # raster.metadata <- metadata.function$Metadata(paste0("results/data-product/", 
  #                                                      variable, 
  #                                                      ".tif"))
  # 
  # # Update the metadata fields
  # raster.metadata$title <- variable
  # raster.metadata$tags <- '"ClimateNA", "1km Grid", "1991-2020 30-year average"'
  # raster.metadata$summary <- "Gridded (1km) annual climate variables for Alberta based on the 1991-2020 30-year average."
  # raster.metadata$description <- "ClimateNA (Version 7.4) is a standalone application that downscales PRISM (Daly et al. 2008) gridded monthly climate normal data (800 x 800 m) to scale-free point locations (Wang et al., 2016). We calculated annual climate variables based on the 30-year average for 1991-2020. Monthly climate variables were also extracted from the 1991-2020 30-year average and used to calculate the BioClim variables using the dismo R package (Version 1.3-14; Robert et al., 2022)."
  # 
  # # Save the metadata to the target raster
  # raster.metadata$save()

}

rm(list=ls())
gc()

#
# Title: On-grid climate summaries
# Created: December 4th, 2023
# Last Updated: March 19th, 2024
# Author: Brandon Allen
# Objectives: Extraction of the climate data from the Climate NA software (On grid sites summaries)
# Keywords: Notes, Initialization, Climate, Hydrotemporal
#

######### The latitude, longitude, and elevation information can't be stored permanently, so once extractions are
# Notes # performed they need to be removed. Lat/long are extracted from site tables, elevation from the stored 30m raster
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################
# Initialization # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ClimateNAr)
library(dismo)
library(reticulate)
library(sf)

# Initialize arcpy
py_discover_config() # We need version 3.9
py_config() # Double check it is version 3.9

# Set python 
use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

###########
# Climate # 
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the site paths, this file must be maintained in the 
# "beta" folder and never uploaded to GitHub
site.path <- read.csv("beta/site-paths_2023.csv")

# Define the site path
site.path <- paste0(site.path$Database[5], site.path$Feature[5])

# Export the sites
arcpy$ExportFeatures_conversion(in_features = site.path, 
                                out_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp")

# Extract the DEM point values
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/dem/elevation_nasadem_30m.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp")

# Extract the pAspen points
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/Populus_tremuloides_brtpred_nofp.asc", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp")

arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/ASPEN9706.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.shp")

# Export the tables
arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.csv")

# Extract the information from ClimateNA
# Standardize the output tables
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_dem.csv")

# Determine which columns identify year information
year.col <- paste0("yr_", c(seq(2003,2019, 1), 2021, 2022, 2023))
site.id <- data.frame(Site = NA,
                      id1 = NA,
                      id2 = NA,
                      lat = NA,
                      long = NA,
                      elev = NA)

for(col.id in year.col) {
  
  temp.data <- input.data[!(input.data[, col.id] %in% "N"), ]
  
  if(nrow(temp.data) == 0) next
    
  temp.data <- data.frame(Site = temp.data$ABMI,
                          id1 = paste0(temp.data$ABMI_ID_Wi, "_", gsub("yr_", "", col.id)),
                          id2 = paste0(temp.data$ABMI_ID_Wi, "_", gsub("yr_", "", col.id)),
                          lat = temp.data$Latitude,
                          long = temp.data$Longitude,
                          elev = temp.data$RASTERVALU)
  
  site.id  <- rbind(site.id, temp.data)
  rm(temp.data)
  
}

site.id <- site.id[-1, ]

# Save the results in the appropriate format appropriate input file
write.csv(site.id[, -1], file = "D:/abmi/ClimateData/data/base/gis/temp/sites_cleaned.csv", row.names = FALSE)

# Call the ClimateNA API to calculate the relevant climate variables
# NOTE: We need to use the double backslash for the executable to read the information correctly
# Yearly averages
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_yearly.csv")

# Monthly averages
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "M", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_monthly.csv")

# Format the paspen information
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")
input.data.na <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.csv") 

# Determine which columns identify year information
year.col <- paste0("yr_", c(seq(2003,2019, 1), 2021, 2022, 2023))

# Create template 
site.id <- data.frame(Site = NA,
                      id1 = NA,
                      id2 = NA,
                      lat = NA,
                      long = NA,
                      pAspen = NA)

# Alberta paspen results
for(col.id in year.col) {
  
  temp.data <- input.data[!(input.data[, col.id] %in% "N"), ]
  
  if(nrow(temp.data) == 0) next
  
  temp.data <- data.frame(Site = temp.data$ABMI,
                          id1 = paste0(temp.data$ABMI_ID_Wi, "_", gsub("yr_", "", col.id)),
                          id2 = paste0(temp.data$ABMI_ID_Wi, "_", gsub("yr_", "", col.id)),
                          lat = temp.data$Latitude,
                          long = temp.data$Longitude,
                          pAspen = temp.data$RASTERVALU)
  
  site.id  <- rbind(site.id, temp.data)
  rm(temp.data)
  
}

# National paspen results
site.id.na <- data.frame(Site = NA,
                      id1 = NA,
                      id2 = NA,
                      lat = NA,
                      long = NA,
                      pAspen_NA = NA)

for(col.id in year.col) {
  
  temp.data <- input.data.na[!(input.data.na[, col.id] %in% "N"), ]
  
  if(nrow(temp.data) == 0) next
  
  temp.data <- data.frame(Site = temp.data$ABMI,
                          id1 = paste0(temp.data$ABMI_ID_Wi, "_", gsub("yr_", "", col.id)),
                          id2 = paste0(temp.data$ABMI_ID_Wi, "_", gsub("yr_", "", col.id)),
                          lat = temp.data$Latitude,
                          long = temp.data$Longitude,
                          pAspen_NA = temp.data$RASTERVALU)
  
  site.id.na  <- rbind(site.id.na, temp.data)
  rm(temp.data)
  
}

site.id <- site.id[-1, ]
site.id.na <- site.id.na[-1, ]

# Add the public site information
public.coordinates <- read.csv("data/lookup/public-coordiantes.csv")
public.coordinates <- public.coordinates[, c("SITE_ID", "PUBLIC_LATTITUDE", "PUBLIC_LONGITUDE",
                                             "ELEVATION", "NATURAL_REGIONS", "NATURAL_SUBREGIONS", 
                                             "LANDUSE_FRAMEWORK")]
colnames(public.coordinates) <- c("Site", "Latitude", "Longitude", "Elevation", 
                                  "NrName", "NsrName", "LufName")

# Add the UTM Easting and Northing information
utm10 <- st_as_sf(public.coordinates, coords=c("Longitude", "Latitude"), crs=4326, remove=FALSE)
utm10 <- st_coordinates(st_transform(utm10, crs = 3400))
public.coordinates$Easting <- utm10[,1]
public.coordinates$Northing <- utm10[,2]

# There are a few missing sites names, so we need to add them
site.id$Site[site.id$id1 %in% c("ST-430-1_2023", "ST-430-2_2023", "ST-430-3_2023")] <- 430
site.id$Site[site.id$id1 %in% c("ST-1135-1_2023", "ST-1135-2_2023")] <- 1135
site.id$Site[site.id$id1 %in% c("ST-1169-1_2023")] <- 1169

site.id <- merge.data.frame(public.coordinates, site.id[, c("Site", "id1", "pAspen")], by = "Site")
site.id <- site.id[, -1]
site.id <- merge.data.frame(site.id, site.id.na[, c("id1", "pAspen_NA")], by = "id1")

# Stitch together the paspen and yearly climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_yearly.csv")
climate.data <- climate.data[, !(colnames(climate.data) %in% c("id2", "Latitude", "Longitude",
                                                               "Elevation", "MAR"))]

climate.1ha <- merge.data.frame(site.id, climate.data, by = "id1")

# Stitch together the yearly and monthly climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_monthly.csv")
rownames(climate.data) <- climate.data$id1
tmax <- as.matrix(climate.data[, 6:17])
tmin <- as.matrix(climate.data[, 18:29])
precip <- as.matrix(climate.data[, 42:53])

derived.climate <- as.data.frame(biovars(prec = precip, 
                                         tmin = tmin, 
                                         tmax = tmax))
derived.climate$id1 <- climate.data$id1
derived.climate <- derived.climate[, c("id1", "bio9", "bio15")]

climate.1ha <- merge.data.frame(climate.1ha, derived.climate, by = "id1")

# Standardize the names
colnames(climate.1ha)[1] <- "Site_Year"
rownames(climate.1ha) <- climate.1ha$Site_Year

# Save
save(climate.1ha, file = "data/processed/sites/ongrid-1ha_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

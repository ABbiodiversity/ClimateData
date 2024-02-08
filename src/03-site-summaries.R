#
# Title: Extraction of the climate data from the Climate NA software (Site summaries)
# Created: December 4th, 2023
# Last Updated: January 20th, 2024
# Author: Brandon Allen
# Objectives: Extract site level summaries for the climate data
# Keywords: Notes, Initialization, On grid Sites, Off grid Sites, Quadrants, NEED TO DO ESID Sites, Camera/ARU Sites, Wetland Sites, Elly Sites
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

#################
# On grid Sites # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_climate.csv")

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

# AB results
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

# NA results
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

# There are a few missing sites names, so we need to add them
site.id$Site[site.id$id1 %in% c("ST-430-1_2023", "ST-430-2_2023", "ST-430-3_2023")] <- 430
site.id$Site[site.id$id1 %in% c("ST-1135-1_2023", "ST-1135-2_2023")] <- 1135
site.id$Site[site.id$id1 %in% c("ST-1169-1_2023")] <- 1169

site.id <- merge.data.frame(public.coordinates, site.id[, c("Site", "id1", "pAspen")], by = "Site")
site.id <- site.id[, -1]
site.id <- merge.data.frame(site.id, site.id.na[, c("id1", "pAspen_NA")], by = "id1")

# Stitch together the paspen and climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_climate.csv")
climate.data <- climate.data[, !(colnames(climate.data) %in% c("id2", "Latitude", "Longitude",
                                                               "Elevation", "MAR"))]

climate.1ha <- merge.data.frame(site.id, climate.data, by = "id1")
colnames(climate.1ha)[1] <- "Site_Year"
rownames(climate.1ha) <- climate.1ha$Site_Year

# Save
save(climate.1ha, file = "results/sites/on-grid-sites_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

##################
# Off grid Sites # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load arcpy back in
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Load the site paths, this file must be maintained in the 
# "beta" folder and never uploaded to GitHub
site.path <- read.csv("beta/site-paths_2023.csv")

# Define the site path
site.path <- paste0(site.path$Database[4], site.path$Feature[4])

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
site.id <- data.frame(Site = input.data$SITE,
                      id1 = paste0(input.data$ProjectID, "_", input.data$Year),
                      id2 = paste0(input.data$ProjectID, "_", input.data$Year),
                      lat = input.data$Latitude,
                      long = input.data$Longitude,
                      elev = input.data$RASTERVALU)

# Remove the NA ones temporarily. Not sure what is going on with these sites
site.id <- site.id[!is.na(site.id$Site), ]

# Save the results in the appropriate format appropriate input file
write.csv(site.id[, -1], file = "D:/abmi/ClimateData/data/base/gis/temp/sites_cleaned.csv", row.names = FALSE)

# Call the ClimateNA API to calculate the relevant climate variables
# NOTE: We need to use the double backslash for the executable to read the information correctly
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_climate.csv")

# Format the paspen information
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")
input.data.na <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_na.csv")

# Determine which columns identify year information
site.id <- data.frame(Site = input.data$SITE,
                      id1 = paste0(input.data$ProjectID, "_", input.data$Year),
                      id2 = paste0(input.data$ProjectID, "_", input.data$Year),
                      lat = input.data$Latitude,
                      long = input.data$Longitude,
                      pAspen = input.data$RASTERVALU)

site.id.na <- data.frame(id1 = paste0(input.data.na$ProjectID, "_", input.data.na$Year),
                         pAspen_NA = input.data.na$RASTERVALU)

# Remove the NA ones temporarily. Not sure what is going on with these sites
site.id <- site.id[!is.na(site.id$Site), ]
site.id.na <- site.id.na[!is.na(site.id.na$id1), ]

# Add the public site information
public.coordinates <- read.csv("data/lookup/public-coordiantes.csv")
public.coordinates <- public.coordinates[, c("SITE_ID", "PUBLIC_LATTITUDE", "PUBLIC_LONGITUDE",
                                             "ELEVATION", "NATURAL_REGIONS", "NATURAL_SUBREGIONS", 
                                             "LANDUSE_FRAMEWORK")]
colnames(public.coordinates) <- c("Site", "Latitude", "Longitude", "Elevation", 
                                  "NrName", "NsrName", "LufName")

site.id <- merge.data.frame(public.coordinates, site.id[, c("Site", "id1", "pAspen")], by = "Site")
site.id <- site.id[, -1]
site.id <- merge.data.frame(site.id, site.id.na[, c("id1", "pAspen_NA")], by = "id1")

# Stitch together the paspen and climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_climate.csv")
climate.data <- climate.data[, !(colnames(climate.data) %in% c("id2", "Latitude", "Longitude",
                                                               "Elevation", "MAR"))]

climate.1ha <- merge.data.frame(site.id, climate.data, by = "id1")
colnames(climate.1ha)[1] <- "Site_Year"
rownames(climate.1ha) <- climate.1ha$Site_Year

# Save
save(climate.1ha, file = "results/sites/off-grid-sites_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

# Stitch together all sites
load("results/sites/off-grid-sites_2023.Rdata")
climate.off <- climate.1ha
load("results/sites/on-grid-sites_2023.Rdata")
climate.1ha <- rbind(climate.1ha, climate.off)
save(climate.1ha, file = "results/sites/terrestrial-sites_2023.Rdata")

rm(list=ls())
gc()

#############
# Quadrants # 
#############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the site paths, this file must be maintained in the "beta" folder and never uploaded to GitHub
site.path <- read.csv("beta/site-paths_2023.csv")

# Define the terrestrial path
site.path <- paste0(site.path$Database[1], site.path$Feature[1])

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Select the quadrants
arcpy$Select_analysis(in_features = site.path, 
                      out_feature_class = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                      where_clause = "Section IN ('NE', 'NW', 'SE', 'SW')")

# Find the centroids of each quadrant (EPSG 4326)
# Need both Lat/Long (for ClimateNA) and the same projection as the DEM
arcpy$management$CalculateGeometryAttributes(in_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                                             geometry_property = "Latitude CENTROID_Y;Longitude CENTROID_X", 
                                             coordinate_system = "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]", 
                                             coordinate_format = "DD")

arcpy$management$CalculateGeometryAttributes(in_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                                             geometry_property = "Y CENTROID_Y;X CENTROID_X", 
                                             coordinate_system = "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]")

# Convert to points
arcpy$management$XYTableToPoint(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                                out_feature_class = "D:/abmi/ClimateData/data/base/gis/temp/sites_centroid.shp", 
                                x_field = "X", 
                                y_field = "Y", 
                                coordinate_system = "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision")

# Extract the DEM point values
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_centroid.shp", 
                              in_raster = "D:/abmi/ClimateData/data/base/gis/dem/elevation_nasadem_30m.tif", 
                              out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp")

# Extract the pAspen points
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_centroid.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/Populus_tremuloides_brtpred_nofp.asc", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp")

arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_centroid.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/ASPEN9706.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.shp")

# Export the tables
arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.csv")

# Standardize the output tables
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_dem.csv")
input.data$Site_Year <- input.data$UID

# Format the kgrid to be the appropriate input file
input.data <- input.data[, c("Site_Year", "Site_Year", "Latitude",  "Longitude", "RASTERVALU")]
colnames(input.data) <- c("id1", "id2", "lat", "long", "elev")

write.csv(input.data, file = "D:/abmi/ClimateData/data/base/gis/temp/sites_cleaned.csv", row.names = FALSE)

# Call the ClimateNA API to calculate the relevant climate variables
# NOTE: We need to use the double backslash for the executable to read the information correctly
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_climate.csv")

# Format the paspen information
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")
input.data.na <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen_NA.csv")

# Determine which columns identify year information
site.id <- data.frame(Site = input.data$ABMI2,
                      id1 = input.data$UID,
                      id2 = input.data$UID,
                      lat = input.data$Latitude,
                      long = input.data$Longitude,
                      pAspen = input.data$RASTERVALU)
rownames(site.id) <- site.id$id1

site.id.na <- data.frame(id1 = input.data$UID,
                         pAspen_NA = input.data.na$RASTERVALU)

# Load the missing site data
missing.sites <- read.csv("data/lookup/nearest-abmi-site.csv")
rownames(missing.sites) <- missing.sites$Site_Year_Quadrant

# Add the nearest site information
site.id$Site <- missing.sites[site.id$id1, "Nearest_ABMI"]

# Add the public site information
public.coordinates <- read.csv("data/lookup/public-coordiantes.csv")
public.coordinates <- public.coordinates[, c("SITE_ID", "PUBLIC_LATTITUDE", "PUBLIC_LONGITUDE",
                                             "ELEVATION", "NATURAL_REGIONS", "NATURAL_SUBREGIONS",
                                             "LANDUSE_FRAMEWORK")]
colnames(public.coordinates) <- c("Site", "Latitude", "Longitude", "Elevation", 
                                  "NrName", "NsrName", "LufName")

site.id <- merge.data.frame(public.coordinates, site.id[, c("Site", "id1", "pAspen")], by = "Site")
site.id <- site.id[, -1]
site.id <- merge.data.frame(site.id, site.id.na[, c("id1", "pAspen_NA")], by = "id1")

# Stitch together the paspen and climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_climate.csv")
climate.data <- climate.data[, !(colnames(climate.data) %in% c("id2", "Latitude", "Longitude",
                                                               "Elevation", "MAR"))]

climate.qa <- merge.data.frame(site.id, climate.data, by = "id1")
colnames(climate.qa)[1] <- "Site_Year_Quadrant"
rownames(climate.qa) <- climate.qa$Site_Year_Quadrant

# Save
save(climate.qa, file = "results/sites/terrestrial-quadrants_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

##############
# EISD Sites # 
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the site paths, this file must be maintained in the 
# "beta" folder and never uploaded to GitHub
site.path <- read.csv("beta/site-paths_2023.csv")

# Define the site path
site.path <- paste0(site.path$Database[5], site.path$Feature[5])

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Export the sites
arcpy$ExportFeatures_conversion(in_features = site.path, 
                                out_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp")

# Extract the DEM point values
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/dem/elevation_nasadem_30m.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp")

# Extract the pAspen points
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/ASPEN9706.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp")

# Export the tables
arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")

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
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_climate.csv")

# Format the paspen information
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")

# Determine which columns identify year information
year.col <- paste0("yr_", c(seq(2003,2019, 1), 2021, 2022, 2023))
site.id <- data.frame(Site = NA,
                      id1 = NA,
                      id2 = NA,
                      lat = NA,
                      long = NA,
                      pAspen = NA)

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

site.id <- site.id[-1, ]

# Add the public site information
public.coordinates <- read.csv("data/lookup/public-coordiantes.csv")
public.coordinates <- public.coordinates[, c("SITE_ID", "PUBLIC_LATTITUDE", "PUBLIC_LONGITUDE",
                                             "ELEVATION", "NATURAL_REGIONS", "NATURAL_SUBREGIONS")]
colnames(public.coordinates) <- c("Site", "Latitude", "Longitude", "Elevation", 
                                  "NrName", "NsrName")

# There are a few missing sites names, so we need to add them
site.id$Site[site.id$id1 %in% c("ST-430-1_2023", "ST-430-2_2023", "ST-430-3_2023")] <- 430
site.id$Site[site.id$id1 %in% c("ST-1135-1_2023", "ST-1135-2_2023")] <- 1135
site.id$Site[site.id$id1 %in% c("ST-1169-1_2023")] <- 1169

site.id <- merge.data.frame(public.coordinates, site.id[, c("Site", "id1", "pAspen")], by = "Site")
site.id <- site.id[, -1]

# Stitch together the paspen and climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_climate.csv")
climate.data <- climate.data[, !(colnames(climate.data) %in% c("id2", "Latitude", "Longitude",
                                                               "Elevation", "MAR"))]

climate.1ha <- merge.data.frame(site.id, climate.data, by = "id1")
colnames(climate.1ha)[1] <- "Site_Year"

# Save
save(climate.1ha, file = "results/sites/on-grid-sites_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

##############
# Camera/ARU # Mess of stuff. No public coordinates, both cam and aru info (done twice, one for camera one for aru).
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the site paths, this file must be maintained in the 
# "beta" folder and never uploaded to GitHub
site.path <- read.csv("beta/site-paths_2023.csv")

# Define the site path
site.path <- paste0(site.path$Database[3], site.path$Feature[3])

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Export the sites
arcpy$ExportFeatures_conversion(in_features = site.path, 
                                out_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp")

# Extract the DEM point values
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/dem/elevation_nasadem_30m.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp")

# Extract the pAspen points
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/ASPEN9706.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp")

# Find the nearest distance of the sites to public locations
arcpy$GenerateNearTable_analysis(in_features = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp", 
                                 near_features = "D:/spatial/ABMI.gdb/ABMI_3x7", 
                                 out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_public.csv",
                                 method = "GEODESIC")

# Export the tables
arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_dem.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")

# Extract the information from ClimateNA
# Standardize the output tables
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_dem.csv")
input.data$Site_Year <- paste0(input.data$Site_ID, "_", input.data$Year)

# Format the kgrid to be the appropriate input file
input.data <- input.data[, c("Site_Year", "Site_Year", "Latitude",  "Longitude", "RASTERVALU")]
colnames(input.data) <- c("id1", "id2", "lat", "long", "elev")

write.csv(input.data, file = "D:/abmi/ClimateData/data/base/gis/temp/sites_cleaned.csv", row.names = FALSE)

# Call the ClimateNA API to calculate the relevant climate variables
# NOTE: We need to use the double backslash for the executable to read the information correctly
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_climate.csv")

# Take site locations and combine with the extracted public location
input.data <- read_sf(dsn = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp")
input.data <- as.data.frame(input.data)
input.data$IN_FID <- as.numeric(rownames(input.data)) - 1
input.data$Site_Year <- paste0(input.data$Site_ID, "_", input.data$Year)

public.site <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_public.csv")
input.data <- merge.data.frame(input.data[, c("IN_FID", "Site_Year")], 
                               public.site[, c("IN_FID", "NEAR_FID")], by = "IN_FID")

# Merge the NEAR_ID field with the original file from the 3x7s
public.gis <- read_sf(dsn = "D:/spatial/ABMI.gdb/", layer = "ABMI_3x7")
public.gis <- as.data.frame(public.gis)
colnames(public.gis)[1] <- "NEAR_FID"
input.data <- merge.data.frame(input.data, 
                               public.gis[, c("NEAR_FID", "ABMI")], by = "NEAR_FID")

# Now that we have the nearest site, combine with the public lookup table.
public.data <- read.csv("D:/abmi/ClimateData/data/lookup/public-coordiantes.csv")
colnames(public.data)[1] <- "ABMI"

public.data <- merge.data.frame(input.data[, c("ABMI", "Site_Year")], public.data, by = "ABMI")

# Format the climate data
climate.in <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_climate.csv")
colnames(climate.in)[1] <- "Site_Year"
site.data <- merge.data.frame(climate.in, public.data, by = "Site_Year")

# Format the paspen information
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")

# Determine which columns identify year information
input.data <- data.frame(Site_Year = paste0(input.data$Site_ID, "_", input.data$Year),
                         deployment = input.data$deployment,
                         pAspen = input.data$RASTERVALU)
site.data <- merge.data.frame(site.data, input.data, by = "Site_Year")

# Mask the latitude, longitude, and elevation information
site.data$Latitude <- 0
site.data$Longitude <- 0
site.data$Elevation <- 0

site.data$Latitude <- site.data$PUBLIC_LATTITUDE
site.data$Longitude <- site.data$PUBLIC_LONGITUDE
site.data$Elevation <- site.data$ELEVATION

# Correct column names
colnames(site.data)[colnames(site.data) %in% "NATURAL_REGIONS"] <- "NrName"
colnames(site.data)[colnames(site.data) %in% "NATURAL_SUBREGIONS"] <- "NsrName"

# Pull out the camera deployments
camera.climate <- site.data[site.data$deployment %in% c("BOTH", "CAM"), ]
aru.climate <- site.data[site.data$deployment %in% c("BOTH", "ARU"), ]

# Format the columns
camera.climate <- camera.climate[, c("Site_Year", "Latitude", "Longitude", "Elevation", "NrName",
                           "NsrName", "pAspen", "MAT", "MWMT", "MCMT", "TD", "MAP",
                           "MSP", "AHM", "SHM", "DD_0", "DD5", "DD_18", "DD18", "NFFD",
                           "bFFP", "eFFP", "FFP", "PAS", "EMT", "EXT", "Eref", "CMD",
                           "RH", "CMI", "DD1040")]

aru.climate <- aru.climate[, c("Site_Year", "Latitude", "Longitude", "Elevation", "NrName",
                               "NsrName", "pAspen", "MAT", "MWMT", "MCMT", "TD", "MAP",
                               "MSP", "AHM", "SHM", "DD_0", "DD5", "DD_18", "DD18", "NFFD",
                               "bFFP", "eFFP", "FFP", "PAS", "EMT", "EXT", "Eref", "CMD",
                               "RH", "CMI", "DD1040")]

# Save
save(camera.climate, file = "results/sites/camera-sites_climate_2023.Rdata")
save(aru.climate, file = "results/sites/aru-sites_climate_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

#################
# Wetland sites # Need to update
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ClimateNAr)

# Load new kgrid
input.data <- read.csv("D:/abmi/ClimateData/beta/wetland-sites.csv")
input.data$Site_Year <- paste0(input.data$ABMISite, "_", input.data$year)

# Format the kgrid to be the appropriate input file
input.data <- input.data[, c("Site_Year", "Site_Year", "LATITUDE",  "LONGITUDE", "RASTERVALU")]
colnames(input.data) <- c("id1", "id2", "lat", "long", "elev")

write.csv(input.data, file = "D:/abmi/ClimateData/beta/wetland-sites_cleaned.csv", row.names = FALSE)

# Call the ClimateNA API to calculate the relevant climate variables
# NOTE: We need to use the double backslash for the executable to read the information correctly
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\beta\\wetland-sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\beta\\wetland-sites_cleaned-climate.csv")

rm(list=ls())
gc()

# Organize

original.data <- read.csv("D:/abmi/ClimateData/beta/wetland-sites.csv")
original.data$Site_Year <- paste0(original.data$ABMISite, "_", original.data$year)

clim <- original.data[, c("Site_Year", "ABMISite", "year", "NRNAME", "NSRNAME", "RASTERVALU")]

# Add public coordinates
public.coord <- read.csv("data/lookup/public-coordiantes.csv")
colnames(public.coord)[1] <- "Site"

clim$Site <- clim$ABMISite
clim$Site <- gsub("W", "", clim$Site)
clim$Site <- gsub("OG-ABMI-", "", clim$Site)
clim$Site <- gsub("-1", "", clim$Site)
clim$Site <- gsub("-2", "", clim$Site)
clim$Site <- gsub("-41", "", clim$Site)
clim$Site <- gsub("-11", "", clim$Site)
clim$Site <- gsub("-21", "", clim$Site)
clim$Site <- gsub("-12", "", clim$Site)
clim$Site <- gsub("B", "", clim$Site)
clim$Site <- as.numeric(clim$Site)

clim <- merge.data.frame(clim, public.coord[, c("Site", "PUBLIC_LATTITUDE", "PUBLIC_LONGITUDE")], by = "Site", all.x = TRUE)
clim <- clim[, c(2,3,4,8,9,5,6,7)]
colnames(clim) <- c("Site_Year", "Site", "Year","Latitude", "Longitude", "NrName", "NsrName", "Elevation")

# Merge with the climate data
climate.data <- read.csv("D:/abmi/ClimateData/beta/wetland-sites_cleaned-climate.csv")
colnames(climate.data)[1] <- "Site_Year"

clim <- merge.data.frame(clim, climate.data[, -c(2,3,4,5)], by = "Site_Year")

# Save
save(clim, file = "results/sites/wetland-climate_2023.Rdata")

# DELETE THE DATA FILES
rm(list=ls())
gc()

##############
# Elly Sites #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the site paths, this file must be maintained in the "beta" folder and never uploaded to GitHub
site.path <- read.csv("beta/site-paths_2023.csv")

# Define the terrestrial path
site.path <- paste0(site.path$Database[7], site.path$Feature[7])

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Select sites
arcpy$Select_analysis(in_features = site.path, 
                      out_feature_class = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret.shp")

arcpy$Select_analysis(in_features = "D:/abmi/ClimateData/beta/Birds_notTopSecret.gdb/elly_birds_not_topsecret_final", 
                      out_feature_class = "D:/abmi/ClimateData/data/base/gis/temp/sites_public.shp")

# Calculate lat and long for the secret sites
arcpy$CalculateGeometryAttributes_management(in_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret.shp", 
                                             geometry_property = "Lat POINT_Y;Long POINT_X", 
                                             coordinate_system='PROJCS["NAD_1983_10TM_AEP_Forest",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-115.0],PARAMETER["Scale_Factor",0.9992],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',
                                             coordinate_format="DD")

# Extract the DEM point values
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/dem/elevation_nasadem_30m.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_dem.shp")

arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_public.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/dem/elevation_nasadem_30m.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_public_dem.shp")

# Extract the pAspen points
arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/ASPEN9706.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_paspen.shp")

arcpy$sa$ExtractValuesToPoints(in_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_public.shp", 
                               in_raster = "D:/abmi/ClimateData/data/base/gis/paspen/ASPEN9706.tif", 
                               out_point_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_public_paspen.shp")

# Find the nearest distance of the sites to public locations
arcpy$GenerateNearTable_analysis(in_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret.shp", 
                                 near_features = "D:/spatial/ABMI.gdb/ABMI_3x7", 
                                 out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_public.csv",
                                 method = "GEODESIC")

# Export the tables
arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_dem.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_dem.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_public_dem.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_public_dem.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_paspen.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_paspen.csv")

arcpy$ExportTable_conversion(in_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_public_paspen.shp", 
                             out_table = "D:/abmi/ClimateData/data/base/gis/temp/sites_public_paspen.csv")

# Standardize the output tables
# Public
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_public_dem.csv")
input.data$Public <- "Public"

# Format to be the appropriate input file
input.data <- input.data[, c("UID", "Public", "latitude", "longitude", "RASTERVALU")]
colnames(input.data) <- c("id1", "id2", "lat", "long", "elev")

# Private
private.input <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_secret_dem.csv")
private.input$Public <- "Private"
private.input <- private.input[, c("UID", "Public", "Lat", "Long", "RASTERVALU")]
colnames(private.input) <- c("id1", "id2", "lat", "long", "elev")

input.data <- rbind(input.data, private.input)

write.csv(input.data, file = "D:/abmi/ClimateData/data/base/gis/temp/sites_cleaned.csv", row.names = FALSE)

# Call the ClimateNA API to calculate the relevant climate variables
# NOTE: We need to use the double backslash for the executable to read the information correctly
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_cleaned.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\base\\gis\\temp\\sites_climate.csv")

# Format the paspen information
paspen.secret <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_secret_paspen.csv")
paspen.secret$Lat <- NA
paspen.secret$Long  <- NA # Remove location

# Add the public coordinates
public.coordinates <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_secret_public.csv")

# Merge the NEAR_ID field with the original file from the 3x7s
public.gis <- read_sf(dsn = "D:/spatial/ABMI.gdb/", layer = "ABMI_3x7")
public.gis <- as.data.frame(public.gis)
colnames(public.gis)[1] <- "NEAR_FID"
public.coordinates <- merge.data.frame(public.coordinates, 
                               public.gis[, c("NEAR_FID", "ABMI")], by = "NEAR_FID")
colnames(paspen.secret)[1] <- "IN_FID"
paspen.secret <- merge.data.frame(paspen.secret, public.coordinates, by = "IN_FID")

public.coordinates <- read.csv("data/lookup/public-coordiantes.csv")
colnames(public.coordinates)[1] <- "ABMI"
paspen.secret <- merge.data.frame(paspen.secret, public.coordinates, by = "ABMI")

paspen.secret <- data.frame(UID = paspen.secret$UID,
                            Public = FALSE,
                            lat = paspen.secret$PUBLIC_LATTITUDE,
                            long = paspen.secret$PUBLIC_LONGITUDE,
                            paspen = paspen.secret$RASTERVALU)

paspen.public <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_public_paspen.csv")
paspen.public <- data.frame(UID = paspen.public$UID,
                            Public = TRUE,
                            lat = paspen.public$latitude,
                            long = paspen.public$longitude,
                            paspen = paspen.public$RASTERVALU)

input.paspen <- rbind(paspen.public, paspen.secret)

# Stitch together the paspen and climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_climate.csv")
climate.data <- climate.data[, !(colnames(climate.data) %in% c("id2", "Latitude", "Longitude",
                                                               "Elevation", "MAR"))]
colnames(climate.data)[1] <- "UID"

climate.data <- merge.data.frame(input.paspen, climate.data, by = "UID")

# Save
save(climate.data, file = "results/sites/bird-point-climate_2024.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

#
# Title: Quadrant climate summaries
# Created: December 4th, 2023
# Last Updated: March 19th, 2024
# Author: Brandon Allen
# Objectives: Extraction of the climate data from the Climate NA software (Quadrant sites summaries)
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
library(terra)

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

# Add the UTM Easting and Northing information
utm10 <- st_as_sf(public.coordinates, coords=c("Longitude", "Latitude"), crs=4326, remove=FALSE)
utm10 <- st_coordinates(st_transform(utm10, crs = 3400))
public.coordinates$Easting <- utm10[,1]
public.coordinates$Northing <- utm10[,2]

site.id <- merge.data.frame(public.coordinates, site.id[, c("Site", "id1", "pAspen")], by = "Site")
site.id <- site.id[, -1]
site.id <- merge.data.frame(site.id, site.id.na[, c("id1", "pAspen_NA")], by = "id1")

# Stitch together the paspen and yearly climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_yearly.csv")
climate.data <- climate.data[, !(colnames(climate.data) %in% c("id2", "Latitude", "Longitude",
                                                               "Elevation", "MAR"))]

climate.qa <- merge.data.frame(site.id, climate.data, by = "id1")

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

climate.qa <- merge.data.frame(climate.qa, derived.climate, by = "id1")

# Standardize the names
colnames(climate.qa)[1] <- "Site_Year_Quadrant"
rownames(climate.qa) <- climate.qa$Site_Year_Quadrant

# The slight mismatch in the number of sites is due to the Alpac sites in saskatchewan (can ignore)

#################
# Hydrotemporal #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the spatial boundaries and the htv raster
zone.shape <- terra::vect("data/base/gis/temp/sites.shp")
htv.raster <- terra::rast("data/base/gis/hydrotemporal/HTV_v2.tif")

zonal.stats <- terra::zonal(x = htv.raster, z = zone.shape,
                            fun = mean, na.rm = TRUE)

zonal.stats$Site_Year_Quadrant <- zone.shape$UID 
colnames(zonal.stats)[1] <- "HTV"

# Merge with the climate data
climate.qa <- merge.data.frame(climate.qa, zonal.stats, by = "Site_Year_Quadrant")

# Save
save(climate.qa, file = "results/sites/abmi-terrestrial-quadrants-climate_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

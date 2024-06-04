#
# Title: Bird point climate summaries
# Created: December 4th, 2023
# Last Updated: June 4th, 2024
# Author: Brandon Allen
# Objectives: Extraction of the climate data from the Climate NA software (ABMI and public bird points)
# Keywords: Notes, Initialization, Climate
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

# Create 150m radius buffer for HTV calculation at the sites
arcpy$PairwiseBuffer_analysis(in_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret.shp", 
                              out_feature_class = "D:/abmi/ClimateData/data/base/gis/temp/sites_secret_buffer.shp", 
                              buffer_distance_or_field = "150 Meters")

arcpy$PairwiseBuffer_analysis(in_features = "D:/abmi/ClimateData/data/base/gis/temp/sites_public.shp", 
                              out_feature_class = "D:/abmi/ClimateData/data/base/gis/temp/sites_public_buffer.shp", 
                              buffer_distance_or_field = "150 Meters")

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

# Add the UTM Easting and Northing information
utm10 <- st_as_sf(input.paspen, coords=c("long", "lat"), crs=4326, remove=FALSE)
utm10 <- st_coordinates(st_transform(utm10, crs = 3400))
input.paspen$Easting <- utm10[,1]
input.paspen$Northing <- utm10[,2]

# Format the yearly climate data
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_yearly.csv")
colnames(climate.data)[1] <- "UID"
site.data <- merge.data.frame(input.paspen, climate.data, by = "UID")

# Stitch together the yearly and monthly climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_monthly.csv")
tmax <- as.matrix(climate.data[, 6:17])
tmin <- as.matrix(climate.data[, 18:29])
precip <- as.matrix(climate.data[, 42:53])

derived.climate <- as.data.frame(biovars(prec = precip, 
                                         tmin = tmin, 
                                         tmax = tmax))
derived.climate$UID <- climate.data$id1
derived.climate <- derived.climate[, c("UID", "bio9", "bio15")]

climate.data <- merge.data.frame(site.data, derived.climate, by = "UID")

# Save
save(climate.data, file = "results/sites/bird-point-climate_2024.Rdata")

#################
# Hydrotemporal #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the spatial boundaries and the htv raster
zone.shape <- terra::vect("data/base/gis/temp/sites_public_buffer.shp")
zone.private.shape <- terra::vect("data/base/gis/temp/sites_secret_buffer.shp")
htv.raster <- terra::rast("data/base/gis/hydrotemporal/HTV_v2.tif")

# Public
zonal.stats <- terra::zonal(x = htv.raster, z = zone.shape,
                            fun = mean, na.rm = TRUE)
colnames(zonal.stats)[1] <- "HTV"
zonal.stats$HTV[is.nan(zonal.stats$HTV)] <- NA
zonal.stats$UID <- zone.shape$UID

# Private
zonal.stats.private <- terra::zonal(x = htv.raster, z = zone.private.shape,
                            fun = mean, na.rm = TRUE)
colnames(zonal.stats.private)[1] <- "HTV"
zonal.stats.private$HTV[is.nan(zonal.stats.private$HTV)] <- NA
zonal.stats.private$UID <- zone.private.shape$UID

zonal.stats <- rbind(zonal.stats, zonal.stats.private)

# Merge with the climate data
climate.data <- merge.data.frame(climate.data, zonal.stats, by = "UID")

# Save
save(climate.data, file = "results/sites/bird-point-climate_2024.Rdata")

######################
# Natural subregions #
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the spatial boundaries and the htv raster
public.shape <- terra::vect("data/base/gis/temp/sites_public.shp")
private.shape <- terra::vect("data/base/gis/temp/sites_secret.shp")
natural.regions <- terra::vect("D:/spatial/NSR/Natural_Regions_Subregions_of_Alberta.shp")

public.nsr <- terra::intersect(x = public.shape, y = natural.regions)
private.nsr <- terra::intersect(x = private.shape, y = natural.regions)

public.nsr <- data.frame(UID = public.nsr$UID,
                         NrName = public.nsr$NRNAME,
                         NsrName = public.nsr$NSRNAME)

private.nsr <- data.frame(UID = private.nsr$UID,
                          NrName = private.nsr$NRNAME,
                          NsrName = private.nsr$NSRNAME)

nsr.data <- rbind(public.nsr, private.nsr)

# Merge with the climate data
climate.data <- merge.data.frame(nsr.data, climate.data, by = "UID")

# Save
save(climate.data, file = "results/sites/bird-point-climate_2024.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

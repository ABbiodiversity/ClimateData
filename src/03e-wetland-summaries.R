#
# Title: Wetland climate summaries
# Created: December 4th, 2023
# Last Updated: March 19th, 2024
# Author: Brandon Allen
# Objectives: Extraction of the climate data from the Climate NA software (Wetland summaries)
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

# Load the site paths, this file must be maintained in the 
# "beta" folder and never uploaded to GitHub
site.path <- read.csv("beta/site-paths_2023.csv")

# Define the site path
site.path <- paste0(site.path$Database[6], site.path$Feature[6])

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
input.data$Site_Year <- paste0(input.data$ABMISite, "_", input.data$year)

# Format the kgrid to be the appropriate input file
input.data <- input.data[, c("Site_Year", "Site_Year", "LATITUDE",  "LONGITUDE", "RASTERVALU")]
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

# Take site locations and combine with the extracted public location
input.data <- read_sf(dsn = "D:/abmi/ClimateData/data/base/gis/temp/sites.shp")
input.data <- as.data.frame(input.data)
input.data$IN_FID <- as.numeric(rownames(input.data)) - 1
input.data$Site_Year <- paste0(input.data$ABMISite, "_", input.data$year)

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

# Add the UTM Easting and Northing information
utm10 <- st_as_sf(public.data, coords=c("PUBLIC_LONGITUDE", "PUBLIC_LATTITUDE"), crs=4326, remove=FALSE)
utm10 <- st_coordinates(st_transform(utm10, crs = 3400))
public.data$Easting <- utm10[,1]
public.data$Northing <- utm10[,2]

# Format the yearly climate data
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_yearly.csv")
colnames(climate.data)[1] <- "Site_Year"
site.data <- merge.data.frame(climate.data, public.data, by = "Site_Year")

# Stitch together the yearly and monthly climate information
climate.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_monthly.csv")
tmax <- as.matrix(climate.data[, 6:17])
tmin <- as.matrix(climate.data[, 18:29])
precip <- as.matrix(climate.data[, 42:53])

derived.climate <- as.data.frame(biovars(prec = precip, 
                                         tmin = tmin, 
                                         tmax = tmax))
derived.climate$Site_Year <- climate.data$id1
derived.climate <- derived.climate[, c("Site_Year", "bio9", "bio15")]

site.data <- merge.data.frame(site.data, derived.climate, by = "Site_Year")

# Format the paspen information
input.data <- read.csv("D:/abmi/ClimateData/data/base/gis/temp/sites_paspen.csv")

# Determine which columns identify year information
input.data <- data.frame(Site_Year = paste0(input.data$ABMISite, "_", input.data$year),
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

# Format the columns
wetland.climate <- site.data[, c("Site_Year", "Latitude", "Longitude", "Elevation", "NrName",
                                     "NsrName", "Easting", "Northing", "pAspen", "MAT", "MWMT", "MCMT", "TD", "MAP",
                                     "MSP", "AHM", "SHM", "DD_0", "DD5", "DD_18", "DD18", "NFFD",
                                     "bFFP", "eFFP", "FFP", "PAS", "EMT", "EXT", "Eref", "CMD",
                                     "RH", "CMI", "DD1040", "bio9", "bio15")]

# Save
save(wetland.climate, file = "results/sites/abmi-wetland-climate_2023.Rdata")

# Delete everything in the temporary folder
files.remove <- list.files("D:/abmi/ClimateData/data/base/gis/temp/", full.names = TRUE)
file.remove(files.remove)

rm(list=ls())
gc()

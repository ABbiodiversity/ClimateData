#
# Title: Extraction of the climate data from the Climate NA software
# Created: July 27, 2023
# Last Updated: March 20, 2024
# Author: Brandon Allen
# Objectives: Create a new kgrid based on the Climate NA data
# Keywords: Notes, Kgrid Standardization, Climate Extraction, Hydrotemporal, Visualization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# The Climate NA software was download on July 25th, 2023 (Version 7.4)
# Kgrid was last updated on July 27th, 2023
# Elevation data was calculated from the following data (mean value with cell):
# https://developers.google.com/earth-engine/datasets/catalog/JAXA_ALOS_AW3D30_V3_2. 
# ALOS World 3D - 30m (AW3D30) is a global digital surface model (DSM) dataset with a horizontal resolution 
# of approximately 30 meters (1 arcsec mesh). The dataset is based on the DSM dataset (5-meter mesh version) 
# of the World 3D Topographic Data. More details are available in the dataset documentation.
#
#########################
# Kgrid Standardization # 
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(sf)

# Load 1km2 data
load("data/base/mapping/kgrid_mapping.Rdata")
kgrid.new <- kgrid
load("data/base/kgrid/kgrid_table_km.Rdata")

# Add the column denoting the two reporting regions (Forested and Prairie)
kgrid$ReportingBoundaries <- ifelse(kgrid$NRNAME %in% c("Grassland", "Parkland") |
                                      (kgrid$NSRNAME == "Dry Mixedwood" & kgrid$POINT_Y < 57), "Prairie", "Forested")
kgrid$ReportingBoundaries[kgrid$NRNAME == "Rocky Mountain"] <- "Forested"
kgrid$ReportingBoundaries[kgrid$NSRNAME == c("Montane") & kgrid$POINT_X > -114] <- "Prairie"

# Subset the old kgrid to the relevant columns
kgrid <- kgrid[, c("Row", "Col", "Row_Col", "Row10", "Col10", "Row10_Col10", "POINT_X", "POINT_Y",
                   "NRNAME", "NSRNAME", "LUF_NAME", "ReportingBoundaries", "Area_km2", "pAspen", "pWater")]

# Clean up the second kgrid and merge
colnames(kgrid.new)[1] <- "Row_Col"
kgrid.new <- kgrid.new[, c("Row_Col", "x", "y", "wS", "wN")]
kgrid <- merge.data.frame(kgrid, kgrid.new, by = "Row_Col")

# Add the new elevation data
dem.data <- read_sf("data/base/gis/dem/kgrid_dem.shp")
dem.data <- data.frame(Row_Col = dem.data$GRID_LABEL,
                       Elevation = dem.data$RASTERVALU)

# There are a few points where the zonal statistic calculation failed (small LinkID polygons)
# Therefore, we manually corrected them to the value of LinkID centroid
dem.data[dem.data$Row_Col == "1143_355", "Elevation"] <- 2443.24
dem.data[dem.data$Row_Col == "1231_398", "Elevation"] <- 1888.308
dem.data[dem.data$Row_Col == "3_91", "Elevation"] <- 591.34
dem.data[dem.data$Row_Col == "4_554", "Elevation"] <- 259.68
dem.data[dem.data$Row_Col == "9_448", "Elevation"] <- 233.72

table(dem.data$Elevation == 0)

kgrid <- merge.data.frame(kgrid, dem.data, by = "Row_Col")

# Add the UTM Easting and Northing information
utm10 <- st_as_sf(kgrid, coords=c("POINT_X", "POINT_Y"), crs=4326, remove=FALSE)
utm10 <- st_coordinates(st_transform(utm10, crs = 3400))
kgrid$Easting <- utm10[,1]
kgrid$Northing <- utm10[,2]

# Organize columns in the appropriate order and rename
kgrid <- kgrid[, c("Row", "Col", "Row_Col", "Row10", "Col10", "Row10_Col10", "POINT_X", "POINT_Y", 
                   "Easting", "Northing", "x", "y", "NRNAME", "NSRNAME", "LUF_NAME", 
                   "ReportingBoundaries", "Area_km2", "Elevation", "pAspen", "pWater", "wS", "wN")]
colnames(kgrid) <- c("Row", "Col", "LinkID", "Row10", "Col10", "LinkID10", "Longitude", "Latitude", 
                     "Easting", "Northing", "X", "Y", "NrName", "NsrName", "LufName", 
                     "ReportingBoundaries", "AreaKm2", "Elevation", "pAspen", "pWater", "wS", "wN")

rm(kgrid.new, dem.data)
comment(kgrid) <- c("Kgrid version 2.0.", 
                    "Created on March 20th, 2024.",
                    "Metadata can be found on the ABMI ClimateData GitHub page.",
                    "Metadata can also be found ABMI ClimateData technical document on the Shared SC Google Drive.")

save(kgrid, file = "results/kgrid/kgrid-no-climate.Rdata")

######################
# Climate Extraction # This is how you can process climate data for whatever information you find useful.
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ClimateNAr)
library(dismo)
library(terra)

# Load new kgrid
load("results/kgrid/kgrid-no-climate.Rdata")

# Format the kgrid to be the appropriate input file
input.data <- kgrid[, c("LinkID", "LinkID", "Latitude", "Longitude", "Elevation")]
colnames(input.data) <- c("id1", "id2", "lat", "long", "elev")

write.csv(input.data, file = "data/base/kgrid/climate-input.csv", row.names = FALSE)

# Call the ClimateNA API to calculate the relevant climate variables
# NOTE: We need to use the double backslash for the executable to read the information correctly
ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "Y", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\kgrid\\climate-input.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\processed\\kgrid_normals_1991_2020_yearly.csv")

ClimateNA_cmdLine(exe = "ClimateNA_v7.40.exe", 
                  wkDir = "D:\\climateNA\\ClimateNA\\", 
                  period = "Normal_1991_2020.nrm", 
                  MSY = "M", 
                  inputFile = "D:\\abmi\\ClimateData\\data\\base\\kgrid\\climate-input.csv", 
                  outputFile = "D:\\abmi\\ClimateData\\data\\processed\\kgrid_normals_1991_2020_monthly.csv")

# Load the csv and merge with the kgrid so we have a single file
input.data <- read.csv("D:\\abmi\\ClimateData\\data\\processed\\kgrid_normals_1991_2020_yearly.csv")
colnames(input.data)[1] <- "LinkID"
input.data <- input.data[, -c(2:5)]

kgrid <- merge.data.frame(kgrid, input.data, by = "LinkID")
kgrid <- kgrid[ , colnames(kgrid) != "MAR"] # MAR is a voided attribute

# Add the monthly variables
input.data <- read.csv("D:\\abmi\\ClimateData\\data\\processed\\kgrid_normals_1991_2020_monthly.csv")
rownames(input.data) <- input.data$id1

tmax <- as.matrix(input.data[, 6:17])
tmin <- as.matrix(input.data[, 18:29])
precip <- as.matrix(input.data[, 42:53])

derived.climate <- as.data.frame(biovars(prec = precip, 
                                         tmin = tmin, 
                                         tmax = tmax))
derived.climate$LinkID <- input.data$id1
derived.climate <- derived.climate[, c("LinkID", "bio9", "bio15")]

kgrid <- merge.data.frame(kgrid, derived.climate, by = "LinkID")

comment(kgrid) <- c("Kgrid version 2.0.", 
                    "Created on March 20th, 2024.",
                    "Metadata can be found on the ABMI ClimateData GitHub page.",
                    "Metadata can also be found ABMI ClimateData technical document on the Shared SC Google Drive.")

save(kgrid, file = "results/kgrid/kgrid_2.0.Rdata")

#################
# Hydrotemporal #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the spatial boundaries and the htv raster
zone.shape <- terra::vect("D:/spatial/GRID1SQKM_AB2020.gdb/", layer = "Grid_1KM_revAB2020")
htv.raster <- terra::rast("data/base/gis/hydrotemporal/HTV_v2.tif")

zonal.stats <- terra::zonal(x = htv.raster, z = zone.shape,
                            fun = mean, na.rm = TRUE)

zonal.stats$LinkID <- zone.shape$GRID_LABEL 
colnames(zonal.stats)[1] <- "HTV"

# Merge with the climate data
kgrid <- merge.data.frame(kgrid, zonal.stats, by = "LinkID")

comment(kgrid) <- c("Kgrid version 2.0.", 
                    "Created on March 20th, 2024.",
                    "Metadata can be found on the ABMI ClimateData GitHub page.",
                    "Metadata can also be found ABMI ClimateData technical document on the Shared SC Google Drive.")

save(kgrid, file = "results/kgrid/kgrid_2.0.Rdata")

rm(list=ls())
gc()

#################
# Visualization # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(raster)
library(sf)

# Load data
load("results/kgrid/kgrid_2.0.Rdata")
load("data/base/mapping/provincial-boundary.Rdata")

#
# Creation of figures for the new climate data
#

for (climate in colnames(kgrid)[c(16, 21:44)]) {
  
  # Define the minimum and maximum values
  min.value <- min(kgrid[, climate])
  max.value <- max(kgrid[, climate])
  
  # Define title
  if (climate == "Elevation") {
    
    fig.title <- "Elevation"
    
  } else {
    
    fig.title <- paste0("1991-2020 Climate Normals ", climate)
    
  }

  # Figure creation
  kgrid$Current <- kgrid[, climate]
  climate.figure <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = kgrid , aes(x = X, y = Y, fill = Current)) +
    scale_fill_gradientn(name = climate, colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "none") +
    ggtitle(fig.title) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          panel.grid.major.y = element_blank(),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16),
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(size=22),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.15, 0.15))
  
  ggsave(filename = paste0("results/figures/version-2/", climate, ".jpeg"), 
         plot = climate.figure,
         height = 900,
         width = 600, 
         dpi = 72,
         quality = 75,
         units = "px")
  
  print(climate)
  
}

rm(list=ls())
gc()

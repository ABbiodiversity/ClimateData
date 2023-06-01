#
# Title: Comparison of the AdaptWest and ABMI climate data
# Created: June 1st, 2023
# Last Updated: June 1st, 2023
# Author: Brandon Allen
# Objectives: Compare the current ABMI climate data to the latest AdaptWest data
# Keywords: Notes, Climate Extraction, Visualization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#
######################
# Climate Extraction # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(raster)
library(sf)

# Load 1km2 data
load("data/base/mapping/kgrid_mapping.Rdata")

# Add redundant X and Y for mapping
kgrid$X <- kgrid$x
kgrid$Y <- kgrid$y

# Convert the grid locations to a spatial object
kgrid <- st_as_sf(x = kgrid[, c(1,2,3,5,6,7,8,17,18)], 
                  coords = c("X", "Y"),
                  crs = "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

# Extract the relevant climate data
for(years in c("1961_1990", "1971_2000", "1981_2010", "1991_2020")) {
  
  # Define raster list
  raster.list <- list.files(paste0("data/base/climate/Normal_", years, "/"), 
                            full.names = TRUE)
  
  # Remove copies with the pyramid information (i.e., aux and over)
  raster.list <- raster.list[!(raster.list %in% raster.list[grep(".aux", raster.list)])]
  raster.list <- raster.list[!(raster.list %in% raster.list[grep(".ovr", raster.list)])]
  
  for (climate in c("MAT", "MWMT", "MCMT", "TD", "MAP", "MSP", "AHM", "SHM",
                    "DD_0", "DD5", "DD_18", "DD18", "NFFD", "FFP", "bFFP",
                    "eFFP", "PAS", "EMT", "EXT", "Eref", "CMD", "RH",
                    "CMI", "DD1040", "Tave_wt", "Tave_sp", "Tave_sm", "Tave_at", 
                    "PPT_wt", "PPT_sp", "PPT_sm", "PPT_at")) {

    # Load raster
    raster.in <- raster(x = raster.list[grep(paste0("_", climate, ".tif"), raster.list)])
    
    # Convert to the proper CRS
    kgrid <- st_transform(kgrid, crs = crs(raster.in))

    # Extract climate data
    kgrid[, climate] <- raster::extract(x = raster.in, 
                                        y = kgrid)
    
    
    print(climate)
    
  }
  
  save(kgrid, file = paste0("data/processed/kgrid_normals_", years, ".Rdata"))
  print(years)
  
}

# Clear memory
rm(list=ls())
gc()


#################
# Visualization # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(raster)
library(sf)

# Load data
load("data/base/kgrid/kgrid_table_km.Rdata")
kgrid.ABMI <- kgrid

load("data/processed/kgrid_normals_1961_1990.Rdata")
kgrid.1961.1990 <- kgrid

load("data/processed/kgrid_normals_1971_2000.Rdata")
kgrid.1971.2000 <- kgrid

load("data/processed/kgrid_normals_1981_2010.Rdata")
kgrid.1981.2010 <- kgrid

load("data/processed/kgrid_normals_1991_2020.Rdata")
kgrid.1991.2020 <- kgrid

load("data/base/mapping/provincial-boundary.Rdata")

rm(kgrid)

# Create table to store the correlation plot
climate.correlation <- data.frame(Variable = c("MAP", "MAT", "MWMT", "MCMT", "FFP", "AHM"),
                                  ABMI_AdaptWest61 = NA,
                                  ABMI_AdaptWest91 = NA,
                                  AdaptWest61_AdaptWest91 = NA)
rownames(climate.correlation) <- climate.correlation$Variable

# Confirm the rows are aligned
kgrid.ABMI <- kgrid.ABMI[kgrid.1961.1990$LinkID, ]

for(climate in c("MAP", "MAT", "MWMT", "MCMT", "FFP", "AHM")) {
  
  climate.correlation[climate, "ABMI_AdaptWest61"] <- cor(kgrid.ABMI[, climate], as.data.frame(kgrid.1961.1990)[, climate])
  climate.correlation[climate, "ABMI_AdaptWest91"] <- cor(kgrid.ABMI[, climate], as.data.frame(kgrid.1991.2020)[, climate])
  climate.correlation[climate, "AdaptWest61_AdaptWest91"] <- cor(as.data.frame(kgrid.1961.1990)[, climate], as.data.frame(kgrid.1991.2020)[, climate])
  
}

# Convert to ggplot friendly format
climate.correlation <- data.frame(Variable = c(climate.correlation$Variable, 
                                               climate.correlation$Variable, 
                                               climate.correlation$Variable),
                                  Comparison = c(rep("ABMI vs \n AdaptWest 1961-1990", 6),
                                                 rep("ABMI vs \n AdaptWest 1991-2020", 6),
                                                 rep("AdaptWest 1961-1990 \n vs AdaptWest 1991-2020", 6)),
                                  Correlation = c(climate.correlation$ABMI_AdaptWest61, 
                                            climate.correlation$ABMI_AdaptWest91, 
                                            climate.correlation$AdaptWest61_AdaptWest91))

cor.plot <- ggplot(data = climate.correlation) +
  geom_tile(aes(x = Comparison, y = Variable, fill = Correlation)) +
  scale_fill_gradientn(name = "Correlation", 
                       colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), 
                       limits = c(0.9, 1), guide = "colourbar") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(filename = paste0("results/figures/correlation.jpeg"), 
       plot = cor.plot,
       height = 600,
       width = 600, 
       dpi = 100,
       quality = 100,
       units = "px")

#
# Comparison of new climate data (2x2 plots)
#

for (climate in colnames(kgrid.1961.1990)[9:40]) {
  
  # Define max and min values so it is standardized between years
  min.value <- min(c(as.data.frame(kgrid.1961.1990)[, climate], as.data.frame(kgrid.1971.2000)[, climate],
                     as.data.frame(kgrid.1981.2010)[, climate], as.data.frame(kgrid.1991.2020)[, climate]))
  max.value <- max(c(as.data.frame(kgrid.1961.1990)[, climate], as.data.frame(kgrid.1971.2000)[, climate],
                     as.data.frame(kgrid.1981.2010)[, climate], as.data.frame(kgrid.1991.2020)[, climate]))
  
  # 1961-1990
  kgrid.1961.1990$Current <- as.data.frame(kgrid.1961.1990)[, climate]
  map.61.91 <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = kgrid.1961.1990 , aes(x = x, y = y, fill = Current)) +
    scale_fill_gradientn(name = climate, colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "none") +
    ggtitle("1961-1990 Climate Normals") +
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
  
  # 1971-2000
  kgrid.1971.2000$Current <- as.data.frame(kgrid.1971.2000)[, climate]
  map.71.00 <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = kgrid.1971.2000 , aes(x = x, y = y, fill = Current)) +
    scale_fill_gradientn(name = climate, colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "none") +
    ggtitle("1971-2000 Climate Normals") +
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
  
  # 1981-2010
  kgrid.1981.2010$Current <- as.data.frame(kgrid.1981.2010)[, climate]
  map.81.10 <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = kgrid.1981.2010 , aes(x = x, y = y, fill = Current)) +
    scale_fill_gradientn(name = climate, colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "none") +
    ggtitle("1981-2010 Climate Normals") +
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
  
  # 1991-2020
  kgrid.1991.2020$Current <- as.data.frame(kgrid.1991.2020)[, climate]
  map.91.20 <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = kgrid.1991.2020 , aes(x = x, y = y, fill = Current)) +
    scale_fill_gradientn(name = climate, colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "none") +
    ggtitle("1991-2020 Climate Normals") +
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
  
  ggsave(filename = paste0("results/figures/", climate, ".jpeg"), 
         plot = ggarrange(map.61.91, map.71.00, 
                          map.81.10, map.91.20, 
                          ncol = 2, nrow = 2),
         height = 1800,
         width = 1200, 
         dpi = 72,
         quality = 75,
         units = "px")
  
  print(climate)
  
}

rm(list=ls())
gc()
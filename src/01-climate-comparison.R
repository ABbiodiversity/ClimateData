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

ggsave(filename = paste0("results/figures/corrleation.jpeg"), 
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



# Climate visualization



#
# Correlation plots
#





#
# OLD !
#



save(kgrid, file = "data/processed/climate-data.Rdata")

# Visualize the climate data
for (climate in c("MAT", "MWMT", "MCMT", "MAP",
                  "MSP", "AHM", "SHM", "FFP",
                  "Eref", "RH", "CMI")) {
  
  # Define maximum abundance for the plots
  temp.data <- as.data.frame(kgrid)
  
  max.value <- max(temp.data[, climate], na.rm = TRUE)
  min.value <- min(temp.data[, climate], na.rm = TRUE)
  temp.data$Current <- temp.data[, climate]
  
  # Current Abundance
  cur.map <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = temp.data , aes(x = x, y = y, fill = Current)) +
    scale_fill_gradientn(name = climate, colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "none") +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          title = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16),
          legend.key.size = unit(1, "cm"), 
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.15, 0.15))
  
  ggsave(filename = paste0("results/figures/climate/", climate, ".jpeg"), 
         plot = cur.map,
         height = 900,
         width = 600, 
         dpi = 72,
         quality = 100,
         units = "px")

  
}

# Assess if there are climate variables that are redundant
cor.matrix <- cor(as.data.frame(kgrid)[, -c(1:6)])
corrplot(cor.matrix)

# A lot of the climate variables are correlated with each other
# We are going to use a combination of CMI, AHM, SHM, and FFP. Find the best single model

species.models <- list()

for (species in colnames(species.wide)[-c(1:7)]) {
  
  # Isolate the species of interest (presence only)
  species.data <- species.wide[species.wide[, species] > 0, ]
  
  # If fewer than 20 detections, skip
  if(nrow(species.data) < 20) {next}
  
  # Project the species data and convert to the same format as the kgrid
  data.projected <- st_as_sf(x = species.data, 
                             coords = c("Longitude", "Latitude"),
                             crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  data.projected <- st_transform(data.projected, crs = "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  
  # Create climate raster and extract
  for (climate in c("MAT", "MWMT", "MCMT", "MAP",
                    "MSP", "AHM", "SHM", "FFP",
                    "Eref", "RH", "CMI")) {
    
    temp.raster <- rasterFromXYZ(xyz = as.data.frame(kgrid)[, c("x", "y", climate)])
    crs(temp.raster) <- "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    species.data[climate] <- extract(x = temp.raster, 
                                     y = data.projected)
    
    
  }
  
  # If there are duplicated space/climate, remove
  species.data <- species.data[!duplicated(species.data[, c("MAT", "MWMT", "MCMT", "MAP",
                              "MSP", "AHM", "SHM", "FFP",
                              "Eref", "RH", "CMI")]), ]
  species.data <- as.data.frame(species.data)
  species.data$Species <- 1
  species.data <- species.data[, c("Species", "MAT", "MWMT", "MCMT", "MAP",
                                   "MSP", "AHM", "SHM", "FFP",
                                   "Eref", "RH", "CMI")]
  
  # Extract 10,000 random points
  background.points <- kgrid[sample(x = 1:nrow(kgrid), 
                                    size = 10000,
                                    replace = FALSE), ]
  
  background.points <- as.data.frame(background.points)
  background.points$Species <- 0
  background.points <- background.points[, c("Species", "MAT", "MWMT", "MCMT", "MAP",
                                             "MSP", "AHM", "SHM", "FFP",
                                             "Eref", "RH", "CMI")]
  
  # If there are duplicated values, remove the pseudo-absence
  species.data <- rbind(species.data, background.points)
  species.data <- species.data[!duplicated(species.data), ]
  
  # Create the appropriate climate models
  climate.models <- list()
  climate.models[[1]] <- try(glm(Species ~ CMI, data = species.data, family = "binomial")) 
  climate.models[[2]] <-try(update(climate.models[[1]], .~.+ AHM))
  climate.models[[3]] <-try(update(climate.models[[1]], .~.+ SHM))
  climate.models[[4]] <-try(update(climate.models[[1]], .~.+ FFP))
  climate.models[[5]] <-try(update(climate.models[[1]], .~.+ AHM + SHM))
  climate.models[[6]] <-try(update(climate.models[[1]], .~.+ AHM + FFP))
  climate.models[[7]] <-try(update(climate.models[[1]], .~.+ SHM + FFP))
  climate.models[[8]] <-try(update(climate.models[[1]], .~.+ AHM + SHM + FFP))
  
  # Pick best climate model
  nModels.sc<-length(climate.models)
  # BIC calculation to select best covariate set Uses BIC for more conservative variable set
  bic.sc <- rep(999999999, nModels.sc)
  for (i in 1:(nModels.sc)) {
    
    if (!is.null(climate.models[[i]]) & class(climate.models[[i]])[1] != "try-error") {
      bic.sc[i] <- BIC(climate.models[[i]])
    }
    
  }
  
  bic.delta.sc <- bic.sc - min(bic.sc)
  bic.exp.sc <- exp(-1 / 2 * bic.delta.sc)
  bic.wt.sc <- bic.exp.sc / sum(bic.exp.sc)
  climate.model <- climate.models[[which.max(bic.wt.sc)]]
  
  species.models[[species]] <- list(ClimateModel = climate.model,
                                    AUC = auc(species.data$Species, predict(climate.model)))

  
  # Visualize
  # Define maximum abundance for the plots
  temp.data <- as.data.frame(kgrid)
  temp.data$Species <- predict(climate.model, newdata = temp.data, type = "response")
  
  max.value <- max(temp.data$Species, na.rm = TRUE)
  min.value <- 0
  
  # Current Abundance
  cur.map <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = temp.data , aes(x = x, y = y, fill = Species)) +
    scale_fill_gradientn(name = paste0("Relative\nAbundance"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(min.value, max.value), guide = "none") +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          title = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16),
          legend.key.size = unit(1, "cm"), 
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.15, 0.15))
  
  ggsave(filename = paste0("results/figures/species/", species, "/climate-model.jpeg"), 
         plot = cur.map,
         height = 900,
         width = 600, 
         dpi = 72,
         quality = 100,
         units = "px")
  
  print(species)
  
}

###################
# Standardization # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(corrplot)
library(ggplot2)
require(ggnewscale)
library(MASS)
library(MetBrewer)
library(pROC)

# Load data
load("data/processed/landcover/veg-hf_800m_wide_simplified.Rdata")
load("data/processed/species/species-wide-form.Rdata")
load("data/base/sites/anbc-climate-summaries.Rdata")
site.lookup <- read.csv("data/lookup/site-lookup-all-surveys.csv")
species.lookup <- read.csv("data/lookup/species-lookup.csv")

# Filter for species of interest
species.lookup <- species.lookup[species.lookup$Species %in% colnames(species.wide)[7:77], ]

# Filter for the projects of interest
site.lookup <- site.lookup[!is.na(site.lookup$SurveyDays), ] # Remove sites with missing survey effort
site.lookup <- site.lookup[site.lookup$Project %in% c("AEP", "ANBC", "ACA"), ] # Keep only the data with surveys
#site.info <- site.info[site.info$NRNAME != "Grassland", ]
#site.lookup <- site.lookup[site.lookup$UID %in% site.info$UID, ]

species.wide$UID <- paste(species.wide$Project, species.wide$SiteID, species.wide$Year, sep = "_")
rownames(species.wide) <- species.wide$UID
species.wide <- species.wide[site.lookup$UID, ]
veg.data <- veg.data[site.lookup$UID, ]
colnames(veg.data)[1] <- "UID"
rownames(climate.data) <- climate.data$UID
climate.data <- climate.data[site.lookup$UID,]

model.data <- merge.data.frame(climate.data, site.lookup[, -c(2:6)], by = "UID") # Merge site and climate
model.data <- merge.data.frame(model.data, veg.data, by = "UID") # Add vegetation
model.data <- merge.data.frame(model.data, species.wide[, -c(1:6)], by = "UID") # Add species abundance
model.data$SurveyDays2 <- model.data$SurveyDays * model.data$SurveyDays

# Determine which species have sufficient data to model
species.list <- colSums(ifelse(species.wide[, -c(1:6)] > 0, 1, 0))
species.list <- species.list[species.list > 20]
species.list <- names(species.list)[!(names(species.list) %in% "UID")]

##################
# Species Models # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Assess redundant variables
corr <- cor(model.data[, c(5:13, 15:29)])
corrplot(corr)

# We are not going to model hard linear features (bees aren't actively using it)

# Define list for storing results
species.results.poisson <- list()
species.results.nb <- list()

# Bombus
for (species in species.list[3:20]) {
  
  # Define the singular model
  model.data$Species <- model.data[, species]
  
  
  species.results.poisson[[species]] <- glm(Species ~ DecidMixed + PineSpruce + TreedBogFenSwamp + Wetland + GrassShrub + 
                                      UrbInd + Crop + Pasture + ForestryYoung + ForestryOld +
                                      SurveyDays, 
                                    family = "poisson",
                                    data = model.data, 
                                    maxit = 250)

  species.results.nb[[species]] <- glm.nb(Species ~ DecidMixed + PineSpruce + TreedBogFenSwamp + Wetland + GrassShrub + 
                                      UrbInd + Crop + Pasture + ForestryYoung + ForestryOld +
                                      SurveyDays, 
                                    data = model.data, 
                                    maxit = 250)
  
  print(cor.test(model.data$Species, 
                 predict(species.results.poisson[[species]], type = "response"), 
                 method = "spearman")$estimate)
  
  print(cor.test(model.data$Species, 
           predict(species.results.nb[[species]], type = "response"), 
           method = "spearman")$estimate)

  
}

# Non bombus
for (species in species.list[c(1,2,21,22)]) {
  
  # Define the singular model
  model.data$Species <- model.data[, species]
  temp.data <- model.data[model.data$Project != "AEP", ]
  
  species.results[[species]] <- glm(Species ~ DecidMixed + PineSpruce + TreedBogFenSwamp + Wetland + GrassShrub + 
                                      UrbInd + Crop + Pasture + ForestryYoung + ForestryOld +
                                      SurveyDays + Project, 
                                    family = "poisson",
                                    data = model.data, 
                                    maxit = 250)
  
  plot(predict(species.results[[species]], type = "response") ~ model.data$Species)
  
}

#################
# Visualization #
#################

# Load the kgrid landcover layer
load("data/processed/landcover/kgrid-processed.Rdata")
load("data/base/mapping/provincial-boundary.Rdata")
load("data/base/mapping/kgrid_mapping.Rdata")

for (species in names(species.results)) {
  
  # Create predictions for the two models
  kgrid.cur[, species] <- predict(species.results[[species]], newdata = cbind(kgrid.cur, data.frame(SurveyDays = 0, SurveyDays2 = 0)), type = "response")
  kgrid.cur[, species] <- ifelse(kgrid.cur[, species] >= quantile(kgrid.cur[, species], 0.99), 
                                  quantile(kgrid.cur[, species], 0.99), 
                                  kgrid.cur[, species])
  
}

# Merge the two kgrid version for visualization
kgrid.pred <- merge.data.frame(kgrid.cur, kgrid[, c(1,5,6)], by = "LinkID")

for (species in names(species.results)) {
  
  # Plot species model (scaled between 0-1)
  kgrid.pred$Abundance <- kgrid.pred[, species] / max(kgrid.pred[, species])
  
  species.plot <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(rep("grey", 6), 0.2)) +
    scale_color_manual(values =  alpha(rep("grey", 6), 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = kgrid.pred , aes(x = x, y = y, fill = Abundance)) +
    scale_fill_gradientn(name = paste0("Relative\nAbundance"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "none") +
    theme_light() +
    ggtitle(species) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          panel.grid.major.y = element_blank(),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16),
          legend.key.size = unit(1, "cm"), 
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.15, 0.15))
  
  ggsave(filename = paste0("results/figures/species/", species, "/map.jpeg"), 
         plot = species.plot,
         height = 900,
         width = 600, 
         dpi = 72,
         quality = 100,
         units = "px")
  
  #
  # Coefficient plots
  #
  
  link <- poisson()$linkinv

  main.coef <- data.frame(Coefficient = names(species.results[[species]]$coefficients),
                          Value = link(species.results[[species]]$coefficients))
  main.coef <- main.coef[2:11, ]
  main.coef$Coefficient <- factor(main.coef$Coefficient, levels = main.coef$Coefficient)
  main.coef$Value <- main.coef$Value / max(main.coef$Value)
  main.coef$Color <-   c("#748838", "#663301", "#448278", "#C89222", "#1D3991",
                         "#9D350B", "#532E8C","#FE9929", "#DCCF63", "#63A70C")
  
  veg.plot <- ggplot(data = main.coef, aes(x = Coefficient, y = Value, fill = Color)) +
    geom_bar(stat = "identity", fill = main.coef$Color) +
    labs(x = "Coefficient", y = "Relative Abundance") +
    theme_light() +
    theme(axis.title = element_text(size=16),
          axis.text.x = element_text(size=16, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size=16),
          title = element_text(size=12),
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  ggsave(filename = paste0("results/figures/species/", species, "/veghf.jpeg"), 
         plot = veg.plot,
         height = 600,
         width = 1200, 
         dpi = 72,
         quality = 100,
         units = "px")
  
  rm(species.plot, veg.plot)
  print(species)
  
}



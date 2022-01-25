#------------------------------------------------------------------------------
# Name: 30_training_dataset
# Author: Lena Perzlmaier, Chris Reudenbach
# Description:  creates the training data frame
#             - stack topo and forest structure  -> pred_stack
#             - get coordinates of the trainSites and extract information of predStack
#             - merge information of the step before with data from the climate station
#             - merge with radiation information
# Input:   see source files
# Output: - pred_stack "C:/Users/Lena/Documents/edu/mpg-envinsys-plygrnd/data/predictors/mc_pred_stack.tif"
#         - trainDF "C:/Users/Lena/Documents/edu/mpg-envinsys-plygrnd/data/auxdata/trainDFmc.rds"
# 
#------------------------------------------------------------------------------

# ---- 1 - load packages----

require(envimaR)
# MANDANTORY: defining the root folder DO NOT change this line
library(envimaR)
root_folder = "~/edu/courses/students-blogs/2021_2/mpg-gis-fe-2020-LeniPe/src/Microclimate/"

# work around for a subproject folder
prefix = ""

# define  additional packages comment if not needed
appendpackagesToLoad = c("lidR","future","lwgeom","tmap","dplyr","rgrass7","sp","link2GI")

# define additional subfolders comment if not needed
tmpPath = paste0("data/lidar/",prefix,"/") 
appendProjectDirList =  c(paste0(tmpPath,"gmetrics"),paste0(tmpPath,"tmetrics"),
                          paste0(tmpPath,"dem"),paste0(tmpPath,"dsm"),
                          paste0(tmpPath,"chm"),tmpPath)


# MANDANTORY: calling the setup script also DO NOT change this line
source(file.path(envimaR::alternativeEnvi(root_folder = root_folder),"src/000_setup.R"))



# trainDF
trainDF <- readRDS(file.path(envrmt$path_measurements_org, "climate_stations_combined.rds"))
head(trainDF)

# shapefile with tree coordinates
trainSites <- read_sf(file.path(envrmt$path_auxdata, "core_study_trees.shp"))

# predstack
topo <- stack(file.path(envrmt$path_data_lev2, "pred_topo.tif"))
forest <- stack(file.path(envrmt$path_data_lev2, "pred_forest_structure.tif"))

# Klimastation Grubenwiese
clim_stat <- readRDS(file.path(envrmt$path_measurements_org, "klimastation_wiese_hourly.rds"))

# projection
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


# ---- 2 - run code ----


# ---- 2.1 - stack raster ----

topo <- raster::crop(topo, raster::extent(forest))
raster::compareRaster(topo, forest, stopiffalse = FALSE, showwarning = TRUE)

pred_stack <- stack(topo, forest)
crs(pred_stack) <- proj4
names(pred_stack) <- c("dtm", "slope", "aspect", "TPI","CHM", "ip_ground", "cluster")
plot(pred_stack)

# save pred_stack
writeRaster(pred_stack, file.path(envrmt$path_data_lev2, "mc_pred_stack.tif"), overwrite = TRUE)
pred_stack <- stack(file.path(envrmt$path_data_lev2, "mc_pred_stack.tif"))
names(pred_stack) <- c("dtm", "slope", "aspect", "TPI","CHM", "ip_ground", "cluster")


# ---- 2.2 - prepare trainDF ----

# day of the year
trainDF$doy <- as.numeric(as.character(trainDF$date, format = "%j"))

# hour
trainDF$hour <- as.numeric(substr(trainDF$date, 12, 13))


# ---- 2.3 - prepare trainSites ----

# relevant tree IDs 
IDs <- unique(trainDF$cst_id)

# filter shapefile to only containt relevant tree IDs
trainSites <- filter(trainSites, trainSites$tree_id %in% IDs)
trainSites <- st_transform(trainSites, proj4)
trainSites <- trainSites[,2]
trainSites <- st_crop(trainSites, raster::extent(forest))

mapview::mapview(trainSites)

# convert trainSites to sp object
trainSitesSp <- as(trainSites, Class = "Spatial")
#plot(trainSites)


# ---- 2.4 - extract data ----

extr <- raster::extract(pred_stack, trainSitesSp, df = TRUE,
                        cellnumbers=TRUE, sp = TRUE)
head(extr)

trainDF <- merge(trainDF, extr@data, by.x="cst_id", by.y="tree_id")
# less observation than before because cst_id with NAs where removed

# --- 2.5 - merge with climate station data ----

trainDF <- merge(trainDF, clim_stat, by.x = "date", by.y = "date_time_hourly")

# --- skip point 2.6 and 2.7


trainDF <- trainDF %>% mutate(doy_hour_id = paste(doy, hour, cst_id, channel, sep = "_"))

#trainDF <- left_join(trainDF, trainDF2, by = c("doy_hour_id" = "doy_hour_id"))

# --- 2.6 - radiation information ----

use_sp()
link2GI::linkGRASS7(pred_stack, gisdbase = envrmt$path_GRASS, location = "MOF2")

# initGRASS(gisBase = "C:/OSGEO4W64/apps/grass/grass78",
#           gisDbase = "C:/Users/Lena/Documents/GIS DataBase/GRASS",
#           location = "MOF2",
#           mapset = "mapset")

doy_hour <- unique(trainDF[,c("doy", "hour")])
doy_hour$hour_chr <- sprintf("%02d", doy_hour$hour)

rad <- lapply(seq(nrow(doy_hour)), function(i){
  cat(i, " of ", nrow(doy_hour),"\n")   
  file_name <- paste("tot_rad_", doy_hour[i,1], "_", doy_hour[i,3], ".00", sep = "")
  
  ras <- raster(rgrass7::readRAST(file_name))
  
  cst_ids <- filter(trainDF, doy == doy_hour[i,1] & hour == doy_hour[i,2])
  cst_ids <- unique(cst_ids$cst_id)
  
  trainSites_filtered <- filter(trainSites, tree_id %in% cst_ids)
  trainSites_filtered <- as(trainSites_filtered, Class = "Spatial")
  
  #plot(raster)
  #plot(trainSites_filtered, add = TRUE)
  
  extr <- raster::extract(ras, trainSites_filtered, df = TRUE,
                          cellnumbers=TRUE, sp = TRUE)
  #head(extr)
  
  df <- data.frame(cst_id = extr@data$tree_id,
                   doy = doy_hour[i,1],
                   hour = doy_hour[i,2],
                   rad = extr@data[,3])
  
})

rad <- do.call("rbind", rad)

plot(rad$hour, rad$rad)

# merge with trainDF

trainDF <- merge(trainDF, rad, by = c("cst_id", "doy", "hour"))

## ---- 2.7 - cloudiness ----

# day of the year
clim_stat$doy <- as.numeric(as.character(clim_stat$date_time_hourly, format = "%j"))

# hour
clim_stat$hour <- as.numeric(substr(clim_stat$date_time_hourly, 12, 13))

doy_hour <- unique(clim_stat[,c("doy", "hour")])
doy_hour$hour_chr <- sprintf("%02d", doy_hour$hour)

clim_stat_sp <- SpatialPoints(coords = cbind(8.6832, 50.8405),
                              proj4string = CRS("+proj=longlat +datum=WGS84"))
clim_stat_sp <- spTransform(clim_stat_sp, proj4)


rad <- lapply(seq(nrow(doy_hour)-1), function(i){
  
  file_name <- paste("tot_rad_", doy_hour[i,1], "_", doy_hour[i,3], ".00", sep = "")
  
  raster <- raster(readRAST(file_name))
  
  extr <- raster::extract(raster, clim_stat_sp, df = TRUE)
  #head(extr)
  
  df <- data.frame(doy = doy_hour[i,1],
                   hour = doy_hour[i,2],
                   rad = extr[,2])
  
})

rad <- do.call("rbind", rad)

saveRDS(rad, file.path(envrmt$path_data, "rad_clim_station.rds"))

plot(rad$hour, rad$rad)

colnames(rad) <- c("doy", "hour", "rad_Klimastation")

trainDF <- merge(trainDF, rad, by = c("doy", "hour"))

plot(trainDF$rad_Klimastation, trainDF$rad_sw_in)



## ---- 3 - export train df ----

saveRDS(trainDF, file.path(envrmt$path_auxdata, "trainDFmc.rds"))
trainDF <- readRDS(file.path(envrmt$path_auxdata, "trainDFmc_fullextent.rds"))

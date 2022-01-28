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

# ---- project setup ----
require(envimaR)
# MANDANTORY: defining the root folder DO NOT change this line
root_folder = "~/edu/agis"
# work around for a subproject folder
prefix = "MOF_lidar_2018"

# define  additional packages comment if not needed
appendpackagesToLoad = c("lidR","future","lwgeom","tmap")

# define additional subfolders comment if not needed
tmpPath = paste0("data/lidar/",prefix,"/") 
appendProjectDirList =  c(paste0(tmpPath,"gmetrics"),paste0(tmpPath,"tmetrics"),
                          paste0(tmpPath,"dem"),paste0(tmpPath,"dsm"),
                          paste0(tmpPath,"chm"),tmpPath)


# MANDANTORY: calling the setup script also DO NOT change this line
source(file.path(envimaR::alternativeEnvi(root_folder = root_folder),"src/000-rspatial-setup.R"))

# projection
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
epsg = 25832

# ---- read preprocessed data ----
# trainDF
trainDF <- readRDS(file.path(envrmt$path_measurements, "climate_stations_combined.rds"))
head(trainDF)

# shapefile with tree coordinates
trainSites <- read_sf(file.path(envrmt$path_auxdata, "core_study_trees.shp"))
trainSites = st_transform(trainSites,crs = 25832)

# predstack
topo <- stack(file.path(envrmt$path_dem,"pred_topo.tif"))
forest <- readRDS(file.path(envrmt$path_MOF_lidar_2018,"pred_forest_structure.rds"))

# Klimastation Grubenwiese
clim_stat <- readRDS(file.path(envrmt$path_measurements, "klimastation_wiese_hourly.rds"))
# coordinate of the climate station
cstation = st_sfc(st_point(cbind(8.6832, 50.8405)),crs = 4326)
cstation = st_transform(st_sf(data.frame(tree_id="grubenwiese", geom=cstation)),crs = 25832)


# ---- 2 - run code ----

# ---- 2.1 - stack raster ----
pred_stack <- stack(topo, forest)
names(pred_stack)[1:4] = c("dtm", "slope", "aspect", "TPI")

# save pred_stack
# writeRaster(pred_stack, file.path(envrmt$path_data_lev2, "mc_pred_stack.tif"), overwrite = TRUE)
# pred_stack <- stack(file.path(envrmt$path_data_lev2, "mc_pred_stack.tif"))

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
trainSites <- st_crop(trainSites, raster::extent(forest))

mapview(pred_stack[[1]],fgb=F)+ trainSites

# ---- 2.4 - extract data ----
extr = exactextractr::exact_extract(pred_stack, st_buffer(trainSites,dist = 1),  force_df = TRUE,
                                    include_cell = TRUE,include_xy = TRUE,full_colnames = TRUE,include_cols = "tree_id") 
extr = dplyr::bind_rows(extr)
extr = filter(extr, coverage_fraction > 0.9)
trainDF <- merge(trainDF, extr, by.x="cst_id", by.y="tree_id")
# less observation than before because cst_id with NAs where removed

# --- 2.5 - merge with climate station data ----
trainDF <- merge(trainDF, clim_stat, by.x = "date", by.y = "date_time_hourly")
# add label col
trainDF <- trainDF %>% mutate(doy_hour_id = paste(doy, hour, cst_id,  sep = "_"))

# --- 2.6 get GRASS radiation information ----

use_sp()
link2GI::linkGRASS7(pred_stack, gisdbase = envrmt$path_GRASS, location = "MOF")

doy_hour <- unique(trainDF[,c("doy", "hour")])
doy_hour$hour_chr <- sprintf("%02d", doy_hour$hour)


cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)
rad <- foreach(i = seq(nrow(doy_hour)),.packages=c("raster","rgrass7","dplyr","sf","exactextractr"),.verbose = TRUE) %dopar% {
  use_sp()
  file_name <- paste("tot_rad_", doy_hour[i,1], "_", doy_hour[i,3], ".00", sep = "")
  ras <- raster(rgrass7::readRAST(file_name))
  cst_ids <- filter(trainDF, doy == doy_hour[i,1] & hour == doy_hour[i,2])
  cst_ids <- unique(cst_ids$cst_id)
  trainSites_filtered <- filter(trainSites, tree_id %in% cst_ids)
  extr = exactextractr::exact_extract(ras, st_transform(st_buffer(trainSites_filtered,dist = 2.),crs = st_crs(ras)),
                                      force_df = TRUE,
                                      include_cols = "tree_id",
                                      include_area =T) 
  extr = dplyr::bind_rows(extr) %>%
    filter(coverage_fraction >= 1) %>% 
    group_by(tree_id) %>% 
    summarise(mean = mean(value), n = n())
  
  df <- data.frame(cst_id = extr$tree_id,
                   doy = doy_hour[i,1],
                   hour = doy_hour[i,2],
                   rad = extr$mean)
}
stopCluster(cl)
rad <- do.call("rbind", rad)

# merge results with trainDF
trainDF <- merge(trainDF, rad, by = c("cst_id", "doy", "hour"))
plot(rad$hour, rad$rad)

## ---- 2.7 - cloudiness ----
# do the same with  cloudiness
# day of the year
clim_stat$doy <- as.numeric(as.character(clim_stat$date_time_hourly, format = "%j"))

# hour
clim_stat$hour <- as.numeric(substr(clim_stat$date_time_hourly, 12, 13))
doy_hour <- unique(clim_stat[,c("doy", "hour")])
doy_hour$hour_chr <- sprintf("%02d", doy_hour$hour)

cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)
cld = foreach(i =  seq(nrow(doy_hour)-1), .packages=c("raster","rgrass7","sf","exactextractr","dplyr"),.verbose = TRUE) %dopar% {
  use_sp()
  file_name <- paste("tot_rad_", doy_hour[i,1], "_", doy_hour[i,3], ".00", sep = "")
  ras <- raster::raster(rgrass7::readRAST(file_name))
  extr = exactextractr::exact_extract(ras, st_transform(st_buffer(cstation,dist = 2.),crs = st_crs(ras)),
                                      force_df = TRUE,
                                      include_cols = "tree_id",
                                      include_area =T) 
  extr = dplyr::bind_rows(extr) %>%
    filter(coverage_fraction >= 1) %>% 
    group_by(tree_id) %>% 
    summarise(mean = mean(value), n = n())
  
  df <- data.frame(cst_id = extr$tree_id,
                   doy = doy_hour[i,1],
                   hour = doy_hour[i,2],
                   rad = extr$mean
  )}
stopCluster(cl)
cld <- do.call("rbind", cld)
plot(cld$hour, cld$rad)
colnames(cld) <- c("cst_id","doy", "hour", "rad_Klimastation")
saveRDS(cld, file.path(envrmt$path_data, "rad_clim_station.rds"))

trainDF <- merge(trainDF, cld, by = c("doy", "hour"))
plot(trainDF$rad_Klimastation, trainDF$rad_sw_in)
saveRDS(trainDF, file.path(envrmt$path_auxdata, "trainDFmc.rds"))

#trainDF <- readRDS(file.path(envrmt$path_auxdata, "trainDFmc_fullextent.rds"))

#------------------------------------------------------------------------------
# Name: 20_CHM_DEM_hulls_tiles.R
# Type: control script 
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  script creates a lidr catalog to derive tree segment based inizes
#               canopy height model, Digital Elevation Models and standard statistics
# Data: regular las LiDAR data sets 
# Copyright:GPL (>= 3) 
# Date: 2021-12-10
#------------------------------------------------------------------------------
# 0 - project setup
#-----------------------------
require(envimaR)
# MANDANTORY: defining the root folder DO NOT change this line
root_folder = "~/edu/agis"
# define  additional packages comment if not needed
appendpackagesToLoad = c("lidR","future","lwgeom","tmap")
# define additional subfolders comment if not needed
appendProjectDirList =  c("data/lidar/","data/lidar/l_raster","data/lidar/l_raw","data/lidar/l_norm","data/lidar/l_vector")

# MANDANTORY: calling the setup script also DO NOT change this line
source(file.path(envimaR::alternativeEnvi(root_folder = root_folder),"src/000-rspatial-setup.R"),local = knitr::knit_global())


#--- further variables
# logical switch 
# if TRUE the calculation is performed 
# if FALSE the results are loaded into memory
calculate = TRUE

#- define current projection ETRS89 / UTM zone 32N
proj4 = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
epsg_number = 25832
prefix="MOF_lidar_2018"
#- tmap options
tmap_options(check.and.fix = TRUE) 

res = 2.0

#- parallelisation in this case 16 Cores

future::plan(multisession, workers = 6)
set_lidr_threads(6)

#------------------------------------------------------------------------------
# 1 - start processing
#-----------------------------
if (calculate){
  #- create path link to the original las file
  las_file = paste0(envrmt$path_l_raw,"/MOF_lidar_2018.las")
  
  #- define lidR catalog
  #- general catalog settings
  ctg <- lidR::readLAScatalog(las_file)
  projection(ctg) <- 25832
  lidR::opt_chunk_size(ctg) = 500
  lidR::opt_chunk_buffer(ctg) <- 50
  lidR::opt_progress(ctg) <- TRUE
  lidR::opt_laz_compression(ctg) <- TRUE
  ctg@output_options$drivers$Raster$param$overwrite <- TRUE
  ctg@output_options$drivers$Vector$param$overwrite <- TRUE
  

  #- height normalisation within the point cloud
  # source: https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
  # calculate dem
  lidR::opt_output_files(ctg) <- paste0(envrmt$path_l_raster,"/",prefix,"/{ID}","_dsm")
  dem <- grid_terrain(ctg, res = res,lidR::knnidw(k = 6L, p = 2))
  
  
  
  #- normalize height (point cloud)
  lidR::opt_output_files(ctg) <- paste0(envrmt$path_l_norm,"/",prefix,"/{ID}","_norm_height")
  ctg <- lidR::normalize_height(ctg,lidR::knnidw())
  
  #- calculate chm
  lidR::opt_output_files(ctg) = paste0(envrmt$path_l_raster,"/",prefix,"/{ID}","_chm")
  chm = grid_canopy(ctg, res = res, lidR::dsmtin())
  ttops_chm = find_trees(chm, lmf(8))
  algo1 = dalponte2016(chm, ttops_chm,th_seed = 0.55,th_tree = 0.65,max_cr = 8)
  algo2 = silva2016(chm, ttops_chm,exclusion = 0.5)
  algo3 = li2012(hmin = 5, R = 5)
  
  #--- tree segmentation based on the point cloud using catalog
  # source: https://gis.stackexchange.com/questions/364546/individual-tree-segmentation-workflow-with-lidr-package
  
  #- catalog modifications
  opt_output_files(ctg) = paste0(envrmt$path_l_raster,"/",prefix,"/HULL_{XCENTER}_{YCENTER}")
  opt_chunk_size(ctg) = 0
  opt_filter(ctg) <- "filter_noise(ctg, sensitivity = 1.2, res = 5)"
  
  #- tree segmentation using the 99 percentile filter
  ctg = lidR::segment_trees(ctg,  algo1 , uniqueness = "bitmerge")
  #laz = do.call(rbind, lapply(list.files(paste0(envrmt$path_l_raster,"/",prefix), pattern = "*.laz", full.names = TRUE),readLAS))
  #plot(laz,color = "treeID")
  saveRDS(ctg,paste0(envrmt$path_lidar,"/ctg.rds"))
  
  
  #- tree filtering and calculate hull 
  opt_filter(ctg) <- "!is.na(treeID)"
  ctg@output_options$drivers$Spatial$extension = ".shp"
  opt_output_files(ctg) = paste0(envrmt$path_l_raster,"/",prefix,"/HULL_sapflow_{XCENTER}_{YCENTER}")
  hulls = catalog_apply(ctg=ctg, FUN = tree_fn)
  
  #- merge shapefiles
  #hulls <- list.files(paste0(envrmt$path_l_raster,"/",prefix), pattern = "*.shp", full.names = TRUE)
  seg = do.call(rbind, lapply(hulls, read_sf))
  st_write(seg,paste0(envrmt$path_l_raster,"/",prefix,"/segmentation_sapflow.shp"),append=F)
  #plot(st_geometry(seg))
  
  #- tmap plot
  tmap_mode("view")
  tm_shape(seg) + tm_fill(col = "zq95") 
  mapview(seg,zcol="zq95", fgb = FALSE)
  
  #- gridmetrics 
  # NOTE: the metrics is performed on the manipulated (see above) ctg! 
  # So the results will differ from the raw lasfile/ctg
  sapflow_metrics <- grid_metrics(ctg, .stdmetrics, 5)
  raster::writeRaster(sapflow_metrics,paste0(envrmt$path_l_raster,"/",prefix,"/segmentation_sapflow.tif"))
  tmap_mode("view")
  mapview(sapflow_metrics) 
  
} else {
  dem = raster::raster(paste0(envrmt$path_l_raster,"/",prefix,"/grid_terrain.vrt"))
  chm = raster(paste0(envrmt$path_l_raster,"/",prefix,"/grid_canopy.vrt"))
  ctg = readRDS(paste0(envrmt$path_lidar,"/ctg.rds"))
  trees = st_read(paste0(envrmt$path_l_raster,"/",prefix,"/segmentation_sapflow.shp"))
  sapflow_metrics = raster::stack(paste0(envrmt$path_l_raster,"/",prefix,"/segmentation_sapflow.tif"))
}
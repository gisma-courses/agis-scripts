#------------------------------------------------------------------------------
# Name: 10_make_CHM_DEM_hulls_Tiles.R
# Type: control script 
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  script creates a lidr catalog to derive tree segment based inizes
#               canopy height model, Digital Elevation Models and standard statistics
# Data: regular las LiDAR data sets 
# Copyright:GPL (>= 3) 
# Date: 2021-12-10
#------------------------------------------------------------------------------
# 0 - specific setup
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

#- define current projection ETRS89 / UTM zone 32N
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
epsg_number = 25832

#- tmap options
tmap_options(check.and.fix = TRUE) 

# 1 - start script
#-----------------------------
#- create path link to the original las file
las_file = paste0(envrmt$path_l_raw,"/las_mof.las")

#- lidR catalog
#- general catalog settings
ctg <- lidR::readLAScatalog(las_file)
projection(ctg) <- 25832
lidR::opt_chunk_size(ctg) = 250
lidR::opt_chunk_buffer(ctg) <- 10
lidR::opt_progress(ctg) <- TRUE
lidR::opt_laz_compression(ctg) <- TRUE
ctg@output_options$drivers$Raster$param$overwrite <- TRUE
ctg@output_options$drivers$Vector$param$overwrite <- TRUE

#- parallelisation in this case 16 Cores
future::plan(multisession, workers = 16L)
set_lidr_threads(16L)


#- height normalisation within the point cloud
# source: https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
# calculate dem
lidR::opt_output_files(ctg) <- paste0(envrmt$path_l_raster,"/{ID}","_dsm")
dem <- grid_terrain(ctg, res = 1.0,lidR::knnidw(k = 6L, p = 2))
# dem = raster(paste0(envrmt$path_l_norm,"/grid_terrain.vrt"))


#- normalize height (point cloud)
lidR::opt_output_files(ctg) <- paste0(envrmt$path_l_norm,"/{ID}","_norm_height")
ctg <- lidR::normalize_height(ctg,lidR::tin())

#- calculate chm
lidR::opt_output_files(ctg) <- paste0(envrmt$path_l_raster,"/{ID}","_chm")
chm <- grid_canopy(ctg, res = 5.0, lidR::pitfree(thresholds = c(0,2,5,10,15), c(0,1.5)))
# chm = raster(paste0(envrmt$path_l_norm,"/grid_canopy.vrt"))


#--- tree segmentation based on the point cloud using catalog
# source: https://gis.stackexchange.com/questions/364546/individual-tree-segmentation-workflow-with-lidr-package

#- catalog modifications
opt_output_files(ctg) = paste0(envrmt$path_l_raster,"/HULL_{XCENTER}_{YCENTER}")
opt_chunk_buffer(ctg) = 20
opt_chunk_size(ctg) = 0
opt_filter(ctg) <- "filter_noise(ctg, sensitivity = 1.2)"

#- tree segmentation using the 99 percentile filter
ctg = lidR::segment_trees(ctg,  li2012(dt1 = 1.4, dt2 = 1.9, hmin = 5, R = 5) , uniqueness = "bitmerge")
laz = do.call(rbind, lapply(list.files(envrmt$path_l_raster, pattern = "*.laz", full.names = TRUE),readLAS))
plot(laz,color = "treeID")
saveRDS(ctg,paste0(envrmt$path_lidar,"/ctg.rds"))
# ctg = readRDS(paste0(envrmt$path_lidar,"/ctg.rds"))

#- tree filtering and calculate hull 
opt_filter(ctg) <- "!is.na(treeID)"
ctg@output_options$drivers$Spatial$extension = ".shp"
opt_output_files(ctg) = paste0(envrmt$path_l_raster,"/HULL_sapflow_{XCENTER}_{YCENTER}")
hulls = catalog_apply(ctg=ctg, FUN = tree_fn)

#- merge shapefiles
# file_list <- list.files(envrmt$path_l_raster, pattern = "HULL4_*.shp", full.names = TRUE)
 seg = do.call(rbind, lapply(hulls, read_sf))
 st_write(seg,paste0(envrmt$path_l_raster,"/segmentation_sapflow.shp"))
 plot(st_geometry(seg))

#- tmap plot
 tmap_mode("view")
 tm_shape(seg) + tm_fill(col = "zq95") 
 mapview(seg,zcol="zq95", fgb = FALSE)
 
 # gridmetrics
  sapflow_metrics <- grid_metrics(ctg, .stdmetrics, 5)
 plot(sapflow_metrics, col = height.colors(50))
 tmap_mode("view")
 tm_shape(sapflow_metrics) + tm_raster(col = "zq95") 
 
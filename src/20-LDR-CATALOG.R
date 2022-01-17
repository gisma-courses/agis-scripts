#------------------------------------------------------------------------------
# Name: make_CHM_Tiles.R
# Type: control script 
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  script creates a canopy height model from generic Lidar 
#              las data using the lidR package
# Data: regular las LiDAR data sets 
# Copyright:GPL (>= 3) 
# Date: 2021-12-10
#------------------------------------------------------------------------------
# 0 - specific setup
#-----------------------------
require(envimaR)

# MANDANTORY: defining the root folder DO NOT change this line
root_folder = "~/edu/agis"

#-- Further customization of the setup by the user this section 
#-- can be freely customized only the definition of additional packages 
#-- and directory paths MUST be done using the two variables 
#-- appendpackagesToLoad and appendProjectDirList
#-- feel free to remove this lines if you do not need them
# define  additional packages comment if not needed
appendpackagesToLoad = c("lidR","future","lwgeom")
# define additional subfolders comment if not needed
appendProjectDirList =  c("data/lidar/","data/lidar/l_raster","data/lidar/l_raw","data/lidar/l_norm")

## define current projection (It is not magic you need to check the meta data or ask your instructor) 
## ETRS89 / UTM zone 32N
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
epsg_number = 25832
lidR.progress = TRUE

# MANDANTORY: calling the setup script also DO NOT change this line
source(file.path(envimaR::alternativeEnvi(root_folder = root_folder),"src/000-rspatial-setup.R"),local = knitr::knit_global())


# 1 - start script
#-----------------------------
#--- create path link to the original las file

las_file = paste0(envrmt$path_data,"/MOF_lidar_2018.las")

#--- create CHM with lidR catalog
#- source: https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
future::plan(multisession, workers = 4L,  future.seed = TRUE)
#set_lidr_threads(4L)
ctg <- lidR::readLAScatalog(las_file)
lidR::projection(ctg) <- 25832
lidR::opt_chunk_size(ctg) = 500
lidR::opt_chunk_buffer(ctg) <- 20
lidR::opt_progress(ctg) <- FALSE
lidR::opt_laz_compression(ctg) <- TRUE
ctg@output_options$drivers$Raster$param$overwrite <- TRUE

#--- height normalisation within the point cloud


lidR::opt_output_files(nh_ctg) <- paste0(envrmt$path_l_raster,"/{ID}","_dsm")
dsm <- grid_terrain(ctg, res = 1.0,lidR::knnidw(k = 6L, p = 2))

#dsm = raster(paste0(envrmt$path_l_norm,"/grid_terrain.vrt"))
lidR::opt_output_files(nh_ctg) <- paste0(envrmt$path_l_norm,"/{ID}","_norm_height")
nh_ctg <- lidR::normalize_height(ctg,lidR::tin())

# check new ctg
las_check(nh_ctg)
# calculate chm
lidR::opt_output_files(nh_ctg) <- paste0(envrmt$path_l_raster,"/{ID}","_chm")
chm <- grid_canopy(nh_ctg, res = 1.0, lidR::pitfree(thresholds = c(0,2,5,10,15), c(0,1.5)))
#chm = raster(paste0(envrmt$path_l_norm,"/grid_canopy.vrt"))


# tree segmentation
# https://gis.stackexchange.com/questions/364546/individual-tree-segmentation-workflow-with-lidr-package
opt_output_files(nh_ctg) = paste0(envrmt$path_l_raster,"/HULL_{XCENTER}_{YCENTER}")
opt_chunk_buffer(nh_ctg) = 20
catalog_apply(nh_ctg, tree_fn, vrt_dsm = chm, vrt_dtm = dsm, th = 2)

# merge shapefiles
file_list <- list.files(envrmt$path_l_raster, pattern = "*shp", full.names = TRUE)
seg = do.call(rbind, lapply(file_list, read_sf))
plot(seg[[1]])

#- mapview plot
mapview::mapview(chm[[1]],layer.name = "pitfree chm 1 m² cells height [m]",col.regions=lidR::height.colors(20))

#- classic plot
plot( chm,
      col = lidR::height.colors(20),
      main = "pitfree chm 1 m² cells",
      cex.main = 1) 

#- tmap plot
tm_shape(chm) +
  tm_raster( title = "pitfree chm 1 m² cells", 
             palette = lidR::height.colors(20)) +
  tm_grid()+
  tm_layout(legend.outside = TRUE)

#- ggplot plot with stars
ggplot() + 
  geom_stars(data = stars::st_as_stars(chm)) + 
  scale_fill_gradientn(colours=lidR::height.colors(20)) +
  coord_equal()+
  guides(fill=guide_legend(title="pitfree chm 1 m² cells"))


saveRDS(out,file= file.path(envrmt$path_level1,"crop_aoimof_tree_segments.rds"))
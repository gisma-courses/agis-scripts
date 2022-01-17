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
appendpackagesToLoad = c("lidR","future")
# define additional subfolders comment if not needed
appendProjectDirList =  c("data/lidar/","data/lidar/l_raster","data/lidar/l_raw","data/lidar/l_norm")

## define current projection (It is not magic you need to check the meta data or ask your instructor) 
## ETRS89 / UTM zone 32N
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
epsg_number = 25832

# MANDANTORY: calling the setup script also DO NOT change this line
source(file.path(envimaR::alternativeEnvi(root_folder = root_folder),"src/agis_setup.R"),echo = TRUE)

# 1 - start script
#-----------------------------

#---- NOTE file size is about 12MB
utils::download.file(url="https://github.com/gisma/gismaData/raw/master/uavRst/data/lidR_data.zip",
                     destfile=paste0(envrmt$path_tmp,"/chm.zip"))
unzip(paste0(envrmt$path_tmp,"/chm.zip"),
      exdir = envrmt$path_tmp,  
      overwrite = TRUE)

#---- Get all *.las files of a folder into a list
las_files = list.files(envrmt$path_tmp,
                       pattern = glob2rx("*.las"),
                       full.names = TRUE)

#--- create CHM with lidR catalog
#- source: https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
future::plan(future::multisession)
ctg <- lidR::readLAScatalog(las_files[[1]])
lidR::projection(ctg) <- 25832
lidR::opt_chunk_size(ctg) = 100
lidR::opt_chunk_buffer(ctg) <- 5
lidR::opt_progress(ctg) <- FALSE
lidR::opt_laz_compression(ctg) <- TRUE
ctg@output_options$drivers$Raster$param$overwrite <- TRUE

#--- height normalisation within the point cloud

lidR::opt_output_files(ctg) <- paste0(envrmt$path_l_norm,"/{ID}","_norm_height")
norm_ctg <- lidR::normalize_height(ctg,lidR::tin())

# reassign the projection
sp::proj4string(las) <- sp::CRS(proj4)

# calculate the chm with the pitfree algorithm
chm = lidR::grid_canopy(las, 0.25, pitfree(c(0,2,5,10,15), c(0,1), subcircle = 0.2))

# write it to tif
raster::writeRaster(chm,file.path(envrmt$path_l_raster,"mof_chm_one_tile.tif"),overwrite=TRUE) 


# - visualize 
-------------------
  
  # call mapview with some additional arguments
  mapview::mapview(chm,
          legend=TRUE, 
          layer.name = "canopy height model",
          col = mvTop(256),
          alpha.regions = 0.7)
#------------------------------------------------------------------------------
# rspatial course rspatial setup
# Type: script
# Name: 000-rspatial-setup.R
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  create/read project folder structure and returns pathes as list
#               load all necessary packages 
#               sources all functions in a defined function folder
# Dependencies: devtools::install_github("envima/envimaR")
# Output: list containing the folder strings as shortcuts
# Copyright: Reudenbach/Nauss 2015-2021, GPL (>= 3)
# Details: Please note that the queries on standard arguments have been set for 
#          pragmatic reasons for a typical course setting. Of course, it is 
#          intended to assign the arguments directly in the createEnvi function
# Date: 2021-12-12
#------------------------------------------------------------------------------
require(envimaR)

# rspatial packages
packagesToLoad = c("mapview", "mapedit", "tmap", "raster", "terra", "stars", "gdalcubes", "sf","dplyr","tidyverse","RStoolbox",
                   "randomForest", "ranger", "e1071", "caret", "link2GI","rgrass7","doParallel","foreach")


# mandantory folder structure
projectDirList   = c("data/",               # data folders the following are obligatory but you may add more
                     "data/data_lev0/",       # data for level 0 raw or original data
                     "data/data_lev1/",       # data for level 1 cleaned data
                     "data/data_lev2/",       # data for level 2 raw or output/productdata
                     "run/",                # temporary runtime data storage
                     "GRASS/",               # GRASS folder
                     "src/",                # scripts and source code; NOTE the subfolder called functions is genereated by default
                     "tmp/",                 # all kind or rsession temporary stuff
                     "doc/")                # documentation and markdown


# append additional packages if defined by calling script

if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") 
{
  projectDirList = append(projectDirList,appendProjectDirList)
}

if (exists("appendpackagesToLoad") && appendpackagesToLoad[[1]] != "") 
{
  packagesToLoad = append(packagesToLoad,appendpackagesToLoad)
}

# Now create/read root direcory, folder structure and load packages
# NOTE root_folder MUST be defined in calling script
if (!exists("root_folder")) {
  stop("variable root_folder is NOT defined by calling script...\n You MUST define a path in the variable 'root_folder'\n")
}
if (!exists("alt_env_id")) {
  message("variable alt_env_id is NOT defined by calling script...\n 'COMPUTERNAME' is set as default\n")
  alt_env_id = "COMPUTERNAME"
}
if (!exists("alt_env_value")) {
  message("variable alt_env_value is NOT defined by calling script...\n 'PCRZP' is set as default\n")
  alt_env_value = "PCRZP"
}
if (!exists("alt_env_root_folder")) {
  message("variable alt_env_root_folder is NOT defined by calling script...\n 'F:/BEN/edu' is set as default\n")
  alt_env_root_folder = "F:/BEN/edu"
}

if (!exists("path_prefix")) {
  message("variable  path_prefix is NOT defined by calling script...\n 'path_' is set as default\n")
  path_prefix =  "path_"
}

root_folder = envimaR::alternativeEnvi(root_folder = root_folder,
                                   alt_env_id = alt_env_id,
                                   alt_env_value = alt_env_value,
                                   alt_env_root_folder = alt_env_root_folder)


if (!exists("fcts_folder")) {
  message("variable fcts_folder is NOT defined by calling script...\n 'src/functions/' is set as default\n")
  fcts_folder =  paste0(root_folder,"/src/functions/")
}

# append additional folders if defined by calling script
if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") 
{
  projectDirList = append(projectDirList,appendProjectDirList)
}

# call central function
envrmt = envimaR::createEnvi(root_folder = root_folder,
                             folders = projectDirList,
                             fcts_folder = fcts_folder,
                             path_prefix = path_prefix,
                             libs = packagesToLoad,
                             setup_script = "000-rspatial-setup.R",
                             source_functions = TRUE,
                             alt_env_id = alt_env_id,
                             alt_env_value = alt_env_value,
                             alt_env_root_folder = alt_env_root_folder)

## set temp path to speed up raster package operations
raster::rasterOptions(tmpdir = envrmt$path_tmp)
# suppres gdal warnings
rgdal::set_thin_PROJ6_warnings(TRUE)

# define some color palettes
mvTop = mapview::mapviewPalette("mapviewTopoColors")
mvSpec = mapview::mapviewPalette("mapviewSpectralColors")
mvVec =	 mapview::mapviewPalette("mapviewVectorColors") 
mvRas =	 mapview::mapviewPalette("mapviewRasterColors")
mapviewOptions(fgb = FALSE)
mapviewOptions(viewer.suppress = FALSE)

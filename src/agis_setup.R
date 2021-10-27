#------------------------------------------------------------------------------
# agis course basic setup
# Type: script
# Name: agis_setup.R
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  create/read project folder structure and returns pathes as list
#               load all necessary packages 
#               sources all functions in a defined function folder
# Dependencies:   
# Output: list containing the folder strings as shortcuts
# Copyright: Chris Reudenbach, thomas Nauss 2019-2021, GPL (>= 3)
# git clone https://github.com/gisma-courses/agis-scripts.git
#------------------------------------------------------------------------------

require(envimaR)

# basic packages
packagesToLoad = c("mapview","mapedit","tmap","raster", "sf","dplyr","tidyverse","RStoolbox",
                   "randomForest","e1071","caret")

# append additional packages if defined by calling script
if (exists("appendpackagesToLoad") && appendpackagesToLoad[[1]] != "") 
{
  projectDirList = append(packagesToLoad,appendpackagesToLoad)
}

# Now create/read root direcory, folder structure and load packages
# NOTE rootDIR MUST be defined in calling script
if (!exists("rootDIR")) {
  cat("variable rootDIR is NOT defined\n '~/edu/agis' is set by default")
  rootDIR = "~/edu/agis"
}
if (!exists("alt_env_id")) {
  cat("variable alt_env_id is NOT defined\n 'COMPUTERNAME' is set by default")
  alt_env_id = "COMPUTERNAME"
}
if (!exists("alt_env_value")) {
  cat("variable alt_env_value is NOT defined\n 'PCRZP' is set by default")
  alt_env_value = "PCRZP"
}
if (!exists("alt_env_root_folder")) {
  cat("variable alt_env_root_folder is NOT defined\n 'F:/BEN/edu' is set by default")
  alt_env_root_folder = "F:/BEN/edu"
}


rootDir = envimaR::alternativeEnvi(root_folder = rootDIR,
                                   alt_env_id = alt_env_id,
                                   alt_env_value = alt_env_value,
                                   alt_env_root_folder = alt_env_root_folder)

# mandantory folder structure
projectDirList   = c("data/",               # data folders the following are obligatory but you may add more
                     "run/",                # folder for runtime data storage
                     "src/",                # source code
                     "tmp",                 # all temp stuff
                     "doc/")                # documentation  and markdown

# append additional folders if defined by calling script
if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") 
{
  projectDirList = append(projectDirList,appendProjectDirList)
}

# call central function
envrmt = envimaR::createEnvi(root_folder = rootDir,
                             folders = projectDirList,
                             fcts_folder = "src/functions/",
                             path_prefix = "path_",
                             libs = packagesToLoad,
                             alt_env_id = "COMPUTERNAME",
                             alt_env_value = "PCRZP",
                             alt_env_root_folder = "F:/BEN/edu")

## set temp path to speed up raster package operations
raster::rasterOptions(tmpdir = envrmt$path_tmp)
# suppres gdal warnings
rgdal::set_thin_PROJ6_warnings(TRUE)


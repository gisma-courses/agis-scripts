#------------------------------------------------------------------------------
# Type: script
# Name: template-script.R
# Author: 
# Description: set necessary variables and calls setup  
# Dependencies: agis_setup.R  
# Output: list of pathes
# Copyright: 2021, GPL (>= 3)
#------------------------------------------------------------------------------
# 0 - specific setup
#-----------------------------
require(envimaR)

# MANDANTORY: defining the root folder DO NOT change this line
rootDIR = "~/edu/agis"

#-- Further customization of the setup by the user this section 
#-- can be freely customized only the definition of additional packages 
#-- and directory paths MUST be done using the two variables 
#-- appendpackagesToLoad and appendProjectDirList
#-- feel free to remove this lines if you do not need them
# define  additional packages uncomment if necessary
# appendpackagesToLoad = c("dummy-package")
# define additional subfolders uncomment if necessary
# appendProjectDirList =  c("data/dymmy-folder/")


# MANDANTORY: calling the setup script also DO NOT change this line
source(file.path(envimaR::alternativeEnvi(root_folder = rootDIR),"src/agis_setup.R"),echo = TRUE)

# 1 - start script
#-----------------------------
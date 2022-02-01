#
# Name: 040_clean_dataset
# Author: Lena Perzlmaier, Chris Reudenbach
# Description:  creates the training data frame
#             - stack topo and forest structure  -> pred_stack
#             - get coordinates of the trainSites and extract information of predStack
#             - merge information of the step before with data from the climate station
#             - merge with radiation information
# Input:   see source files
# 
#

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

# load trainDF from script 30
trainDF <- readRDS(file.path(envrmt$path_auxdata, "trainDFmc.rds"))


# have a look at ecowitt data
# it is only available for short time periods
trainDF %>% filter(type == "ecowitt") %>%  group_by(cst_id.x) %>% 
  ggplot(aes(x = date, y = cst_id.x))+
  geom_point()

# only keep treetalker
trainDFclean <- filter(trainDF, type == "treetalker")
trainDFclean$type = NULL
id=trainDFclean$cst_id.x
doy=trainDFclean$doy
hour=trainDFclean$hour
rad_sw_in=trainDFclean$rad_sw_in
rad_klimastation = trainDFclean$rad_Klimastation
rad = trainDFclean$rad
doy_hour_id = trainDFclean$doy_hour_id

##---  cleaning the training data for a random forest model training
traintmp = trainDFclean[ , !(names(trainDFclean) %in% c("cst_id.y","zq5","WSDiag_2","WSDiag_1","cell","doy_hour_id","Albedo","date","cst_id.x","station_id","channel","n","area","x","y","cell_coverage_fraction","heatflux_soil","cst_id.y"))]
summary(traintmp)

## ---- 3.x cloudiness index ----

# calculate the ratio of observed versus calculate irradiance to obtain an indicator of cloudiness

cloud <- vector(length = nrow(traintmp))
for (i in 1:nrow(traintmp)) {
  if(traintmp[i,]$rad_Klimastation == 0){
    cloud[i] <- 1
  } else {
    cloud[i] <- traintmp[i,]$rad_sw_in/traintmp[i,]$rad_Klimastation
  }
}

summary(cloud)
hist(cloud)

traintmp$cloudiness <- cloud
traintmp <- traintmp %>% mutate(day = if_else(rad > 0, 1, 0))
cness = traintmp$cloudiness
cloudi_daily <- traintmp %>%  group_by(doy) %>% summarise(cloudi_daily = mean(cloudiness, na.rm = TRUE))
plot(cloudi_daily)




##--- filter zero or near-zero values
# nzv = nearZeroVar(traintmp,freqCut = 90/10)
# if (length(nzv) > 0) traintmp = traintmp[, -nzv]

##--- filter correlations that are > cor_cutoff
filt = findCorrelation(cor(traintmp, use = "complete"), cutoff = 0.7)
traintmp = traintmp[,-filt]

##--- re-add the necessary variables for model training
traintmp$cst_id = id
traintmp$doy = doy
traintmp$hour = hour
traintmp$rad_klimastation = rad_klimastation
traintmp$rad_sw_in = rad_sw_in
traintmp$cloudiness = cness
traintmp$doy_hour_id = doy_hour_id
traintmp$rad = rad

##--- remove rows with NA
traintmp = traintmp[complete.cases(traintmp) ,]

##--- check manually if there are still NA values around
summary(traintmp)
sapply(traintmp, function(y) sum(length(which(is.na(y)))))

##--- define  the classes to be factor - is obligate for rr
traintmp$cst_id = as.factor(traintmp$cst_id)


# the variables rad_Klimastation and cloudiness have "spikes" in their distribution
hist(traintmp$rad_klimastation, 24)
hist(traintmp$cloudiness, 24)


## -----4 output------
saveRDS(traintmp, file.path(envrmt$path_auxdata, "trainDFmcClean.rds"))
trainDF <- readRDS(file.path(envrmt$path_auxdata, "trainDFmcClean.rds"))


## ----5 visualizations -----


ggplot(trainDFclean, aes(x = rad, y = temp))+
  geom_point( aes(color = hour))+
  facet_wrap(vars(hour))

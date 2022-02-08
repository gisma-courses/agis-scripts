#------------------------------------------------------------------------------
# Name: XXX_RF_model_building
# Author: Lena Perzlmaier
# Description:  
# 
#------------------------------------------------------------------------------

# --- 0 - load packages ----

# ---- project setup ----
require(envimaR)
# MANDANTORY: defining the root folder DO NOT change this line
root_folder = "~/edu/agis"
# work around for a subproject folder
prefix = "MOF_lidar_2018"

# define  additional packages comment if not needed
appendpackagesToLoad = c("lidR","future","lwgeom","sperrorest","doParallel","CAST")

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

trainDF = readRDS(file.path(envrmt$path_auxdata, "trainDFmcScaled.rds"))
# shapefile with tree coordinates
trainSites <- read_sf(file.path(envrmt$path_auxdata, "core_study_trees.shp"))
trainSites = st_transform(trainSites,crs = 25832)
set.seed(100)
predictors = names(trainDF[ , !(names(trainDF) %in% c("doy","hour","day","Wind_direction_10m","Wind_direction_3m","rad_klimastation","Windspeed_10m","RlNet","temp","cst_id","coverage_fraction","rel_humidity","LUpCo", "rad_sw_in","rad_lw_out","partition","doy_hour_id"))])
 
response <- "temp"


# --- 3 - start code ----

# partition data spatially with partitions_kmeans
trainSites <- filter(trainSites, trainSites$tree_id %in% trainDF$cst_id)

x_partition = as.vector(partition_kmeans(st_coordinates(trainSites), 
                                         coords = c("X", "Y"), 
                                         nfold = 5, seed1 = 1, 
                                         return_factor = TRUE,  
                                         repetition = 1, 
                                         balancing_steps = 1))[[1]]

partitionDF = data.frame(cst_id = trainSites$tree_id,
                         partition = x_partition)

trainDF = merge(trainDF, partitionDF, by = "cst_id")
trainSites = inner_join(trainSites, partitionDF, by = c("tree_id" = "cst_id"))
mapview(trainSites,zcol="partition",fgb=F)

set.seed(100)
Index <- createDataPartition(trainDF$partition, p = 0.01, list = FALSE)
trainDF_small <- trainDF[Index,]
# create space (10 cluster) time (10 days) folds
trainDF_small$timevar = substr(trainDF_small$doy_hour_id,1,2)
folds <- CreateSpacetimeFolds(trainDF_small, spacevar = "partition", k = 5,timevar = "timevar")

# forward feature selection
#cl <- makeCluster(8)
#registerDoParallel(cl)

ctrl <- trainControl(method = "cv", 
                     index = folds$index,
                     indexOut = folds$indexOut,
                     savePredictions = TRUE)

ffs_model <- CAST::ffs(trainDF_small[,predictors],
                       trainDF_small[,response],
                       method = "rf",
                       metric="RMSE",
                       importance =TRUE,
                       tuneGrid = expand.grid(mtry = 2),
                       ntree = 100,
                       trControl = ctrl)
#stopCluster(cl)

ffs_model$selectedvars


saveRDS(ffs_model, file.path(envrmt$path_auxdata, "rf_2_ffs.rds"))
ffs_model = readRDS(file.path(envrmt$path_auxdata, "rf_2_ffs.rds"))

# 2 - final hyperparameter tuning

predictors = ffs_model$selectedvars

ctrl <- trainControl(method = "cv", 
                     savePredictions = TRUE,
                     index = folds$index, 
                     indexOut = folds$indexOut)

tunegrid = expand.grid(.mtry = c(2:4))

modellist <- list()
cl <- makeCluster(30)
registerDoParallel(cl)

for (ntree in c(200, 400, 600)) {
  
  set.seed(100)

  fit = train(trainDF_small[,predictors],
              trainDF_small[,response],
              method = "rf",
              metric = "RMSE",
              ntree = ntree,
              tuneGrid = tunegrid,
              trControl = ctrl)
 
   key <- toString(ntree)
  
  modellist[[key]] <- fit
}

stopCluster(cl)

rf_tuned = resamples(modellist)
summary(rf_tuned)
plot(rf_tuned)

# best mtry is 5
# highest accuracy was calculated with ntree = 600

saveRDS(rf_tuned, file.path(envrmt$path_auxdata, "rf_3_final_tuning.rds"))
rf_tuned = readRDS(file.path(envrmt$path_auxdata,"rf_3_final_tuning.rds"))

# 4 - final model

predictors = rf_ffs$selectedvars

ctrl <- trainControl(method = "cv", 
                     savePredictions = TRUE,
                     index = folds$index, 
                     indexOut = folds$indexOut)

mtry = 3
tunegrid = expand.grid(.mtry = mtry)

ntree = 600

cl = makeCluster(16)

registerDoParallel(cl)
  
set.seed(100)
  
rf_final = train(trainDF_small[,predictors],
              trainDF_small[,response],
              method = "rf",
              metric = "RMSE",
              ntree = ntree,
              tuneGrid = tunegrid,
              trControl = ctrl)


stopCluster(cl)

rf_final

plot(varImp(rf_final))

saveRDS(rf_final, file.path(envrmt$path_auxdata, "rf_4_final.rds"))
rf_final = readRDS(file.path(envrmt$path_auxdata,"rf_4_final.rds"))

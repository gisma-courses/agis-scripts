#------------------------------------------------------------------------------
# Name: XXX_RF_model_building
# Author: Lena Perzlmaier
# Description:  
# 
#------------------------------------------------------------------------------

# --- 0 - load packages ----

library(CAST)
library(caret)
library(doParallel)
library(sperrorest)
library(dplyr)
#library(lattice)

# --- 1 - source files ----

root_folder = "D:/Uni_Marburg/Arbeit"
source(file.path(root_folder, "/MicroclimateMOF/src/000_setup.R"))

trainDF <- readRDS(file.path(envrmt$path_auxdata, "trainDFmcScaled.rds"))
trainSites <- read_sf(file.path(envrmt$path_auxdata, "core_study_trees.shp"))

# --- 2 - define variables ----

set.seed(100)
predictors = names(trainDF[ , !(names(trainDF) %in% c("temp","cst_id","coverage_fraction"))])
 
response <- "temp"

proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

cl <- makeCluster(16)


# --- 3 - start code ----

# partition data spatially with partitions_kmeans
trainSites <- st_transform(trainSites, proj4)
trainSites <- filter(trainSites, trainSites$tree_id %in% trainDF$cst_id)
xy <- as.data.frame(st_coordinates(trainSites))


x_partition = partition_kmeans(xy, coords = c("X", "Y"), nfold = 5, seed1 = 1, return_factor = TRUE)

x_partition = data.frame(cst_id = trainSites$tree_id,
                         partition = as.vector(x_partition[[1]]))

trainDF = merge(trainDF, x_partition, by = "cst_id")

# reduce size of trainDF by randomly selecting 20% of the data

set.seed(100)
Index <- createDataPartition(trainDF$partition, p = 0.2, list = FALSE)
trainDF_small <- trainDF[Index,]

# create folds

folds <- CreateSpacetimeFolds(trainDF_small, 
                              spacevar = "partition", 
                              k = 5)



# pipeline for parameter tuning and feature selection inspired from https://stats.stackexchange.com/a/323899

## --- .1 - do preliminary hyperparameter tuning ----- 

ctrl <- trainControl(method = "cv", 
                     savePredictions = TRUE,
                     index = folds$index, 
                     indexOut = folds$indexOut, 
                     search = "random")

registerDoParallel(cl)

set.seed(100)
rf_random <- train(trainDF_small[,predictors],
                   trainDF_small[,response],
                   method = "rf",
                   metric = "RMSE",
                   ntree= 200,    # 500 default of the randomforest package
                   tuneLength = 5,
                   trControl = ctrl)

stopCluster(cl)

saveRDS(rf_random, file.path(envrmt$path_model, "rf/rf_1_parameter_tuning.rds"))

print(rf_random)
plot(rf_random)

# 2 - feature selection

ctrl <- trainControl(method = "cv", 
                     savePredictions = TRUE,
                     index = folds$index, 
                     indexOut = folds$indexOut)

mtry = rf_random$bestTune$mtry
tunegrid = expand.grid(.mtry = mtry)

registerDoParallel(cl)

set.seed(100)
rf_ffs <- ffs(trainDF_small[,predictors],
              trainDF_small[,response],
              method = "rf",
              metric = "RMSE",
              ntree= 200,    # 500 default of the randomforest package
              tuneGrid = tunegrid,
              trControl = ctrl)

stopCluster(cl)

print(rf_ffs)
saveRDS(rf_ffs, file.path(envrmt$path_model, "rf/rf_2_ffs.rds"))
rf_ffs = readRDS(file.path(envrmt$path_model, "rf/rf_2_ffs.rds"))

# 3 - final hyperparameter tuning

predictors = rf_ffs$selectedvars

ctrl <- trainControl(method = "cv", 
                     savePredictions = TRUE,
                     index = folds$index, 
                     indexOut = folds$indexOut)

tunegrid = expand.grid(.mtry = c(2:8))

modellist <- list()

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
dotPlot(rf_tuned)

# best mtry is 5
# highest accuracy was calculated with ntree = 600

saveRDS(rf_tuned, file.path(envrmt$path_model, "rf/rf_3_final_tuning.rds"))
rf_tuned = readRDS(file.path(envrmt$path_model,"rf/rf_3_final_tuning.rds"))

# 4 - final model

predictors = rf_ffs$selectedvars

ctrl <- trainControl(method = "cv", 
                     savePredictions = TRUE,
                     index = folds$index, 
                     indexOut = folds$indexOut)

mtry = 5
tunegrid = expand.grid(.mtry = mtry)

ntree = 600

cl = makeCluster(8)

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

varImp(rf_final)

saveRDS(rf_final, file.path(envrmt$path_model, "rf/rf_4_final.rds"))
rf_final = readRDS(file.path(envrmt$path_model,"rf/rf_4_final.rds"))

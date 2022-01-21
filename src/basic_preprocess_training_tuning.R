#------------------------------------------------------------------------------
# basic trainingdata cleaning and tuning hints
# Type: snippet
# Name: basic_preprocess_training_tuning.R
# Author: Chris Reudenbach, creuden@gmail.com
# Dependencies: devtools::install_github("envima/envimaR")
# Date: 2021-01-15
#------------------------------------------------------------------------------


require(envimaR)
appendpackagesToLoad = c("mapview", "raster", "sf", "caret", "exactextractr",  
                         "doParallel", "CAST", "ranger","tuneRanger","mlr")


##--- Beside the caret help have a look at the following links

##--- Tuning 
## http://zevross.com/blog/2017/09/19/predictive-modeling-and-machine-learning-in-r-with-the-caret-package/#automated-and-semi-automated-parameter-tuning
## https://stackoverflow.com/questions/61151082/tunerf-vs-caret-tunning-for-random-forest oder auch 
## https://rpubs.com/phamdinhkhanh/389752 oder auch das caret Handbuch 
## https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
## https://bookdown.org/gmli64/do_a_data_science_project_in_10_days/tuning-models-parameters.html
## https://philipppro.github.io/Tuning_random_forest/
## https://mlr.mlr-org.com/articles/tutorial/measures.html

##--- Importance 
## https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/

##--- imbalance 
## https://topepo.github.io/caret/subsampling-for-class-imbalances.html
## https://www.svds.com/learning-imbalanced-classes/

## preprocessing of training data 
## https://topepo.github.io/caret/pre-processing.html
##--- for random forest especially 
## https://topepo.github.io/caret/pre-processing.html#zero--and-near-zero-variance-predictors
## https://topepo.github.io/caret/pre-processing.html#identifying-correlated-predictors
## https://topepo.github.io/caret/pre-processing.html#linear-dependencies

##--- metrics
## https://stats.stackexchange.com/a/312787/1352
## https://stats.stackexchange.com/questions/368949/example-when-using-accuracy-as-an-outcome-measure-will-lead-to-a-wrong-conclusio


## --- START
##------------------------------------------------------------------------------
##---  cleaning the training data tuning the parameters derive models & classify
## note the response and predictor names as well as the data frame positions 
## MUST be adapted to yyour current dataset
rawtrainDF =readRDS("your raw traindata dataset")


#--- define seed for all operations
seed=12345

##--- balance the data to the smallest training class
downTrainDF <- downSample(x = rawtrainDF[, -ncol(rawtrainDF)],
                          y = as.factor(rawtrainDF$class))

# create data partition
trainids = createDataPartition(downTrainDF$class,list = FALSE,p = 0.001)
trainDF =  downTrainDF[trainids,]

##---  cleaning the training data for a random forest model training
##--- remove NA and linear combos 
traintmp = trainDF[ , !(names(trainDF) %in% c("class","OBJ_ID","Class"))]
tDF = trainDF[ , !(names(trainDF) %in% c("class","OBJ_ID"))]
##--- filter zero or near-zero values
nzv = nearZeroVar(traintmp)
if (length(nzv) > 0) traintmp = traintmp[, -nzv]

##--- filter correlations that are > cor_cutoff
filt = findCorrelation(cor(traintmp, use = "complete"), cutoff = 0.9)
traintmp = traintmp[,-filt]

##--- re-add the necessary variables for model training
traintmp$class = trainDF$class

##--- remove rows with NA
traintmp = traintmp[complete.cases(traintmp) ,]
traintmp = traintmp[ ,rowSums(is.na(traintmp)) == 0]

##--- now find and eventually remaining linear combinations of columns
cinfo = findLinearCombos(traintmp[, which(!names(traintmp) %in% c("class"))])
if (!is.null(cinfo$remove)) traintmp = traintmp[, -cinfo$remove]

##--- check manually if there are still NA values around
summary(traintmp)
sapply(traintmp, function(y) sum(length(which(is.na(y)))))
tDF=traintmp

## ----  cleaning the training data for a random forest model training
# saveRDS(tDF,paste0(envrmt$path_data,"clean_TDF_seed12345_p0_1.rds"))

##--- define  the classes to be factor - is obligate for rr
tDF$class <- as.factor(tDF$Class)

##--- Define pred&respo
predictors = tDF[,1:ncol(tDF)-1]
response = tDF[,"Class"]

##------------------------
##--- Tuning Tuning Tuning

##--- classical tune approach via caret & random search using some metrics
## random search is best if we know nothing
control <- trainControl(method="repeatedcv", number=10, repeats=5,search="random")
metrics <- c("Kappa","Accuracy")
rf=list()
set.seed(seed)
mtry <- floor(sqrt(ncol(predictors)))
for (metric in metrics){
  rf[[metric]] <- caret::train(class ~., data=tDF, method="rf", metric=metric, trControl=control,tuneLength=ncol(tDF)-1)
}
rf
print(rf[[1]])
plot(rf[[1]])
print(rf[[2]])
plot(rf[[2]])

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(floor(sqrt(ncol(predictors))):(ncol(tDF) - 1)))
rf_gridsearch <- train(class~., data=tDF, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

##--- ranger tune approach wich provides in common a higher performing in tuning the parameters
## essentially this doesn't mean the resulting model will be better!
mr.task = makeClassifTask(data = tDF, target = "class")
r.tune_full_kappa = tuneRanger::tuneRanger(mr.task, 
                                measure = list(kappa),
                                num.trees = 500, 
                                iters = 70, 
                                save.file.path = NULL)
r.tune_full_kappa$recommended.pars

r.tune$results$


##--- making model making model making model

##---  RANDOM FOREST via caret and ranger
# create 1D for the above tuned features
tgrid <- expand.grid(
  mtry = 1 ,  # r.tune$recommended.pars[1], # 1
  splitrule = "gini",
  min.node.size = 2  #r.tune$recommended.pars[[2]] # 2
)


##--- ranger via caret
n_cores <- detectCores() - 2 
cl <- makeCluster(n_cores)
registerDoParallel(cl)

set.seed(seed)
# control the parameters of the train function
ctrl <- trainControl(method="cv",
                     number = 10, #  number of folds
                     savePredictions = TRUE,
                     allowParallel = TRUE)

# train the model
ranger_model <- caret::train(predictors,
                             response,
                             method = "ranger",
                             trControl = ctrl,
                             tuneGrid = tgrid,
                             metric = "Kappa",
                             num.trees = 500,
                             importance = "permutation")


# stop parallel cluster
stopCluster(cl)


##--- rf via caret 

# train model
n_cores <- detectCores() - 2 
cl <- makeCluster(n_cores)
registerDoParallel(cl)
set.seed(seed)
ctrl <- trainControl(method="cv",
                     number = 10, #  number of folds
                     verbose         = TRUE,
                     classProbs      = FALSE,
                     returnResamp    = "all",
                     savePredictions = TRUE)

rf_model = caret::train(predictors,
                        response,
                        method = "rf",
                        metric = "Kappa",
                        trControl = ctrl,
                        returnResamp = "all",
                        importance =TRUE,
                        tuneLength = length(predictors) - 1
)

stopCluster(cl)

##--- Predict Predict Predict

# ranger
prediction_ranger <- raster::predict(rgbStack, ranger_model, na.rm = TRUE,progress = "text")
# reclass it to sealed/other
ranger_rec=raster::reclassify(prediction_ranger, c(0,2,0, 2,3,1))
# map
tm_shape(ranger_rec) +
  tm_raster(style = "cat", palette = c("gray","red"))

# random forest
prediction_rf <- raster::predict(rgbStack, rf_model, na.rm = TRUE,progress = "text")
# reclass it to sealed/other
rf_rec=raster::reclassify(prediction_rf, c(0,2,0, 2,3,1))
# map
tm_shape(rf_rec) +
  tm_raster(style = "cat", palette = c("gray","red"))

## pretty similar
tm_shape(ranger_rec-rf_rec)

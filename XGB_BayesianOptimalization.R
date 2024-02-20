source("Functions.R")
#How many hours prior to be analyzed and what is the maximum hours of open position?
#analyse #open
DataCleaning(analyse = 10, open = 20)


#Which dates are for Training, and which are for Live back-testing?
#Train #Live ( 0 means from the very beginning OR until the very last day)
SplitDate(data = modeldata, trainFrom = 0, trainUntil = "2021-10-10", liveFrom = "2021-10-10", liveUntil = "2022-12-30", analyse = analyse)


params <- list(
  objective = "multi:softprob",
  num_class = 5,  
  booster = "gbtree",
  eval_metric = "mlogloss", 
  max_depth = 100,
  eta = 1,
  gamma = 1,
  subsample = 1,
  colsample_bytree = 1
)

set.seed(1)
xgb_model <- xgboost(
  params = params,
  data = train_data_matrix,
  label = train_data$YieldCategory,
  nrounds = 100
)

prediction <- predict(xgb_model, newdata = test_data_matrix)

matrixTest<-data.frame(matrix((NA),nrow(test_data),(level_n+2)))
colnames(matrixTest)<- c(levels,"prediction", "actual")
k<-1
for (j in 1:nrow(matrixTest)) {
  for (i in 1:5) {
    matrixTest[j,i]<- prediction[k]
    k<-k+1
  }
}
matrixTest$actual<-test_y
colnames(matrixTest[matrixTest[1,1:5]==max(matrixTest[1,1:5]),])
for (i in 1:nrow(matrixTest)){
  matrixTest[i,6]<-colnames(matrixTest)[which.max(matrixTest[i,1:5])]
}

matrixTest$GoodPred<- matrixTest$prediction == matrixTest$actual


matrixTest$prediction<- as.factor(matrixTest$prediction)
confusionMatrix(matrixTest$prediction, matrixTest$actual)

#################################
library(xgboost)
library(caret)
library(ParBayesianOptimization)
library(doParallel)
scoringFunction <- function(max_depth, eta, gamma, subsample, colsample_bytree) {
  
  params <- list(
    objective = "multi:softprob",  
    num_class = 5, 
    booster = "gbtree",
    eval_metric = "mlogloss",  
    max_depth = max_depth,
    eta = eta,
    gamma = gamma,
    subsample = subsample,
    colsample_bytree = colsample_bytree
  )
  
  xgbcv <- xgb.cv(
    params = params,
    data = train_data_matrix,
    label = train_y_num,
    nrounds = 100,
    prediction = TRUE,
    showsd = TRUE,
    early_stopping_rounds = 5,
    maximize = FALSE,
    verbose = 0,
    nfold = 3)
  
  return(
    list( 
      Score = -min(xgbcv$evaluation_log$train_mlogloss_mean),
      nrounds = xgbcv$best_ntreelimit
    )
  )
}

###########################################################################
################# B A Y E S I A N   O P T I M A L I Z A T I O N  ##########



bounds <- list( 
  max_depth = c(5L, 200L),
  eta = c(0.01, 3),
  gamma = c(0, 5),
  subsample = c(0.6, 1),
  colsample_bytree = c(0.6, 1)
)

cl<-makeCluster(8)
registerDoParallel(cl)
clusterEvalQ(cl,{
  library(xgboost)
})
clusterExport(cl, c("train_data_matrix","train_y","train_y_num"))

set.seed(1)
optObj <- bayesOpt(
  FUN = scoringFunction,
  bounds = bounds,
  initPoints = 8,
  iters.n = 16,
  kappa = 2.576,
  acq = "ucb",
  parallel = TRUE,
  iters.k = 2,
  plotProgress = F,
  verbose = 2
)
stopCluster(cl)
registerDoSEQ()

BestParameters<-data.frame(optObj$scoreSummary[optObj$scoreSummary$Score == max(optObj$scoreSummary$Score)][1,c(3:7,13)])

params <- list(
  objective = "multi:softprob",
  num_class = 5,  
  booster = "gbtree",
  eval_metric = "mlogloss", 
  max_depth = BestParameters[1,1],
  eta = BestParameters[1,2],
  gamma = BestParameters[1,3],
  subsample = BestParameters[1,4],
  colsample_bytree = BestParameters[1,5]
)

set.seed(1)
xgb_model <- xgboost(
  params = params,
  data = train_data_matrix,
  label = train_data$YieldCategory,
  nrounds = BestParameters[1,6]
)

predictionOpt <- predict(xgb_model, newdata = test_data_matrix)

matrixOpt<-data.frame(matrix((NA),nrow(test_data),(level_n+2)))
colnames(matrixOpt)<- c(levels,"prediction", "actual")
k<-1
for (j in 1:nrow(matrixOpt)) {
  for (i in 1:5) {
    matrixOpt[j,i]<- predictionOpt[k]
    k<-k+1
  }
}
matrixOpt$actual<-test_y
colnames(matrixOpt[matrixOpt[1,1:5]==max(matrixOpt[1,1:5]),])
for (i in 1:nrow(matrixOpt)){
  matrixOpt[i,6]<-colnames(matrixOpt)[which.max(matrixOpt[i,1:5])]
}

matrixOpt$GoodPred<- matrixOpt$prediction == matrixOpt$actual


matrixOpt$prediction<- as.factor(matrixOpt$prediction)
confusionMatrix(matrixOpt$prediction, matrixOpt$actual)

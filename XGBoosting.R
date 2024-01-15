#LIBARIES-------------------------------------------------------------------------------------------------------------------
library(xgboost)
library(Metrics)
library(caret)
library(rpart)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(pdp)

############################################# TESLA STOCK ###################################################
#DATA IMPORT--------------------------------------------------------------------------------------------------
TSLA <- read.csv("C:/Users/gergo/Desktop/Stocks/AnalysisStocks/TSLA/TESLA14-23.csv", sep=";")
TSLA$X.DATE. <- as.Date(as.character(TSLA$X.DATE.), format = "%Y%m%d")
TSLA$X.TIME. <- paste0("X", TSLA$X.TIME.)

colnames<-c("Date", "X93000", "X93100", "X93200", "X93300", "X93400", "X93500", "X93600", "X93700", "X93800",
            "X93900", "X94000", "X94100", "X94200", "X94300", "X94400", "X94500", "X94600", "X94700", "X94800",
            "X94900", "X95000", "X95100", "X95200", "X95300", "X95400", "X95500", "X95600", "X95700", "X95800",
            "X95900", "X100000", "X100100", "X100200", "X100300", "X100400", "X100500", "X100600", "X100700", "X100800",
            "X100900", "X101000", "X101100", "X101200", "X101300", "X101400", "X101500", "X101600", "X101700", "X101800",
            "X101900", "X102000", "X102100", "X102200", "X102300", "X102400", "X102500", "X102600", "X102700", "X102800",
            "X102900", "X103000", "X103100", "X103200", "X103300", "X103400", "X103500", "X103600", "X103700", "X103800",
            "X103900", "X104000", "X104100", "X104200", "X104300", "X104400", "X104500", "X104600", "X104700", "X104800",
            "X104900", "X105000", "X105100", "X105200", "X105300", "X105400", "X105500", "X105600", "X105700", "X105800",
            "X105900", "X110000", "X110100", "X110200", "X110300", "X110400", "X110500", "X110600", "X110700", "X110800",
            "X110900", "X111000", "X111100", "X111200", "X111300", "X111400", "X111500", "X111600", "X111700", "X111800",
            "X111900", "X112000", "X112100", "X112200", "X112300", "X112400", "X112500", "X112600", "X112700", "X112800",
            "X112900", "X113000", "X113100", "X113200", "X113300", "X113400", "X113500", "X113600", "X113700", "X113800",
            "X113900", "X114000", "X114100", "X114200", "X114300", "X114400", "X114500", "X114600", "X114700", "X114800",
            "X114900", "X115000", "X115100", "X115200", "X115300", "X115400", "X115500", "X115600", "X115700", "X115800",
            "X115900", "X120000", "X120100", "X120200", "X120300", "X120400", "X120500", "X120600", "X120700", "X120800",
            "X120900", "X121000", "X121100", "X121200", "X121300", "X121400", "X121500", "X121600", "X121700", "X121800",
            "X121900", "X122000", "X122100", "X122200", "X122300", "X122400", "X122500", "X122600", "X122700", "X122800",
            "X122900", "X123000", "X123100", "X123200", "X123300", "X123400", "X123500", "X123600", "X123700", "X123800",
            "X123900", "X124000", "X124100", "X124200", "X124300", "X124400", "X124500", "X124600", "X124700", "X124800",
            "X124900", "X125000", "X125100", "X125200", "X125300", "X125400", "X125500", "X125600", "X125700", "X125800",
            "X125900", "X130000", "X130100", "X130200", "X130300", "X130400", "X130500", "X130600", "X130700", "X130800",
            "X130900", "X131000", "X131100", "X131200", "X131300", "X131400", "X131500", "X131600", "X131700", "X131800",
            "X131900", "X132000", "X132100", "X132200", "X132300", "X132400", "X132500", "X132600", "X132700", "X132800",
            "X132900", "X133000", "X133100", "X133200", "X133300", "X133400", "X133500", "X133600", "X133700", "X133800",
            "X133900", "X134000", "X134100", "X134200", "X134300", "X134400", "X134500", "X134600", "X134700", "X134800",
            "X134900", "X135000", "X135100", "X135200", "X135300", "X135400", "X135500", "X135600", "X135700", "X135800",
            "X135900", "X140000", "X140100", "X140200", "X140300", "X140400", "X140500", "X140600", "X140700", "X140800",
            "X140900", "X141000", "X141100", "X141200", "X141300", "X141400", "X141500", "X141600", "X141700", "X141800",
            "X141900", "X142000", "X142100", "X142200", "X142300", "X142400", "X142500", "X142600", "X142700", "X142800",
            "X142900", "X143000", "X143100", "X143200", "X143300", "X143400", "X143500", "X143600", "X143700", "X143800",
            "X143900", "X144000", "X144100", "X144200", "X144300", "X144400", "X144500", "X144600", "X144700", "X144800",
            "X144900", "X145000", "X145100", "X145200", "X145300", "X145400", "X145500", "X145600", "X145700", "X145800",
            "X145900", "X150000", "X150100", "X150200", "X150300", "X150400", "X150500", "X150600", "X150700", "X150800",
            "X150900", "X151000", "X151100", "X151200", "X151300", "X151400", "X151500", "X151600", "X151700", "X151800",
            "X151900", "X152000", "X152100", "X152200", "X152300", "X152400", "X152500", "X152600", "X152700", "X152800",
            "X152900", "X153000", "X153100", "X153200", "X153300", "X153400", "X153500", "X153600", "X153700", "X153800",
            "X153900", "X154000", "X154100", "X154200", "X154300", "X154400", "X154500", "X154600", "X154700", "X154800",
            "X154900", "X155000", "X155100", "X155200", "X155300", "X155400", "X155500", "X155600", "X155700", "X155800",
            "X155900")



#DATA MANIPULATION-------------------------------------------------------------------------------
#Minute-Open Prices------------------------------------------------------------------------------

TSLArownum<- length(unique(TSLA$X.DATE.))
TSLAcolnum<- length(colnames)
TSLAPriceOpen<- data.frame(matrix(NA, TSLArownum, TSLAcolnum))

colnames(TSLAPriceOpen)<- colnames
dates <- unique(TSLA$X.DATE.)
TSLAPriceOpen$Date <- dates

for (i in 1:nrow(TSLA)) {
  date<-TSLA[i,1]
  time<-TSLA[i,2]
  price<-TSLA[i,3]
  row<- which(TSLAPriceOpen$Date == date)
  col<-which(colnames(TSLAPriceOpen)== time)
  TSLAPriceOpen[row,col] <- price
}

sum(is.na(TSLAPriceOpen))

TSLAFilledOpen <- TSLAPriceOpen[, 2:ncol(TSLAPriceOpen)]
for (i in 1:nrow(TSLAPriceOpen)) {
  rowvalues <- TSLAPriceOpen[i, 2:ncol(TSLAPriceOpen)]
  naindex <- which(!is.na(rowvalues))
  interpolatedvalues <- approx(x = naindex, y = rowvalues[naindex], xout = 1:length(rowvalues))$y
  TSLAFilledOpen[i, ] <- interpolatedvalues
}

TSLAPriceOpen<- cbind(TSLAPriceOpen$Date,TSLAFilledOpen)
sum(is.na(TSLAPriceOpen))

TSLAPriceOpen<-na.omit(TSLAPriceOpen)
sum(is.na(TSLAPriceOpen))

colnames(TSLAPriceOpen)[1]<- "Date"

#Minute-Lowest Prices------------------------------------------------------------------------------
TSLAPriceLow<- data.frame(matrix(NA, TSLArownum, TSLAcolnum))

colnames(TSLAPriceLow)<- colnames
dates <- unique(TSLA$X.DATE.)
TSLAPriceLow$Date <- dates

for (i in 1:nrow(TSLA)) {
  date<-TSLA[i,1]
  time<-TSLA[i,2]
  price<-TSLA[i,5]
  row<- which(TSLAPriceLow$Date == date)
  col<-which(colnames(TSLAPriceLow)== time)
  TSLAPriceLow[row,col] <- price
}

sum(is.na(TSLAPriceLow))

TSLAFilledLow <- TSLAPriceLow[, 2:ncol(TSLAPriceLow)]
for (i in 1:nrow(TSLAPriceLow)) {
  rowvalues <- TSLAPriceLow[i, 2:ncol(TSLAPriceLow)]
  naindex <- which(!is.na(rowvalues))
  interpolatedvalues <- approx(x = naindex, y = rowvalues[naindex], xout = 1:length(rowvalues))$y
  TSLAFilledLow[i, ] <- interpolatedvalues
}

TSLAPriceLow<- cbind(TSLAPriceLow$Date,TSLAFilledLow)
sum(is.na(TSLAPriceLow))

TSLAPriceLow<-na.omit(TSLAPriceLow)
sum(is.na(TSLAPriceLow))

colnames(TSLAPriceLow)[1]<- "Date"

#Minute-Highest Prices------------------------------------------------------------------------------
TSLAPriceHigh<- data.frame(matrix(NA, TSLArownum, TSLAcolnum))

colnames(TSLAPriceHigh)<- colnames
dates <- unique(TSLA$X.DATE.)
TSLAPriceHigh$Date <- dates

for (i in 1:nrow(TSLA)) {
  date<-TSLA[i,1]
  time<-TSLA[i,2]
  price<-TSLA[i,4]
  row<- which(TSLAPriceHigh$Date == date)
  col<-which(colnames(TSLAPriceHigh)== time)
  TSLAPriceHigh[row,col] <- price
}

sum(is.na(TSLAPriceHigh))

TSLAFilledHigh <- TSLAPriceHigh[, 2:ncol(TSLAPriceHigh)]
for (i in 1:nrow(TSLAPriceHigh)) {
  rowvalues <- TSLAPriceHigh[i, 2:ncol(TSLAPriceHigh)]
  naindex <- which(!is.na(rowvalues))
  interpolatedvalues <- approx(x = naindex, y = rowvalues[naindex], xout = 1:length(rowvalues))$y
  TSLAFilledHigh[i, ] <- interpolatedvalues
}

TSLAPriceHigh<- cbind(TSLAPriceHigh$Date,TSLAFilledHigh)
sum(is.na(TSLAPriceHigh))

TSLAPriceHigh<-na.omit(TSLAPriceHigh)
sum(is.na(TSLAPriceHigh))

colnames(TSLAPriceHigh)[1]<- "Date"

sum((TSLAPriceHigh$Date == TSLAPriceLow$Date)== F)


#DATA PREPARATION------------------------------------------------------------------------------

TSLAFirstHalf<-data.frame(matrix(NA,2492,195))
for (i in 1:nrow(TSLAPriceOpen)) {
  for (j in 3:197) {
    TSLAFirstHalf[i,j-2]<- (TSLAPriceOpen[i,j]/TSLAPriceOpen[i,j-1])*100-100
  }
}

for (i in 1:nrow(TSLAPriceHigh)) {
  TSLAFirstHalf$Target[i]<- (max(TSLAPriceHigh[i,(ncol(TSLAPriceHigh)/2):ncol(TSLAPriceHigh)])/TSLAPriceOpen[i,ncol(TSLAPriceOpen)/2])*100-100
}

TSLAData <- cbind(TSLAPriceOpen$Date,TSLAFirstHalf)
colnames(TSLAData)[1]<- "Date"

TSLAResults<- data.frame(matrix(NA,3,1))
rownames(TSLAResults)<- c("MAE", "RSME", "R2")

TSLAtrain_data = TSLAData[1:2242,-1]
TSLAtest_data = TSLAData[2243:2492,-1]
TSLAtrain_y <- TSLAtrain_data$Target
TSLAtest_y <- TSLAtest_data$Target

#BUILDING MODELS-----------------------------------------------------------------------------
#DEFAULT XGB MODEL---------------------------------------------------------------------------
set.seed(1)
TSLAxgb_model_default <- xgboost(data = as.matrix(TSLAtrain_data[1:195]), label = TSLAtrain_data$Target, nrounds = 100)
TSLApredictions_default <- predict(TSLAxgb_model_default, newdata = as.matrix(TSLAtest_data[1:195]))

TSLAMAE_xgb_default <- mean(abs(TSLApredictions_default - TSLAtest_y))
TSLARMSE_xgb_default <- sqrt(mean((TSLApredictions_default - TSLAtest_y)^2))
TSLAR2_xgb_default <- 1 - sum((TSLAtest_y - TSLApredictions_default)^2) / sum((TSLAtest_y - mean(TSLAtest_y))^2)


#TESTING THE STRATEGY-------------------------------------------------------------------------
TSLACompare_xgb_def<- data.frame(cbind(TSLAPriceOpen[2243:2492,c(1,195)],TSLApredictions_default,TSLAtest_y))
TSLACompare_xgb_def<-TSLACompare_xgb_def[,-2]
TSLACompare_xgb_def$Residual<- TSLACompare_xgb_def$TSLApredictions_default-TSLACompare_xgb_def$TSLAtest_y
TSLACompare_xgb_def$BuyPrice<- TSLAPriceOpen[2243:2492,195]
for (i in 1:nrow(TSLACompare_xgb_def)) {
  df<-TSLAPriceHigh[2243:2492,195:391]
  TSLACompare_xgb_def$MaxPrice[i]<-max(df[i,])
}
TSLACompare_xgb_def$Grow<-(TSLACompare_xgb_def$MaxPrice/TSLACompare_xgb_def$BuyPrice)*100-100
TSLACompare_xgb_def$CloseAvg<-(TSLAPriceHigh[2243:2492,391]+TSLAPriceLow[2243:2492,391])/2
TSLACompare_xgb_def$PredictedSell<- (((TSLACompare_xgb_def$TSLApredictions_default-0)/100)*TSLACompare_xgb_def$BuyPrice)+TSLACompare_xgb_def$BuyPrice
TSLACompare_xgb_def$CloseDeal<- NA
for (i in 1:nrow(TSLACompare_xgb_def)) {
  TSLACompare_xgb_def$CloseDeal[i]<- if(TSLACompare_xgb_def$MaxPrice[i]>TSLACompare_xgb_def$PredictedSell[i]){TRUE}else{FALSE}
}
TSLACompare_xgb_def$ClosePercentage<-(TSLACompare_xgb_def$CloseAvg/TSLACompare_xgb_def$BuyPrice)*100-100
TSLACompare_xgb_def$Profit<- NA
for (i in 1:nrow(TSLACompare_xgb_def)) {
  if (TSLACompare_xgb_def$CloseDeal[i] == TRUE) {TSLACompare_xgb_def$Profit[i]<- ((TSLACompare_xgb_def$TSLApredictions_default[i]-0)/100)*1000}
  else{TSLACompare_xgb_def$Profit[i]<- (TSLACompare_xgb_def$ClosePercentage[i]/100)*1000}
}
sum(TSLACompare_xgb_def$Profit,na.rm = T)

sum(TSLACompare_xgb_def$CloseDeal == T)/250
sum(TSLACompare_xgb_def$TSLApredictions > 1.3 & TSLACompare_xgb_def$CloseDeal == T)

#TUNING XGB MODEL-----------------------------------------------------------------------------
TSLAparam_grid <- list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eval_metric = "rmse",
  max_depth = c(6,9,12,15),
  eta = c(0.01,0.05,0.09,0.13,0.16),
  gamma = c(1,2),
  subsample = c(0.8,0.9,1),
  colsample_bytree = c(1),
  nrounds = c(100,200,300)
)

TSLAresults <- list()
for (depth in TSLAparam_grid$max_depth) {
  for (eta in TSLAparam_grid$eta) {
    for (gamma in TSLAparam_grid$gamma) {
      for (subsample in TSLAparam_grid$subsample) {
        for (colsample_bytree in TSLAparam_grid$colsample_bytree) {
          for (nrounds in TSLAparam_grid$nrounds) {
            params <- list(
              objective = TSLAparam_grid$objective,
              booster = TSLAparam_grid$booster,
              eval_metric = TSLAparam_grid$eval_metric,
              max_depth = depth,
              eta = eta,
              gamma = gamma,
              subsample = subsample,
              colsample_bytree = colsample_bytree
            )
            cat("Parameters for Iteration:", 
                "depth =", depth,
                "eta =", eta,
                "gamma =", gamma,
                "subsample =", subsample,
                "colsample_bytree =", colsample_bytree,
                "nrounds =", nrounds, "\n")
            
            
            TSLAcv_results <- xgb.cv(
              params = params,
              data = xgb.DMatrix(as.matrix(TSLAtrain_data[1:195]), label = TSLAtrain_y),
              nrounds = nrounds,
              nfold = 20,
              early_stopping_rounds = 10,
              verbose = TRUE
            )
            
            result_key <- paste("depth", depth, "_eta", eta, "_gamma", gamma, "_subsample", subsample, "_colsample", colsample_bytree, "_nrounds", nrounds)
            TSLAresults[[result_key]] <- TSLAcv_results$evaluation_log
          }
        }
      }
    }
  }
}
TSLAbest_params<- names(TSLAresults)[which.min(sapply(TSLAresults, function(x) min(x$test_rmse_mean)))]


#TUNED XGB MODEL-----------------------------------------------------------------------------
params <- list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eval_metric = "rmse",
  max_depth = 12,
  eta = 0.01,
  gamma = 1,
  subsample = 0.8,
  colsample_bytree = 1
)

set.seed(1)
TSLAxgb_model <- xgboost(params = params, data = as.matrix(TSLAtrain_data[1:195]), label = TSLAtrain_data$Target,nrounds = 300)

TSLApredictions <- predict(TSLAxgb_model, newdata = as.matrix(TSLAtest_data[1:195]))

TSLAMAE_xgb <- mae(TSLApredictions, TSLAtest_y)
TSLARMSE_xgb <- round(sqrt(mean((TSLApredictions - TSLAtest_y)^2)), digits = 2)


TSLAmean_y <- mean(TSLAtest_y)
TSLAsst_xgb <- sum((TSLAtest_y - TSLAmean_y)^2)
TSLAssr_xgb <- sum((TSLAtest_y - TSLApredictions)^2)
TSLAR2_xgb <- 1 - (TSLAssr_xgb / TSLAsst_xgb)

TSLAResults[1,1]<- TSLAMAE_xgb
TSLAResults[2,1]<- TSLARMSE_xgb
TSLAResults[3,1]<- TSLAR2_xgb
colnames(TSLAResults)<- "Value"

#TESTING THE STRATEGY-------------------------------------------------------------------------
TSLACompare_xgb<- data.frame(cbind(TSLAPriceOpen[2243:2492,c(1,195)],TSLApredictions,TSLAtest_y))
TSLACompare_xgb<-TSLACompare_xgb[,-2]
TSLACompare_xgb$Residual<- TSLACompare_xgb$TSLApredictions-TSLACompare_xgb$TSLAtest_y
TSLACompare_xgb$BuyPrice<- TSLAPriceOpen[2243:2492,195]
for (i in 1:nrow(TSLACompare_xgb)) {
  df<-TSLAPriceHigh[2243:2492,195:391]
  TSLACompare_xgb$MaxPrice[i]<-max(df[i,])
}
TSLACompare_xgb$Grow<-(TSLACompare_xgb$MaxPrice/TSLACompare_xgb$BuyPrice)*100-100
TSLACompare_xgb$CloseAvg<-(TSLAPriceHigh[2243:2492,391]+TSLAPriceLow[2243:2492,391])/2
TSLACompare_xgb$PredictedSell<- (((TSLACompare_xgb$TSLApredictions-0)/100)*TSLACompare_xgb$BuyPrice)+TSLACompare_xgb$BuyPrice
TSLACompare_xgb$CloseDeal<- NA
for (i in 1:nrow(TSLACompare_xgb)) {
  TSLACompare_xgb$CloseDeal[i]<- if(TSLACompare_xgb$MaxPrice[i]>TSLACompare_xgb$PredictedSell[i]){TRUE}else{FALSE}
}
TSLACompare_xgb$ClosePercentage<-(TSLACompare_xgb$CloseAvg/TSLACompare_xgb$BuyPrice)*100-100

TSLACompare_xgb$Profit<- NA
for (i in 1:nrow(TSLACompare_xgb)) {
  if (TSLACompare_xgb$CloseDeal[i] == TRUE) {TSLACompare_xgb$Profit[i]<- ((TSLACompare_xgb$TSLApredictions[i]-0)/100)*1000}
  else{TSLACompare_xgb$Profit[i]<- (TSLACompare_xgb$ClosePercentage[i]/100)*1000}
}
sum(TSLACompare_xgb$Profit,na.rm = T)
sum(TSLACompare_xgb$CloseDeal == T)/250
sum(TSLACompare_xgb$TSLApredictions > 1.3 & TSLACompare_xgb$CloseDeal == T)

#LINEAR REGRESSION-------------------------------------------------------------------
set.seed(1)
TSLAlinear_model <- lm(Target ~ ., data = TSLAtrain_data)
TSLApredictions_linear <- predict(TSLAlinear_model, newdata = TSLAtest_data[1:195])

TSLAMAE_linear <- mae(TSLApredictions_linear, TSLAtest_y)
TSLARMSE_linear <- round(sqrt(mean((TSLApredictions_linear - TSLAtest_y)^2)), digits = 2)

TSLAmean_y_linear <- mean(TSLAtest_y)
TSLAsst_linear <- sum((TSLAtest_y - TSLAmean_y_linear)^2)
TSLAssr_linear <- sum((TSLAtest_y - TSLApredictions_linear)^2)
TSLAR2_linear <- 1 - (TSLAssr_linear / TSLAsst_linear)

TSLAResults_linear <- data.frame(Value = c(TSLAMAE_linear, TSLARMSE_linear, TSLAR2_linear), row.names = c("MAE", "RSME", "R2"))

#TESTING THE STRATEGY-------------------------------------------------------------------------
TSLAComparison_linear<- data.frame(cbind(TSLAPriceOpen[2243:2492,c(1,195)],TSLApredictions_linear,TSLAtest_y))
TSLAComparison_linear<-TSLAComparison_linear[,-2]
TSLAComparison_linear$Residual<- TSLAComparison_linear$TSLApredictions_linear-TSLAComparison_linear$TSLAtest_y
TSLAComparison_linear$BuyPrice<- TSLAPriceOpen[2243:2492,195]
for (i in 1:nrow(TSLAComparison_linear)) {
  df<-TSLAPriceHigh[2243:2492,195:391]
  TSLAComparison_linear$MaxPrice[i]<-max(df[i,])
}
TSLAComparison_linear$Grow<-(TSLAComparison_linear$MaxPrice/TSLAComparison_linear$BuyPrice)*100-100
TSLAComparison_linear$CloseAvg<-(TSLAPriceHigh[2243:2492,391]+TSLAPriceLow[2243:2492,391])/2
TSLAComparison_linear$PredictedSell<- (((TSLAComparison_linear$TSLApredictions_linear-0)/100)*TSLAComparison_linear$BuyPrice)+TSLAComparison_linear$BuyPrice
TSLAComparison_linear$CloseDeal<- NA
for (i in 1:nrow(TSLAComparison_linear)) {
  TSLAComparison_linear$CloseDeal[i]<- if(TSLAComparison_linear$MaxPrice[i]>TSLAComparison_linear$PredictedSell[i]){TRUE}else{FALSE}
}
TSLAComparison_linear$ClosePercentage<-(TSLAComparison_linear$CloseAvg/TSLAComparison_linear$BuyPrice)*100-100

TSLAComparison_linear$Profit<- NA
for (i in 1:nrow(TSLAComparison_linear)) {
  if (TSLAComparison_linear$TSLApredictions_linear[i]>0 ) {
    if (TSLAComparison_linear$CloseDeal[i] == TRUE) {TSLAComparison_linear$Profit[i]<- ((TSLAComparison_linear$TSLApredictions_linear[i]-0)/100)*1000}
    else{TSLAComparison_linear$Profit[i]<- (TSLAComparison_linear$ClosePercentage[i]/100)*1000}
  }else{TSLAComparison_linear$Profit[i]<-0}
}
sum(TSLAComparison_linear$Profit,na.rm = T)
sum(TSLAComparison_linear$CloseDeal == T)/(sum(TSLAComparison_linear$CloseDeal == T)+sum(TSLAComparison_linear$CloseDeal == F))

#SUMMARY-------------------------------------------------------------------------
TSLASummary<-data.frame(matrix(NA,3,4))
colnames(TSLASummary)<- c("MAE", "RMSE", "R2", "PROFIT")
rownames(TSLASummary)<- c("Linear", "Default Boosting", "Tuned Boosting")
TSLASummary[1,1]<-TSLAMAE_linear
TSLASummary[1,2]<-TSLARMSE_linear
TSLASummary[1,3]<-TSLAR2_linear
TSLASummary[1,4]<-sum(TSLAComparison_linear$Profit,na.rm = T)
TSLASummary[2,1]<-TSLAMAE_xgb_default
TSLASummary[2,2]<-TSLARMSE_xgb_default
TSLASummary[2,3]<-TSLAR2_xgb_default
TSLASummary[2,4]<-sum(TSLACompare_xgb_def$Profit,na.rm = T)
TSLASummary[3,1]<-TSLAMAE_xgb
TSLASummary[3,2]<-TSLARMSE_xgb
TSLASummary[3,3]<-TSLAR2_xgb
TSLASummary[3,4]<-sum(TSLACompare_xgb$Profit,na.rm = T)


#PLOTS---------------------------------------------------------------------------
par(mfrow = c(1, 2))
importance_matrix_xgb_def <- xgb.importance(model = TSLAxgb_model_default)
xgb.plot.importance(importance_matrix_xgb_def[1:10,], main = "Top 10 Variables for Default XGB", cex.axis = 0.8)

importance_matrix_xgb <- xgb.importance(model = TSLAxgb_model)
xgb.plot.importance(importance_matrix_xgb[1:10,], main = "Top 10 Variables for Tuned XGB", cex.axis = 0.8)

par(mfrow = c(1, 2))
plot(TSLApredictions_default, TSLAtest_y, main = "Default XGBoost: Predicted vs. Actual", 
     xlab = "Predicted Values", ylab = "Actual Values", 
     cex.lab = 1.2, cex.axis = 1.2, 
     xlim = c(0, 4), ylim = c(0, 5))
abline(a = 0, b = 1, col = "red")
abline(v = 1.3, col = "blue")
plot(TSLApredictions, TSLAtest_y, main = "Tuned XGBoost: Predicted vs. Actual", 
     xlab = "Predicted Values", ylab = "Actual Values", 
     cex.lab = 1.2, cex.axis = 1.2, 
     xlim = c(0, 4), ylim = c(0, 5))
abline(a = 0, b = 1, col = "red")
abline(v = 1.3, col = "blue")

plot(TSLApredictions_linear, TSLAtest_y, main = " SLR: Predicted vs. Actual", xlab = "Predicted Values", ylab = "Actual Values")
abline(a = 0, b = 1, col = "red")









library(randomForest)
library(caret)
library(pROC)
library(kableExtra)


#LOAD
load("C:/Users/gergo/Desktop/COURSES/II. Seminar Data Science for Marketing Analytics/Assignment2/TransportModeSweden.RData")
Transport<- data
rm(data)


#MANIPULATE
Transport$age<- as.numeric(Transport$age)
integer_vars <- sapply(Transport, function(x) is.integer(x))
Transport[, integer_vars] <- lapply(Transport[, integer_vars], function(x) as.factor(x))

#CLEAN


#DESCRPITIVE ANALYSIS
sum(Transport$mode==1)/(sum(Transport$mode==1)+sum(Transport$mode==0))

#TRAINING/TEST SET
set.seed(1)
sample_size = floor(0.8*nrow(Transport))
picked = sample(seq_len(nrow(Transport)),size = sample_size)
train_data = Transport[picked,]
test_data = Transport[-picked,]


#CREATE A BOOSTRAPPED DATASET FROM THE TRAINING SET
#RandomForest package will do this

#PARAMETER TUNING
num_trees <- seq(500, 1000, by = 100)
mtry_values <- 2:6

#USING SET.SEED(1) ONLY DURING THE REPORT
oob_errors1 <- matrix(0, nrow = length(num_trees), ncol = length(mtry_values))

for (i in 1:length(num_trees)) {
  n_trees <- num_trees[i]
  for (j in 1:length(mtry_values)) {
    mtry_val <- mtry_values[j]
    set.seed(1)
    rf_model1 <- randomForest(mode ~., data = train_data, ntree = n_trees, mtry = mtry_val)
    oob_errors1[i, j] <- rf_model1$err.rate[nrow(rf_model1$err.rate), "OOB"]
    cat("Iteration:", i, "| Num Trees:", n_trees, "| mtry:", mtry_val, "| OOB Error:", oob_errors1[i, j], "\n")
  }
}


#TRAIN MODEL
set.seed(1)
rf_model_mtry3 <- randomForest(mode ~ ., data = train_data,ntree = 500, mtry = 3)

#VARIABLES IMPORTANCE
var_importance <- importance(rf_model_mtry3)
var_importance <- data.frame(var_importance)
var_importance$variables <- rownames(var_importance)
var_importance <- var_importance[order(var_importance$MeanDecreaseGini), ]
par(mfrow = c(1,1),mar = c(5, 7, 4, 2))
barplot(var_importance$MeanDecreaseGini, names.arg = var_importance$variables,
        main = "Variable Importance", xlab = "Mean Decrease in Gini",
        horiz = TRUE, col = "red", las = 2, xlim = c(0, 500))
var_importance

#PREDICTION
predictions3 <- predict(rf_model_mtry3, newdata = test_data)

confusion_matrix <- confusionMatrix(predictions3, test_data$mode)
confusion_matrix$table
Accuracy <- confusion_matrix$overall["Accuracy"]
Precision <- confusion_matrix$byClass["Precision"]
Recall <- confusion_matrix$byClass["Recall"]
F1_score <- confusion_matrix$byClass["F1"]

evaluation<- cbind(Accuracy,Precision,Recall,F1_score)
rownames(evaluation)<- "Percentage"
evaluation

#PLOTS OF THE NTREE AND NSPLITS VALUES
par(mfrow = c(1,2))
par(mar = c(5, 4, 4, 2) + 0.1)
#1
plot(num_trees, oob_errors1[,1], type = "l", col = 8, lty = 1, lwd = 2,
     xlab = "Number of Trees", ylab = "OOB Error Rate",
     main = "OOB Error for Different mtry, on Range: 0.195-0.21",
     ylim = c(0.195,0.21), cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.8)
for (j in 2:length(mtry_values)) {
  lines(num_trees, oob_errors1[,j], type = "l", col = 8 + j - 1, lty = 1, lwd = 2)
}
legend("topright", legend = as.character(mtry_values), col = 8:14, lty = 1, title = "m=",cex = 0.8)

#2
par(mar = c(5, 4, 4, 2) + 0.1)
plot(num_trees, oob_errors1[,1], type = "l", col = 8, lty = 1, lwd = 2,
     xlab = "Number of Trees", ylab = "OOB Error Rate",
     main = "OOB Error for Different mtry, on Range: 0.15-0.3",
     ylim = c(0.15,0.30), cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.8)
for (j in 2:length(mtry_values)) {
  lines(num_trees, oob_errors1[,j], type = "l", col = 8 + j - 1, lty = 1, lwd = 2)
}
legend("topright", legend = as.character(mtry_values), col = 8:14, lty = 1, title = "m=",cex = 0.8)

par(mfrow = c(1,1),mar = c(5, 7, 4, 2))
barplot(var_importance$MeanDecreaseGini, names.arg = var_importance$variables,
        main = "Variable Importance", xlab = "Mean Decrease in Gini",
        horiz = TRUE, col = "red", las = 2, xlim = c(0, 500),cex.names = 0.5, cex.main = 0.8, cex.axis = 0.8)

kable(evaluation, caption = "Evaluation metrics")
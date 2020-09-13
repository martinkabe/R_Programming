
# Company classification --------------------------------------------------

### Packages:
library(ggplot2)
library(dplyr)
library(caret) # for tunning parameters use modelLookup, e.g.: modelLookup("svmRadial")
library(Boruta)
library(PerformanceAnalytics)

set.seed(107)
inTrain <- createDataPartition(
  y = data2$ClassCat,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

data2$ClassCat = factor(data2$ClassCat)
training <- data2[ inTrain, c(1:9, 12)]
testing  <- data2[-inTrain, c(1:9, 12)]

# Variable importance
borutaMod <- Boruta(ClassCat ~ ., data = training, doTrace=1)
borutaMod

# Retain confirmed and tentative
boruta_signif <- getSelectedAttributes(borutaMod, withTentative = TRUE)
print(boruta_signif)

# Do a rough fix on tentative variables
roughFixMod <- TentativeRoughFix(borutaMod)
boruta_signif <- getSelectedAttributes(roughFixMod, withTentative = TRUE)

# plot variable importance chart
plot(roughFixMod, cex.axis=.1, las=1, xlab = "", main="Variable Importance", xaxt="n", yaxt="n")
axis(1, at=1:7, labels = FALSE, cex.axis=1.0)
text(seq(1, 9, by=1), par("usr")[3] - 3.0, labels = c("CT", "UCSize", "UCShape", "MAF", "ECSize", "BN", "BC", "NN", "M"),
     srt = 45, pos = 1, xpd = TRUE)
axis(2, at=seq(10, 100, by=10),
     labels=seq(10, 100, by=10), las = 2, cex.axis=1.0)

# Compute importance score
impsc <- attStats(roughFixMod)
impsc$variable <- rownames(impsc)
impsc %>% filter(decision == "Confirmed") %>% arrange(desc(meanImp))
imp_sort <- impsc[order(-impsc$meanImp),]
rownames(imp_sort)

# Classification Model ----------------------------------------------------
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary,
                           allowParallel=TRUE,
                           verboseIter = TRUE)

# Gradient boosting - expand grid
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

# glm net - expand grid
glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),
                        .lambda = seq(.01, .2, length = 40))

# nnnet - expand grid
nnetGrid <- expand.grid(.size = 1:10,
                        .decay = c(0, .1, 1, 2))

# svm - expand grid
svmGrid <- expand.grid(sigma = c(.01, .015, 0.2),
                       C = c(0.75, 0.9, 1, 1.1, 1.25)
)

# naive bayes - expand grid
nbGrid <- data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1.0))

# c5.0 - expand grid
c50Grid <- expand.grid(.trials = c(1:9, (1:10)*10),
                       .model = c("tree", "rules"),
                       .winnow = c(TRUE, FALSE))

# rpart - expand grid
rpartGrid <- expand.grid(cp=seq(0, 0.05, 0.005))

# xgboost - expand grid
xgbGrid = expand.grid(nrounds = c(1, 2, 10), 
                      max_depth = c(1, 5, 10, 15), 
                      eta = c(0.01, 0.001, 0.0001, .1, .4), 
                      gamma = c(1, 2, 3), 
                      colsample_bytree = c(0.4, 0.7, 1.0), 
                      min_child_weight = c(0.5, 1, 1.5),
                      subsample = c(.8, 1))

# xgboost - training model
xgboost_fit <-train(ClassCat ~ ., data = training,
                    method="xgbTree",
                    trControl=fitControl,
                    tuneGrid=xgbGrid,
                    verbose=TRUE,
                    metric="ROC",
                    preProc = c("center", "scale"),
                    nthread = 3
)

# ada boost model - training model
ada_fit <- train(ClassCat ~ ., data = training, 
                 method = "ada", 
                 trControl = fitControl,
                 metric = "ROC", 
                 preProc = c("center", "scale"))

# c5.0 - training model
c50_fit <- train(ClassCat ~ ., data = training,
                 tuneGrid=c50Grid,
                 preProc = c("center", "scale"),
                 trControl=fitControl,
                 metric = "ROC",
                 method="C5.0",
                 importance=TRUE)

# random forest - training model
rf_fit <- train(ClassCat ~ ., data = training,
                method = "rf",
                metric = "ROC",
                preProc = c("center", "scale"),
                importance = TRUE,
                tuneLength = 10,
                ntrees = 1000,
                tuneGrid = data.frame(.mtry=c(1, 2, 3, 4)),
                trControl = fitControl)

# naive bayes - training model
nb_fit <- train(ClassCat ~ ., data = training,
                method = "nb",
                metric = "ROC",
                preProc = c("center", "scale"),
                tuneGrid = nbGrid,
                fit = FALSE,
                trControl = fitControl)

# knn - training model
knn_fit <- train(ClassCat ~ ., data = training,
                 method = "knn",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k = c(4*(0:5)+1,
                                              20*(1:5)+1,
                                              50*(2:9)+1)),
                 trControl = fitControl)

# svm - training model
svm_fit <- train(ClassCat ~ ., data = training,
                 method = "svmRadial",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneGrid = svmGrid,
                 fit = FALSE,
                 trControl = fitControl)

# Neural networks - training model
maxSize <- max(nnetGrid$.size)
numWts <- 1*(maxSize * (length(training) + 1) + maxSize + 1)
nnet_fit <- train(ClassCat ~ ., data = training,
                  method = "nnet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = numWts,
                  ## ctrl was defined in the previous chapter
                  trControl = fitControl)

# Gradient boosting - training model
gbm_fit <- train(ClassCat ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 preProc = c("center", "scale"),
                 ## Specify which metric to optimize
                 metric = "ROC")

logreg_fit <- train(ClassCat ~ ., data = training,
                    method = "glm",
                    family='binomial',
                    #tuneGrid = glmnGrid,
                    preProc = c("center", "scale"),
                    metric = "ROC",
                    trControl = fitControl)


### save models
save(training, testing, logreg_fit, gbm_fit, nnet_fit, svm_fit, knn_fit, nb_fit, rf_fit, c50_fit, ada_fit, xgboost_fit, 
     file = "models.RData")
# To load the data again
load("models.RData")

### Results for training data
results <- resamples(list(XGBoost=xgboost_fit,
                          AdaBoost=ada_fit,
                          c50=c50_fit,
                          RandomForest=rf_fit,
                          NaiveBayes=nb_fit,
                          Knn=knn_fit,
                          SVM=svm_fit,
                          NNET=nnet_fit,
                          GBM=gbm_fit,
                          LogReg=logreg_fit))

## summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results, scales(col="black"))

### Results for testing data
# models
models <- list(xgboost_fit, ada_fit, c50_fit, rf_fit, nb_fit, knn_fit, svm_fit, nnet_fit, gbm_fit, logreg_fit)
results_df <- data.frame("Accuracy"=rep(0,length(models)),
                         "Sensitivity"=rep(0,length(models)),
                         "Specificity"=rep(0,length(models)))
for(i in 1:length(models)) {
  pred_mod <- predict(models[[i]], newdata = testing)
  cm <- confusionMatrix(data = pred_mod, reference = testing$Class)
  # names(cm)
  results_df[i,1] <- cm$overall[[1]] # Accuracy
  results_df[i,2] <- cm$byClass[[1]] # Sensitivity
  results_df[i,3] <- cm$byClass[[2]] # Specificity
}

rownames(results_df) <- c("XGBoost","AdaBoost","c50","RandomForest","NaiveBayes",
                          "Knn","SVM","NNET","GBM","LogReg")

results_df = results_df %>% arrange(desc(Accuracy))

# Graphical comparison
dotchart(results_df$Accuracy, labels=row.names(results_df), cex=1.3, main="Accuracy", pch=19)
dotchart(results_df$Sensitivity, labels=row.names(results_df), cex=1.3, main="Sensitivity", pch=19)
dotchart(results_df$Specificity, labels=row.names(results_df), cex=1.3, main="Specificity", pch=19)


library(ROCR)

pred <- prediction(as.numeric(predict(models[[1]], newdata = testing)), as.numeric(testing$ClassCat))
pred2 <- prediction(as.numeric(predict(models[[2]], newdata = testing)), as.numeric(testing$ClassCat))
perf <- performance( pred, "tpr", "fpr" )
perf2 <- performance(pred2, "tpr", "fpr")
plot( perf, colorize = TRUE)
plot(perf2, add = TRUE, colorize = TRUE)


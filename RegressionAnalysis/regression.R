# library(PerformanceAnalytics)
# install.packages("MASS")
# install.packages("quadprog")
# install.packages("leaps")

library(MASS)
library(leaps)

dataSet = mtcars
head(dataSet,3)

par(mfrow=c(2,2))
plot(dataSet$cyl, dataSet$mpg, col="blue", pch=19, main="mpg ~ cyl")
plot(dataSet$disp, dataSet$mpg, col="blue", pch=19, main="mpg ~ disp")
plot(dataSet$hp, dataSet$mpg, col="blue", pch=19, main="mpg ~ hp")
plot(dataSet$wt, dataSet$mpg, col="blue", pch=19, main="mpg ~ wt")
par(mfrow=c(1,1))

x = 1:10
xinv = 1/x
par(mfrow=c(1,2))
plot(x, type="l", col="blue")
plot(xinv, type="l", col="blue")
par(mfrow=c(1,1))

chart.Correlation(dataSet[1:5], histogram = TRUE)

# stepwise regression
full.model <- lm(mpg ~., data = dataSet)
step.model <- stepAIC(full.model, direction = "both", trace = TRUE)
summary(step.model)

# best of subset
leaps<-regsubsets(mpg ~ disp + cyl + hp + wt, data=dataSet,nbest=10)
# view results 
# summary(leaps)
plot(leaps,scale="r2")

models = list(
 model1 = mpg ~ cyl,
 model2 = mpg ~ hp,
 model3 = mpg ~ wt,
 model4 = mpg ~ cyl + hp + wt,
 model5 = mpg ~ I(1/disp)
)

boot_fce <- lapply(models, function(model) {
 unlist(lapply(1:100, function(x) {
  rn_sample = dataSet[sample(1:nrow(dataSet), replace=TRUE),]
  summary(lm(model, data=rn_sample))$r.squared
 }))
})

boxplot(boot_fce, col="blue", main="Box Plot for Rsquare of linear models", xlab="Model", ylab="Rsquare")

models_unstacked = as.data.frame(do.call(cbind, boot_fce))
models_stacked = stack(models_unstacked)
colnames(models_stacked) <- c("rsquare","model")
anova_model = aov(rsquare ~ model, data=models_stacked)
TukeyHSD(anova_model)

rmsd = function(ypred, yobs) {
 return(sqrt(mean(ypred - yobs)^2))
}

# model = mpg ~ cyl
kfold_algo = function(model) {
 rmsd_storage <- vector("numeric", length(unique(fold)))
 fold = cut(1:nrow(dataSet), breaks=5, labels=FALSE)
 for (i in 1:length(unique(fold))) {
  testing = dataSet[which(fold==i, arr.ind=F),]
  training = dataSet[-which(fold==i, arr.ind=F),]
  lin_model = lm(model, data=training)
  ypred = predict(lin_model, testing)
  rmsd_storage[i] = rmsd(ypred, testing[[1]])
 }
 return(rmsd_storage)
}

cv_models = lapply(models, function(model) {kfold_algo(model)})

par(mfrow=c(2,1))
boxplot(boot_fce, col="blue", main="Box Plot for Rsquare of linear models (Bootstrap)", xlab="Model", ylab="Rsquare")
boxplot(cv_models, col="blue", main="Cross Validation algo for linear models", xlab="Model", ylab="RMSD")
par(mfrow=c(1,1))

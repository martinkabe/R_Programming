
# Load Libs ---------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(kableExtra))
suppressMessages(library(corrgram))
suppressMessages(library(psych))
suppressMessages(library(factoextra))
suppressMessages(library(gridExtra))
suppressMessages(library(corrplot))
suppressMessages(library(summarytools))
suppressMessages(library(caret))


# Load the Data -----------------------------------------------------------
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
#Change the column headings
data = read.csv(file = url, header = FALSE,
                 col.names = c("SCNo","CT", "UCSize", "UCShape", "MAF", "ECSize", "BN", "BC", "NN","M", "Dia"))

data = data %>% select(-SCNo) %>% 
  mutate(
    Response = ifelse(Dia == 4, 1, 0),
    Response = as.integer(Response),
    BN = strtoi(BN)
  )

table(data$Response)
nrow(data[is.na(data$BN),])
data2 = data[!is.na(data$BN),]


# Basic Stats -------------------------------------------------------------
data2$ClassCat[data2$Dia == 4] = 'Malignant'
data2$ClassCat[data2$Dia == 2] = 'Benign'

malignant = summarytools::descr(data2[,c(1:9, 12)] %>% filter(ClassCat == "Malignant") %>% select(-ClassCat))
benign = summarytools::descr(data2[,c(1:9, 12)] %>% filter(ClassCat == "Benign") %>% select(-ClassCat))
kbl(malignant)

# PCA ---------------------------------------------------------------------
all_pca = prcomp(data2[,1:9], cor=TRUE, scale = TRUE)
summary(all_pca)
fviz_eig(all_pca, addlabels=TRUE, ylim = c(0, 75), geom = c("bar", "line"), barfill = "pink",  
         barcolor="blue",linecolor = "red", ncp = 10) +
  labs(title = "PCA for all predictors, excluded NAs in BN predictor",
       x = "Principal Components", y = "% of Variance")


all_var = get_pca_var(all_pca)
all_var
corrplot(all_var$cos2, is.corr=FALSE)
corrplot(all_var$contrib, is.corr=FALSE)


p1 = fviz_contrib(all_pca, choice = "var", axes=1, fill = "pink", color = "grey", top = 10)
p2 = fviz_contrib(all_pca, choice = "var", axes=2, fill = "skyblue", color = "grey", top = 10)
grid.arrange(p1, p2, ncol = 2)


fviz_pca_biplot(all_pca, col.ind = data2$ClassCat, col = "black",
                palette = "jco", geom = "point", repel = TRUE,
                legend.title = "Diagnosis", addEllipses = TRUE)

pca_coordinates = as.data.frame(all_pca$x) %>% select(PC1, PC2) %>% cbind(data2[,c(1:9, 12)])

fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, type = 'scatter',
               mode = 'markers', symbol = ~Species, symbols = c('circle','x','o'),
               text = ~paste("Sepal.Length: ", Sepal.Length, '$<br>Petal.Length:', Petal.Length),
               color = ~Species, marker = list(size = 10))

fig


# ML Part -----------------------------------------------------------------

set.seed(107)
inTrain <- createDataPartition(
  y = data2$ClassCat,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training <- data2[ inTrain, c(1:9, 12)]
testing  <- data2[-inTrain, c(1:9, 12)]

ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  number = 10,
  classProbs = TRUE,
  verboseIter = TRUE,
  search = "grid"
)

## Random Forest
set.seed(123)
rf <- train(
  ClassCat ~ .,
  data = training,
  method = "rf",
  metric = "Accuracy",
  tuneLength = 15,
  trControl = ctrl,
  tuneGrid = expand.grid(.mtry=c(1:15))
)

rfImp <- varImp(rf, scale = FALSE)
plot(rfImp)




temp.scale<- scale(temp,center=TRUE,scale=TRUE)
tempCor <- cor(temp.scale)
corrplot(tempCor, order = "hclust")

library(corrplot)
library(caret)
library(xgboost)
library(e1071)
set.seed(123)

setwd("~/Dropbox/DM/DataOnFeatureSelect")
train.ig.300 <- readRDS("train_ig_300.rds")
test.ig.300 <- readRDS("test_ig_300.rds")

temp <- train.ig.300[,-301]
rm(train.ig.300)

## read ID for test
ID <- read.csv("ID.csv", header = T)

## read target variable
y <- read.csv("y.csv", header = T) 
y <- y$target
## y$target <- as.factor(y$target)
## y <- y$target

corr.Matrix <- cor(temp)
corr.75 <- findCorrelation(corr.Matrix, cutoff = 0.75)
train.300.75 <- temp[, corr.75]
rm(temp)

train <- train.300.75
test <- subset(test.ig.300, select = colnames(train.300.75))
rm(train.300.75, test.ig.300)

dtrain.simp <- xgb.DMatrix(as.matrix(train), label = y)
xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)

clf.simp <- xgboost(data        = dtrain.simp, 
                    #data        = data.matrix(train),
                    #label       = train$target,
                    nrounds     = 20,
                    objective   = "binary:logistic",
                    eval_metric = "auc",
                    ##             max_depth   = 10, ## cause over-fitting, bad results 
                    verbose     = 2 
)

predict.simp <- predict(clf.simp, xgtest)
cat("saving the submission file\n")
submission.simp <- data.frame(ID = ID, target = as.character(gsub("Seg", "", predict.simp)))
write.csv(submission.simp, "result_simp.csv", row.names=FALSE)


svm.model <- svm(x = train, y = y, type = "C", kernel = "linear", probability=T)

svm.model <- svm( x = train,
                  y = y, 
                  type = "C",
                  kernel = "linear",
                  fitted = F,
                  probability=T, 
                  ## k = 5,
                  cachesize = 400
                  )

xgb.plot.tree(agaricus.train$data@Dimnames[[2]], model = bst)

xgb.plot.tree(colnames(train), model = clf.simp)

library(xgboost)
library(readr)

script.dir <- paste0(dirname(sys.frame(1)$ofile),'\\')
#script.dir <- "~/Desktop/DataMining"
setwd(script.dir)

cat("reading data, including train and test\n")

data.dir <- "H:\\Dropbox\\DM\\DataOnFeatureSelect\\"
#train <- read_csv(paste0(data.dir, "train.csv", collapse = ""))
#test <- read_csv(paste0(data.dir, "test.csv", collapse = ""))
train <-  loadRDS(paste0(data.dir, "train_ig_300.rds", collapse = ""))
test <-  loadRDS(paste0(data.dir, "test_ig_300.rds", collapse = ""))
rm(path)

y <- train$target
train <- subset(train, select = -target)
ID <- test$id


hold <- sample(1:nrow(train), 20000)
dtrain <- xgb.DMatrix(as.matrix(train[-hold,]), label = y[-hold])
dtest <- xgb.DMatrix(as.matrix(train[hold,]), label = y[hold])
xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)

watchlist <- list(train=dtrain, test=dtest)

param <- list(   eta                 = 1, #0.06, #0.01,
                 #gamma               =    ,
                 max_depth           = 8,  # changed from default of 8
                 #min_child_weight    =  , 
                 #subsample           = 0.8,
                 #colsample_bytree    = 0.7,
                 #num_parallel_tree   =   , 
                 objective           = "binary:logistic",
                 eval_metric         = "auc"    
)

clf.adv <- xgb.train(    params              = param, 
                         data                = dtrain, 
                         nrounds             = 30, #300, #280, #125, #250, # changed from 300
                         verbose             = 2, 
                         #early.stop.round   = 5,
                         watchlist           = watchlist
                         #maximize           = TRUE
)

dtrain.simp <- xgb.DMatrix(as.matrix(train), label = y)
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
predict.adv <- predict(clf.adv, xgtest)

cat("saving the submission file\n")
submission.adv <- data.frame(ID = ID, target = as.character(gsub("Seg", "", predict.adv)))
write.csv(submission.adv, "result_adv.csv", row.names=FALSE)

cat("saving the submission file\n")
submission.simp <- data.frame(ID = ID, target = as.character(gsub("Seg", "", predict.simp)))
write.csv(submission.simp, "result_simp.csv", row.names=FALSE)

xgb.save(clf.simp, "xgboost_simp.model")
xgboost.simp <- xgb.load("xgboost_simp.model")

xgb.save(clf.simp, "xgboost_adv.model")
xgboost.adv <- xgb.load("xgboost_adv.model")
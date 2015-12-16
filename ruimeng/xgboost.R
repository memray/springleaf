library(xgboost)
library(readr)

script.dir <- paste0(dirname(sys.frame(1)$ofile),'\\')
setwd(script.dir)

cat("reading data, including train and test\n")

data.dir <- "H:\\Dropbox\\DM\\DataOnFeatureSelect\\"
#train <- read_csv(paste0(data.dir, "train.csv", collapse = ""))
#test <- read_csv(paste0(data.dir, "test.csv", collapse = ""))

feature_set = "ig_600"
round_number = 100

train <-  readRDS(paste0(data.dir, "train_",feature_set, ".rds", collapse = ""))
test <-  readRDS(paste0(data.dir, "test_",feature_set, ".rds", collapse = ""))

y <- train$target
y = as.numeric(levels(y))[y]
train <- subset(train, select = -target)
ID <- read_csv(paste0(data.dir, "id.csv", collapse = ""))


hold <- sample(1:nrow(train), 20000)
dtrain <- xgb.DMatrix(as.matrix(train[-hold,]), label = y[-hold])
dtest <- xgb.DMatrix(as.matrix(train[hold,]), label = y[hold])
xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)

watchlist <- list(train=dtrain, test=dtest)

param <- list(# Initial one
                 #eta                 = 1, #0.06, #0.01,
                 #gamma               =    ,
                 #max_depth           = 8,  # changed from default of 8
                 #min_child_weight    =  , 
                 #subsample           = 0.8,
                 #colsample_bytree    = 0.7,
                 #num_parallel_tree   =   , 
                 #objective           = "binary:logistic",
                 #eval_metric         = "auc"    
  # From .79833
  #eta = 0.0060,
  #max_depth = 8,
  #subsample = 0.65,
  #colsample_bytree = 0.7,
  
  # From #7
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth=18,
  colsample_bytree=0.3,
  min_child_weight=10,
  subsample=0.8,
  num_round=9666,
  eta=0.006
)

ptm <- proc.time()

clf.adv <- xgb.train(    params              = param, 
                         data                = dtrain, 
                         verbose             = 2, 
                         #early.stop.round   = 5,
                         watchlist           = watchlist,
                         #maximize           = TRUE
                         nrounds = round_number
                         
                         
)

cat("Training time:\n")
print(proc.time()-ptm)


ptm <- proc.time()
cat("Testing time:\n")
predict.adv <- predict(clf.adv, xgtest)
print(proc.time()-ptm)

cat("saving the submission file\n")
submission.adv <- data.frame(ID = ID, target = as.character(gsub("Seg", "", predict.adv)))
write.csv(submission.adv, paste0("xgb_", feature_set,"_", round_number ,".csv"), row.names=FALSE)

xgb.save(clf.adv, paste0("xgb_", feature_set,"_", round_number ,".model"))
#xgboost.adv <- xgb.load("xgboost_adv.model")
#library(ggplot2)
library(class) # for knn
#library(MASS) # for the example dataset
library(plyr) # for recoding data
library(ROCR) # for plotting roc
library(e1071) # for NB and SVM
library(rpart) # for decision tree
library(ada) # for adaboost
# -----------------------------------
# read data and do some preprocess
# -----------------------------------
load.data <- function(data.filename) {
  colClasses <-
    c(
      "factor","double","double","double","double","double","double","double","integer"
    )
  dataset = read.csv(
    data.filename, header = TRUE, sep = ",", colClasses =
      colClasses
  )
  
  library(car)
  library(MASS)
  dataset$SEX <- as.numeric(recode(dataset$SEX,"'M'=0;'F'=1;'I'=2"))
  
  colnames(dataset)[ncol(dataset)] = 'y'
  dataset$y <- as.numeric(dataset$y > 12)
  
  for(colname in colnames(dataset)){
    if(colname=='y')
      next
    mean = mean(dataset[[colname]])
    dataset[colname] = dataset[[colname]]-mean
    max = max(dataset[[colname]])
    min = min(dataset[[colname]])
    if(max == min)
      next
    dataset[[colname]] = dataset[[colname]]/(max-min)
  }
  
  return(dataset)
  
}

do.classify <- function(dataset , cl.name, do.cv = F) {
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.cols <- ncol(dataset) - 1 # no. of predictors
  cat('nmy dataset:',
      n.obs,'observations ',
      n.cols,'predictors ','\n')
  # print(dataset[1:3,])
  #   cat('label (y) distribution:')
  #   hist(dataset$y)
  if (do.cv)
    return(k.fold.cv(dataset, cl.name))
}


#Perform 10 fold cross validation
k.fold.cv <-
  function(dataset, cl.name, k.fold = 10, prob.cutoff = 0.5) {
    ## default: 10-fold CV, cut-off 0.5
    n.obs <- nrow(dataset) # no. of observations
    s = sample(n.obs)
    probs = NULL
    actuals = NULL
    #s = c(1:n.obs)
    errors = dim(k.fold)
    for (k in 1:k.fold) {
      test.idx = which(s %% k.fold == (k - 1)) # use modular operator
      train.set = dataset[-test.idx,]
      test.set = dataset[test.idx,]
      cat(
        k.fold,'-fold CV run',k,cl.name,':',
        '#training:',nrow(train.set),
        '#testing',nrow(test.set),'\n'
      )
      prob = do.classification(train.set, test.set, cl.name)
      predicted = as.numeric(prob > prob.cutoff)
      actual = test.set$y
      confusion.matrix = table(actual,factor(predicted,levels = c(0,1)))
      confusion.matrix
      error = (confusion.matrix[1,2] + confusion.matrix[2,1]) / nrow(test.set)
      errors[k] = error
      cat('\t\terror=',error,'\n')
      probs = c(probs,prob)
      actuals = c(actuals,actual)
    }
    return(list(probs, actuals))
  }

do.evaluation <- function(cl.name, probs, actuals, globalenv, prob.cutoff = 0.5) {
  predicted = as.numeric(probs > prob.cutoff)
  confusion.matrix = table(actuals,factor(predicted,levels = c(0,1)))
  avg.error = (confusion.matrix[1,2] + confusion.matrix[2,1]) / length(probs)
  
  cat('Total results:','avg error=',avg.error,'\n')
  
  ## plot ROC
  result = data.frame(probs,actuals)
  pred = prediction(result$probs,result$actuals)
  perf = performance(pred, "tpr","fpr")
  
  ## get other measures by using 'performance'
  get.measure <- function(pred, measure.name = 'auc') {
    perf = performance(pred,measure.name)
    m <- unlist(slot(perf, "y.values"))
    #     print(slot(perf, "x.values"))
    #     print(slot(perf, "y.values"))
    m
  }
  
  auc = get.measure(pred, 'auc')
  plot(perf, main=paste(cl.name,", AUC=",auc))
  
  err = mean(get.measure(pred, 'err'))
  accuracy = sum(predicted==actuals)/length(probs)
  
  precision = mean(get.measure(pred, 'prec'),na.rm = T)
  recall = mean(get.measure(pred, 'rec'),na.rm = T)
  fscore = mean(get.measure(pred, 'f'),na.rm = T)
  cat('accuracy=',accuracy,'error=',err,'precision=',precision,'recall=',recall,'f-score',fscore,'\n')
  cat('auc=',auc,'\n')
  results <<- c(results, c(accuracy,precision, recall, fscore, auc))
}
do.classification <- function(train.set, test.set,
                              cl.name , verbose = F) {
  ## note: to plot ROC later, we want the raw probabilities,
  ## not binary decisions
  switch(
    cl.name,
    knn = {
      # here we test k=3; you should evaluate different k's
      prob = knn(train.set[,-1], test.set[,-1], cl = train.set[,1], k = 3, prob =
                   T)
      prob = attr(prob,"prob")
      #print(cbind(prob,as.character(test.set$y)))
      return(prob)
    },
    knn_5 = {
      # here we test k=3; you should evaluate different k's
      prob = knn(train.set[,-1], test.set[,-1], cl = train.set[,1], k = 5, prob =
                   T)
      prob = attr(prob,"prob")
      #print(cbind(prob,as.character(test.set$y)))
      return(prob)
    },
    lr = {
      # logistic regression
      model = glm(y ~ ., family = binomial, data = train.set)
      if (verbose) {
        print(summary(model))
      }
      prob = predict(model, newdata = test.set, type = "response")
      #print(cbind(prob,as.character(test.set$y)))
      return(prob)
    },
    nb = {
      model = naiveBayes(y ~ ., data = train.set)
      prob = predict(model, newdata = test.set, type = "raw")
      #print(cbind(prob,as.character(test.set$y)))
      prob = prob[,2] / rowSums(prob) # renormalize the prob.
      return(prob)
    },
    dtree = {
      model = rpart(y ~ ., data = train.set)
      prob = predict(model, newdata = test.set)
      return(prob)
    },
    dtree_prune = {
      model = rpart(y ~ ., data = train.set)
      prob = predict(model, newdata = test.set)
      
      ## you should evaluate different size of tree
      ## prune the tree
      pfit <-
        prune(model, cp = model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
      prob = predict(pfit, newdata = test.set)
      return(prob)
    },
    svm = {
      model = svm(y ~ ., data = train.set, probability = T,kernel =
                    "linear")
      prob = predict(model, newdata = test.set, probability = T)
      
      prob[which(prob > 1)]=1
      prob[which(prob < 0)]=0
      
      return(prob)
    },
    svm_radial = {
      model = svm(y ~ ., data = train.set, probability = T)
      # fine-tune the model with different kernel and parameters
      ## evaluate the range of gamma parameter between 0.000001 and 0.1
      ## and cost parameter from 0.1 until 10
      tuned <- tune.svm(
        y ~ ., data = train.set,
        kernel = "radial",
        gamma = 10 ^ (-6:-1), cost = 10 ^ (-1:1)
      )
      #print(summary(tuned))
      gamma = tuned[['best.parameters']]$gamma
      cost = tuned[['best.parameters']]$cost
      model = svm(
        y ~ ., data = train.set, probability = T,
        kernel = "radial", gamma = gamma, cost = cost
      )
      prob = predict(model, newdata = test.set, probability = T)
      
      prob[which(prob > 1)]=1
      prob[which(prob < 0)]=0
      
      return(prob)
    },
    svm_poly = {
      model = svm(y ~ ., data = train.set, probability = T)
      # fine-tune the model with different kernel and parameters
      ## evaluate the range of gamma parameter between 0.000001 and 0.1
      ## and cost parameter from 0.1 until 10
      tuned <- tune.svm(
        y ~ ., data = train.set,
        kernel = "polynomial",
        degree = (2:6), cost = 10^(-1:1)
      )
      #print(summary(tuned))
      degree = tuned[['best.parameters']]$degree
      cost = tuned[['best.parameters']]$cost
      model = svm(
        y ~ ., data = train.set, probability = T,
        kernel = "polynomial", degree = degree, cost = cost
      )
      prob = predict(model, newdata = test.set, probability = T)
      
      prob[which(prob > 1)]=1
      prob[which(prob < 0)]=0
      
      #prob = attr(prob,"probabilities")
      #print(cbind(prob,as.character(test.set$y)))
      #print(dim(prob))
      #prob = prob[,which(colnames(prob) == 1)] / rowSums(prob)
      return(prob)
    },
    ada = {
      model = ada(y ~ ., data = train.set)
      prob = predict(model, newdata = test.set, type = 'probs')
      #print(cbind(prob,as.character(test.set$y)))
      prob = prob[,2] / rowSums(prob)
      return(prob)
    }
  )
}


# ----------------------------Start of Main Program--------------------------------#
setwd("H:\\NetDrive\\WorkSpace\\R\\INFSCI2160_DataMining\\hw3")

# -----------Load data---------------#
dataset = load.data("abalone.csv")


# -----------Prepare for different model---------------#
#Apply different classification techniques (incl.
#  logistic regression, kNN, Naive Bayesian, decision tree,
#  SVM, and Ensemble methods)
cl.names = c('knn','knn_5','lr','nb','dtree','dtree_prune','svm','svm_radial','svm_poly','ada')
# cl.names = c('knn_5','lr','nb','dtree_prune','svm','ada')
results = NULL

attach(mtcars)
par(mfrow=c(3,4))

# -----------Do classification and Evaluation-------------#
for (cl.name in cl.names) {
  # -------CV and Prediction------#
  cat('\n\n\n>>>>>>>>>>>>>>>>>>>>>>>>Start to run ',cl.name,'<<<<<<<<<<<<<<<<<<<<<<<<<<\n')
  result = do.classify(dataset , cl.name,T)
  # -------do evaluation and plot------#
  do.evaluation(cl.name,result[[1]], result[[2]], results)
}
results <- data.frame(matrix(unlist(results), nrow=length(cl.names), byrow=T))
results <- t(results[,1:ncol(results)])
rownames(results) = c("Accuracy","Precision","Recall","F-Score","AUC")
colnames(results) = cl.names

# -------draw bar chart------#
f_score <- results[4,]
auc <- results[5,]

par(mfrow=c(1,1))
barplot(f_score, main="F-score", 
        names.arg=cl.names)
barplot(auc, main="AUC", 
        names.arg=cl.names)
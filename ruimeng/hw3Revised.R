library(plyr) # function revalue, recoding the data
library(ROCR) # for plotting roc
library(e1071) # for NB and SVM
library(rpart) # for decision tree
library(ada) # for adaboost
library(class) # for KNN
#library(caret) # this is for cross-validation 
#library(MASS) # for the example dataset 

set.seed(123) # set the seed so you can get exactly the same results whenever you run the code

load.data.task <- function() {
      data.url = 'http://www.yurulin.com/class/fall2015_datamining/data/'
      dataset <- read.csv(sprintf("%s/abalone.csv",data.url))
      ## do some preprocessing here
      ## ...
      return(dataset) # need a return value
}

do.classification <- function(train.set, test.set, cl.name, verbose=F, k=3, prone=F, kernel="linear", tune = T) {
      # k is for KNN, the number of neighbours
      # prone is for decision tree, prone the tree or not
      ## note: to plot ROC later, we want the raw probabilities,
      ## not binary decisions
      prob = NULL
      switch(cl.name, 
             knn = { # here we test k=3; you should evaluate different k's
                   prob = knn(train.set[,-9], test.set[,-9], cl=train.set[,9], k = k, prob=T)
                   prob = attr(prob,"prob")
                   #print(cbind(prob,as.character(test.set$y)))
                   prob
             },
             lr = { # logistic regression
                   model = glm(RINGS~., family=binomial, data=train.set)
                   if (verbose) {
                         print(summary(model))             
                   }
                   prob = predict(model, newdata=test.set, type="response") 
                   #print(cbind(prob,as.character(test.set$y)))
                   prob
             },
             nb = {
                   model = naiveBayes(RINGS~., data=train.set)
                   prob = predict(model, newdata=test.set, type="raw") 
                   #print(cbind(prob,as.character(test.set$y)))
                   prob = prob[,2]/rowSums(prob) # renormalize the prob.
                   prob
             },
             dtree = {
                   model = rpart(RINGS~., data=train.set)
                   if (verbose) {
                         print(summary(model)) # detailed summary of splits
                         printcp(model) # print the cross-validation results
                         plotcp(model) # visualize the cross-validation results
                         ## plot the tree
                         plot(model, uniform=TRUE, main="Classification Tree")
                         text(model, use.n=TRUE, all=TRUE, cex=.8)
                   }           
                   prob = predict(model, newdata=test.set)
                   
                   # if (0) mean False, if(1) mean True in R
                   if (prone) { # here we use the default tree, 
                         ## you should evaluate different size of tree
                         ## prune the tree 
                         pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
                         prob = predict(pfit, newdata=test.set)
                         ## plot the pruned tree 
                         plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
                         text(pfit, use.n=TRUE, all=TRUE, cex=.8)             
                   }
                   #print(cbind(prob,as.character(test.set$y)))
                   prob = prob[,2]/rowSums(prob) # renormalize the prob.
                   prob
             },
             svm = {
                   # four different kernels: polynomial, linear, radial, sigmoid
                   model = svm(RINGS~., data=train.set, probability=T, kernel=kernel)
                   if (tune) { # fine-tune the model with different kernel and parameters
                         ## evaluate the range of gamma parameter between 0.000001 and 0.1
                         ## and cost parameter from 0.1 until 10
                         tuned <- tune.svm(RINGS~., data = train.set, 
                                           kernel=kernel, 
                                           gamma = 10^(-6:-1), cost = 10^(-1:1))
                         #print(summary(tuned))
                         gamma = tuned[['best.parameters']]$gamma
                         cost = tuned[['best.parameters']]$cost
                         model = svm(RINGS~., data = train.set, probability=T, 
                                     kernel=kernel, gamma=gamma, cost=cost)                        
                   }
                   prob = predict(model, newdata=test.set, probability=T)
                   prob = attr(prob,"probabilities")
                   #print(cbind(prob,as.character(test.set$y)))
                   #print(dim(prob))
                   prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
                   prob
             },
             ada = {
                   model = ada(RINGS~., data = train.set)
                   prob = predict(model, newdata=test.set, type='probs')
                   #print(cbind(prob,as.character(test.set$y)))
                   prob = prob[,2]/rowSums(prob)
                   prob
             }
      ) 
      return (prob)
}

classifier <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5, verbose=F, k=3, prone=F, kernel="linear", tune=F) {
      ## default: 10-fold CV, cut-off 0.5 
      n.obs <- nrow(dataset) # no. of observations 
      s = sample(n.obs)
      errors = dim(k.fold)
      probs = NULL
      actuals = NULL
      for (k in 1:k.fold) {
            test.idx = which(s %% k.fold == (k-1) ) # use modular operator
            train.set = dataset[-test.idx,]
            test.set = dataset[test.idx,]
            # cat(k.fold,'-fold CV run',k,cl.name,':',
            #     '#training:',nrow(train.set),
            #     '#testing',nrow(test.set),'\n')
            prob = do.classification(train.set, test.set, cl.name, verbose, k, prone, kernel, tune)
            predicted = as.numeric(prob > prob.cutoff)
            actual = test.set$RINGS
            confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
            confusion.matrix
            error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
            errors[k] = error
            # cat('\t\terror=',error,'\n')
            probs = c(probs,prob)
            actuals = c(actuals,actual)
            ## you may compute other measures and store them in arrays
      }
      avg.error = mean(errors)
      avg.accuracy = 1 - avg.error
      # cat(k.fold,'-fold CV results:','avg error=',avg.error,'\n')
      
      ## plot ROC
      result = data.frame(probs,actuals)
      pred = prediction(result$probs,result$actuals)
      perf = performance(pred, "tpr","fpr")
      plot(perf)  
      
      ## get other measures by using 'performance'
      get.measure <- function(pred, measure.name='auc') {
            perf = performance(pred,measure.name)
            m <- unlist(slot(perf, "y.values"))
            #     print(slot(perf, "x.values"))
            #     print(slot(perf, "y.values"))
            m
      }
      err = mean(get.measure(pred, 'err'))
      precision = mean(get.measure(pred, 'prec'),na.rm=T)
      recall = mean(get.measure(pred, 'rec'),na.rm=T)
      fscore = mean(get.measure(pred, 'f'),na.rm=T)
      #cat('error=',err,'precision=',precision,'recall=',recall,'f-score',fscore,'\n')
      auc = get.measure(pred, 'auc')
      #cat('auc=',auc,'\n')
      res = c(result, "accuracy"=avg.accuracy, "precision"=precision, "recall"=recall, "Fscore"=fscore, "AUC"=auc)
      return(res)
}

config <- function(dataset) {
      # 1 represents Female, 2 represents Male and 0 represents Infant
      dataset$SEX <- revalue(dataset$SEX, c("F" = "1", "I"="0","M"="2"))
      
      # RINGS > 12 revalue to 1, RINGS < 12 revalue to 0
      total <- length(dataset$RINGS)
      for (i in 1:total) {
            if (dataset$RINGS[i] > 12) {
                  dataset$RINGS[i] = 1
            } else {
                  dataset$RINGS[i] = 0
            }
      }
      dataset$RINGS <- as.factor(dataset$RINGS) 
      return(dataset)
}

# loading data
abalone <- load.data.task()
abalone <- config(abalone)

# lr, knn, nb, dtree, svm, ada
# polynomial, linear, radial, sigmoid
lr <- classifier(abalone, 'lr')
knn3 <- classifier(abalone, 'knn', k=3)
knn5 <- classifier(abalone, 'knn', k=5)
knn8 <- classifier(abalone, 'knn', k=8)
nb <- classifier(abalone, 'nb')
dtree.default <- classifier(abalone, 'dtree')
dtree.prune <- classifier(abalone, 'dtree', prone=T)
svm.linear <- classifier(abalone, "svm", kernel="linear", tune=T)
svm.radial <- classifier(abalone, "svm", kernel="radial", tune=T)
svm.sigmoid <- classifier(abalone, "svm", kernel="sigmoid", tune=T)
svm.polynomial <- classifier(abalone, "svm", kernel="polynomial", tune=T)
ada <- classifier(abalone, 'ada')


tab <- cbind('lr'=lr[3:7],'knn3'=knn3[3:7], 'knn5'=knn5[3:7], 'knn8'=knn8[3:7], 'nb'=nb[3:7],
             'dtree.default'=dtree.default[3:7], 'dtree.prune'=dtree.prune[3:7],
             'svm.linear'=svm.linear[3:7], 'svm.radial'=svm.radial[3:7], 'svm.sigmoid'=svm.sigmoid[3:7],
             'svm.polynomial'=svm.polynomial[3:7], 'ada'=ada[3:7])

# use this 
tab.rbind <- rbind('lr'=lr[3:7],'knn3'=knn3[3:7], 'knn5'=knn5[3:7], 'knn8'=knn8[3:7], 'nb'=nb[3:7],
             'dtree.default'=dtree.default[3:7], 'dtree.prune'=dtree.prune[3:7],
             'svm.linear'=svm.linear[3:7], 'svm.radial'=svm.radial[3:7], 'svm.sigmoid'=svm.sigmoid[3:7],
             'svm.polynomial'=svm.polynomial[3:7], 'ada'=ada[3:7])

tab.rbind <- rbind('lr'=as.vector(lr[3:7]),'knn3'=as.vector(knn3[3:7]), 'knn5'=as.vector(knn5[3:7]),
                   'knn8'=as.vector(knn8[3:7]), 'nb'=as.vector(nb[3:7]),
                   'dtree.default'=as.vector(dtree.default[3:7]), 'dtree.prune'=as.vector(dtree.prune[3:7]),
                   'svm.linear'=as.vector(svm.linear[3:7]), 'svm.radial'=as.vector(svm.radial[3:7]),
                   'svm.sigmoid'=as.vector(svm.sigmoid[3:7]),
                   'svm.polynomial'=as.vector(svm.polynomial[3:7]), 'ada'=as.vector(ada[3:7]) )

summary.table <- as.data.frame(tab.rbind)



# lr, knn8, nb, dtree.default, svm.linear, ada
ROC <- function(lr, knn, nb, dtree, svm, ada) {
      pred.lr <- prediction(lr$probs, lr$actuals)
      perf.lr <- performance(pred.lr, "tpr", "fpr")
      
      pred.knn <- prediction(knn$probs, knn$actuals)
      perf.knn <- performance(pred.knn, "tpr", "fpr")
      
      pred.nb <- prediction(nb$probs, nb$actuals)
      perf.nb <- performance(pred.nb, "tpr", "fpr")
      
      pred.dtree<- prediction(dtree$probs, dtree$actuals)
      perf.dtree <- performance(pred.dtree, "tpr", "fpr")
      
      pred.svm <- prediction(svm$probs, svm$actuals)
      perf.svm <- performance(pred.svm, "tpr", "fpr")
      
      pred.ada <- prediction(ada$probs, ada$actuals)
      perf.ada <- performance(pred.ada, "tpr", "fpr")
      
      plot(perf.lr, col=1, main="ROC plot") 
      lines(perf.knn@x.values[[1]], perf.knn@y.values[[1]], col=2)
      lines(perf.nb@x.values[[1]], perf.nb@y.values[[1]], col=3)
      lines(perf.dtree@x.values[[1]], perf.dtree@y.values[[1]], col=4)
      lines(perf.svm@x.values[[1]], perf.svm@y.values[[1]], col=5)
      lines(perf.ada@x.values[[1]], perf.ada@y.values[[1]], col=6)
      legend("bottomright", c("lr","knn","nb","dtree","svm","ada"), lty=c(1,1,1,1,1,1), lwd=c(2.5,2.5,2.5,2.5,2.5,2.5), col=c(1,2,3,4,5,6), cex=.6)    
}

ROC(lr, knn8, nb, dtree.default, svm.linear, ada)

bar.AUC <- function() {
      AUC <- cbind('lr'=lr[7],'knn3'=knn3[7], 'knn5'=knn5[7], 'knn8'=knn8[7], 'nb'=nb[7],
                      'dtree.default'=dtree.default[7], 'dtree.prune'=dtree.prune[7],
                      'svm.linear'=svm.linear[7], 'svm.radial'=svm.radial[7], 'svm.sigmoid'=svm.sigmoid[7],
                      'svm.polynomial'=svm.polynomial[7], 'ada'=ada[7])
      barplot(AUC, main="AUC", ylim = c(0,1),
              xlab="Model Name", ylab="AUC", cex.names=0.8)
}

bar.Fscore <- function() {
      Fscore <- cbind('lr'=lr[6],'knn3'=knn3[6], 'knn5'=knn5[6], 'knn8'=knn8[6], 'nb'=nb[6],
                      'dtree.default'=dtree.default[6], 'dtree.prune'=dtree.prune[6],
                      'svm.linear'=svm.linear[6], 'svm.radial'=svm.radial[6], 'svm.sigmoid'=svm.sigmoid[6],
                      'svm.polynomial'=svm.polynomial[6], 'ada'=ada[6])
      barplot(Fscore, main="F-score", ylim = c(0,0.7),
              xlab="Model Name", ylab="F-score", cex.names=0.8)
}

bar.AUC()
bar.Fscore()

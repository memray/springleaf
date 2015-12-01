library(plyr)
library(readr)
setwd("~/Desktop/DataMining")

cat("reading data, including train and test\n")
path <- "/Users/fujun/Desktop/DataMining/"
train <- read_csv(paste0(path, "train.csv", collapse = ""))
test <- read_csv(paste0(path, "test.csv", collapse = ""))
rm(path)

cat("removing ID and target, store target into y\n")
y <- train$target
train <- train[,c(-1,-1934)]
test <- test[, -1]

cat("remove duplicated columns\n")
table(duplicated(as.list(train)))
train <- subset(train, select=!duplicated(as.list(train)))
test <- subset(test, select=colnames(train))

cat("remove constant\n")
## col.const <- sapply(train, function(x) length(unique(x)[!is.na(unique(x))]))
col.const <- sapply(train, function(x) length(unique(x))==1)
table(col.const)
train <- subset(train, select = !col.const)
test <- subset(test, select = !col.const)
rm(col.const)

cat("replace \"\" and [] to NA\n")
train[train==""] <- NA
test[test==""] <- NA
train[train=="[]"] <- NA
test[test=="[]"] <- NA

cat("coerce character to factor")
feature.names <- names(train)
char.names <- vector()
for (f in feature.names) {
      if (class(train[[f]])=="character") {
            char.names <- c(char.names, f)
            train[[f]] <- as.factor(train[[f]])
            test[[f]] <- as.factor(test[[f]])
      }
}
rm(f)

cat("summmary on factor variables")
train.chars <- subset(train, select = char.names)
summary(train.chars)
## delete: VAR_0008 VAR_0044 VAR_0202 VAR_0214 VAR_0216 VAR_0222 VAR_0226 VAR_0230 VAR_0236 VAR_0237 VAR_0274 VAR_0404 VAR_0342 VAR_0493
## delete time: VAR_0073 VAR_0156 VAR_0157 VAR_0158 VAR_0159 VAR_0166 VAR_0167 VAR_0168 VAR_0169 VAR_0176 VAR_0177 VAR_0178 VAR_0179

## time: VAR_0075 VAR_0200 VAR_0204 VAR_0217
## logical VAR_0232
## location variable: VAR_0237 VAR_0274
## job: VAR_0404 VAR_0493
## check: VAR_0200 VAR_0283 VAR_0305 VAR_0325 VAR_0342 VAR_0404 VAR_0467     
 
VAR_0200 <- unique(c(train$VAR_0200, test$VAR_0200))
length(VAR_0200) ## 12458 levels: deleted
VAR_0283 <- unique(c(train$VAR_0283, test$VAR_0283))
length(VAR_0283) ## 9 levels
VAR_0305 <- unique(c(train$VAR_0305, test$VAR_0305))
length(VAR_0305) ## 9 levels
VAR_0325 <- unique(c(train$VAR_0325, test$VAR_0325))
length(VAR_0325) ## 10 levels
VAR_0342 <- unique(c(train$VAR_0342, test$VAR_0342))
length(VAR_0342) ## 51 levels
VAR_0404 <- unique(c(train$VAR_0404, test$VAR_0404))
length(VAR_0404) ## 1874 levels: deleted
VAR_0467 <- unique(c(train$VAR_0467, test$VAR_0467))
length(VAR_0467) ## 5 levels
VAR_0493 <- unique(c(train$VAR_0493, test$VAR_0493))
length(VAR_0493) ## 620 levels

## deleted unused variable
rm(VAR_0200)
rm(VAR_0283)
rm(VAR_0305)
rm(VAR_0325)
rm(VAR_0342)
rm(VAR_0404)
rm(VAR_0467)
rm(VAR_0493)

deleted <- c("VAR_0008", "VAR_0044", "VAR_0202", "VAR_0214", "VAR_0216", "VAR_0222", "VAR_0226", "VAR_0230", 
             "VAR_0236", "VAR_0237", "VAR_0274", "VAR_0404", "VAR_0342", "VAR_0493", "VAR_0073", "VAR_0156", 
             "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", 
             "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179")
train <- subset(train, select = c(-VAR_0008, -VAR_0044, -VAR_0202, -VAR_0214, -VAR_0216, -VAR_0222, -VAR_0226, -VAR_0230, 
                                  -VAR_0236, -VAR_0237, -VAR_0274, -VAR_0404, -VAR_0342, -VAR_0493, -VAR_0073, -VAR_0156, 
                                  -VAR_0157, -VAR_0158, -VAR_0159, -VAR_0166, -VAR_0167, -VAR_0168, 
                                  -VAR_0169, -VAR_0176, -VAR_0177, -VAR_0178, -VAR_0179))
test <- subset(test, select = c(-VAR_0008, -VAR_0044, -VAR_0202, -VAR_0214, -VAR_0216, -VAR_0222, -VAR_0226, -VAR_0230, 
                                -VAR_0236, -VAR_0237, -VAR_0274, -VAR_0404, -VAR_0342, -VAR_0493, -VAR_0073, -VAR_0156, 
                                -VAR_0157, -VAR_0158, -VAR_0159, -VAR_0166, -VAR_0167, -VAR_0168, 
                                -VAR_0169, -VAR_0176, -VAR_0177, -VAR_0178, -VAR_0179))

cat("deleted VAR_0200\n")
train <- subset(train, select = -VAR_0200)
test <- subset(test, select = -VAR_0200)

cat("summary on remaining factor variables")
feature.names <- names(train)
factor.names <- vector()
for (f in feature.names) {
      if (class(train[[f]])=="factor") {
            factor.names <- c(factor.names, f)
      }
}
rm(f)
cat("summmary on factor variables")
train.factor <- subset(train, select = factor.names)
summary(train.factor)

cat("imputation\n")
library(mice)
cat("calculate percentile of missing data\n")
pMiss <- function(x){sum(is.na(x))/length(x)*100}
missing <- apply(train,2,pMiss)
## temp.train <- mice(train,m=5,maxit=5,meth='pmm',seed=500)
temp.train <- mice(train,method='sample',seed=500)
train <- complete(temp.train, 1)

cat("coerce time to weeks\n")
coerce.weeks <- function(char) {
      return (difftime(as.Date(format(as.POSIXct(char, format = "%d%b%y:%H:%M:%S"), "20%y-%m-%d")), 
                       as.Date("2000-1-1"), units = "weeks"))
}
train$VAR_0075 <- sapply(train$VAR_0075, coerce.weeks)
train$VAR_0204 <- sapply(train$VAR_0204, coerce.weeks)
train$VAR_0217 <- sapply(train$VAR_0217, coerce.weeks)

## missing value percentile on train.chars
colMeans(is.na(train.chars))


## separate numeric and non nonmeric columns
## VAR_0241: zip code

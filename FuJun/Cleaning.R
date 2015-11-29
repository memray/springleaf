library(plyr)
library(readr)
setwd("~/Desktop/DataMining")

# logical variable: VAR_0008 VAR_0009 VAR_0010 VAR_0011 VAR_0012 VAR_0043 VAR_0196 VAR_0226 VAR_0229 VAR_0230 VAR_0232 VAR_0236 VAR_0239
# time stamp: VAR_0073 VAR_0075 VAR_0204 VAR_0217
# missing values(""): VAR_0073 VAR_0156 VAR_0157 VAR_0158 VAR_0159 VAR_0166 VAR_0167 VAR_0168 VAR_0169 VAR_0176 VAR_0177 VAR_0178 VAR_0179 VAR_0214
# location: VAR_0200 VAR_0237(state, "", 47 level) VAR_0274(state, "" "-1", 58 levels)

path <- "/Users/fujun/Desktop/DataMining/"
## reading data
cat("reading the train data\n")
train <- read_csv(paste0(path, "train.csv", collapse = ""))
y <- train$target
cat("reading the test data\n")
test <- read_csv(paste0(path, "test.csv", collapse = ""))

## let train and test have the exactly same columns
y <- train$target
train <- train[,c(-1,-1934)]
test <- test[, -1]

#remove constant
col.const <- sapply(train, function(x) length(unique(x))==1)
table(col.const)
train <- subset(train, select = !col.const)
test <- subset(test, select = !col.const)

## delete: VAR_0044 VAR_0202 VAR_0222 VAR_0214 VAR_0192 VAR_0200
train <- subset(train, select = c(-VAR_0044, -VAR_0202, -VAR_0222, -VAR_0214, -VAR_0192, -VAR_0200))
test <- subset(test, select = c(-VAR_0044, -VAR_0202, -VAR_0222, -VAR_0214, -VAR_0192, -VAR_0200))

feature.names <- names(train)
char.names <- vector()
for (f in feature.names) {
      if (class(train[[f]])=="character") {
            char.names <- c(char.names, f)
            #levels <- unique(c(train[[f]], test[[f]]))
            #train[[f]] <- as.integer(factor(train[[f]], levels=levels))
            #test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
      }
}

missing.names.train <- vector()
missing <- sapply(train, function(x) sum(is.na(x)))
for (i in 1:1921) {
      if (missing[i] != 0) {
            missing.names.train <- c(missing.names.train, names(missing)[i])
      }
}

for (f in char.names) {
#      if (class(train[[f]])=="character") {
            levels <- unique(c(train[[f]], test[[f]]))
            train[[f]] <- as.factor(train[[f]])
            test[[f]] <- as.factor(test[[f]])
#           train[[f]] <- as.integer(factor(train[[f]], levels=levels))
#           test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
#      }
}

## time stamp: VAR_0073 VAR_0075 VAR_0204 VAR_0217
## location: VAR_0200 VAR_0237(state, "", 47 level) VAR_0274(state, "" "-1", 58 levels)
## remove time stamp and location
train <- subset(train, select = c(-VAR_0073, -VAR_0075, -VAR_0204, -VAR_0217, -VAR_0237, -VAR_0274))
test <- subset(test, select = c(-VAR_0073, -VAR_0075, -VAR_0204, -VAR_0217, -VAR_0237, -VAR_0274))

## remove duplicated columns 
table(duplicated(as.list(train)))
train <- subset(train, select=!duplicated(as.list(train)))
test <- subset(test, select=!duplicated(as.list(train)))

## remove: VAR_0008



## summary on char.names

train.chars <- subset(train, select = char.names)

for (i in char.names) {
      summary(train[[i]])
}

## sapply(train, function(x) all(is.na(x)) )

## cleaning
## H       Q     R 
## 59829   509   84893 
VAR_0001 <- unique(c(train$VAR_0001, test$VAR_0001))
train$VAR_0001 <- as.factor(train$VAR_0001)
test$VAR_0001 <- as.factor(test$VAR_0001)

## "[]" ""
##  56  145175
try.VAR_0044 <- unique(c(train$VAR_0044, test$VAR_0044))
train$VAR_0044 <- as.factor(train$VAR_0044)
test$VAR_0044 <- as.factor(test$VAR_0044)
levels(train$VAR_0044) <- c(0,1)
levels(test$VAR_0044) <- c(0,1)

## B      C      N      S 
## 71449  54299  16605  2878 
try.VAR_0005 <- unique(c(train$VAR_0005, test$VAR_0005))
train$VAR_0005 <- as.factor(train$VAR_0005)
test$VAR_0005 <- as.factor(test$VAR_0005 )

## "BatchInquiry" "" 
##  ""       BatchInquiry 
##  56       145175 
VAR_0202 <- unique(c(train$VAR_0202, test$VAR_0202))
train$VAR_0202 <- as.factor(train$VAR_0202)
test$VAR_0202 <- as.factor(test$VAR_0202)
levels(train$VAR_0202) <- c(0,1)
levels(test$VAR_0202) <- c(0,1)

## ""  "DS"
## 56  145175
VAR_0216 <- unique(c(train$VAR_0216, test$VAR_0216))
train$VAR_0216 <- as.factor(train$VAR_0216)
test$VAR_0216 <- as.factor(test$VAR_0216)
levels(train$VAR_0216) <- c(0,1)
levels(test$VAR_0216) <- c(0,1)

## ""  "C6"
## 56  145175
VAR_0222 <- unique(c(train$VAR_0222, test$VAR_0222))
train$VAR_0222 <- as.factor(train$VAR_0222)
test$VAR_0222 <- as.factor(test$VAR_0222)
levels(train$VAR_0222) <- c(0,1)
levels(test$VAR_0222) <- c(0,1)

## "S"  "P"  "H"  "-1" ""   "R"  "U"  "M"  "F" 
VAR_0305 <- unique(c(train$VAR_0305, test$VAR_0305))
train$VAR_0305 <- as.factor(train$VAR_0305)
test$VAR_0305 <- as.factor(test$VAR_0305)
levels(train$VAR_0305) <- c(0:8)
levels(test$VAR_0305) <- c(0:8)


VAR_0342 <- unique(c(train$VAR_0342, test$VAR_0342))
train$VAR_0342 <- as.factor(train$VAR_0342)
test$VAR_0342 <- as.factor(test$VAR_0342)
levels(train$VAR_0342) <- c(0:50)
levels(test$VAR_0342) <- c(0:50)

## "O"  "R"  "U"  "-1" ""  
VAR_0352 <- unique(c(train$VAR_0352, test$VAR_0352))

## "U"  "R"  "O"  "-1" "" 
VAR_0353 <- unique(c(train$VAR_0353, test$VAR_0353))

## "O"  "R"  "-1" "U"  ""  
VAR_0354 <- unique(c(train$VAR_0354, test$VAR_0354))

## job title
VAR_0404 <- unique(c(train$VAR_0404, test$VAR_0404))

## "-1"  "Discharged"   "Dismissed"    ""  "Discharge NA", 5 levels
VAR_0467 <- unique(c(train$VAR_0467, test$VAR_0467))
## job
VAR_0493 <- unique(c(train$VAR_0493, test$VAR_0493))

## "IAPS"   "RCC"    "BRANCH" "MOBILE" "CSC" 
VAR_1934 <- unique(c(train$VAR_1934, test$VAR_1934))


# VAR_0005
# VAR_0202
# VAR_0216 
# VAR_0222 # category
# VAR_0305 
# VAR_0342
VAR_0352 
VAR_0353 
VAR_0354
VAR_0404
VAR_0467
VAR_0493
VAR_1934

## try example
VAR_0434 VAR_0435
VAR_0345 VAR_0346
VAR_0201

## deleted 
VAR_0214 <- unique(c(train$VAR_0214, test$VAR_0214))
train$VAR_0214 <- as.factor(train$VAR_0214)
test$VAR_0214 <- as.factor(test$VAR_0214)

## deleted
VAR_0192 <- unique(c(train$VAR_0192, test$VAR_0192))
train$VAR_0192 <- as.factor(train$VAR_0192)
test$VAR_0192 <- as.factor(test$VAR_0192)

VAR_0237 <- unique(c(train$VAR_0237, test$VAR_0237))
VAR_0274 <- unique(c(train$VAR_0274, test$VAR_0274))

VAR_0200 <- unique(c(train$VAR_0200, test$VAR_0200))
train$VAR_0200 <- as.factor(train$VAR_0200)

feature.names <- names(train)
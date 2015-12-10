## correlation matrix filters
library(caret)
library(FSelector)

cat("feature selection by removing redudant features\n")
set.seed(123)
correlationMatrix <- cor(train.num)

highlyCorrelated.65 <- findCorrelation(correlationMatrix, cutoff = 0.65)
highlyCorrelated.75 <- findCorrelation(correlationMatrix, cutoff = 0.75)
highlyCorrelated.85 <- findCorrelation(correlationMatrix, cutoff = 0.85)

train.num.select65 <- train.num[, -highlyCorrelated.65]
## test.num.select65 <- test.num[, -highlyCorrelated65]

train.num.select75 <- train.num[, -highlyCorrelated.75]
## test.num.select75 <- test.num[, -highlyCorrelated75]

train.num.select85 <- train.num[, -highlyCorrelated.85]
## test.num.select85 <- test.num[, -highlyCorrelated85]

y$target <- as.factor(y$target)

temp <- cbind(train.char, train.weeks)
temp <- cbind(temp, y)

train.65 <- cbind(train.num.select65, temp)
train.75 <- cbind(train.num.select75, temp)
train.85 <- cbind(train.num.select85, temp)

write.csv(train.65, "train_65.csv", row.names = F)
write.csv(train.75, "train_75.csv", row.names = F)
write.csv(train.85, "train_85.csv", row.names = F)
## train.num.select75 <- cbind(train.num.select75, y)
## train.num.select75$target <- as.factor(train.num.select75$target)
## weights <- symmetrical.uncertainty(target~., train.num.select75) 
## weights <- gain.ratio(target~., train.num.select75) 

## symmetrical uncertainty
## cat("entropy based feature selection on factor and time variables\n")
## train.char <- cbind(train.weeks, train.char)
## train.char <- cbind(temp.train, y)
## train.char$y <- as.factor(train.char$y)
## imputation on missing value
## train.char[is.na(train.char)] <- 0
## weights <- symmetrical.uncertainty(y~., train.char) ## may use target instead of y 
## print(weights)

subset.30 <- cutoff.k(weights, 30)
subset.20 <- cutoff.k(weights, 20)
subset.10 <- cutoff.k(weights, 10)

## subset <- cutoff.biggest.diff(weights)
## f <- as.simple.formula(subset, "target")
temp.train1 <- subset(train.char, select = subset.30)
temp.train1 <- cbind(temp.train1, train.num.select1)
temp.train1 <- cbind(temp.train1, y)

temp.train2 <- subset(train.char, select = subset.20)
temp.train2 <- subset(temp.train2, train.num.select2)
temp.train2 <- cbind(temp.train2, y)

temp.train3 <- subset(train.char, select = subset.10)
temp.train3 <- subset(temp.train3, train.num.select3)
temp.train3 <- cbind(temp.train3, y)
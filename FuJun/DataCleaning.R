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
train <- subset(train, select=-c(ID,target))
test <- subset(test, select=-ID)
## train <- train[,c(-1,-1934)]
## test <- test[, -1]

cat("remove constant\n")
## col.const <- sapply(train, function(x) length(unique(x)[!is.na(unique(x))]))
col.const <- sapply(train, function(x) length(unique(x))==1)
table(col.const)
train <- subset(train, select = !col.const)
test <- subset(test, select = !col.const)
rm(col.const)

cat("remove duplicated columns\n")
table(duplicated(as.list(train)))
train <- subset(train, select=!duplicated(as.list(train)))
test <- subset(test, select=colnames(train))

cat("replace \"\" to NA\n")
train[train==""] <- NA
test[test==""] <- NA
train[train=="[]"] <- NA
test[test=="[]"] <- NA

cat("separate numeric and non numeric columns\n")
train.num <- train[, sapply(train, is.numeric)]
train.char <- train[, sapply(train, is.character)]
test.num <- test[, sapply(train, is.numeric)]
test.char <- test[, sapply(test, is.character)]

cat("separate chars and time columns")
train.date <- train.char[,grep("JAN1|FEB1|MAR1", train.char)]
train.char <- train.char[, !colnames(train.char) %in% colnames(train.date)]
test.date <- subset(test.char, select = colnames(train.date))
test.char <- test.char[, !colnames(test.char) %in% colnames(test.date)]

cat("coerce time to weeks\n")
coerce.weeks <- function(char) {
      return (difftime(as.Date(format(as.POSIXct(char, format = "%d%b%y:%H:%M:%S"), "20%y-%m-%d")), 
                       as.Date("2000-1-1"), units = "weeks"))
}
train.weeks <- as.data.frame(lapply(train.date, coerce.weeks))
test.weeks <- as.data.frame(lapply(test.date, coerce.weeks))
train.weeks <- as.data.frame(lapply(train.weeks, as.numeric))
test.weeks <- as.data.frame(lapply(test.weeks, as.numeric))

cat("coerce character variables to factor\n")
## train.char <- as.data.frame(lapply(train.char, as.factor))
## test.char <- as.data.frame(lapply(test.char, as.factor))
## summary(train.char)
char.names <- names(train.char)
for (f in char.names) {
            levels <- unique(c(train.char[[f]], test.char[[f]]))
            train.char[[f]] <- as.integer(factor(train.char[[f]], levels=levels))
            test.char[[f]]  <- as.integer(factor(test.char[[f]],  levels=levels))
}
rm(char.names, f, levels)
rm(train.date, test.date)

## city: VAR_0200, Zip: VAR_0241
## could delte them 
## imbalanced: VAR_0008 VAR_0044 VAR_0202 VAR_0214 VAR_0216  VAR_0222
## imbalanced 2 : VAR_0226 VAR_0230 VAR_0236

cat("replace missing values with -9999\n")
train.num[is.na(train.num)] <- -9999
test.num[is.na(test.num)]   <- -9999
train.weeks[is.na(train.weeks)] <- -9999
test.weeks[is.na(test.weeks)] <- -9999
train.char[is.na(train.char)] <- -9999
test.char[is.na(test.char)] <- -9999

cat("feature selection by removing redudant features\n")
set.seed(123)
library(caret)
correlationMatrix <- cor(train.num)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
train.num.select <- train.num[, -highlyCorrelated]
test.num.select <- test.num[, -highlyCorrelated]
## train.char, train.weeks

rm(x, y, z, tf, my_char, my_div, my_name, my_seq, my_sqrt, num_vect)
## train.nonnumeric <- merge(train.weeks, train.char)
y <- read.csv("y.csv")

cat("entropy based feature selection on factor and time variables\n")
temp.train <- cbind(train.weeks, train.char)
temp.train <- cbind(temp.train, y)
temp.train$target <- as.factor(temp.train$target)
library(FSelector)
weights <- symmetrical.uncertainty(target~., temp.train)
print(weights)
subset <- cutoff.k(weights, 32)
## subset <- cutoff.biggest.diff(weights)
## f <- as.simple.formula(subset, "target")
temp.train <- subset(temp.train, select = subset)
temp.train <- cbind(temp.train, train.num.select)
temp.train <- cbind(temp.train, y)

cat("megre temporary test files\n")
final.names <- names(temp.train)[1:ncol(temp.train)-1]
temp.test <- cbind(test.weeks, test.char)
temp.test <- cbind(temp.test, test.num.select)
temp.test <- subset(temp.test, select = final.names)

write.csv(temp.train, "temp_train.csv", row.names = F)
write.csv(temp.test, "temp_test.csv", row.names = F)

rm(correlationMatrix, test, test.char, test.num, test.num.select, test.weeks, train, train.char, train.num, train.num.select, train.weeks,weights,y)
rm(final.names, highlyCorrelated, subset)
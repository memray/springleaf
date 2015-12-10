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
ID <- test$ID
y <- as.data.frame(y)
ID <- as.data.frame(ID)
write.csv(y, "y.csv", row.names = F)
write.csv(ID, "ID.csv", row.names = F)
train <- subset(train, select=-c(ID,target))
test <- subset(test, select=-ID)
## train <- train[,c(-1,-1934)]
## test <- test[, -1]

cat("replace \"\" and [] to NA\n")
train[train==""] <- NA
test[test==""] <- NA
train[train=="[]"] <- NA
test[test=="[]"] <- NA

cat("check missing value percentile\n")
mean(is.na(train)) ## overall missing value
mean(is.na(test))
apply(train, 2, function(x) sum(is.na(x))/length(x)) ## percentile of NA for each variable
sapply(train, function(x) sum(is.na(x))) ## number of NA for each variable

cat("imputation on missing value to zero")
## NA represents a pattern that we don't know
train[is.na(train)] <- 0
test[is.na(test)] <- 0

cat("remove constant\n")
## col.const <- sapply(train, function(x) length(unique(x)[!is.na(unique(x))]))
col.const <- sapply(train, function(x) length(unique(x))==1)
table(col.const)
train <- subset(train, select = !col.const)
test <- subset(test, select = !col.const)
rm(col.const)

cat("remove duplicated columns\n")
table(duplicated(as.list(train)))
train <- subset(train, select = !duplicated(as.list(train)))
test <- subset(test, select = colnames(train))

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
## 2000-1-1 is the basic unit
coerce.weeks <- function(char) {
      return (difftime(as.Date(format(as.POSIXct(char, format = "%d%b%y:%H:%M:%S"), "20%y-%m-%d")), 
                       as.Date("2000-1-1"), units = "weeks"))
}
train.weeks <- as.data.frame(lapply(train.date, coerce.weeks))
test.weeks <- as.data.frame(lapply(test.date, coerce.weeks))
train.weeks <- as.data.frame(lapply(train.weeks, as.numeric)) ## ingore decimal points
test.weeks <- as.data.frame(lapply(test.weeks, as.numeric))
rm(train.date, test.date)

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

saveRDS(train.char, "tran_char.rds")
saveRDS(test.char, "test_char.rds")
saveRDS(train.weeks, "train_weeks.rds")
saveRDS(test.weeks, "test_weeks.rds")
saveRDS(train.num, "train_num.rds")
saveRDS(test.num, "test_num.rds")

temp.train <- cbind(train.char, train.weeks)
temp.train <- cbind(temp.train, y)
temp.train <- cbind(train.num, temp.train)
saveRDS(temp.train, "train_final.RDS")
## write.csv(temp.train, "train_final.csv", row.names = F)

temp.test <- cbind(test.char, test.weeks)
temp.test <- cbind(test.num, temp.test)
saveRDS(temp.test, "test_final.RDS")
## write.csv(temp.test, "test_final.csv", row.names = F)

## city: VAR_0200, Zip: VAR_0241
## could delte them 
## imbalanced: VAR_0008 VAR_0044 VAR_0202 VAR_0214 VAR_0216  VAR_0222
## imbalanced 2 : VAR_0226 VAR_0230 VAR_0236
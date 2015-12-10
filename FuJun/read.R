setwd("~/Desktop/DataMining")

train.num <- readRDS("train_num.rds")
train.weeks <- readRDS("train_weeks.rds")
train.char <- readRDS("train_char.rds")
train.final <- readRDS("train_final.rds")

test.num <- readRDS("test_num.rds")
test.weeks <- readRDS("test_weeks.rds")
test.char <- readRDS("test_char.rds")
test.final <- readRDS("test_final.rds")

## read ID for test
ID <- read.csv("ID.csv", header = T)

## read target variable
y <- read.csv("y.csv", header = T) 
y$target <- as.factor(y$target)

train.ig.300 <- readRDS("train_ig_300.rds")
train.ig.600 <- readRDS("train_ig_600.rds")
train.ig.900 <- readRDS("train_ig_900.rds")
train.ig.1200 <- readRDS("train_ig_1200.rds")
train.ig.1500 <- readRDS("train_ig_1500.rds")

test.ig.300 <- readRDS("test_ig_300.rds")
test.ig.600 <- readRDS("test_ig_600.rds")
test.ig.900 <- readRDS("test_ig_900.rds")
test.ig.1200 <- readRDS("test_ig_1200.rds")
test.ig.1500 <- readRDS("test_ig_1500.rds")
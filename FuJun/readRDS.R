setwd("~/Desktop/DataMining")
library(caret)

train.num <- readRDS("train_num.rds")
train.weeks <- readRDS("train_weeks.rds")
train.char <- readRDS("train_char.rds")
correlationMatrix <- readRDS("corrMatrix.rds")

## read test data 
test.num <- readRDS("test_num.rds")
test.weeks <- readRDS("test_weeks.rds")
test.char <- readRDS("test_char.rds")

y <- read.csv("y.csv", header = T) ## column name: target
y$target <- as.factor(y$target)

highlyCorrelated.75 <- findCorrelation(correlationMatrix, cutoff = 0.75)
train.num.select75 <- train.num[, -highlyCorrelated.75]

hold <- sample(1:nrow(train.num.select75), 5000)
samp.y <- y[hold, ]
samp.y <- as.data.frame(samp.y)
colnames(samp.y) <- c("target")
samp <- train.num.select75[hold, ]

samp.dist <- dist(samp)
samp.mds <- cmdscale(samp.dist)

plot(samp.mds, type = 'n')
text(samp.mds, labels = samp.y$target)

data <- data.frame(x = samp.mds[, 1], y = samp.mds[, 2], target = samp.y$target, id = row.names(samp))
ggplot(data, aes(x = x, y = y, color = target)) + geom_point()
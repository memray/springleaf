setwd("~/Desktop/DataMining")
## setwd("~/Dropbox/DM/DataOnFeatureSelect")
## library(caret)
## correlationMatrix <- readRDS("corrMatrix.rds")
## highlyCorrelated.75 <- findCorrelation(correlationMatrix, cutoff = 0.75)
## train.num.select75 <- train.num[, -highlyCorrelated.75]

train.num <- readRDS("train_num.rds")
y <- read.csv("y.csv", header = T) ## column name: target
y$target <- as.factor(y$target)

set.seed(123)
hold <- sample(1:nrow(train.num), 5000)
samp.y <- y[hold, ]
samp.y <- as.data.frame(samp.y)
colnames(samp.y) <- c("target")
samp <- train.num[hold, ]

samp.dist <- dist(samp)
samp.mds <- cmdscale(samp.dist)

plot(samp.mds, type = 'n')
text(samp.mds, labels = samp.y$target)

data <- data.frame(x = samp.mds[, 1], y = samp.mds[, 2], target = samp.y$target, id = row.names(samp))
ggplot(data, aes(x = x, y = y, color = target)) + geom_point()
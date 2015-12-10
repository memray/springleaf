library(corrplot)
#corrplot: the library to compute correlation matrix.

datMy <- read.table("data.csv", header = TRUE)
#read the tab file using the read table function.

train.num[is.na(train.num)] <- 0

train.num.scale <- scale(train.num, center=TRUE,scale=TRUE)
#scale all the features (from feature 2 bacause feature 1 is the predictor output)

train.num.cor <- cor(train.num.scale)
#compute the correlation matrix

install.packages("corrplot")
library(corrplot)
corrplot(train.num.cor, order = "hclust")
#visualize the matrix, clustering features by correlation index.

highly.Cor <- findCorrelation(train.num.cor, 0.75)

#Apply correlation filter at 0.75
#then we remove all the variable correlated with more 0.75

filtered.scale <- train.num.scale[,-highly.Cor]

corr.num.filter <- cor(filtered.scale)

corrplot(corr.num.filter, order = "hclust")
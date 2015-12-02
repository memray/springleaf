library(corrplot)
#corrplot: the library to compute correlation matrix.

## scale all the features
datMy.scale<- scale(train.num,center=TRUE,scale=TRUE);

## compute the correlation matrix
corMatMy <- cor(datMy.scale)

## visualize the matrix, clustering features by correlation index.
corrplot(corMatMy, order = "hclust")

## Apply correlation filter at 0.75
## then we remove all the variable correlated with more 0.7.
highlyCor <- findCorrelation(corMatMy, 0.75)


datMyFiltered.scale <- datMy.scale[,-highlyCor]
corMatMy <- cor(datMyFiltered.scale)
corrplot(corMatMy, order = "hclust")

rm()
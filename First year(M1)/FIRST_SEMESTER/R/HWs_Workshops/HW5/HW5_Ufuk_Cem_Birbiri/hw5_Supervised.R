#
#UFUK CEM BİRBİRİ
#
#HW5
#
library(readr)
library(datasets)
library(dplyr)
library(tidyverse)
library(mlbench)
library(caret)
library(doMC)
library(corrplot)
set.seed(71)
data("Glass")
dim(Glass)
head(Glass)

Glass %>% filter(Type==4)


levels(Glass$Type)



# list types for each attribute
sapply(Glass, class)
# standard deviations and mean for each class
y<-sapply(Glass[,1:9], mean)
sapply(Glass[,1:9], sd)
xn<-colnames(Glass[,1:9])
x<-c(1:9)
y<-sapply(Glass[,1:9], mean)
barplot(y, main = "Average Value For Each Feature",
        xlab = "Feature Name",
        ylab = "Average Value", col="skyblue")

sapply(Glass, class)
unique(Glass$Type)



# distribution of class variable
y <- Glass$Type
cb <- cbind(freq=table(y), percentage=prop.table(table(y))*100)
barplot(table(y), main = "Frequency Distribution of All Classes",
        xlab = "Class Name",
        ylab = "Number of Data Points", legend = TRUE, col="orange")


# calculate a correlation matrix for numeric variables
correlations <- cor(Glass[,1:9])
# display the correlation matrix
print(correlations)
corrplot(correlations, method = "circle")

head(Glass)

shuffle_index <- sample(1:nrow(Glass))
Glass <- Glass[shuffle_index, ] 
head(Glass)

x <- Glass[,2:9]
head(x)
y <- Glass[,10]


dataset <-Glass
dataset[,1:9] <- scale(dataset[,1:9])

# 5. Evaluate Algorithms
# Run algorithms using 10-fold cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# SVM

#SVMRadial is non-linear SVM classifier
grid <- expand.grid(.sigma=c(0.01,0.05,0.1,0.5,1,10), .C=c(1))
fit.svm <- train(Type~., data=dataset, method="svmRadial", 
                 metric="Accuracy", tuneGrid=grid, 
                 trControl=control)

results <- list(SVM=fit.svm)
results

confusionMatrix(fit.svm)


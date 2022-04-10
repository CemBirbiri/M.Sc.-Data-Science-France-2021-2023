library(readr)
library(datasets)
library(dplyr)
library(tidyverse)
library(mlbench)
#
#UFUK CEM BİRBİRİ
#
#HW5
#

library(caret)
library(doMC)
library(corrplot)

IR<-data("iris")

IR <- iris 

IR_data <- IR[ ,1:4] # We are sub-setting IR object such as to include 'all rows' and columns 1 to 4.
IR_species <- IR[ ,5] # We are sub-setting IR object such as to include 'all rows' and column 5.


library(Rtsne)

tsne_results <- Rtsne(IR_data, perplexity=30, check_duplicates = FALSE) # You can change the value of perplexity and see how the plot changes


plot(tsne_results$Y, col = "black", bg= IR_species, pch = 21, cex = 1.5)


library(ggfortify)
df <- iris[1:4]
pca_res <- prcomp(df, scale. = TRUE)

autoplot(pca_res, data = iris, colour = 'Species', label = FALSE, label.size = 3)

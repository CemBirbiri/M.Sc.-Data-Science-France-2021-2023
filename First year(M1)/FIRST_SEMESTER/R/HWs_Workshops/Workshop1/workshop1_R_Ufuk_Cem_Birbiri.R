#Ufuk Cem Birbiri

#1a
v <- seq(1,10,by=0.5)
v
class(v)

#1b
fahrenheit_to_celsius <- function(F){
  C <- (F-32)*(5/9)
  return(C)
}
fahrenheit_to_celsius(32)

#1c

t <- sample(v,100, replace= TRUE, prob=runif(19))



Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

Mode(t)
hist(t,
     main="Samples with replacements",
     col="yellow"
)

#1d
my_matrix <- matrix(rnorm(7*8, mean=1, sd=2), nrow=7, ncol=8 )

meann <- mean(my_matrix)
meann
my_matrix

my_matrix <- ifelse(my_matrix  < mean(my_matrix), my_matrix+1, my_matrix)

my_matrix
#1e
data(iris)
#head(iris)
boxplot(iris$Sepal.Length ~ iris$Species)
boxplot(iris$Sepal.Width ~ iris$Species)
boxplot(iris$Petal.Length ~ iris$Species)
boxplot(iris$Petal.Width ~ iris$Species)


my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(iris[,1:4], pch = 19,  cex = 0.5,
      col = my_cols[iris$Species],
      lower.panel=NULL)
#1f

x1 = rgamma(1000, shape = 1)
x2 = rgamma(1000, shape = 2)
x3 = rgamma(1000, shape = 3)
x4 = rgamma(1000, shape = 4)

plot(density(x1),col = "red",lwd = 3)
lines(density(x2),col = "blue",lwd = 2)
lines(density(x3),col = "green",lwd = 2)
lines(density(x4),col = 1,lwd = 1)


######################################################################################
###########################################
###########################################
#Exercise-2
#install.packages("nycflights13")
#install.packages("tidyverse")
library(dplyr)
#2a
df <- nycflights13::flights
head(df)

df1 <- filter(df, year==2013 & day==8 & month==4) %>% filter( dep_delay==min(dep_delay, na.rm=TRUE))
head(df1)

#2b
head(df)

df2 <- filter(df, dep_delay > 4 & arr_delay > 4)
head(df2)



#Step-1: Group the rows by month (df4)
#Step-2: Find the average dep_delay and add it as a column named AVG (df5)
#Step-3: Group again by month and find the highest dep_delay (df6)
#Step-4: Select the columns "month","day", "AVG" and create a new dataframe(df7)
df4 <- group_by(df, month)
df5 <- mutate(df4, AVG = mean(dep_delay, na.rm=TRUE))
df6 <- group_by(df5,month) %>% filter( dep_delay==max(dep_delay, na.rm=TRUE))
df7 <- subset(df6, select=c("month","day", "AVG"))
head(df7)

#2c
head(df)
colnames(df)
dff1 <- filter(df, dest=="LAX")
dff2 <- subset(dff1, select=c("dep_delay","arr_delay", "dest"))
dff3 <- arrange(dff2, desc(dep_delay))
mean(dff3$dep_delay, na.rm=TRUE)
mean(dff3$arr_delay, na.rm=TRUE)

##############################################################################
####################################################
##########################
#Exercise-3
#
#############################################
#3a

age <- floor(runif(100, 20, 40))
weight <- runif(100, 50, 90)


zeros_vector<-replicate(40,0)
zeros_vector
ones_vector <-replicate(60,1)
Graduated<- c(zeros_vector, ones_vector)
Graduated<- sample(Graduated)

new_df <- data.frame(age, weight, Graduated)
#head(new_df)

#############################################
#3b BUT THERE IS NO QUESTION FOR 3B
#############################################
#3c


for(i in 1:ncol(new_df)) {       # for-loop over columns
  
  #While determining the random indexes in colums, we need to 
  #find 5 unique index. They shouldn't be same
  uniqueness_flag=1
  while(uniqueness_flag==1){
    indexes = floor(runif(5, 1, nrow(new_df)))
    if(length(unique(indexes))==5){
      #Now we have unique indexes
      uniqueness_flag=0
    }
    else{uniqueness_flag=1}
  }
  
  #Now put the NA's to random locations
  new_df[ indexes[1] , i] <- NA
  new_df[ indexes[2] , i] <- NA
  new_df[ indexes[3] , i] <- NA
  new_df[ indexes[4] , i] <- NA
  new_df[ indexes[5] , i] <- NA
  
}
#We can check how many NA's are there:
#sum(is.na(new_df))

#############################################
#3d
colnames(new_df)[3] <- "Driving_License"
sum(is.na(new_df)) #Count the missing values in the dataframe

new_df_modified <- na.omit(new_df) #Removes the rows with NA's
new_df_modified


library(data.table)
new_df_modified_2 <- copy(new_df_modified) #Copy the dataframe

vect<- new_df_modified[ ,1] #vect is the first column of the original dataFrame
new_df_modified_2[ ,1]<-(vect - min(vect)) / (max(vect)-min(vect))

vect2<- new_df_modified[ ,2] #vect2 is the second column of the original dataFrame
new_df_modified_2[ ,2]<-(vect2 - min(vect2)) / (max(vect2)-min(vect2))
#head(new_df_modified)
#head(new_df_modified_2)

#############################################
#3f

library(data.table)
new_df_modified_3 <- copy(new_df_modified) #Copy the dataframe

vect<- new_df_modified[ ,1] #vect is the first column of the original dataFrame
new_df_modified_3[ ,1] <- (vect - mean(vect)) / var(vect)

vect2<- new_df_modified[ ,2] #vect2 is the second column of the original dataFrame
new_df_modified_3[ ,2] <- (vect - mean(vect)) / var(vect)
#head(new_df_modified)
#head(new_df_modified_3)




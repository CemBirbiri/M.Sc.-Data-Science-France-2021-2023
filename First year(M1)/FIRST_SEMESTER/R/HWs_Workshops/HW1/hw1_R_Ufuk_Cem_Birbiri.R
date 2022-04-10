#Ufuk Cem Birbiri
#1.A
summ <- 0
for(i in 1:100) {
  summ <- summ +i
}

print(summ)

#1.B

#install.packages("dslabs")
#library(dslabs) 
#str(murders)
head(murders)
#names(murders)
a <- murders$abb
class(a)

#1.C
b<-murders[["abb"]]
identical(a,b)

#1.D
table(murders$state, murders$region)

#1.E
vect1<- c(1,2,3)
vect2<- c("a","b","c","d","e")
vect3 <- vect1[1:2]
vect3 <- append(vect3, vect2)
vect3 <- append(vect3, vect1[3:length(vect1)])
vect3

#1.F
# generate 100 random variables, uniformly distributed between 0 and 1 
rand.unif <- runif(100, min = 0, max = 1)
hist(rand.unif, freq = FALSE, xlab = 'x', density = 20, breaks=10)
countt <- sum(rand.unif>0.5)
countt

#1.G
#Compute the per 100,000 murder rate for each state and 
#store it in the object murder_rate. Then compute the 
#average murder rate for the US using the function mean. What is the average? 
murder_rate <- murders$total / murders$population * 100000
mean(murder_rate) # which is 2.779125
head(murders)


#1.H
library(datasets)
data(iris)
dataset <- iris[1:4] #This is the vector data
dim(dataset)
dataset<-dataset[!is.na(dataset)] #This isremoving NA elements

#1.I
hist(murders$population,  density = 20, breaks=10)
#1.J
boxplot(population~region, data = murders)
#1.K
Normalize_vector <- function(vector) {
  return(vector/sqrt(sum(vector^2)))
}


y<-Normalize_vector(c(1, 4, 2, 2, 10))

#1.L
#install.packages("dplyr")

library(datasets)
data(iris)
summary(iris)
class(iris)
dim(iris) #dim(iris) = 150 5
#The 1st,2nd,3rd and 4th columns are numeric in iris dataset.
#So i-> 1:4
for(i in 1:(length(iris)-1)){
  iris[i]<- Normalize_vector((iris[i]))
}
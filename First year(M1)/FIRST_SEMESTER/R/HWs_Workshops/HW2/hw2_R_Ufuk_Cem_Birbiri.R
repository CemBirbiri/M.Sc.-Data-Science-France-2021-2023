#Ufuk Cem Birbiri
########################################################################
#2-A)
#1
c <- runif(100)
length(c)
########################
#2
summ=0
for(i in 1:length(c)){
  summ<-summ+c[i]
}

summ
mean <-summ/length(c)
mean
#OR
i=1
summ2=0
repeat{
  summ2<- summ2+ c[i]
  i=i+1
  if(i >100){
    break
  }
  
}

mean2 <- summ2/length(c)
mean2

########################
#3
var_sum=0
for(i in 1:length(c)){
  difference_sum<-((c[i]-mean)*(c[i]-mean))
  var_sum = var_sum+difference_sum
}
var<- var_sum/length(c)
var

#OR

i=1
var_sum2=0
repeat{
  difference_sum2<-((c[i]-mean2)*(c[i]-mean2))
  var_sum2 = var_sum2 + difference_sum2

  i=i+1
  if(i >100){
    break
  }
  
}
var2<- var_sum/length(c)
var2


#####################################################
#2-B
#1
data("airquality")
head(airquality)
#2
p_na <- vector(length=6)
for(i in seq(1:length(airquality[1]))){
  p_na[i] <-  (sum(is.na(airquality[i]))*100) / nrow(airquality[i])
}
p_na
head(airquality)
#3
for(i in seq(0:length(airquality[1]))){
  if(p_na[i] > 0.5 ){
    airquality_modified <- select(airquality, -colnames(airquality[i]))
  }
  
}
head(airquality_modified)


#4
#This is the list of columns that have NA's in its rows
list_na <- colnames(airquality_modified)[ apply(airquality_modified, 2, anyNA) ]
list_na

#Find the mean values of the columns
mean_values <- vector(length=length(list_na))
for(i in seq(1:length(list_na))){
  mean_values[i]<- mean(airquality_modified[[list_na[i]]], na.rm=TRUE)
}

mean_values
#Replace the NA's with mean values
for(i in seq(1:length(mean_values))){
  airquality_replace <- airquality_modified %>%
    mutate(Solar.R  = ifelse(is.na(Solar.R), mean_values[i], Solar.R))
}


head(airquality_modified)
head(airquality_replace)

###############################################
#2-C
#1 and 2
data("iris")
numeric_iris <- iris[,c(1:4)]
lapply(numeric_iris, mean)
lapply(numeric_iris, sd)







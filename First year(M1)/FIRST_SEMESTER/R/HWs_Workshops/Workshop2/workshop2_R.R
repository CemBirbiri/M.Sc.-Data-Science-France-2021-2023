#############################
#
# UFUK CEM BIRBIRI
#
#
#############################
library(dslabs)
library(dplyr)
library(tidyverse)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("ggrepel")
library(ggrepel )
#install.packages("gridExtra")
library(gridExtra)
library(scales)

data(iris)
head(iris)

############################################################
#Exercise 1

#1a) Using the variable 'Sepal.Length" create an histogram:
ggplot(data=iris, aes(Sepal.Length)) + 
  geom_histogram(binwidth = 0.1,col="red", 
                 fill="green", 
                 alpha = .2)

#1b) Draw the histogram for the variable "Sepal.Length", with 50 blue bins, 
#where the y-axis represents the densities. Add a density red line to the plot. 

ggplot(data=iris, aes(Sepal.Length)) + 
  geom_histogram(aes(Sepal.Length, y=..density..),binwidth = 0.1, 
                 fill="blue", 
                 bins=50)+ 
  labs(title="Histogram for Sepal Length",  y="Density") +
  geom_density(aes(x =Sepal.Length),col = 'red') 

#1c) Draw a scatterplot of Sepal.Length and Sepal.Width where color and shape depend on the Species

p <- iris %>% ggplot(aes(Sepal.Length, Sepal.Width, label = Species)) +
  geom_text_repel(nudge_x = 0.05, max.overlaps = 10) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("iris dataset")
p + geom_point(aes(col=Species,shape=Species), size = 3 )

# 1d)Add a separate regression line for each group. 

p <-  ggplot(iris, aes(Sepal.Length, Sepal.Width,label = Species)) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Sepal Length (log scale)") +
  ylab("Sepal Width (log scale)") +
  ggtitle("iris dataset")
p + geom_point(aes(color=Species,shape=Species, size = 0.5) )+
  geom_smooth(method = "lm",aes( color=Species))

# 1e) Then overall a smooth line (method = “loess”)

p <-  ggplot(iris, aes(Sepal.Length, Sepal.Width,label = Species)) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Sepal Length (log scale)") +
  ylab("Sepal Width (log scale)") +
  ggtitle("iris dataset")
p + geom_point(aes(color=Species,shape=Species, size = 0.5) )+
  geom_smooth(method = "lm",aes( color=Species))+
  geom_smooth(method = "loess")


#1f) Draw a separate scatter plot with a regression line, one for 
#each level of the variable Species 

#Species: setosa versicolor virginica
setosa <- iris %>% filter(Species=="setosa")
versicolor <- iris %>% filter(Species=="versicolor")
virginica <- iris %>% filter(Species=="virginica")

p1<-data.frame(x = setosa$Sepal.Length, y = setosa$Sepal.Width) %>% ggplot(aes(x, y)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Setosa")
p2<-data.frame(x = versicolor$Sepal.Length, y = versicolor$Sepal.Width) %>% ggplot(aes(x, y)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Versicolor")
p3<-data.frame(x = virginica$Sepal.Length, y = virginica$Sepal.Width) %>% ggplot(aes(x, y)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Virginica")

grid.arrange(p1, p2,p3, ncol = 3)


########################################################
##########################################

#Exercise 2
#2a) From “mpg” dataset draw a scatter plot for `displ` (x-axis) and
#`hwy` (y-axis)
data(mpg)
head(mpg)

p<-ggplot(mpg, aes(displ, hwy)) +
  xlab("x-axis = displ") +
  ylab("y-axis = hwy") +
  ggtitle("mpg dataset")
p+ geom_point(aes(displ, hwy))



#2b) Modify the previous scatter plot in such a way that the colour
#depends on the class and the shape on the year

p<-ggplot(mpg, aes(displ, hwy)) +
  xlab("x-axis = displ") +
  ylab("y-axis = hwy") +
  ggtitle("mpg dataset")
p + geom_point(aes(displ, hwy, color=class,  shape=as.factor(year), size = 0.5) )

#2c)display the same data conditionally on one categorical variable
#(here the class variable)- Hint: ?mapping

p1<-ggplot(mpg, aes(x = displ, y = hwy)) 

p1 + geom_point(aes(col=class)) + facet_wrap(~class)+ theme_economist_white()

#2d) Load the diamonds dataset. Draw a scatter plot of 'carat' ` (x-axis)
#and 'price’ (y-axis) where the colour depends on the variable 'cut',
#add also a smooth line (‘lm’) and display conditionally on the variable 'color’.
data("diamonds")

p <-  ggplot(diamonds, aes(carat, price))

p + geom_point(aes(color=cut, size = 0.5) )+
  geom_smooth(method = "lm",formula='y ~ x')+facet_wrap(~color)+
  theme_economist_white()

########################################################
##########################################
#Exercise 3
#Load the starwars dataset (dplyr::starwars).
#3a) Which variable (column) has the highest number of missing values?

data(starwars)

highest<-""
max_na_number<-0
for(i in colnames(starwars)){
  print(i)
  print(sum(is.na(starwars[i])))
  if(sum(is.na(starwars[i])) > max_na_number){
    max_na_number<- sum(is.na(starwars[i]))
    highest<-i
  }
}
highest

#3b) How many humans are contained in starwars dataset? show them by gender.

number_of_humans <-nrow(starwars %>% select("species") %>% filter(species=="Human") )
number_of_humans

#show them by gender:
starwars %>% filter(species == "Human") %>%
  group_by(gender) %>%
  count()

#3c) From which homeworld do the most individuals (rows) come from? 

temp_df<- starwars %>% 
  group_by(homeworld) %>%
  count()
temp_df
head(temp_df,1)

#3d) Create a barplot of the gender distribution of the starwars Universe, set the title :
#"Gender distribution of the sw Universe". Make the colours of the columns depend on the
#gender, modify the colour using the command : scale_fill_manual.

cols <- c("feminine" = "blue", "masculine" = "Seagreen")

p<-ggplot(data = starwars,aes(x=gender)) + geom_bar(aes(fill = gender))+
     ggtitle("Gender distribution of the sw Universe")
p + scale_fill_manual(name = "Gender:", values = cols, na.value = "black") 

#3e) There is no question for 3e
#3f) Draw the densities for the height variable of feminines and masculines only.
head(starwars)
df_temp <- starwars %>%filter(gender!="NA" ) %>% select("height","gender")%>%filter(height!="NA")
df_temp
ggplot(data = df_temp, mapping = aes(x = height, fill = gender)) +
  geom_density(alpha = 0.5) 

#3g) Draw a segmented barplot for the variable 'sex'. The colors depend on the hair_colours.
#Show the proportions [0,1] on the y-axis.
head(starwars)
ggplot(data = starwars, aes(x=sex, fill=factor(hair_color))) + geom_bar(position = "fill")

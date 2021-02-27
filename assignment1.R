#installing required packages

install.packages("tidyverse")
install.packages("dplyr")
install.packages("hexbin")


getwd()  # get working directory (where is R currently looking)
setwd("D:/HSRW/3rd semester/Data Mining/Assignment 1")  # set working directory
getwd()

#loading necessary libraries
library(ggplot2)   
library(tidyr)
library(dplyr)
library(tidyverse) 
library(Hmisc)
library(hexbin)

d1 <- read.csv("census_2011_mod.csv",header=TRUE)   #loading data-set into variable d1

#df<- separate(d1,col = 'A','B',c('region_id','region','age','employed_male','unemployed_male','inactive_male','employed_female','unemployed_female','inactive_female'),sep =';',remove = TRUE,convert = FALSE, extra = "warn", fill = "warn")


head(d1) #checking first 6 rows of the data
dim(d1)  #checking dimension of the data (300 rows, 9 column)

v <- nchar(gsub("\\D", "", d1$region_id)) #counting the number of digits in region_id

#observatory data-set 1
ds1 <- subset(d1, v == 4, select = c(1:9))  #separating into a data=set (ds1) where region_id == 4
write.csv(ds1, "dataset1.csv")  #exporting the dataset into csv
head(ds1)
dim(ds1)
describe(ds1)   #description of the data-set
summary(ds1)  #summarize statistics of ds1


#summary stats for male
summary(ds1[c("employed_male", 	"unemployed_male", 	"inactive_male")])  #summarize statistics of ds1

#summary stats for female
summary(ds1[c("employed_female", 	"unemployed_female", 	"inactive_female")])  #summarize statistics of ds1


#observatory data-set 2
ds2 <- subset(d1, v == 7, select = c(1:9))  #separating into a data=set (ds1) where region_id == 7
write.csv(ds2, "dataset2.csv")   #exporting the data-set into csv
head(ds2)
dim(ds2)
describe(ds2)  #description of the data-set
summary(ds2)   #summarize statistics of ds2


#summary stats for male
summary(ds2[c("employed_male", 	"unemployed_male", 	"inactive_male")])  #summarize statistics of ds2


#ggplot

ggplot(ds1,aes(employed_male,employed_female, col = region))+geom_point(size = 4)+facet_wrap(vars(age))+ labs(title = "Employment ration based on age",
                                                                                                              x = "Number of employed male",
                                                                                                              y = "Number of employed female") +theme_bw() +theme(text=element_text(size = 16))

ggplot(ds2,aes(employed_male,employed_female, col = region))+geom_point(size = 2)+facet_wrap(vars(age))+ labs(title = "Employment ration based on age",
                                                                                    x = "Number of employed male",
                                                                                    y = "Number of employed female") +theme_bw() +theme(text=element_text(size = 16))

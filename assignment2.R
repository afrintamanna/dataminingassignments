install.packages("arules")
install.packages("arulesViz")
install.packages("knitr")
install.packages("lubridate")
install.packages("plyr")
install.packages("RColorBrewer")

#loading necessary libraries
library(ggplot2)   
library(tidyr)
library(dplyr)
library(plyr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(arulesViz)
library(arules)
library(knitr)
library(lubridate)
library(RColorBrewer)


getwd()  # get working directory (where is R currently looking)
setwd("D:/HSRW/3rd semester/Data Mining/Assignment 2")  # set working directory

d2 <- read.csv("assignment2_data.csv",header=TRUE)   #loading data-set into variable d2

head(d2) #checking first 6 rows of the data
dim(d2)  #checking dimension of the data (455682 obs, 7 variables)


#complete.cases(data) will return a logical vector indicating which rows have no missing values. 
#Then use the vector to get only rows that are complete using retail

df <- d2[complete.cases(d2), ]  #(4556252 obs, 7 variables)


df %>% mutate(article_name = as.factor(article_name))  #before splitting, coverting as factor type
df %>% mutate(article_name = as.factor(article_group))


#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
shopping_basket <- ddply(df,c("basket_id","date"),
                         function(df1)paste(df1$article_name,
                                            collapse = ","))



#set column basket_id of dataframe transactionData  
shopping_basket$basket_id <- NULL

#set column Date of dataframe transactionData
shopping_basket$date <- NULL

#Rename column to items
colnames(shopping_basket) <- c("products")

#Show Dataframe shopping_basket
shopping_basket

#the basket format data is being stored as csv file
write.csv(shopping_basket,"D:/HSRW/3rd semester/Data Mining/Assignment 2/transaction_data.csv", quote = FALSE, row.names = FALSE)

retail_transactions <- read.transactions('D:/HSRW/3rd semester/Data Mining/Assignment 2/transaction_data.csv', format = 'basket', sep=',')
summary(retail_transactions)

itemFrequencyPlot(retail_transactions,topN=12,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(retail_transactions,topN=12,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")


#mining the rules
# Min Support as 0.001, confidence as 0.1.
association.rules <-  apriori(retail_transactions, parameter = list(supp=0.001, conf=0.1,maxlen=100))
summary(association.rules)
inspect(association.rules[1:10])


#customer who bought bath towels
outdoor_towel.association.rules <- apriori(retail_transactions, parameter = list(supp=0.001, conf=0.1),appearance = list(default="lhs",rhs="outdoor towel"))
inspect(head(outdoor_towel.association.rules))

#customer who bought bath towels also bought
outdoor_towel.association.rules <- apriori(retail_transactions, parameter = list(supp=0.001, conf=0.1),appearance = list(lhs="outdoor towel",default="rhs"))
inspect(outdoor_towel.association.rules)


#graph based
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

plot(association.rules, method="grouped", control = list(k=10))


# Filter top 10 rules with highest lift
subRules<-head(subRules, n=10, by="lift")
plot(subRules, method="paracoord")

#installing required packages 

install.packages("readr") 
install.packages("arules") 
install.packages("arulesViz") 
install.packages("splitstackshape") 
install.packages("plyr") 
install.packages("dplyr") 
install.packages("tidyverse") 
install.packages("RColorBrewer") 
install.packages("ggplot2") 
install.packages("knitr")
install.packages("grid")
install.packages("rCBA")

#loading required packages
require(readr) 
require(arules) 
require(splitstackshape) 
require(plyr)
require(dplyr) 
require(tidyverse) 
require(RColorBrewer) 
library(readxl) 
library(arulesViz)


#load excel file
snacks<-read_excel("C:/Users/Desktop/Academics/PA/Apriori/snacksNew.xlsx") 

dtnew<- split(snacks$labels,snacks$order_id)


# Converting data to a class of transactions 

dt2new = as(dtnew,"transactions")

#Plotting 

itemFrequencyPlot(dt2new,topN=20,type="absolute")

rulesNew = apriori(dt2new, parameter=list(support=0.1, confidence=0.8, minlen = 3)) 
plot(rulesNew,control=list(col=brewer.pal(11,"Spectral")),main="")
length(rulesNew) 
inspect(rulesNew[1:10])

# Remove Unnecessary Rules 
subset.rules =
which(colSums(is.subset(rulesNew, rulesNew)) > 1)


# get subset rules in vector"which() returns the position of elements in the vector for which
# value is TRUE. colSums() forms a row and column sums for dataframes and numeric arrays. is.subset() 
# Determines if elements of one vector contain all the elements of other" length(subset.rules)


# to delete the subset from superset as superset will have subset 
rules = rulesNew[-subset.rules]
length(rules)

# What are customers likely to buy before they purchase "Boondi Raita" 
rulesBoondiRaita<-apriori(dt2new, parameter=list(supp=0.1,conf = 0.8), 
    appearance = list(default="lhs",rhs="Boondi Raita"))

rulesBoondiRaita<-sort(rulesBoondiRaita, decreasing=TRUE,by="confidence") 
inspect(rulesBoondiRaita[1:10])

toprules = rules[1:15] 
plot(rules, method = "graph")


plot(toprules, method = "graph", engine = "htmlwidget")


toprules_lift = head(rules, n=20, by ="lift") 

plot(toprules_lift, method="paracoord")

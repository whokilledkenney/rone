#An ANOVA (analysis of variance) is an extension of the t-test that allows you to compare the means of >2 groups.

install.packages("tidyr")
library(tidyr)

#read in your data file, attach
data <- read.csv(file.choose(),header=TRUE,na.string=".")
attach(data)
names(data) #shows header names of your data file

#####CHANGE THESE LINES TO MATCH HEADER NAMES#############
treatment1 <- c(ColumnName1)
treatment2 <- c(ColumnName2)
treatment3 <- c(ColumnName3)
#you will need to add additional lines if you have > 3 treatments

#calculate means of each treatment group
mean(treatment1)
mean(treatment2)
mean(treatment3)

#calculate standard errors of each group
se <- function(x) {sd(x,na.rm=TRUE)/sqrt(length(x))}
se(treatment1)
se(treatment2)
se(treatment3)

############CHANGE "ColumnName1:ColumnName3" TO YOUR HEADER NAMES##############
newdata <- gather(data, treatment, measurement, ColumnName1:ColumnName3, factor_key=TRUE) #converts data into form usable by anova

#One-way ANOVA
summary(aov(lm(measurement~treatment, data = newdata)))

#pairwise comparisons
TukeyHSD(aov(lm(measurement~treatment, data = newdata)))

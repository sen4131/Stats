#A completer prediction analysis
# install.packages("psych")
# install.packages("ggplot2")

#open data
MyData <- read.csv(file="/home/sen/Desktop/fall2017data.csv", header=TRUE, sep=",")

# Dependend variable as a global variable
y <<- MyData$metsAtt
ind_var <<- 'mestsAll'


#Histogram for all variables
for(i in names(MyData)){
  hist(MyData[,i], main = i , xlab = i, density=10, freq = FALSE)
  curve(dnorm(x, mean=mean(MyData[,i]), sd=sd(MyData[,i])), add = TRUE, col="darkblue")
  }

#Scatterplot for all variables
for(i in names(MyData)){
  plot(MyData[,i], y, xlab = i)}

#Sequence charts for all variables
for(i in names(MyData)){
  plot(MyData[,i], ylab = i)}

#discriptive stats
library(psych)
describe(MyData)

#correlation matrix
round(cor(MyData),2)

#summary of regression
#
#NEED TO ADJ THE OUTPUT VARIABLE
model = lm(metsAtt ~ ., data = MyData)
summary(model)

#plot of residuals
hist(resid(model), xlab = 'resid', main = 'Histogram of residuals', freq = FALSE)
curve(dnorm(x, mean=mean(resid(model)), sd=sd(resid(model))), add = TRUE, col="darkblue")

#regression line
library(ggplot2)


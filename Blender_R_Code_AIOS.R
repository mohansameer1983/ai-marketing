
#load data 
df <- read.csv(file.choose())
####### Display the structure of the R object                   
str(df)

# basic descriptive statistics 
summary(df)

# check basic data suitability
# generalized pair graphs to check for bivariate correlations
#install.packages("gpairs") # only run once
library(gpairs)
library(corrplot)
library(ggplot2)
library(coefplot)

require(gpairs)

gpairs(df)

# Bi-variate plots
plot(sales_units~Price, data=df, xlab="Price", ylab="Sales_Units")

#Transforming all Advertisement Channels to square root
#df$log_online <- log(df$online_ads)
df$sqrt_TV <- sqrt(df$TV_ads)
df$sqrt_Online <- sqrt(df$online_ads)
df$sqrt_Print <- sqrt(df$Print_ads)

gpairs(df)

#Creating pairwise Variables
#Sqrt(TV*Online)
df$sqrt_TVOnline<- sqrt(df$TV_ads*df$online_ads)
#Sqrt(TV*Print)
df$sqrt_TVPrint<- sqrt(df$TV_ads*df$Print_ads)
#Sqrt(Online*Print)
df$sqrt_OnlinePrint<- sqrt(df$online_ads*df$Print_ads)

gpairs(df)

#check for correlation
corrplot.mixed(cor(df[ , c(2, 2:6)]), upper="ellipse")

#split the data into training and test sample

train.df<-df[1:112,]
test.df<-df[112:150,]

#model 1 -> Advertising Spending -> Train

m1.train <- lm(sales_units ~sqrt_TV + sqrt_Online + sqrt_Print , data=train.df)
summary(m1.train)
par(mfrow=c(2,2))
plot(m1.train)

#model 1 -> Test
m1.test <- predict(m1.train,test.df)
summary(m1.test)
par(mfrow=c(2,2))
plot(m1.test)

# Compute R-squared in the test sample 
# R-squared = Explained variation / Total variation
SSE = sum((test.df$sales_units - m1.test)^2) # Explained variation
SST = sum((test.df$sales_units - mean(test.df$sales_units))^2) # Total Variation
Rsq = 1 - SSE/SST
Rsq


#model 2 -> Advertising Spending + Controls -> Train
m2.train <- lm(sales_units ~sqrt_TV + sqrt_Online + sqrt_Print + Price + Lag, data=train.df)
summary(m2.train)
plot(m2.train)

#lagged sales not relevant, dropping lag sales
m2.train <- lm(sales_units ~sqrt_TV + sqrt_Online + sqrt_Print + Price , data=train.df)
summary(m2.train)
plot(m2.train)


#model 2 -> Test
m2.test <- predict(m2.train,test.df)
SSE = sum((test.df$sales_units - m2.test)^2) # Explained variation
SST = sum((test.df$sales_units - mean(test.df$sales_units))^2) # Total Variation
Rsq = 1 - SSE/SST
Rsq

anova(m1.train, m2.train)


#model 3 -> Advertising Spending + Controls + Pairwise -> Train

m3.train <- lm(sales_units ~sqrt_TV + sqrt_Online + sqrt_Print + Price + Lag + sqrt_TVOnline + sqrt_OnlinePrint + sqrt_TVPrint, data=train.df)
summary(m3.train)

#Model 3.1 Dropping lag sales
m3.train <- lm(sales_units ~sqrt_TV + sqrt_Online + sqrt_Print + Price  + sqrt_TVOnline + sqrt_OnlinePrint + sqrt_TVPrint, data=train.df)
summary(m3.train)

#Model 3.2 Dropping sqrt_TVPrint
m3.train <- lm(sales_units ~sqrt_TV + sqrt_Online + sqrt_Print + Price  + sqrt_TVOnline + sqrt_OnlinePrint, data=train.df)
summary(m3.train)

#Model 3.3 Dropping sqrt_online
m3.train <- lm(sales_units ~sqrt_TV + sqrt_Print + Price  + sqrt_TVOnline + sqrt_OnlinePrint, data=train.df)
summary(m3.train)

#Model 3.4 Dropping sqrt_OnlinePrint
m3.train <- lm(sales_units ~sqrt_TV + sqrt_Print + Price  + sqrt_TVOnline, data=train.df)
summary(m3.train)

plot(m3.train)

#model 3 - test
m3.test <- predict(m3.train,test.df)
SSE = sum((test.df$sales_units - m3.test)^2) # Explained variation
SST = sum((test.df$sales_units - mean(test.df$sales_units))^2) # Total Variation
Rsq = 1 - SSE/SST
Rsq

anova(m2.train, m3.train)

coefplot(m3.train, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Sales Units")


# visualize the fitted values 

plot(test.df$sales_units,type='l', col="blue", xlab=NA, ylab=NA)
# Create a title with a red, bold/italic font
title(main="Holdout Prediction", col.main="red", font.main=4)
# Graph trucks with red dashed line and square points
lines(m1.test, pch=22, lty=2, lwd=2,col="red")
lines(m2.test, lty=3,lwd=2, col="orange")
lines(m3.test, lty=3,lwd=2, col="green")
# Label the x and y axes with dark green text
title(xlab="Time", col.lab=rgb(0,0.5,0))
title(ylab="Sales_Unit", col.lab=rgb(0,0.5,0))

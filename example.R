# Introduction to R
# Copyright 2013 by Ani Katchova
# Set working directory to where csv file is located
setwd("C:/Econometrics/Data")
# Read the data
mydata<- read.csv("C:/Econometrics/Data/intro_auto.csv")
attach(mydata)
# List the variables
names(mydata)
# Show first lines of data
head(mydata)
mydata[1:10,]
# Descriptive statistics
summary(mpg)
sd(mpg)
length(mpg)
summary(price)
sd(price)
# Sort the data
sort(make)
# Frequency tables
table(make)
table (make, foreign)
# Correlation among variables
cor(price, mpg)
# T-test for mean of one group
t.test(mpg, mu=20)
# ANOVA for equality of means for two groups
anova(lm(mpg ~ factor(foreign)))
# OLS regression - mpg (dependent variable) and weight, length and foreign
(independent variables)
olsreg <- lm(mpg ~ weight + length + foreign)
summary(olsreg)
# summary(lm(mpg ~ weight + length + foreign))
# Plotting data
plot (mpg ~ weight)
olsreg1 <- lm(mpg ~ weight)
abline(olsreg1)
# Redefining variables
Y <- cbind(mpg)
X <- cbind(weight, length, foreign)
summary(Y)
summary(X)
olsreg <- lm(Y ~ X)
summary(olsreg)
# Install and use packages
# install.packages("plm")
# library(plm)
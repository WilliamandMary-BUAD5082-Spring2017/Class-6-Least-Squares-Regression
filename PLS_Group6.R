##################################
#Chapter 6 - Lab 6.7.2 - Page 258#
##################################

rm(list=ls())
#use library ISLR to obtain Hitters data set
library(ISLR)

#Initial Set Up for the Chapter 6 Lab 3 - PLS 
#investigate the Hitters data set
names(Hitters)
dim(Hitters)

#need to remove NA's from our dataset
Hitters=na.omit(Hitters)

#need to pass x in as a vector
#remove the first column, row names, in order to analyze data
x=model.matrix(Salary~.,Hitters)[,-1]
#need to pass y in as a vector 
y=Hitters$Salary

set.seed(1)
#set up the train and the test set
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Partial Least Squares

#Use the pls package and install below if necessary
#install.packages("pls")
library(pls)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
#Lowest Cross-Validation error occurs when M=2

#Now evaluate corresponding test MSE (for 2 components)
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
#Test MSE for M=2 is comparable (but slightly higher than) the test MSE
#obtained using ridge regression, the lasso, and PCR

#Perform PLS using the full data set, using M=2, the number of components
#identified by cross-validation
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
#Notice that the 46.40% of variance in Salary, with M=2, is almost as much as 
#the M=7 model PCR fit of 46.69%. PCR only attempts to maximize
#amount of variance explained in the predictors, while PLS searches for directions
#that explain variance in both the predictors and the response.

##########################################
#More PLS Practice - College data in ISLR#
##########################################

rm(list=ls())
#Use College data set in ISLR library
library(ISLR)

#Look at the Boston data set
names(College)
dim(College)

x=model.matrix(Grad.Rate~.,College)[,-1]
y=College$Grad.Rate

set.seed(1)
#Set up the train and the test set
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

#Partial Least Squares, Interactive Version
#install.packages("pls")
library(pls)

#1. Use pls.fit in order to fit Grad.Rate


#2. Print the summary for pls.fit


#3. What number of components has the lowest Cross-Validation?


#4. What is the cross validation rate of that number of components?


#5. Use the validation plot in order to visualize the number of components versus "MSEP"


#6. Determine the Test MSE for the number of components used above (2 lines)


#7. Perform PLS using the full data set using the number of components
#identified by cross-validation


#8. Print the summary for pls.fit



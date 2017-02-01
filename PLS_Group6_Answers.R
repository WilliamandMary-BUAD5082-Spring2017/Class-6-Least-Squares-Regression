rm(list=ls())
#Use College data set in ISLR library
library(ISLR)

#Look at the College data set
names(College)
dim(College)

x=model.matrix(Grad.Rate~.,College)[,-1]
y=College$Grad.Rate

set.seed(1)
#Set up the train and the test set
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Partial Least Squares, Interactive Version
#install.packages("pls")
library(pls)

#1. Use pls.fit in order to fit Grad.Rate
pls.fit=plsr(Grad.Rate~., data=College,subset=train,scale=TRUE, validation="CV")

#2. Print the summary for pls.fit
summary(pls.fit)

#3. What number of components has the lowest Cross-Validation?
#4 components

#4. What is the cross validation rate of that number of components?
#12.38

#5. Use the validation plot in order to visualize the number of components versus "MSEP"
validationplot(pls.fit,val.type="MSEP")

#6. Determine the Test MSE for the number of components used above (2 lines)
pls.pred=predict(pls.fit,x[test,],ncomp=4)
mean((pls.pred-y.test)^2)

#7. Perform PLS using the full data set using the number of components
#identified by cross-validation
pls.fit=plsr(Grad.Rate~., data=College,scale=TRUE,ncomp=4)

#8. Print the summary for pls.fit
summary(pls.fit)

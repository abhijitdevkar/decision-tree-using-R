#Classification of data using decision tree ctree method

install.packages('party') 
library('party')
str(iris)
set.seed(1200)
ind <-sample(2,nrow(iris),replace = TRUE,prob = c(0.7,0.3))
trainData <-iris[ind==1,]
testData <-iris[ind==2,]
formula<-Species~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
#predict on train data
iris_ctree <- ctree(formula,data=trainData)
table(predict(iris_ctree),trainData$Species)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree,type = "simple")
#Predict on test data
iris_ctree1 <- ctree(formula,data=testData)
table(predict(iris_ctree1),testData$Species)
print(iris_ctree1)
plot(iris_ctree1)
plot(iris_ctree1,type = "simple")


#Classification of data using decision tree C5.0 method:
#get the C5.0 package 

install.packages('C50') 
library('C50') # load the package  

ir <- datasets::iris# open iris dataset 
# summary, boxplot, pairs plot 
summary(ir) 
boxplot(ir[-5], main = 'Boxplot of Iris data by attributes') 
pairs(ir[,-5], main="Edgar Anderson's iris Data", pch=21, bg = c("black", "red", "blue")[unclass(ir$Classification)]) 
cTree <- C5.0(ir[,-5], ir[,5]) 
summary(cTree) # view the model components  

plot(cTree, main = 'Iris decision tree') # view the model graphically [unclass(ir$Classification)])

# build a rules set  
irRules <- C5.0(ir[,-5], ir[,5], rules = TRUE) 
summary(irRules) # view the ruleset 


#Classification of data using decision tree CART method:

install.packages('rpart') 
library('rpart') 

# create a label for our formula
f = ir$Classification ~ ir$SL + ir$SW + ir$PL + ir$PW  

f = ir$Species ~ ir$Sepal.Length + ir$Sepal.Width + ir$Petal.Length + ir$Petal.Width

# train the tree 
irrTree = rpart(f, method = 'class') 

# view the tree summary 
summary(irrTree) 
# view a text version of the tree 
print(irrTree)


par(xpd = TRUE) # define graphic parameter 
plot(irrTree, main = 'Iris regresion tree') # plot the tree 
text(irrTree, use.n = TRUE) # add text labels to tree 

rpart.plot(irrTree, main = 'Iris regresion tree') # a better tree plot 




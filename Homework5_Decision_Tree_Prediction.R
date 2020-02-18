#read in the dataset
adult <- read.csv("C:/Users/P2190101/KDD/DataSet/AdultUCI.csv",na.strings=c("NA",""))

head(adult)
#Load required packages
# Load needed packages
install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('tree')
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(tree)

#Data Cleaning
summary(adult)

adult3 <- na.omit(adult,cols=income) #Response variable has a lot missing value whch could affect accuracy negatively, hence ommited.
summary(adult3)

adult3$native.country <- NULL # native.country has factor levels more than 32.


#Data Partition
set.seed(101)
sample <- sample.split(adult3$income, SplitRatio = 0.8)
df.train = subset(adult3, sample ==TRUE)
df.valid = subset(adult3, sample ==FALSE)

# Build a decision tree model
tree.model <- rpart(income~.,method="class",data=df.train)
printcp(tree.model)
prp(tree.model,type=2,extra=106)


#Build & Plot Decision Tree

adultpred_tree <- predict (tree.model,df.valid,type="class")
accuracy <- sum(adultpred_tree==df.valid$income)/nrow(df.valid)
accuracy

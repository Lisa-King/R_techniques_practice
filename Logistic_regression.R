#working on the Titanic dataset for logistic regression

#set the working directory
setwd("~/Desktop/data-5/39/8cb82054b06d221bd12080581cf3d088")
#import Titanic dataset
#The na.strings argument is for substitution within the body of the file, that is, matching strings that should be replaced with NA.
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

#get first few rows of the observations
head(training.data.raw)

#only extract some columns and built a new dataframe: data 
data <- training.data.raw[,c(2,3,5,6,7,8,10,12)]

#Missing value processing
#if the values of age is NA, using the mean value of non-NA valus of age to fill them;
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

#the rows which has data$Embarked as NA are removed;
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

#Split data into train and test, build a model on the training dataset.
train <- data[1:800,]
test <- data[801:889,]
model <- glm(Survived ~., family=binomial(link='logit'), data=train)
# let's have a look at the summary of the model
summary(model)

#Interpreting the results of our logistic regression model¶
# Pr(>chi) < 0.05 the last one in Pr(>chi) should be less than 0.05
anova(model, test="Chisq")

#make a prediction on the test dataset:
fitted.results <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type='response')
#the fitted resulted is a float number
fitted.results

#reset the final results as 1 if they > 0.5, otherwise 0; 
fitted.results <- ifelse(fitted.results > 0.5,1,0)
#MisclassificationError -> predicated result is not same with actual result 
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

# Plot the sensitivity-specificity curve and precision-recall curve for model evaluation
library(precrec)
library(ggplot2)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
precrec_obj <- evalmod(scores = p, labels = test$Survived)
autoplot(precrec_obj)

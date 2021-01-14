# Decision Tree: Tree based model
library (tree)
library (ISLR)

Carseats = read.csv("Carseats.csv",header = T, na.strings = "?")
Carseats = na.omit(Carseats)
head(Carseats)
attach(Carseats)

# Target variable definition
High = ifelse(Sales <=8,"No","Yes")
High <- as.factor(High)

# combine target variable with data.frame
Carseats <- data.frame(Carseats, High)
# Combine the two datasets together -- another method.
#wine <- rbind(red, white) 
head(Carseats)

# build the tree
tree.carseats = tree(High~.-Sales,Carseats)
# have a look at the summary info of the tree
summary(tree.carseats)
str(Carseats) # 400 observations and 12 variables

# plot the tree
plot(tree.carseats)
text(tree.carseats ,pretty =0)

# In order to properly evaluate the performance of a classification tree on these data, we must estimate the test error rather than simply computing the training error. 
# We split the observations into a training set and a test set, build the tree using the training set, and evaluate its performance on the test data.
set.seed(2)
# sample 50% of the data as training set
train=sample(1:nrow(Carseats), 200)
# test set is whatever left
Carseats.test=Carseats[-train,]
# target variable vector for test set (evaluation purpose later)
High.test=High[-train]
# build the tree on training set
tree.carseats = tree(High~.-Sales ,Carseats ,subset =train )
# make the prediction on test set
tree.pred=predict(tree.carseats ,Carseats.test ,type = "class")
# construct confusion table: prediction and actual result
table(tree.pred ,High.test)

# calculate the accuracy: 0.77
(104+50) /200

## Tree pruning
# The function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity. 
# We use the argument FUN=prune.misclass in order to indicate that we want the classification error rate to guide the cross-validation and pruning process.
set.seed(10)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)

# look at the cv result, k corresponds to α(which is a coefficient).
cv.carseats
# $size means the number of tree terminal nodes.
# $dev means the classification error rate, the lower the better.
# Therefore, when the tree has 9 terminal nodes, the classification error rate is the lowest 64.

# The tree with 9 terminal nodes results in the lowest cross-validation error rate, with 50 cross-validation errors.
# We plot the error rate as a function of both size and k.
par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

# Now apply the prune.misclass() function in order to prune the tree to obtain the nine-node tree (use prune.tree)
prune.carseats = prune.misclass(tree.carseats, best =9)
plot(prune.carseats)
text(prune.carseats,pretty =0)

# How well does this pruned tree perform on the test data set? Once again, we apply the predict() function.
tree.pred=predict(prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)

# accuracy is improved: 0.775
(97+58) /200

# ROC curve can be plotted since it's classification problem.
library(precrec)
library(ggplot2)

tree.pred=predict(prune.carseats, Carseats.test ,type="vector")
precrec_obj <- evalmod(scores = tree.pred[,2], labels = as.vector(Carseats.test$High))
precrec_obj

autoplot(precrec_obj)
detach(Carseats)

######################################################################################################
## Bagging and Random Forest
# The randomForest() function can be used to perform both random forests and bagging.
library(randomForest)
library(MASS)

Boston <- read.csv("BostonHousing.csv", header = T, na.strings = "?")
Boston = na.omit(Boston)
head(Boston)
str(Boston) # 506 obs. of  14 variables.


# define training data set
train = sample(1: nrow(Boston), nrow(Boston)/2) # 253 observations
# test set labels
boston.test=Boston[-train ,"medv"]
head(Boston)

# The argument mtry=13 indicates that all 13 predictors should be considered for each split of the tree.
# In other words, that bagging should be done.
set.seed (1)
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance =TRUE)
bag.boston

# making a prediction
yhat.bag = predict(bag.boston ,newdata =Boston[-train ,])
plot(yhat.bag, boston.test) # plot between the prediction and actual results
abline (0,1)

mean((yhat.bag - boston.test)^2)
# now we use a smaller value of the mtry argument
set.seed(1)
rf.boston =randomForest(medv~.,data=Boston ,subset =train ,mtry=5, ntree=500, importance =TRUE)
yhat.rf = predict (rf.boston ,newdata = Boston [-train ,])
mean(( yhat.rf - boston.test)^2)

# we can view the importance of each variable by using importance function
importance(rf.boston)

# two measures for variable importance which are:
# 1. mean decrease of accuracy in predictions
# 2. a measureof the total decrease in node impurity
varImpPlot(rf.boston)

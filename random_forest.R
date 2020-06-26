# random forest with CTG data
# by: Mohsin Uddin

# libraries used
library(randomForest)
library(caret)

# reading data
data = read.csv(file.choose(), header = T)
str(data)

# variable NSP is a factor. let's convert it.
data$NSP = as.factor(data$NSP)

# data partitioning
set.seed(123)
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train = data[ind == 1,]
test = data[ind == 2,]

# random forest
set.seed(234)
rf = randomForest(NSP~., data = train, ntree = 300,
                  mtry = 8, importance = T, proximity = T)
rf


# prediction & confusion matrix for train data
p1 = predict(rf, train)
confusionMatrix(p1, train$NSP)

# prediction & confusion matrix for test data
p2 = predict(rf, test)
confusionMatrix(p2, test$NSP)

# error rate of rf
plot(rf)

# tuning mtry
t = tuneRF(train[,-22], train[,22], stepFactor = 0.5, plot = T,
           ntreeTry = 300, trace = T, improve = 0.05)

# no. of nodes for trees
hist(treesize(rf), main = "no. of nodes for trees in random forest",
     col = "green")

# variable importance
varImpPlot(rf, sort = T, n.var = 10, main = "Importance of Variables")
importance(rf)
varUsed(rf)

# partial dependence plot
partialPlot(rf, train, ASTV, "2")

# single tree extraction from the forest
getTree(rf, 1, labelVar = T)

# multi dimension scaling plot of proximity matrix
MDSplot(rf, train$NSP)

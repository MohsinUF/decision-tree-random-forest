# decision tree with CTG data
# by: Mohsin Uddin

# libraries used
library(tidyverse)
library(party)

# reading data
data = read.csv(file.choose(), header = T)
str(data)

# variable NSP is actually a factor. so it needs to be converted.
data$NSP = as.factor(data$NSP)

# decision tree
tree = ctree(NSP~LB+AC+FM, data,
             controls = ctree_control(mincriterion = 0.9, 
                                      minsplit = 50))
tree             
plot(tree)

# misclassification error
tab = table(predict(tree), data$NSP)
tab
1 - sum(diag(tab))/sum(tab) # 18.43%

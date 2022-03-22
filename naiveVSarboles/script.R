if(!require(readr)){
    install.packages("readr")
    library(readr)
}

lens <- read_csv("ContactLens.csv", col_types = "fffff")

print(summary(lens))

lens$Age = factor(lens$Age, levels = c("young","pre-presbyopic","presbyopic"), labels = c(1.0, 2.0, 3.0))

lens$ContactLens = factor(lens$ContactLens, levels = c("none","soft","hard"), labels = c(0.0, 1.0, 2.0))

lens$SpectaclePrescrip = factor(lens$SpectaclePrescrip, levels = c("myope","hypermetrope"), labels = c(1.0, 2.0))

lens$Astigmatism = factor(lens$Astigmatism, levels = c("no","yes"), labels = c(0.0, 1.0))

lens$TearProdRate = factor(lens$TearProdRate, levels = c("reduced","normal"), labels = c(1.0, 0.0))

print(summary(lens))

set.seed(1234)
sample_set <- sample(nrow(lens), round(nrow(lens)*.80), replace = FALSE)

lens_train <- lens[sample_set,]
lens_test <- lens[-sample_set,]

# Naive Bayes
if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
}

lens_Naive <- naiveBayes(ContactLens ~ ., data = lens_train, laplace = 1)
lens_Naive_pred <- predict(lens_Naive, lens_test, type = "class")
lens_Naive_pred_table <- table(lens_test$ContactLens, lens_Naive_pred)
print(lens_Naive_pred_table)
print(lens_Naive)
print(sum(diag(lens_Naive_pred_table))/nrow(lens_test))

# Arbol
if(!require(rpart)){
   install.packages("rpart")
   library(rpart)
}

lens_Tree <- rpart(ContactLens ~ ., method = "class", data = lens_train, minsplit = 2, minbucket = 1, cp = -1)

if(!require(rpart.plot)){
   install.packages("rpart.plot")
   library(rpart.plot)
}

rpart.plot(lens_Tree)
lens_Tree_pred <- predict(lens_Tree, lens_test, type = "class")
lens_Tree_pred_table <- table(lens_test$ContactLens, lens_Tree_pred)
print(lens_Tree_pred_table)
print(lens_Tree)
print(sum(diag(lens_Tree_pred_table))/nrow(lens_test))

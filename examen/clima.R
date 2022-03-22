if(!require(readr)){
    install.packages("readr")
    library(readr)
}
if(!require(rpart)){
    install.packages("rpart")
    library(rpart)
}
if(!require(rpart.plot)){
    install.packages("rpart.plot")
    library(rpart.plot)
}
if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
}
if(!require(caret)){
    install.packages("caret")
    library(caret)
}

df <- read_csv("weather1.csv", col_types = "fnnff")

View(df)

set.seed(9876)
ind <- sample(nrow(df), round(nrow(df)*0.8),replace = FALSE)
df.train <- df[ind,]
df.test <- df[-ind,]

df.tree <- rpart(play ~ ., data = df.train, method = "class", minsplit = 2, minbucket = 1, cp = -1)

print(df.tree)

rpart.plot(df.tree)

df.pred <- predict(df.tree, df.test, type = "class")

df.predTable <- table(df.test$play, df.pred)

print(df.predTable)

print(sum(diag(df.predTable))/nrow(df.test))

df.Bayes <- naiveBayes(play ~ ., data = df.train)

#Entrenamiento
#print(confusionMatrix(df.train$play, predict(df.Bayes, df.train)))
#Validación
print(confusionMatrix(df.test$play, predict(df.Bayes, df.test)))


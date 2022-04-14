if(!require(readr)){
    install.packages("readr")
    library(readr)
}
if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
}
if(!require(gmodels)){
    install.packages("gmodels")
    library(gmodels)
}

df <- read_csv("anthrokids.csv", col_types = "nnnff")

set.seed(2347)
ind <- sample(nrow(df), round(nrow(df)*0.7),replace = FALSE)
df.train <- df[ind,]
df.test <- df[-ind,]

svm.model <-svm(Race ~., data = df.train, cost =100, gamma =1)

svm_pred_train <-predict(svm.model, df.train[,-5])

print("Conjunto de Entrenamiento")
CrossTable(df.train$Race, svm_pred_train, prop.chisq =FALSE, prop.c =FALSE, prop.r =FALSE, dnn =c('actual default', 'predicted default'))

svm_pred_test <-predict(svm.model, df.test[,-5])


print("Conjunto de Testeo")
CrossTable(df.test$Race, svm_pred_test, prop.chisq =FALSE, prop.c =FALSE, prop.r =FALSE, dnn =c('actual default', 'predicted default'))

if(!require(readr)){
    install.packages("readr")
    library(readr)
}
if(!require(class)){
    install.packages("class")
    library(class)
}
if(!require(gmodels)){
    install.packages("gmodels")
    library(gmodels)
}

df <- read_csv("BostonHousing.csv", col_types = "nnnfnnnnnnnnnf")
#View(df)


set.seed(1942)
ind <- sample (2, nrow(df), replace = TRUE, prob = c(0.6, 0.4))

df.train <- df[ind == 1,1:12]
df.test <- df[ind == 2,1:12]

df.trainLabels <- df[ind == 1,13,drop = TRUE]
df.testLabels <- df[ind == 2,13,drop = TRUE]

df_pred <- knn(train = df.train, test = df.test, cl = df.trainLabels, k=1)

#K1 <- knn(train = df.train, test = df.test, cl = df.trainLabels, k=1)
#K2 <- knn(train = df.train, test = df.test, cl = df.trainLabels, k=2)
#K3 <- knn(train = df.train, test = df.test, cl = df.trainLabels, k=3)
#K4 <- knn(train = df.train, test = df.test, cl = df.trainLabels, k=4)
#K5 <- knn(train = df.train, test = df.test, cl = df.trainLabels, k=5)

print(summary(df_pred))

CrossTable(x=df_pred, y=df.testLabels, prop.chisq = FALSE)

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

df <- read_csv("hsbdemo.csv", col_types = "nffffnnnnnfff")
View(df)


set.seed(1942)
ind <- sample (2, nrow(df), replace = TRUE, prob = c(0.75, 0.25))

df.train <- df[ind == 1,6:10]
df.test <- df[ind == 2,6:10]

df.trainLabels <- df[ind == 1,5,drop = TRUE]
df.testLabels <- df[ind == 2,5,drop = TRUE]

df_pred <- knn(train = df.train, test = df.test, cl = df.trainLabels, k=3)

print(summary(df_pred))

CrossTable(x=df_pred, y=df.testLabels, prop.chisq = FALSE)

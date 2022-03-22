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

df <- read_csv("FlightDelays.csv", col_types = "nfnfnfnffffff")

df$CRS_DEP_TIME = cut(df$CRS_DEP_TIME, seq(600,2200,200), labels = 1:8, include.lowest = TRUE)

set.seed(4583)
ind <- sample(nrow(df), round(nrow(df)*0.8),replace = FALSE)
df.train <- df[ind,]
df.test <- df[-ind,]

df.tree <- rpart(FlightStatus ~ CRS_DEP_TIME + CARRIER + DEST + DISTANCE + FL_DATE + FL_NUM + ORIGIN + Weather + DAY_WEEK + TAIL_NUM, data = df.train, method = "class")

#print(df.tree)

df.pred <- predict(df.tree, df.test, type = "class")

df.predTable <- table(df.test$FlightStatus, df.pred)

print(df.predTable)

print(sum(diag(df.predTable))/nrow(df.test))

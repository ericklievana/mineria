housing <- read.csv("USA_Housing.csv", header = TRUE, sep = ",")

print(head(housing))

if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}

print(ggplot(data=housing, aes(housing$Price)) + geom_histogram(aes(y =..density..), fill = "orange") + geom_density())

if(!require(psych)){
    install.packages("psych")
    library(psych)
}

psych::describe(housing)

if(!require(reshape)){
    install.packages("reshape")
    library(reshape)
}

meltData <- melt(housing)

p <- ggplot(meltData, aes(factor(variable), value)) + geom_boxplot() + facet_wrap(~variable, scale="free")

if(!require(corrgram)){
    install.packages("corrgram")
    library(corrgram)
}

corrgram(housing, order=TRUE)

if(!require(caret)){
    install.packages("caret")
    library(caret)
}

index <- createDataPartition(housing$Price, p = .70, list = FALSE)

train <- housing[index, ]

test <- housing[-index, ]

dim(train)

lmModel <- lm(Price ~ . , data = train)

print(lmModel)

print(summary(lmModel))

print(AIC(lmModel))

print(BIC(lmModel))

print(names(lmModel))

if(!require(Metrics)){
    install.packages("Metrics")
    library(Metrics)
}

rmse(actual = train$Price, predicted = lmModel$fitted.values)

hist(lmModel$residuals, color = "grey")

plot(lmModel)

if(!require(fmsb)){
    install.packages("fmsb")
    library(fmsb)
}

source("vif_fun.r")

vif_func(housing[,1:5])

if(!require(lmtest)){
    install.packages("lmtest")
    library(lmtest)
}

dwtest(lmModel)

test$PreditedPrice <- predict(lmModel, test)

print(head(test[ , c("Price", "PreditedPrice")]))

actual <- test$Price
preds <- test$PreditedPrice
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
print(rsq)

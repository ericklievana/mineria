if(!require(MASS)){
    install.packages("MASS")
    library(MASS)
}
if(!require(caret)){
    install.packages("caret")
    library(caret)
}
data(Boston)
str(Boston)

model <- knnreg(medv ~ ., data = Boston)
print(model)
plot(model)

set.seed(1493)
model2 <- train(medv ~ ., data = Boston, method = 'knn', preProcess = c("center", "scale"))
print(model2)
plot(model2)

set.seed(1978)
inTraining <- createDataPartition(Boston$medv, p = .80, list = FALSE)
training <- Boston[inTraining,]
testing <- Boston[-inTraining,]
model3 <- train(medv ~ ., data = training, method = 'knn', preProcess = c("center", "scale"))
print(model3)

test.features = subset(testing, select=-c(medv))
test.target = subset(testing, select=medv)[,1]
predictions = predict(model3, newdata = test.features)
print(sqrt(mean((test.target - predictions)^2)))
print(cor(test.target, predictions) ^ 2)

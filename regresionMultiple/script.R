car.df <- read.csv("ToyotaCorolla.csv")
car.df <- car.df[1:1000, ]
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
View(car.df)
set.seed(1433)
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
car.lm <- lm(Price ~ ., data = train.df)
options(scipen = 999)
summary(car.lm)
if(!require(forecast)){
    install.packages("forecast")
    library(forecast)
}
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20], "Residual" = some.residuals)
options(scipen=999, digits = 3)
accuracy(car.lm.pred, valid.df$Price)
car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
if(!require(leaps)){
    install.packages("leaps")
    library(leaps)
}
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))
train.df <- cbind(train.df[,-4], Fuel_Type[,])
head(train.df)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
print(sum$which)
print(sum$rsq)
print(sum$adjr2)
print(sum$Cp)
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
car.lm.null <- lm(Price~1, data = train.df)
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step)
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

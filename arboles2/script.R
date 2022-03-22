if(!require(TH.data)){
    install.packages("TH.data")
    library(TH.data)
}

data("bodyfat", package = "TH.data")

str(bodyfat)

print(dim(bodyfat))

print(attributes(bodyfat))

print(bodyfat[1:5,])

set.seed(1234)

ind <- sample(2, nrow(bodyfat),replace=TRUE,prob=c(0.9,0.1))

bodyfat.train <- bodyfat[ind==1,]

bodyfat.test <- bodyfat[ind==2,]

if(!require(rpart)){
    install.packages("rpart")
    library(rpart)
}

myFormula <- DEXfat~age+waistcirc+hipcirc+elbowbreadth+kneebreadth

bodyfat_rpart <- rpart(myFormula,data = bodyfat.train, control=rpart.control(minsplit=10))

attributes(bodyfat_rpart)

print(bodyfat_rpart$cptable)

print(bodyfat_rpart)

plot(bodyfat_rpart)

text(bodyfat_rpart, use.n=TRUE)

opt <- which.min(bodyfat_rpart$cptable[,"xerror"])

cp <- bodyfat_rpart$cptable[opt,"CP"]

bodyfat_prune <- prune(bodyfat_rpart, cp = cp)

print(bodyfat_prune)

DEXfat_pred <- predict(bodyfat_prune, newdata = bodyfat.test)

xlim <- range(bodyfat$DEXfat)

#plot(DEXfat_pred ~ DEXfat, data = bodyfat.test, xlab = "Observed", ylab = "Predicted", ylim = xlim, xlim = xlim, jitter = T)

#abline(a=0,b=1)

x1s <- c(.5,1,1,2,3,3.5, 1,3.5,4,5,5.5,6)

x2s <- c(3.5,1,2.5,2,1,1.2, 5.8,3,4,5,4,1)

ys <- c(rep(+1,6), rep(-1,6))

my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))

plot(my.data[,-3],col=(ys+3)/2, pch=19); abline(h=0,v=0,lty=3)

invisible(readline(prompt="Press [enter] to continue"))

if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
}

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear',scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)

invisible(readline(prompt="Press [enter] to continue"))

points(my.data[svm.model$index,c(1,2)],col="blue",cex=2)

w <- t(svm.model$coefs) %*% svm.model$SV

b <- -svm.model$rho

abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

invisible(readline(prompt="Press [enter] to continue"))

observations <- data.frame(x1=c(1,3.5),x2=c(4,3.5))

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)

invisible(readline(prompt="Press [enter] to continue"))

points(observations[1,], col="green", pch=19)

points(observations[2,], col="blue", pch=19)

abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

invisible(readline(prompt="Press [enter] to continue"))

predict(svm.model, observations)

data(iris)

svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear")

plot(iris$Sepal.Length, iris$Sepal.Width, col = as.integer(iris[, 5]), pch = c("o","+")[1:150 %in% svm.model$index + 1], cex = 2, xlab = "Sepal length", ylab = "Sepal width")

invisible(readline(prompt="Press [enter] to continue"))

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, slice = list(sepal.width = 1, sepal.length = 2))

invisible(readline(prompt="Press [enter] to continue"))

svm.pred <- predict(svm.model, iris[,-5])

table(pred = svm.pred, true = iris[,5])

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='polynomial', degree=8, gamma=0.1, coef0=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)

invisible(readline(prompt="Press [enter] to continue"))

points(my.data[svm.model$index,c(1,2)],col="blue",cex=2)

invisible(readline(prompt="Press [enter] to continue"))

svm.pred <- predict(svm.model, my.data[,-3])

table(pred = svm.pred, true = my.data[,3])

svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, slice = list(Sepal.Width = 1, Sepal.Length = 2))

invisible(readline(prompt="Press [enter] to continue"))

svm.pred <- predict(svm.model, iris[,-5])

table(pred = svm.pred, true = iris[,5])

svm.model <- svm(Species ~ ., data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, slice = list(Petal.Width = 3, Petal.Length = 2.5))

invisible(readline(prompt="Press [enter] to continue"))

svm.pred <- predict(svm.model, iris[,-5])

table(pred = svm.pred, true = iris[,5])

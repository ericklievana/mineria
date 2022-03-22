if(!require(ggvis)){
    install.packages("ggvis")
    library(ggvis)
}
if(!require(class)){
    install.packages("class")
    library(class)
}
if(!require(gmodels)){
    install.packages("gmodels")
    library(gmodels)
}

options(browser = "firefox")

print(iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species))
print(iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points)

print(iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species))
print(iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points)

set.seed(1234)
ind <- sample (2, nrow (iris), replace = TRUE, prob = c(0.67, 0.33))
iris.training <- iris [ind == 1, 1: 4]
iris.test <- iris [ind == 2, 1: 4]

iris.trainLabels <- iris[ind==1,5]
iris.testLabels <- iris[ind==2,5]

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

print(summary(iris_pred))

CrossTable(x=iris_pred, y=iris.testLabels, prop.chisq = FALSE)

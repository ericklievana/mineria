train <- data.frame(ClaimID = c(1,2,3), RearEnd = c(TRUE,FALSE,TRUE), Fraud = c(TRUE,FALSE,TRUE))

print(train)
invisible(readline(prompt="Press [enter] to continue"))

if(!require(rpart)){
    install.packages("rpart")
    library(rpart)
}

mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class")

print(mytree)
invisible(readline(prompt="Press [enter] to continue"))

mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", minsplit = 2, minbucket = 1)

print(mytree)
invisible(readline(prompt="Press [enter] to continue"))

if(!require(rattle)){
    install.packages("rattle")
    library(rattle)
}

if(!require(rpart.plot)){
    install.packages("rpart.plot")
    library(rpart.plot)
}

if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
}

fancyRpartPlot(mytree,caption = NULL)
invisible(readline(prompt="Press [enter] to continue"))

mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", parms = list(split = "information"), minsplit = 2, minbucket = 1)

print(mytree)

train <- data.frame(ClaimID = c(1,2,3), RearEnd = c(TRUE,FALSE,TRUE), Fraud = c(TRUE,FALSE,FALSE))
print(train)

mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", minsplit = 2, minbucket = 1)

print(mytree)

mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", minsplit = 2, minbucket = 1, cp = -1)

print(mytree)

fancyRpartPlot(mytree,caption = NULL)
invisible(readline(prompt="Press [enter] to continue"))


mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", minsplit = 2, minbucket = 1, weights = c(0.4,0.4,0.2))

invisible(readline(prompt="Press [enter] to continue"))

fancyRpartPlot(mytree,caption = NULL)
invisible(readline(prompt="Press [enter] to continue"))

train <- data.frame(ClaimID = 1:7, RearEnd = c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE), Whiplash = c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE), Fraud = c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE))

print(train)

mytree <- rpart(Fraud ~ RearEnd + Whiplash, data = train, method = "class", maxdepth = 1, minsplit = 2, minbucket = 1)

fancyRpartPlot(mytree,caption = NULL)

invisible(readline(prompt="Press [enter] to continue"))

lossmatrix <- matrix(c(0,1,3,0), byrow = TRUE, nrow = 2)
print(lossmatrix)

mytree <- rpart(Fraud ~ RearEnd + Whiplash, data = train, method = "class", maxdepth = 1, minsplit = 2, minbucket = 1, parms = list(loss = lossmatrix))

fancyRpartPlot(mytree,caption = NULL)

invisible(readline(prompt="Press [enter] to continue"))

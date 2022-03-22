if(!require(rpart)){
  install.packages("rpart")
  library(rpart)
}

if(!require(rpart.plot)){
  install.packages("rpart.plot")
  library(rpart.plot)
}

mower.df <- read.csv("RidingMowers.csv")

class.tree <- rpart(Ownership ~ ., data = mower.df, control = rpart.control(maxdepth = 2), method = "class")

prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)

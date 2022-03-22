if(!require(rpart)){
  install.packages("rpart")
  library(rpart)
}

if(!require(rpart.plot)){
  install.packages("rpart.plot")
  library(rpart.plot)
}

bank.df <- read.csv("UniversalBank.csv")

bank.df <- bank.df[ ,-c(1,5)]

set.seed(1)

train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)

train.df <- bank.df[train.index, ]

valid.df <- bank.df[-train.index, ]

default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")

prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
invisible(readline(prompt="Press [enter] to continue"))

deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0, minsplit = 1)

length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, box.col = ifelse(deeper.ct$frame$var == "<leaf>", "gray", "white"))

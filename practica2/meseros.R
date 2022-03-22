if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}

Dataset = read_csv("tipjoke.csv")

View(Dataset)

Dataset <- select(Dataset, 0:2)

Dataset$Card = factor(Dataset$Card, levels = c("None","Joke","Ad"), labels = c(0.0, 1.0, 2.0))

View(Dataset)

if(!require(caTools)){
    install.packages("caTools")
    library(caTools)
}

set.seed(9876)

split = sample.split(Dataset$Card, SplitRatio = 0.8)
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)

if(!require(rpart)){
    install.packages("rpart")
    library(rpart)
}

mytree <- rpart(Tip ~ Card, data = training_set, method = "class", minsplit = 2, minbucket = 1, cp = -1)


print(mytree)


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

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}

Dataset = read_csv("overdrawn.csv")

Dataset <- select(Dataset, -1)

Dataset <- Dataset[complete.cases(Dataset$Overdrawn),]

Dataset <- Dataset[complete.cases(Dataset$Sex),]

Dataset$Age = ifelse(is.na(Dataset$Age),ave(Dataset$Age,FUN = function (x)mean(x, na.rm = TRUE)),Dataset$Age)

Dataset$DaysDrink = ifelse(is.na(Dataset$DaysDrink),ave(Dataset$DaysDrink,FUN = function (x)mean(x, na.rm = TRUE)),Dataset$DaysDrink)

View(Dataset)

Dataset$DaysDrink = cut(Dataset$DaysDrink, seq(0,35,7), labels = 0:4, include.lowest = TRUE)

View(Dataset)

if(!require(caTools)){
    install.packages("caTools")
    library(caTools)
}

set.seed(9876)

split = sample.split(Dataset$Age, SplitRatio = 0.7)
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)

if(!require(rpart)){
    install.packages("rpart")
    library(rpart)
}

mytree <- rpart(Overdrawn ~ Age + Sex + DaysDrink, data = training_set, method = "class")

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

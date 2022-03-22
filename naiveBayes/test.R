if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}
if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
}
if(!require(caret)){
    install.packages("caret")
    library(caret)
}

accidents.df <- read_csv("Accidents.csv", col_types = "nnnnnnnnnnf")

accidents.df$INJURY <- factor(accidents.df$MAX_SEV, levels = c("no-injury","non-fatal","fatal"), labels = c("no","yes","yes"))

set.seed(22)
train.index <- sample(c(1:dim(accidents.df)[1]), dim(accidents.df)[1]*0.6)
train.df <- accidents.df[train.index,]
valid.df <- accidents.df[-train.index,]

vars <- c("INJURY","WRK_ZONE","WKDY","INT_HWY","LGTCON_day","LEVEL","SPD_LIM","SUR_COND_dry","TRAF_two_way","WEATHER_adverse")

nbTotal <- naiveBayes(INJURY ~ ., data = train.df[,vars])

print(nbTotal)

print(confusionMatrix(train.df$INJURY, predict(nbTotal, train.df[, vars]), positive = "yes"))



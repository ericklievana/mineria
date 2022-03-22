if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}

heart <- read_csv("heart.csv", col_types = "nffnnffnfnfnff")

glimpse(heart)

print(summary(heart))

set.seed(1234)

sample_set <- sample(nrow(heart), round(nrow(heart)*.75), replace = FALSE)

heart_train <- heart[sample_set,]
heart_test <- heart[-sample_set,]

#round(prob.table(table(select(heart,heartDisease))),2)
print(round(sweep(table(select(heart,heartDisease)),1,margin.table(table(select(heart,heartDisease))),"/"),2))

#round(prob.table(table(select(heart_train,heartDisease))),2)
print(round(sweep(table(select(heart_train,heartDisease)),1,margin.table(table(select(heart_train,heartDisease))),"/"),2))

#round(prob.table(table(select(heart_test,heartDisease))),2)
print(round(sweep(table(select(heart_test,heartDisease)),1,margin.table(table(select(heart_test,heartDisease))),"/"),2))

if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
}

heart_mod <- naiveBayes(heartDisease ~ ., data = heart_train, laplace = 1)

heart_pred <- predict(heart_mod, heart_test, type = "class")

heart_pred_table <- table(heart_test$heartDisease, heart_pred)

print(heart_pred_table)

print(heart_mod)

print(sum(diag(heart_pred_table))/nrow(heart_test))

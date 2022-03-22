if(!require(tidyverse)){
   install.packages("tidyverse")
   library(tidyverse)
}

permits <- read_csv("permits.csv", col_types = "ffffffnnnnfffff")

print(glimpse(permits))

print(summary(permits))

permits <-permits %>% mutate(valuation = ifelse(valuation < 1, NA ,valuation)) %>% mutate(floorArea = ifelse(floorArea < 1, NA, floorArea)) %>% mutate(numberUnits = ifelse(numberUnits < 1, NA, numberUnits)) %>% mutate(stories = ifelse(stories < 1, NA, stories)) %>% mutate(stories = ifelse(stories > 73, NA, stories))

print(summary(select(permits, valuation,floorArea,numberUnits,stories)))

if(!require(dplyr)){
   install.packages("dplyr")
   library(dplyr)
}

permits <- permits %>% select(permitType, permitSubtype, initiatingOffice, permitCategory)

set.seed(1234)

sample_set <- sample(nrow(permits), round(nrow(permits)*.80),replace = FALSE)
permits_train <- permits[sample_set,]
permits_test <- permits[-sample_set,]
round(prop.table(table(select(permits,permitCategory))),2)

round(prop.table(table(select(permits_train,permitCategory))),2)

round(prop.table(table(select(permits_test,permitCategory))),2)

if(!require(rpart)){
   install.packages("rpart")
   library(rpart)
}

permits_mod <- rpart(permitCategory ~ ., method = "class", data = permits_train)

if(!require(rpart.plot)){
   install.packages("rpart.plot")
   library(rpart.plot)
}

rpart.plot(permits_mod)

permits_pred <- predict(permits_mod, permits_test, type = "class")

permits_pred_table <- table(permits_test$permitCategory,permits_pred)

print(permits_pred_table)

print(sum(diag(permits_pred_table))/nrow(permits_test))

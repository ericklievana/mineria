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

accidents <- read_csv("Accidents.csv", col_types = "nnnnnnnnnnf")

accidents$INJURY <- factor(accidents$MAX_SEV, levels = c("no-injury","non-fatal","fatal"), labels = c("no","yes","yes"))

View(accidents)

# Parte A

print("Probabilidad de que si haya un herido")
cat("Heridos: ",length(which(accidents$INJURY == "yes")),"\n")
cat("No Heridos: ",length(which(accidents$INJURY == "no")),"\n")
cat("Total: ",nrow(accidents),"\n")
cat((length(which(accidents$INJURY == "yes"))/nrow(accidents))*100,"%\n")

# Parte B

accidents12 <- accidents[1:12,c(9,10,12)]
View(accidents12)

## parte B.1

#print("Tabla Dinamica")
wea0traf0 <- length(which(accidents12$INJURY == "yes" & accidents12$WEATHER_adverse == 0 & accidents12$TRAF_two_way == 0))
wea0traf1 <- length(which(accidents12$INJURY == "yes" & accidents12$WEATHER_adverse == 0 & accidents12$TRAF_two_way == 1))
wea1traf0 <- length(which(accidents12$INJURY == "yes" & accidents12$WEATHER_adverse == 1 & accidents12$TRAF_two_way == 0))
wea1traf1 <- length(which(accidents12$INJURY == "yes" & accidents12$WEATHER_adverse == 1 & accidents12$TRAF_two_way == 1))

dinamica <- data.frame(WEATHER_adverse = c(0,0,1,1),
                       TRAFF_two_way = c(0,1,0,1),
                       INJURY = c(wea0traf0, wea0traf1, wea1traf0, wea1traf1))

View(dinamica)

## parte B.2

totalInjury <- length(which(accidents12$INJURY == "yes"))
probInjury <- length(which(accidents12$INJURY == "yes"))/nrow(accidents12)

# clima = 0, trafico = 0
numerador <- (length(which(accidents12$WEATHER_adverse == 0 & accidents12$TRAF_two_way == 0 & accidents12$INJURY == "yes"))/totalInjury)*probInjury
denominador <- (length(which(accidents12$WEATHER_adverse == 0 & accidents12$TRAF_two_way == 0)))/nrow(accidents12)
prob1 <- numerador/denominador

# clima = 0, trafico = 1
numerador <- (length(which(accidents12$WEATHER_adverse == 0 & accidents12$TRAF_two_way == 1 & accidents12$INJURY == "yes"))/totalInjury)*probInjury
denominador <- (length(which(accidents12$WEATHER_adverse == 0 & accidents12$TRAF_two_way == 1)))/nrow(accidents12)
prob2 <- numerador/denominador

# clima = 1, trafico = 0
numerador <- (length(which(accidents12$WEATHER_adverse == 1 & accidents12$TRAF_two_way == 0 & accidents12$INJURY == "yes"))/totalInjury)*probInjury
denominador <- (length(which(accidents12$WEATHER_adverse == 1 & accidents12$TRAF_two_way == 0)))/nrow(accidents12)
prob3 <- numerador/denominador

# clima = 1, trafico = 1
numerador <- (length(which(accidents12$WEATHER_adverse == 1 & accidents12$TRAF_two_way == 1 & accidents12$INJURY == "yes"))/totalInjury)*probInjury
denominador <- (length(which(accidents12$WEATHER_adverse == 1 & accidents12$TRAF_two_way == 1)))/nrow(accidents12)
prob4 <- numerador/denominador


probabilidades <- data.frame(WEATHER_adverse = c(0,0,1,1),
                       TRAFF_two_way = c(0,1,0,1),
                       Probabilidad = c(prob1,prob2,prob3,prob4))

View(probabilidades)

## Parte B.3

predictor <- accidents12

predictor$Probabilidades <- c(0.4,1.0,0.4,0.4,0.4,0.4,0.4,1.0,0.4,0.4,0.4,0.4)

predictor$Prediccion <- ifelse(predictor$Probabilidades>.5,"yes","no")

View(predictor)

## Parte B.4

#No se puede, por que no hay casos de Clima = 1 y Trafico = 1, asi que eso tendria una probabilidad de 0/12 haciendo el resultado 0

## Parte B.5
naive12 <- naiveBayes(INJURY ~ ., data = accidents12)

predict12 <- predict(naive12, newdata = accidents12, type = "raw")

View(predict12)

# Parte C
## Parte C.2
accidentGroup <- select(accidents,-MAX_SEV)
set.seed(1234)
sample_set <- sample(nrow(accidentGroup), round(nrow(accidentGroup)*0.60),replace = FALSE)
accident_train <- accidentGroup[sample_set,]
accident_test <- accidentGroup[-sample_set,]
accident_bayes <- naiveBayes(INJURY ~ ., data = accident_train)
print("Entrenamiento")
print(confusionMatrix(accident_train$INJURY, predict(accident_bayes, accident_train), positive = "yes"))

## Parte C.3
print("Validación")
print(confusionMatrix(accident_test$INJURY, predict(accident_bayes, accident_test), positive = "yes"))

## Parte C.5
print(accident_bayes)

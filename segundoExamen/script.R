if(!require(forecast)){
  install.packages("forecast")
  library(forecast)
}

df <- read.csv("ToyotaCorolla.csv")
View(df[,c(2,8,11)])

summary(df)

unique(df$Model)
unique(df$Color)
unique(df$Fuel_Type)
unique(df$Guarantee_Period)
unique(df$Doors)
unique(df$Cylinders)
unique(df$Gears)
unique(df$Mfg_Year)

df.mod <- df[,-1]

df.mod$Color <- factor(df.mod$Color)
df.mod$Fuel_Type <- factor(df.mod$Fuel_Type)
df.mod$Doors <- factor(df.mod$Doors)
df.mod$Cylinders <- factor(df.mod$Cylinders)
df.mod$Gears <- factor(df.mod$Gears)
df.mod$Guarantee_Period <- factor(df.mod$Guarantee_Period)

df.mod$CNG <- ifelse(df.mod$Fuel_Type == "CNG", 1, 0)
df.mod$Diesel <- ifelse(df.mod$Fuel_Type == "Diesel", 1, 0)
df.mod$Petrol <- ifelse(df.mod$Fuel_Type == "Petrol", 1, 0)
df.mod <- within(df.mod, rm("Fuel_Type"))

df.mod$Blue <- ifelse(df.mod$Color == "Blue", 1, 0)
df.mod$Silver <- ifelse(df.mod$Color == "Silver", 1, 0)
df.mod$Black <- ifelse(df.mod$Color == "Black", 1, 0)
df.mod$White <- ifelse(df.mod$Color == "White", 1, 0)
df.mod$Grey <- ifelse(df.mod$Color == "Grey", 1, 0)
df.mod$Red <- ifelse(df.mod$Color == "Red", 1, 0)
df.mod$Green <- ifelse(df.mod$Color == "Grenn", 1, 0)
df.mod$Yellow <- ifelse(df.mod$Color == "Yellow", 1, 0)
df.mod$Violet <- ifelse(df.mod$Color == "Violet", 1, 0)
df.mod$Beige <- ifelse(df.mod$Color == "Beige", 1, 0)
df.mod <- within(df.mod, rm("Color"))

df.mod$"2Doors" <- ifelse(df.mod$Doors == "2", 1, 0)
df.mod$"3Doors" <- ifelse(df.mod$Doors == "3", 1, 0)
df.mod$"4Doors" <- ifelse(df.mod$Doors == "4", 1, 0)
df.mod$"5Doors" <- ifelse(df.mod$Doors == "5", 1, 0)
df.mod <- within(df.mod, rm("Doors"))

df.mod$"3Gears" <- ifelse(df.mod$Gears == "3", 1, 0)
df.mod$"4Gears" <- ifelse(df.mod$Gears == "4", 1, 0)
df.mod$"5Gears" <- ifelse(df.mod$Gears == "5", 1, 0)
df.mod$"6Gears" <- ifelse(df.mod$Gears == "6", 1, 0)
df.mod <- within(df.mod, rm("Gears"))

df.mod$"4Cylinders" <- ifelse(df.mod$Cylinders == "4", 1, 0)
df.mod <- within(df.mod, rm("Cylinders"))

View(df.mod[,c(1,34:55)])

df.cor <- df[,-c(1,2,6,8,11,15)]

cor.matrix <- round(suppressWarnings(cor(df.cor)),2)

heatmap(cor.matrix, Rowv = NA, Colv = NA)

df.model <- df.cor

set.seed(1434)
train.rows <- sample(rownames(df.model), dim(df.model)[1]*0.5)
valid.rows <- sample(setdiff(rownames(df.model), train.rows),dim(df.model)[1]*0.3)
test.rows <- setdiff(rownames(df.model), union(df.model, df.model))

df.train <- df.model[train.rows, ]
df.valid <- df.model[valid.rows, ]
df.test <- df.model[test.rows, ]

model <- lm(Price ~ ., data = df.train)

predi <- predict(model,df.valid)

residuos <- df.valid$Price - predi

print("Validacion")
print(length(residuos[which(residuos>-1090 & residuos<1090)])/length(df.valid$Price))

predi <- predict(model,df.test)

residuos <- df.test$Price - predi

print("Testeo")
print(length(residuos[which(residuos>-1090 & residuos<1090)])/length(df.test$Price))

hist(residuos, breaks = 25, xlab="Residuos", main ="")

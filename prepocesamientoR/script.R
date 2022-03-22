# Instalacion de paquetes
if(!require(readr)){
    install.packages("readr")
    library(readr)
}

# Cargar los datos
Dataset = read_csv("Dataset.csv")

#Ver los datos
print("Datos originales")
View(Dataset,"Datos")
invisible(readline(prompt="Press [enter] to continue"))

# Reemplazar datos faltantes
# Edad
Dataset$Age = ifelse(is.na(Dataset$Age),ave(Dataset$Age,FUN = function (x)mean(x, na.rm = TRUE)),Dataset$Age)

print("Datos edad faltante")
View(Dataset)
invisible(readline(prompt="Press [enter] to continue"))

# Salario
Dataset$Salary = ifelse(is.na(Dataset$Salary),ave(Dataset$Salary,FUN = function (x)mean(x, na.rm = TRUE)),Dataset$Salary)

print("Datos salario faltante")
View(Dataset)
invisible(readline(prompt="Press [enter] to continue"))

# Codificacion de datos categóricos
# Paises
Dataset$Country = factor(Dataset$Country, levels = c("France","Spain","Germany"), labels = c(1.0, 2.0, 3.0))

print("Datos cambio paises")
View(Dataset)
invisible(readline(prompt="Press [enter] to continue"))

# Compras
Dataset$Purchased = factor(Dataset$Purchased, levels = c("No","Yes"), labels = c(0, 1))
Dataset$Purchased[is.na(Dataset$Purchased)] <- 0
as.factor(Dataset$Purchased)

print("Datos cambio compras")
View(Dataset)
invisible(readline(prompt="Press [enter] to continue"))

# Cargar el paquete caTools
if(!require(caTools)){
    install.packages("caTools")
    library(caTools)
}

# Creacion de los grupos de entrenamiento y prueba
set.seed(123)
split = sample.split(Dataset$Purchased,SplitRatio = 0.8)
training_set = subset(Dataset,split == TRUE)
test_set = subset(Dataset,split == FALSE)
print(training_set)
print(test_set)

# Escalamiento de los grupos
training_set[,2:3] = scale(training_set[,2:3])
test_set[,2:3] = scale(test_set[,2:3])
print(training_set)
print(test_set)

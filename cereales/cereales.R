df <- read.csv("Cereals.csv")

df$carbo = ifelse(is.na(df$carbo),ave(df$carbo,FUN = function (x)mean(x, na.rm = TRUE)),df$carbo)
df$sugars = ifelse(is.na(df$sugars),ave(df$sugars,FUN = function (x)mean(x, na.rm = TRUE)),df$sugars)
df$potass = ifelse(is.na(df$potass),ave(df$potass,FUN = function (x)mean(x, na.rm = TRUE)),df$potass)


medias <- data.frame(variable = character(),
                     media = double(),
                     min = double(),
                     max = double(),
                     desviacion = double())

anadirData <- function(data, nombre, tabla){
  media <- mean(data, na.rm = TRUE)
  min <- min(data, na.rm = TRUE)
  max <- max(data, na.rm = TRUE)
  desviacion <- sd(data, na.rm = TRUE)
  tabla[nrow(tabla)+1,] = c(nombre,media,min,max,desviacion)
  tabla
}

medias <- anadirData(df$calories, "calories", medias)
medias <- anadirData(df$sodium, "sodium", medias)
medias <- anadirData(df$fiber, "fiber", medias)
medias <- anadirData(df$sugars, "sugars", medias)
medias <- anadirData(df$potass, "potass", medias)
medias <- anadirData(df$vitamins, "vitamins", medias)
medias <- anadirData(df$carbo, "carbo", medias)
medias <- anadirData(df$weight, "weight", medias)
medias <- anadirData(df$cups, "cups", medias)
medias <- anadirData(df$rating, "rating", medias)

hist(df$calories, main = "Calories")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$sodium, main = "Sodium")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$fiber, main = "Fiber")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$sugars, main = "Sugars")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$potass, main = "Potass")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$vitamins, main = "Vitamins")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$weight, main = "Weight")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$cups, main = "Cups")
invisible(readline(prompt="Press [enter] to continue"))
hist(df$rating, main = "Rating")
invisible(readline(prompt="Press [enter] to continue"))

boxplot(df$calories ~ df$type,
        col='steelblue',
        main='Calories by type',
        xlab='type',
        ylab='calories',
        horizontal=TRUE)
invisible(readline(prompt="Press [enter] to continue"))

boxplot(df$rating ~ df$shelf,
        col='steelblue',
        main='Rating by shelf',
        xlab='Rating',
        ylab='Shelf',
        horizontal=TRUE)
invisible(readline(prompt="Press [enter] to continue"))

df.cor <- df[,-c(1,2,3)]
cor.matrix <- round(suppressWarnings(cor(df.cor)),2)
heatmap(cor.matrix, Rowv = NA, Colv = NA)
invisible(readline(prompt="Press [enter] to continue"))

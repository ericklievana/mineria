if(!require(readr)){
    install.packages("readr")
    library(readr)
}

dataOriginal <- read_csv("Tayko.csv", col_types = "nffffffffffffffffnnnffffn")

df <- subset(dataOriginal, select = -c(sequence_number,source_a,source_c,source_b,source_d,source_e,source_m,source_o,source_h,source_r,source_s,source_t,source_u,source_p,source_x,source_w))

df$updateRange = cut(df$last_update_days_ago, c(0,1000,2000,3000,4000,5000),labels = c("0-1000","1001-2000","2001-3000","3001-4000","4001+"), include.lowest = TRUE)

df$SpendingRange = cut(df$Spending, c(0,300,600,900,1200,1500,1800),labels = c("0-300","301-600","601-900","901-1200","1201-1500","1500+"), include.lowest = TRUE)

#Tabla Dinamica
if(!require(pivottabler)){
    install.packages("pivottabler")
    library(pivottabler)
}
options(browser="firefox")

#Address_is_res
print("---------------------")
print("Direccion Residencial")
print("---------------------")
#Tabla pivote
pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("Address_is_res", caption="Residencial={value}")
pt$addRowDataGroups("SpendingRange", caption="Spending={value}")
pt$defineCalculation(calculationName="Spending", summariseExpression="n()")
pt$renderPivot()
direccion <- pt$asDataFrame()
#Seleccion de las filas que cumplen la condicion
temp <- df[df$Address_is_res == 1,]
#Impresion media y desviacion
media <- mean(temp$Spending)
print("Promedio")
print(media)
print("Desviacion estandar")
desviacion <- sd(temp$Spending)
print(desviacion)

#lastUpdate
print("---------------------")
print("Ultima Actualizacion")
print("---------------------")
#Tabla pivote
pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("updateRange", caption="LastUpdate={value}")
pt$addRowDataGroups("SpendingRange", caption="Spending={value}")
pt$defineCalculation(calculationName="Spending", summariseExpression="n()")
pt$renderPivot()
actualizacion <- pt$asDataFrame()
#Impresion media y desviacion
media <- mean(df$Spending)
print("Promedio")
print(media)
print("Desviacion estandar")
desviacion <- sd(df$Spending)
print(desviacion)

#Web
print("---------------------")
print("Web Order")
print("---------------------")
#Tabla pivote
pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("Weborder", caption="Web={value}")
pt$addRowDataGroups("SpendingRange", caption="Spending={value}")
pt$defineCalculation(calculationName="Spending", summariseExpression="n()")
pt$renderPivot()
web <- pt$asDataFrame()
#Seleccion de las filas que cumplen la condicion
temp <- df[df$Weborder == 1,]
#Impresion media y desviacion
media <- mean(temp$Spending)
print("Promedio")
print(media)
print("Desviacion estandar")
desviacion <- sd(temp$Spending)
print(desviacion)

#Gender
print("---------------------")
print("Gender")
print("---------------------")
#Tabla pivote
pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("Gender", caption="Gender={value}")
pt$addRowDataGroups("SpendingRange", caption="Spending={value}")
pt$defineCalculation(calculationName="Spending", summariseExpression="n()")
pt$renderPivot()
genero <- pt$asDataFrame()
#Seleccion de las filas que cumplen la condicion
temp <- df[df$Gender == 1,]
#Impresion media y desviacion
media <- mean(temp$Spending)
print("Promedio")
print(media)
print("Desviacion estandar")
desviacion <- sd(temp$Spending)
print(desviacion)

#AddressUS
print("---------------------")
print("US")
print("---------------------")
#Tabla pivote
pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("US", caption="US={value}")
pt$addRowDataGroups("SpendingRange", caption="Spending={value}")
pt$defineCalculation(calculationName="Spending", summariseExpression="n()")
pt$renderPivot()
us <- pt$asDataFrame()
#Seleccion de las filas que cumplen la condicion
temp <- df[df$US == 1,]
#Impresion media y desviacion
media <- mean(temp$Spending)
print("Promedio")
print(media)
print("Desviacion estandar")
desviacion <- sd(temp$Spending)
print(desviacion)

#Freq
print("---------------------")
print("Frecuencia")
print("---------------------")
#Tabla pivote
pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("Freq", caption="Frecuencia={value}")
pt$addRowDataGroups("SpendingRange", caption="Spending={value}")
pt$defineCalculation(calculationName="Spending", summariseExpression="n()")
pt$renderPivot()
freq <- pt$asDataFrame()
#Impresion media y desviacion
media <- mean(df$Spending)
print("Promedio")
print(media)
print("Desviacion estandar")
desviacion <- sd(df$Spending)
print(desviacion)

if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}

plot(df$Freq, df$Spending, main="Freq vs Spending")
abline(lm(df$Spending~df$Freq),col="yellow")

plot(df$last_update_days_ago, df$Spending, main="Last Update vs Spending")
abline(lm(df$Spending~df$last_update_days_ago),col="yellow")

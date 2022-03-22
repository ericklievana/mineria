if(!require(readr)){
    install.packages("readr")
    library(readr)
}

df <- read_csv("Tayko.csv", col_types = "nffffffffffffffffnnnffffn")

#df2 <- subset(df, select = -c(sequence_number,source_a,source_c,source_b,source_d,source_e,source_m,source_o,source_h,source_r,source_s,source_t,source_u,source_p,source_x,source_w))

#Tabla Dinamica
if(!require(pivottabler)){
    install.packages("pivottabler")
    library(pivottabler)
}
#options(browser="firefox")

pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("Purchase")
pt$addRowDataGroups("Address_is_res")
pt$defineCalculation(calculationName="Compras", summariseExpression="n()")
pt$renderPivot()


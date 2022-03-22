boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE)

print("HEAD")
print(head(boston.housing.df, 9))

print("SUMMARY")
print(summary(boston.housing.df))

print("CRIM STATISTICS")
print(mean(boston.housing.df$CRIM))
print(sd(boston.housing.df$CRIM))
print(min(boston.housing.df$CRIM))
print(max(boston.housing.df$CRIM))
print(median(boston.housing.df$CRIM))
print(length(boston.housing.df$CRIM))
print(sum(is.na(boston.housing.df$CRIM)))

resumen <- data.frame(mean=sapply(boston.housing.df, mean), sd=sapply(boston.housing.df, sd), min=sapply(boston.housing.df, min), max=sapply(boston.housing.df, max), median=sapply(boston.housing.df, median), length=sapply(boston.housing.df, length), miss.val=sapply(boston.housing.df, function(x) sum(length(which(is.na(x))))))

print("RESUMEN")
print(resumen)

correlacion <- round(cor(boston.housing.df),2)

print("CORRELACION")
print(correlacion)

chas <- table(boston.housing.df$CHAS)

print("TABLA PIVOTE")
print(chas)


boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

print("PROMEDIO MEDV PARA CHAS Y RM")
agregado <- aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin,CHAS=boston.housing.df$CHAS), FUN=mean)
print(agregado)

if(!require(reshape)){
    install.packages("reshape")
    library(reshape)
}

mlt <- melt(boston.housing.df, id=c("RM.bin", "CHAS"), measure=c("MEDV"))

print("HEAD melt")
print(head(mlt,5))

print("PIVOTE melt")
print(cast(mlt, RM.bin ~ CHAS, subset=variable=="MEDV", margins=c("grand_row", "grand_col"), mean))

if(!require(ggmap)){
    install.packages("ggmap")
    library(ggmap)
}

tbl <- table(boston.housing.df$CAT..MEDV, boston.housing.df$ZN)
prop.tbl <- prop.table(tbl, margin=2)
barplot(prop.tbl, xlab="ZN", ylab="", yaxt="n",main="Distribution of CAT.MEDV by ZN")
axis(2, at=(seq(0,1, 0.2)), paste(seq(0,100,20), "%"))

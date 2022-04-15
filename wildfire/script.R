df <- read.csv("StudyArea.csv")

#df$STARTDATED <- sub("/.+","",df$STARTDATED)
#df$STARTDATED <- as.integer(df$STARTDATED)
##df$CONTRDATED <- sub( 0:00,"",df$CONTRDATED)
##df$OUTDATED <- sub(" 0:00","",df$OUTDATED)

large <- subset(df, TOTALACRES>1000)
#small <- subset(df, TOTALACRES<=1000)
#
#
#years <- sort(unique(df$YEAR_),decreasing=FALSE)
#df.overtime <- data.frame(years)
#cuantity <- vector()
#area <- vector()
#
#sStart <- vector()
#sEnd <- vector()
#
#for (x in years){
#  tmp <- length(which(df$YEAR_==x))
#  cuantity <- append(cuantity,tmp)
#  tmp <- with(df, sum(df$TOTALACRES[df$YEAR_==x]))/length(which(df$YEAR_==x))
#  area <- append(area,tmp)
#  tmp <- sort(unique(df$STARTDATED[df$YEAR_==x]))
#  sStart <- append(sStart,tmp[1])
#  sEnd <- append(sEnd,tmp[length(tmp)])
#}
#
#
#df.overtime$area <- area
#df.overtime$cuantity <- cuantity
#df.overtime$sStart <- sStart
#df.overtime$sEnd <- sEnd
#
#if(!require(pivottabler)){
#  install.packages("pivottabler")
#  library(pivottabler)
#}
#
#
#pt <- PivotTable$new()
#pt$addData(df)
#pt$addColumnDataGroups("STARTDATED", caption="MES={value}")
#pt$addRowDataGroups("YEAR_")
#pt$defineCalculation(calculationName="Años/meses", summariseExpression="n()")
#pt$renderPivot()
#meses <- pt$asDataFrame()
#
#pt <- PivotTable$new()
#pt$addData(df)
#pt$addColumnDataGroups("ORGANIZATI")
#pt$addRowDataGroups("YEAR_")
#pt$defineCalculation(calculationName="Años/meses", summariseExpression="n()")
#pt$renderPivot()
#organi <- pt$asDataFrame()
#
graf.incendioAnos <- hist(large$YEAR_,breaks=c(1979,seq(1980,2016,1)),include.lowest=TRUE)
text(graf.incendioAnos$mids,graf.incendioAnos$counts,labels=graf.incendioAnos$counts,adj=c(0.5,-0.5))

#invisible(readline(prompt="Press [enter] to continue"))


#graf.area<- hist(analisis$area) #,breaks=c(1979,seq(1980,2016,1)),include.lowest=TRUE)
#text(graf.area$mids,graf.area$counts,labels=graf.area$counts,adj=c(0.5,-0.5))


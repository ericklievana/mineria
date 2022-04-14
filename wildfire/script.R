df <- read.csv("StudyArea.csv")

df$STARTDATED <- sub("/.+","",df$STARTDATED)
df$STARTDATED <- as.integer(df$STARTDATED)
#df$CONTRDATED <- sub( 0:00,"",df$CONTRDATED)
#df$OUTDATED <- sub(" 0:00","",df$OUTDATED)


years <- sort(unique(df$YEAR_),decreasing=FALSE)
df.overtime <- data.frame(years)
cuantity <- vector()
area <- vector()
sStart <- vector()
sEnd <- vector()


for (x in years){
  tmp <- length(which(df$YEAR_==x))
  cuantity <- append(cuantity,tmp)
  tmp <- with(df, sum(df$TOTALACRES[df$YEAR_==x]))/length(which(df$YEAR_==x))
  area <- append(area,tmp)
  for (i in 1:12){
    assign(paste("month",i),vector())
    tmp <- with(df, length(df[df$YEAR_==x && df$STARTDATED==i]))
    print(x)
    print(i)
    print(tmp)
    assign(paste("month",i),append(i,tmp))
  }
}

df.overtime$area <- area
df.overtime$cuantity <- cuantity


#graf.incendioAnos <- hist(df$YEAR_,breaks=c(1979,seq(1980,2016,1)),include.lowest=TRUE)
#text(graf.incendioAnos$mids,graf.incendioAnos$counts,labels=graf.incendioAnos$counts,adj=c(0.5,-0.5))

#invisible(readline(prompt="Press [enter] to continue"))


#graf.area<- hist(analisis$area) #,breaks=c(1979,seq(1980,2016,1)),include.lowest=TRUE)
#text(graf.area$mids,graf.area$counts,labels=graf.area$counts,adj=c(0.5,-0.5))


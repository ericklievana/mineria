#if(!require(reshape)){
#    install.packages("reshape")
#    library(reshape)
#}

cereals.df <- read.csv("Cereals.csv")
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating))
print(summary(pcs))
print(pcs$rot)
scores <- pcs$x
print(head(scores, 5))
pcs <- prcomp(na.omit(cereals.df[,-c(1:3)]))
print(summary(pcs))
print(pcs$rot[,1:5])
pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
print(summary(pcs.cor))
plot(cereals.df$calories, cereals.df$rating, xlab = "Calories", ylab = "Rating")
#plot(0.847:-0.532, type="l")

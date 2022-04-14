df <- read.csv("Wine.csv")

pcs.cor <- prcomp(df[,-1])

summary(pcs.cor)

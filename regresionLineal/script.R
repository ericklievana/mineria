Aptitude <- c(45, 81, 65, 87, 68, 91, 77, 61, 55, 66, 82, 93, 76, 83, 61, 74)
Performance <- c(56, 74, 56, 81, 75, 84, 68, 52, 57, 82, 73, 90, 67, 79, 70, 66)

df <- data.frame(Aptitude,Performance)

df.reg <- lm(Performance ~ Aptitude, data = df)

print(summary(df.reg))

print(coefficients(df.reg))

print(confint(df.reg))

print(fitted(df.reg))

print(residuals(df.reg))

print(predict(df.reg, data.frame(Aptitude = c(85,62))))

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

options(browser = "firefox")

#print(ggplot(df, aes(x=Aptitude,y=Performance))+geom_point()+geom_smooth(method=lm))

#print(ggplot(df.reg, aes(x=fitted(df.reg),y=residuals(df.reg)))+geom_point()+geom_hline(yintercept=0,linetype="dashed"))

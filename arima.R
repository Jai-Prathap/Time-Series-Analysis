###x=c(130,127,141,138,129,151,117,135,160)
###boxplot(x,horizontal = TRUE)
################################################################################
data("mtcars")
df=mtcars
colSums(is.na(df))
round(cor(mtcars), 3)
pairs(mtcars)
library(corrgram)
corrgram(mtcars)
library(ellipse)
plotcorr(cor(mtcars),type = "full")
plot(prcomp(mtcars, scale = TRUE)) 
plot(prcomp(mtcars, scale = TRUE),type="line")
library(bpca)
plot(bpca(prcomp(mtcars, scale = TRUE), d=1:3))
summary(prcomp(mtcars, scale=TRUE))
prcomp(mtcars, scale = TRUE)
biplot(prcomp(mtcars, scale = TRUE))
prcomp(mtcars, scale = TRUE)$x[,1:2]
library(psych) 
library(GPArotation)
principal(mtcars, nfactors = 2, rotate="varimax")
biplot(principal(mtcars, nfactors = 2, rotate="varimax"))
biplot(principal(mtcars, nfactors = 2, rotate="varimax"), lab=row.names(mtcars))
principal(mtcars, nfactors = 2, rotate="varimax")$scores
library(fpp)
train <- window(hsales, end=c(1989,12))
fit <- auto.arima(train)
refit <- Arima(hsales, model=fit)
fc <- window(fitted(refit), start=1990)



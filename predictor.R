dataset <- read.csv("~/Programacion/R/CharlesSchwab2020/CSC2020Data.csv")
data <- dataset[,-2]
data <- data[,-2]
View(data)
View(dataset)
plot(data)

for(i in 1:8){
  data[, i] <- as.numeric(data[, i])
}

library(corrplot)
corrplot(cor(data), method= 'color')

Pos <- data$POSITION
SGTotal <- data$SG.T
SGPutting <- data$SG.P
SGAPR <- data$SG.APR
SGOTT <- data$SG.OTT
SGARG <- data$SG.ARG
model <- lm(SGTotal ~ SGPutting + SGAPR + SGOTT + SGARG)
model2 <- lm(Pos ~ SGTotal)

predictor <- function(putting, approach, offthetee, aroundgreen){
  x <- predict(model, newdata = data.frame(SGPutting = putting, SGAPR = approach, SGOTT = offthetee, SGARG = aroundgreen))
  return(predict(model2, newdata = data.frame(SGTotal = x)))
}

#D. Berger 2021 SG:OTT=0.457 SG:APR=0.619 SG:ARG=-0.123 SG:P=0.568
predictor(0.568,0.619,-0.457,-0.123)


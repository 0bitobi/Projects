library(ggplot2)
library(memisc)
library(lmtest)
library(dplyr)
library(foreign)
library(vcd)
library(devtools)
library(hexbin)
library(pander)
library(sjPlot)
library(knitr)

BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)
  
glimpse(BD)
qplot(data = BD, totsp, price)
str(BD) 
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)

#1
example <- function()
{
  print(BD)
  bg <- qplot(data = BD, n, log(price), col = "brown1",
              main = "Квартиры в МСК", xlab = "Цена", ylab = "Количество")
  bg + geom_hex()
}
example()

# Акак в функции вызвать док ? #qplot(data = BD, n, price, col = "brown1",main = 
#         "Квартиры в МСК(Так выглядит без всего)", xlab = "Цена в 1000$", ylab = "Количество")

#2#                               Черная магия через qplot
qplot(data = BD, log(price))
qplot(data = BD, log(price), fill = brick, position = "dodge") 
qplot(data = BD, log(price), fill = brick, geom = "density")
g2 <- qplot(data = BD, log(price), fill = brick, geom = "density", alpha = 0.5,
            xlab = "Цена в 10000$")
g2 + facet_grid(walk ~ floor)
#                                       По заданию 
attach(BD)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(metrdist,main = "Расстояние до метро в минутах", xlab = "Минуты", ylab = " ",
     col = "deeppink1",border = "gray0")
hist(log(totsp),main = "Площадь", xlab = "Средняя площадь квартиры в кв.м.", ylab = " ",
     col = "brown1",border = "blue")
hist(log(price),main = "Стоимость", xlab = "Средняя цена квартиры в Москве в 10000$", ylab = " ",
     col = "yellow",border = "firebrick1")
########################

#3 Мб сдлать под свою бд
#Apply
m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)
apply(m, 2, mean)
apply(m, 2, function(x) length(x[x<0]))
apply(m, 2, function(x) is.matrix(x))
apply(m, 2, is.vector)
apply(m, 2, function(x) mean(x[x>0]))
#sapply & lapply
sapply(1:3, function(x) x^2)
lapply(1:3, function(x) x^2)
sapply(1:3, function(x) x^2, simplify=F)
unlist(lapply(1:3, function(x) x^2))


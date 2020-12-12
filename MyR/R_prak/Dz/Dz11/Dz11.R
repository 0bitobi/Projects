library("dplyr")
library("knitr")
library("ggplot2")
library("glmnet")
library("car") 
library("MASS") 
library("quantreg") 
library("foreign")
library("LinRegInteractive")
library("interplot")
library(corrplot)
library(ISwR)
library(psych)

BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)

glimpse(BD)
str(BD)
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
attach(BD)

m1 <- lm(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = BD)

#               Однофакторный дисперсионный анализ

oneway.test(dist ~ floor)

oneway.test(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = BD, subset = tail(seq_along(n), 2040))

oneway.test(price ~ code, data = BD)

#
install.packages("faraway")
library(faraway)

interaction.plot(brick, walk, price)

interaction.plot(code, walk, price)
 
#
kruskal.test(dist ~ floor, data = BD)

#             Двухфакторный 
stripchart(price ~ livesp, data = BD, pch = 19,
           col = c("blue", "red"),
           ylab = "", xlab = "")

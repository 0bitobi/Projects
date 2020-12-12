library(ggplot2)
library(gganimate)
library(memisc)
library(lmtest)
library(lattice)
library(dplyr)
library(foreign)
library(vcd)
library(devtools)
library(hexbin)
library(pander)
library(sjPlot)
library(knitr)
library(HSAUR2)
library(sm)
library(boot)
library(gridExtra)
library(MASS)
library(nortest) # прикол да

BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)

glimpse(BD)
str(BD)
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
attach(BD)
#2
mean(livesp)
t.test(livesp, mu = 48)

tapply(livesp, totsp, mean)

t.test(livesp ~ floor)
t.test(livesp ~ floor, var.equal = TRUE)

t.test(livesp ~ floor, alternative = "greater")
t.test(livesp ~ floor, alternative = "less")
t.test(livesp ~ floor, alternative = "two.sided")

#3
#Фишер
var.test(livesp ~ floor, alternative = c("two.sided"),
         conf.level = 0.95) 

#Левене
library(car)
leveneTest(livesp ~ floor)

#Бартлетта
bartlett.test(livesp ~ floor)

#Флигнера-Килин
fligner.test(livesp ~ floor, data = InsectSprays)

#проверку гипотез об однородности дисперсий
aov(livesp ~ floor, data = BD) 
lm(livesp ~ floor)











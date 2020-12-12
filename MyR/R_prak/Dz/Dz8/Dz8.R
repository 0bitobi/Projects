
#2
BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)
library(HSAUR2)
require(doBy)
attach(BD)

aov(livesp ~ floor, data = BD) 
aov(livesp ~ floor + brick, data = BD) 
aov(livesp ~ floor + brick + floor:brick, data = BD) 

stripchart(livesp ~ floor, data = BD, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "Условия", xlab = "Вес (кг)")

summaryBy(livesp ~ floor + brick + floor:brick, data = BD)

stripchart(livesp ~ floor + brick + floor:brick, data = BD, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "", xlab = "")

#3
#По Пирсону
cor(livesp,floor)
#По Спирмену
cor.test(n,floor)
cor.test(livesp, floor, method = "spearman")
cor.test(livesp, floor, method = "pearson")

#По Кендаллу
cor.test(livesp, floor, method = "kendall")


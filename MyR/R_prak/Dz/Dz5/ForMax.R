library(ggplot2)
library(gganimate)
library(dplyr)
library(devtools)
library(sjPlot)
library(knitr)
library(sm)
library(HSAUR2)
library(aplpack)
##############
library(outliers)
library(Hmisc)
library(pan)
library(mix)
library(mitools)

# 1. С помощью функций пакета outliers с помощью формальных критериев для идентификации выбросов (Шовене, Граббса, Пирса, Q-тест Диксона и др.) проверить в данных, является ли максимальное наблюдение выбросом.
# 2. Воспользоваться функциями mitools() , pan() , mix() из одноименных пакетов, а также aregImpute() и transcan() из пакета Hmisc для заполнения пропусков в данных.

BD <- read.csv2("C:/Users/Obito/Desktop/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)
glimpse(BD)
str(BD) 
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
#View(BD)
#Вообще выбросы можно показать с помощью boxplot
#Черная черточка это медиана (среднее значение)
#то что в прямоугольнике вверх и в низ серым цветом это 25% данных 
#пунктир это 75% моих данных 
#все остольное это выбросы которые обозначаються кружками (аномальные данные от которых нужно избавляться)
boxplot(BD$price ~ BD$livesp, data=mtcars, subset=cyl %in% c(4,6))
########################################################################
# ну что начнем (посмотрим на какое распределение похоже)
hist(BD$livesp, breaks = 20, freq = FALSE, col = "brown1",
     xlab = "жилая площадь квартиры, кв.м",
     ylab = "Плотность вероятности",
     main = "Гистограмма, совмещенная с кривой плотности ")
lines(density(BD$livesp), col = "blue", lwd = 2)
#Похоже на нормальное распределение (значит что?) да мы можем с ним работать 
#1   
example <- function()
{
  MaxL <- max(BD$livesp)
  Vi2 <- outlier(BD$livesp, opposite = FALSE ,logical = FALSE)
  if(MaxL == Vi2)
  {
    print(max(BD$livesp))
    print("Максимальное наблюдение являеться выбросом")
  }
}
example()

#                                        Q-тест Диксона

MinL <- min(BD$livesp)        # Минимальный элемент
MaxL <- max(BD$livesp)        # Максимальный элемент 
R <- MaxL-MinL                #Разность между наименьшим и наибольшим значением
S <- mean(BD$livesp)          # Среднее арифметическое
Dispersion <- var(BD$livesp)  #выборочное среднеквадратическое отклонение(Дисперсия)

QTest <- function()
{
  sort(BD$livesp)
  Q_test <- abs(MinL-BD$livesp/R)
  SEmpg = sd(BD$livesp)/sqrt(length(BD$livesp))
  print("По Q тесту Диксона")
  print(SEmpg)
}
QTest()
#Часто спрашивают о том, где взять таблицы для числа измерений больших 10. 
#Ответ прост – нигде.
#Дело в том, что для больших массивов данных меняется сама процедура вычисления Q-критерия.


#                                             Шовене
Showena <- function()
{
  t <- abs( MaxL - S )/Dispersion
  P = (1 - t)*2
  print("По Шовене")
  print(P)
  if(P<0.5)
    print("сомнительное значение считают грубой ошибкой")
}
Showena()


#                                             Граббса \крапса )
Grabsa <- function()
{
  sum=0
  for ( i in BD$livesp ){
    sum= sum+(i - S)^2
  }
  so <- sqrt(sum/(2040 - 1))
  U <- abs( MaxL - S)/so
  print("По Граббсу")
  print(U)
}
Grabsa()  
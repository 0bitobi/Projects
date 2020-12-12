# library(installr)
# updateR()
# install.packages("lmtest")
# install.packages("dplyr")
# install.packages("foreign")
# install.packages("vcd")
# install.packages("devtools")
# install.packages("hexbin")
# install.packages("pander")
# install.packages("sjPlot")
# install.packages("knitr")
# install.packages("ggplot2")
# install.packages("installr")
# installr::updateR()
# install.packages("vroom") # добавление в R exel файлика
# install.packages("readxl") # работа с листами в exel
# install.packages("googlesheets4") # чтение гугл таблиц
# devtools::install_github("tidyverse/googlesheets4")
# library(vroom)
#ga_data <- vroom(file("C:/Users/Obito/Desktop/R Практ/Dz/Dz1/test.txt" )) #TXT
#ga_sep <- voom(file = "C:/Users/Obito/Desktop/R Практ/Dz/Dz1/ga_sep.csv" )
#ga_oct <- voom(file=".csv")
#1########################################################################################
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


BD <- read.csv2(file.choose(new = FALSE), sep=';', dec=".")
BZ <- read.csv2("C:/Users/Obito/Desktop/R_prak/Dz/Dz1/Ru_USD.csv", sep=';', dec=".")
#glimpse(BZ)
S <- read.table("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt",
                header = FALSE)
s <- read.table("C:/Users/Obito/Desktop/R_prak/Dz/Dz1/test.txt", header = FALSE)

f <- read.csv("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.csv", header = FALSE)
F <- read.csv("C:/Users/Obito/Desktop/R_prak/Dz/Dz1/test.csv", header = FALSE)
#2############################################################
#class(BD$dt)
#BD$dt <- strptime(BD$dt, format = "%d.%m.%Y")
BD$dt = as.Date(BD$dt, format = "%Y.%m.%d")
matplot(BD, xlim = NULL,type = 'o', ylim=NULL, pch = 20, col = "Red",main = "Ruble/USD", ylab = "Rub/USD", xlab = "Дни")


qplot(data = BD, dt, price, xlab = "Время", ylab = "RU_USD")
qplot(data = BD, log(price), xlab = "Коэфицент Руб")
qplot(data = BD, price, geom = "density")

#проверить что у меня в табличке все данные больше 0
#для всех данных указать промежуток и мб нарисавать те кто попал в промежуток
#3
Anime <- BD$price

for(i in 1:25)
{
  if (Anime[i]<0)
  {
    print(Anime[i])
  }
  else
  {
    print("ВСЕ ОК")
  }
  #else
  #{
    #if(Anime[i]>76)
    #{
      #  print(Anime[i])
   #}
  #}
}

#if(BD$price > 0){
 # print("Non-negative number")
#} else{
 # print("Negative number")
#}
#for
#for (val in price) {
  #if(val < 0)  
   # print("negative number")
#}
#print(price)



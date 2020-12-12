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

BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)
glimpse(BD)
str(BD) 
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
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
  if(P<1/2)
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

#2    mitools() , pan() , mix(), aregImpute(), transcan()
# Так как у меня нету пропуска в данных сделаем их

BD$livesp[3] <- NA
BD$livesp[10] <- NA
BD$livesp[40] <- NA
BD$livesp[98] <- NA
BD$livesp[228] <- NA
BD$livesp[1337] <- NA

#                         DELETE =)ahhaahahahaahahGAHAHGAHGHAGHAGAAAHHAHAHAHA =(((((((((
 fmla <- as.formula(paste(" ~ ", paste(BD$dist, collapse=" +")))
 ~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 +
   x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 +
   x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 +
   x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40 + x41 + x42 +
  x43 + x44 + x45 + x46 + x47 + x48 + x49 + x50 + x51 + x52 +
   x53 + x54 + x55 + x56 + x57 + x58
 g <- aregImpute(formula = fmla, n.impute=5)
 data(marijuana)
 View(marijuana)
 attach(marijuana)
 pred <- with(marijuana,cbind(int,dummy1,dummy2,dummy3,dummy4,dummy5))
 glimpse(marijuana)
 xcol <- 1:6
 zcol <- 1
 prior <- list(a=1,Binv=1,c=1,Dinv=1)
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=13579,iter=1000)
 plot(1:1000,log(result$sigma[1,1,]),type="l")
 acf(log(result$sigma[1,1,]))
 plot(1:1000,log(result$psi[1,1,]),type="l")
 acf(log(result$psi[1,1,]))
 par(mfrow=c(3,2))
 for(i in 1:6) plot(1:1000,result$beta[i,1,],type="l")
 for(i in 1:6) acf(result$beta[i,1,])
 y1 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=9565,iter=100,start=result$last)
 y2 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=6047,iter=100,start=result$last)
 y3 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=3955,iter=100,start=result$last)
 y4 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=4761,iter=100,start=result$last)
 y5 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=9188,iter=100,start=result$last)
 y6 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=9029,iter=100,start=result$last)
 y7 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=4343,iter=100,start=result$last)
 y8 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=2372,iter=100,start=result$last)
 y9 <- result$y
 result <- pan(y,subj,pred,xcol,zcol,prior,seed=7081,iter=100,start=result$last)
 y10 <- result$y
 d1 <- data.frame(y=y1,subj,pred)
 d2 <- data.frame(y=y2,subj,pred)
 d3 <- data.frame(y=y3,subj,pred)
 d4 <- data.frame(y=y4,subj,pred)
 d5 <- data.frame(y=y5,subj,pred)
 d6 <- data.frame(y=y6,subj,pred)
 d7 <- data.frame(y=y7,subj,pred)
 d8 <- data.frame(y=y8,subj,pred)
 d9 <- data.frame(y=y9,subj,pred)
 d10 <- data.frame(y=y10,subj,pred)
 require(mitools)
 d <- imputationList(list(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
 w <- with(d,lm(y~-1+pred))
 MIcoinemb(w)
 if(require(lme4)) {
   w2 <- with(d,lmer(y~-1+pred+(1|subj)))
   b <- MIextract(w2,fun=fixef)
   Var <- function(obj) unlist(lapply(diag(vcov(obj)),function(m) m))
   v <- MIextract(w2,fun=Var)
   MIcombine(b,v)
   detach(marijuana)
 }
data(bitest)
attach(bitest)
y <- with(bitest,cbind(y1,y2))

subj <- c(clusterid)
pred <- cbind (int, x1, x2, x3)
xcol <- 1:4
zcol <- 1
a <- 2
c <- 2
id2 <- matrix(c(1,0,0,1),ncol=2,nrow=2)
Binv <- a*id2
Dinv <- c*id2
prior <- list(a=a, Binv=Binv, c=c, Dinv=Dinv)
result <- pan(y, subj, pred, xcol, zcol, prior, seed=12345, iter=1000)
 # }
View(marijuana)


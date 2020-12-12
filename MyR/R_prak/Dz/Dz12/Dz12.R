# 3. Модифицировать функцию regr() для расчета возраста Вселенной с помощью бутстрепа.
# 4. Изучить коэффициент детерминации (R-squared и adjusted R-squared) как критерий качества аппроксимации моделью.
library(dplyr)
library(car)
BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)

glimpse(BD)
str(BD)
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
attach(BD)
summary(BD)

#                                  2
#
states <- as.data.frame(BD[,c("n", "price", "totsp", "livesp", "dist")])
M <- lm(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = states)
M <- cor(states)
col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#00007F"))
corrplot(M, method="color", col=col4(10), cl.length=11,
         order = "AOE", addCoef.col="green")

col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#00007F"))
corrplot(Corttt)
corrplot(states,title = "Correlation Plot", method = "square", outline = T, addgrid.col = "darkgray",
         order="hclust", mar = c(4,0,4,0), addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b",
         tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5)
corrplot(M, method = "color")
###############################################################

states <- as.data.frame(BD[,c("n", "price", "totsp", "livesp", "dist")])
DD <- lm(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = states)
M <- cor(states)
# corrplot(states)
corrplot(M, method="color", col=col4(10), cl.length=11,
         order = "AOE", addCoef.col="red")
#

m1 <- lm(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = BD)

confint(m1)

confint(m1, level = 0.99)
library(arm)

coefplot(m1, parm = -2)
df <- glm(n ~ ., data = BD)
coefplot(df)
#     regr нужно переписать так, чтобы сразу оценивать возраст Вселенной, а не постоянную Хаббла - и сравнить с тем, что получалось на семинаре.              3
# Оценка доверительных интервалов параметра бутстрепом:
library(gamair)
data(hubble)
library(boot)

regr_new <-function(data, indices){
  M <- lm(y ~ x - 1, data = hubble)
  hub.const <- 76.581/3.09e19
  age <- 1/hub.const
  Vozrast <- age/(60^2*24*365)
  return(summary(M)$coefficients[1])
}
results_new <- boot(data = hubble, statistic = regr_new, R = 1000)
results_new
quantile(results_new$t, c(0.025, 0.975))
##################################################################### 3
regr1 <- function(data, indices) {
  dat1 <- dat[indices, ]
  fit1 <- lm(x ~ I(y(60^2*24*365)/3.09e19)-1, data = dat1)
  return((summary(fit)$coefficients[1]))
}
results1<- boot(data = hubble, statistic = regr1, R = 1000)
results1
quantile(results1$t, c(0.025, 0.975))
plot(results1)

#                     Тоже рабочая версия но без функции с более точной оценкой 
library(gamair)
data(hubble)

M <- lm(y ~ x - 1, data = hubble)

summary(M)
beta <- summary(M)$coefficients[1]
SE <- summary(M)$coefficients[2]

ci.lower <- beta - qt(0.975, df = 23)*SE
ci.upper <- beta + qt(0.975, df = 23)*SE
ci.lower
ci.upper
results_new
Uni.upper <- 1/(ci.lower*60^2*24*365.25/3.09e19)
Uni.lower <- 1/(ci.upper*60^2*24*365.25/3.09e19)
Uni.lower
Uni.upper
Pqwe<-((Uni.lower+Uni.upper)/2)
Pqwe
##################################################################################
regr <- function(data, indices) {
  # вектор indices будет формироваться функцией boot() 
  dat <- data[indices, ] 
  fit <- lm(y ~ -1 + x, data = dat)
  return(summary(fit)$coefficients[1])
}

library(boot)
results <- boot(data = hubble, statistic = regr, R = 1000)
#Просмотр полученных результатов
results

plot(results)
quantile(results$t, c(0.025, 0.975))
U.lower <- 1/(85.73249*60^2*24*365.25/3.09e19)
U.upper <- 1/(67.07360*60^2*24*365.25/3.09e19)
U.lower
U.upper
#Чтобы избежать смещенных оценок
boot.ci(results, type = "bca")
#########################################################################       4

hubble$fit = fitted(M)

p1 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_hline(aes(yintercept=mean(hubble$y)), color = "blue") +
  geom_segment(aes(x = x, y = y, xend = x, yend = mean(hubble$y))) +
  ggtitle("TSS")

p2 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_segment(aes(x = x, y = y, xend = x, yend = fit)) +
  ggtitle("RSS")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)




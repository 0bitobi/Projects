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


#1##############################################################################
rsq <- function(formula, data, indices)
{
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

bootobject <- boot(data=BD, statistic=rsq,
                   R=1000, formula=n~kitsp+dist)
bootobject
plot(bootobject)

boot.ci(bootobject, type="bca")

#2 and 3 ##############################################################################

malo = data.frame(
  Factor = rep(c("A", "B"), each = 50),
  Variable = c(rnorm(50, 5, 2),
               rnorm(50, 4, 3)))

mnogo = data.frame(
  Factor = rep(c("A", "B"), each = 2500),
  Variable = c(rnorm(2500, 5),
               rnorm(2500, 4)))

#Exponential
set.seed(123)
lambda <- 0.2
n <- 5000
data_expMnogo <- data.frame(x = rexp(n, lambda))

q <- 100
data_expMalo <- data.frame(x = rexp(q, lambda))

#
graf <- ggplot(malo, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
MyGraf <- ggplot(BD, aes(BD$livesp, group = FALSE, fill = "int")) +
  geom_density(alpha = 1/2)
graf2 <- ggplot(mnogo, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
ExponentialMnogo <- (ggplot(data_expMnogo, aes(x = x)) + ylab("") +
                  geom_histogram(aes(y=..density..), binwidth=0.5, colour="black", fill="white") +
                  ggtitle("???????????????? ?????????????") +
                  geom_density(alpha = 0.2, fill = "#FF6666"))
ExponentialMalo <- (ggplot(data_expMalo, aes(x = x)) + ylab("") +
                       geom_histogram(aes(y=..density..), binwidth=0.5, colour="black", fill="white") +
                       ggtitle("???????????????? ?????????????") +
                       geom_density(alpha = 0.2, fill = "#FF6666"))

graf + facet_grid()
graf2 + facet_grid()
MyGraf + facet_grid()
plot(ExponentialMnogo)
plot(ExponentialMalo)
#Обьяденяю в 1 графки

grid.arrange(graf, ExponentialMalo, graf2, ExponentialMnogo, ncol = 2)

# графки квантилей
qqnorm(malo$Variable); qqline(malo$Variable)
qqnorm(BD$livesp);qqline(BD$livesp)
qqnorm(mnogo$Variable); qqline(mnogo$Variable)
qqnorm(data_expMnogo$x);qqline(data_expMnogo$x)
qqnorm(data_expMalo$x);qqline(data_expMalo$x)
#

S <- mean(BD$livesp)
D<-var(BD$livesp)
(l.est <- S/D)
(g.est <- ((S)^2)/D)

#
x <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(x), sd(x))

maloVariableVariable <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(malo$Variable), sd(malo$Variable))
#

#
shapiro.test(BD$livesp)
ad.test(BD$livesp)
cvm.test(BD$livesp)
lillie.test(BD$livesp)
sf.test(BD$livesp)
# malo
shapiro.test(malo$Variable)
ad.test(malo$Variable)
cvm.test(malo$Variable)
lillie.test(malo$Variable)
sf.test(malo$Variable)
# mnogo
shapiro.test(mnogo$Variable)
ad.test(mnogo$Variable)
cvm.test(mnogo$Variable)
lillie.test(mnogo$Variable)
sf.test(mnogo$Variable)
#? Exp
shapiro.test(data_expMalo$x)
ad.test(data_expMalo$x)
cvm.test(data_expMalo$x)
lillie.test(data_expMalo$x)
sf.test(data_expMalo$x)
# Exp
shapiro.test(data_expMnogo$x)
ad.test(data_expMnogo$x)
cvm.test(data_expMnogo$x)
lillie.test(data_expMnogo$x)
sf.test(data_expMnogo$x)
######################################
x <- seq(0,20, .1)
plot(x, dgamma(x, scale=2, shape=1), type="l", ylim=c(0,.5), ylab="y")
for(shape in 1:8){
  lines(x, dgamma(x, scale=2, shape=shape), col=shape)
}

Anime <- c(2,4,6,12,24)
plot(dgamma.wrapper, from=0, to=10)
for (i in seq_along(Anime))
  curve(dgamma(x,shape= Anime[i] , scale = 1) , from=0, to=10, col=i,add=TRUE)

#Методы генерации случайных числел
?RNGkind

# Воспроизводимость случайных чисел
example = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2), rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

tapply(example$Variable, example$Factor, summary)

example1 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example2 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example3 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example4 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

#Подключаем ggplot2
library(ggplot2)

#aes: Construct aesthetic mappings
p1 = ggplot(example1, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p2 = ggplot(example2, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p3 = ggplot(example3, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p4 = ggplot(example4, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)

library(gridExtra) # для функции grid.arrange()
grid.arrange(p1, p2, p3, p4, ncol = 2)

#Фиксируем начальную точку для генератора псевдослучайных чисел
set.seed(1020)

example1fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example2fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example3fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example4fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

p1 = ggplot(example1, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p2 = ggplot(example2, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p3 = ggplot(example3, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p4 = ggplot(example4, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)

grid.arrange(p1, p2, p3, p4, ncol = 2)

#  Законы распределения вероятностей, реализованные в R

#Префиксы для функций для работы с распределениями
#d- («density» – плотность): функции плотности вероятности
#p- («probability» – вероятность): функции распределения вероятностей
#q- («quantile» – квантиль): функции для нахождения квантилей
#r- («random» – случайный): функции для генерации случайных чисел

dnorm(-1)
pnorm(-1)
qnorm(c(0.25, 0.75))
rnorm(10, mean = 0, sd = 1)

#Полезные пакеты: VGAM, actuar, gamlss и ActuDistns

dstr = rnorm(5000)
polyCurve <- function(x, y, from, to, n = 50, miny,
                      col = "red", border = col) {
  drawPoly <- function(fun, from, to, n = 50, miny, col, border) {
    Sq <- seq(from = from, to = to, length = n)
    polygon(x = c(Sq[1], Sq, Sq[n]),
            y = c(miny, fun(Sq), miny),
            col = col, border = border)
  }
  lf <- length(from)
  stopifnot(identical(lf, length(to)))
  if(length(col) != lf)
    col <- rep(col, length.out = lf)
  if(length(border) != lf)
    border <- rep(border, length.out = lf)
  if(missing(miny))
    miny <- min(y)
  interp <- approxfun(x = x, y = y)
  mapply(drawPoly, from = from, to = to, col = col, border = border,
         MoreArgs = list(fun = interp, n = n, miny = miny))
  invisible()
}
plot(sort(dstr), (1:5000)/5000, type = "l", pch = 19, col = "blue",
     panel.first = polyCurve(sort(dstr), (1:5000)/5000, 
                             from = -3, to = -1,
                             col = "red", border = "black"),
     ylab = "P", xlab = "x")
abline(v = -1, lty = 2)
abline(h = pnorm(-1), lty = 2)


# Подбор закона и параметров распределения в R
library(MASS)
set.seed(0)
x.gam <- rgamma(200, rate = 0.5, shape = 3.5) 

# Метод моментов 
med.gam <- mean(x.gam)                  # выборочное среднее 
var.gam<-var(x.gam)                     # выборочная дисперсия 
(l.est <- med.gam/var.gam)              ## оценка параметра масштаба rate
(g.est <- ((med.gam)^2)/var.gam) 	## -оценка параметра формы shape

# Поиск решения системы уравнений
library(rootSolve)
f1 <- function(x){c(F1 = x[1]/x[2] - med.gam, F2 = x[1]/x[2]^2 - var.gam)}
multiroot(f1, c(3, 0.6))

# Распределение Вейбулла со случайным параметром масштаба
set.seed(1946)
x = sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
x  # Отсортированные значения 
summary(x)  # Выборочные характеристики

hist(x, freq = FALSE, breaks = 15, col = "grey88", 
     main="Гистограмма и ядерная плотность")
lines(density(x), lwd = 2, col = "blue")



# Функция для вывода графиков теоретической и эмпирической кумулятивной (КФР)
# и функций плотности (ФПР):
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("Эмпирическая ФР и ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("Эмпирическая плотность и ", main_name))
  plot(density(x), lwd = 2, col = "blue", main = mn) 
  lines(x, pd, col = "red", lwd = 2)
  par(op)
}

# оценка параметров нормального распределения: 
(dof <- fitdistr(x,"normal"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x,pnorm, mean = ep1, sd = ep2)
graph_distr(x, pnorm(x, mean = ep1, sd = ep2),
            dnorm(x, mean = ep1, sd = ep2),
            "нормальное распределение")

# оценка параметров лог-нормального распределения:
(dof <- fitdistr(x,"log-normal")) 
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x, plnorm, meanlog = ep1, sdlog = ep2)
graph_distr(x, plnorm(x, meanlog = ep1, sdlog = ep2),
            dlnorm(x, meanlog = ep1, sdlog = ep2),
            "логнормальное распределение")


# Проверка на нормальность распределения методом огибающих
set.seed(1946)
x <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(x), sd(x))

# Квантиль-квантильный график
qqnorm(x); qqline(x)

# Определяем и рисуем огибающие
z <- (x - mean(x))/sqrt(var(x))  #  Стандартизация выборки
x.qq <- qqnorm(z, plot.it = FALSE)
x.qq <- lapply(x.qq, sort)

library(boot)
# Генерация 999 бутстреп-выборок (т.е. случайных выборок из 
#  нормального распределения с параметрами выборки z)
x.gen <- function(dat, mle) rnorm(length(dat))
x.qqboot <- boot(z, sort, R = 999, 
                 sim = "parametric",ran.gen = x.gen)
sapply(1:999,function(i) lines(x.qq$x, x.qqboot$t[i,],
                               type = "l", col = "grey"))
points (x.qq, pch = 20)
lines(c(-3, 3), c(-3, 3), col = "red", lwd = 2)


library(car)
qqPlot(x, dist = "norm", col = palette()[1], pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭР и НР")

library(sm)
sm.density(x, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения")

# Тесты на нормальность:
shapiro.test(x)
library(nortest)
ad.test(x)
cvm.test(x)
lillie.test(x)
sf.test(x)


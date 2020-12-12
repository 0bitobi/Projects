# Таблица с данными для построения модели:
y <- c(
  109.14, 117.55, 106.76, 115.26, 117.13, 125.39, 121.03,
  114.03, 124.83, 113.92, 122.04, 109.41, 131.61, 103.93,
  116.64, 117.06, 111.73, 120.41, 112.98, 101.20, 120.19,
  128.53, 120.14, 108.70, 130.77, 110.16, 129.07, 123.46,
  130.02, 130.31, 135.06, 129.17, 137.08, 107.62, 139.77,
  121.47, 130.95, 138.15, 114.31, 134.58, 135.86, 138.49,
  110.01, 127.80, 122.57, 136.99, 139.53, 127.34, 132.26,
  120.85, 124.99, 133.36, 142.46, 123.58, 145.05, 127.83,
  140.42, 149.64, 151.01, 135.69, 138.25, 127.24, 135.55,
  142.76, 146.67, 146.33, 137.00, 145.00, 143.98, 143.81,
  159.92, 160.97, 157.45, 145.68, 129.98, 137.45, 151.22,
  136.10, 150.60, 148.79, 167.93, 160.85, 146.28, 145.97,
  135.59, 156.62, 153.12, 165.96, 160.94, 168.87, 167.64,
  154.64, 152.46, 149.03, 159.56, 149.31, 153.56, 170.87,
  163.52, 150.97)

c(mean(y), sd(y)) # среднее значение и станд. отклонение

shapiro.test(y) # тест на нормальность распределения

library(ggplot2)
ggplot(data = data.frame(y), aes(x = y)) + geom_histogram() + 
  ylab("Частота") + xlab("Давление, мм рт. ст.")


# Способы имитации данных

set.seed(101) # для воспроизводимости результата
y.new.1 <- rnorm(n = 100, mean = 135.16, sd = 16.96)

set.seed(101)
y.new.2 <- 135.16 + rnorm(n = 100, mean = 0, sd = 16.96)

# проверим, идентичны ли оба вектора?
all(y.new.1 == y.new.2)

# Оценка параметров линейной модели:
y.lm <- lm(y ~ 1) # формула для оценки только свободного члена
summary(y.lm)

# Генерация нескольких реализаций линейной модели:
library(arm)
set.seed(102) # для воспроизводимости результата
y.sim <- sim(y.lm, 4)
# y.sim - объект класса S4, который содержит
# слоты coef (коэффициенты модели) и sigma 
# (станд. отклонения остатков модели):
str(y.sim)

# Извлекаем альтернативные реализации среднего из y.sim:
y.sim@coef

# Извлекаем альтернативные реализации ст.отклонений остатков:
y.sim@sigma

# Создание 1000 вариантов модели:
y.sim <- sim(y.lm, 1000)

# Инициализация пустой матрицы, в которой мы будем сохранять
# данные, сгенерированные на основе 1000 альтернативных 
# реализаций модели:
y.rep <- array(NA, c(1000, 100))

# Заполняем матрицу y.rep имитированными данными:
for(s in 1:1000){
  y.rep[s, ] <- rnorm(100, y.sim@coef[s], y.sim@sigma[s])
}

# гистограммы выборочных распределений первых 12 реализаций базовой модели
par(mfrow = c(5, 4), mar = c(2, 2, 1, 1))
for(s in 1: 12){ hist(y.rep[s, ], xlab = "", ylab = "", breaks = 20, main = "")}

# Расчет интерквартильного размаха (ИКР) для каждого из 1000 имитированных
# распределений значений кровяного давления:
test.IQR <- apply(y.rep, MARGIN = 1, FUN = IQR)

# гистограмма значений ИКР для 1000 имитаций:
hist(test.IQR, xlim = range(IQR(y), test.IQR),
     main = "ИКР", xlab = "", ylab = "Частота", breaks = 20)
lines(rep(IQR(y), 2), c(0, 100), col = "blue", lwd = 4)

# Добавляем один предиктор - возраст:
x <- rep(seq(16, 65, 1), each = 2)

# Объединяем значения возраста и давления крови в одну таблицу:
Data <- data.frame(Age = x, BP = y)

ggplot(data = Data, aes(x = Age, BP)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_rug(color = "gray70", sides = "tr") + 
  ylab("Давление") + xlab("Возраст, лет")

summary(lm(BP ~ Age, data = Data)) 
#Тестовые данные были сгенерированы c помощью модели вида
#y_i=97.078+0.949*Age_i+epsilon_i, где epsilon_i~N(0, 9.563)

#Модель для оценки постоянной Хаббла
# Создадим таблицу с данными и построим модель:
library(gamair)
data(hubble)
str(hubble)

M <- lm(y ~ x - 1, data = hubble)
summary(M)
#anova(M)

#Оценим возраст Вселенной
(hub.const <- 76.581/3.09e19)
(age <- 1/hub.const)
age/(60^2*24*365)

# Оценка доверительных интервалов регрессии:
CPI.df <- cbind(predict(M,interval ="conf"), predict(M,interval ="pred"))  
CPI.df <- CPI.df[,-4] 
colnames(CPI.df) <- c("Y_fit","CI_l","CI_u","PI_l","PI_u")
head(CPI.df)

par(mfrow = c(1, 1))
matplot(hubble$x, CPI.df, type = "l", 
        lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4),
        ylab = "Скорость,км/с",xlab="Расстояние,Мпс")
with(hubble, matpoints(x, y, pch = 20))

# Оценка доверительных интервалов параметра:

#Параметрический подход
beta <- summary(M)$coefficients[1]
SE <- summary(M)$coefficients[2]
#Стандартная ошибка
SE
ci.lower <- beta - qt(0.975, df = 23)*SE
ci.upper <- beta + qt(0.975, df = 23)*SE
c(ci.lower, ci.upper)
Uni.upper <- 1/(ci.lower*60^2*24*365.25/3.09e19)
Uni.lower <- 1/(ci.upper*60^2*24*365.25/3.09e19)
c(Uni.lower, Uni.upper)

#Бутсреп
library(gamair)
data(hubble)

boots = vector(mode="list", length=6)

for(i in 1:6){
  boots[[i]] = hubble[sample(1:24, 24, replace = TRUE), 2:3]  
}

boots = do.call(rbind.data.frame, boots)
boots$reps = rep(c("A", "B", "C", "D", "E", "F"), each = 24)

ggplot(boots, aes(x, y)) + geom_point() + facet_wrap(~reps)


# Оценка доверительных интервалов параметра бутстрепом:
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

# Оценка доверительных интервалов параметра методом имитаций:
library(arm)
simulations <- sim(M, 1000)
hist(simulations@coef, breaks = 30)
sd(simulations@coef)
quantile(simulations@coef, c(0.025, 0.975))


#Геометрическая интерпретация общей суммы квадратов TSS
#и суммы квадратов остатков RSS
M <- lm(y ~ x - 1, data = hubble)
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


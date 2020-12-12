#Генерация псевдослучайных чисел (100 штук) из нормального распределения с соответствующими параметрами
X <- rnorm(n = 100, mean = 15, sd  = 5)

#Опция freq = FALSE позволяет рисовать гистограмму с нормированными стоблцами (площадь=1)
hist(X, breaks = 20, freq = FALSE, col = "lightblue")
#Функция density используется для рисования (ядерной) оценки плотности #Изучить параметры!
plot(density(X))
plot(density(X, bw = 0.8))
hist(X, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "Переменная X",
     ylab = "Плотность вероятности",
     main = "Гистограмма, совмещенная с кривой плотности")
lines(density(X), col = "red", lwd = 2)
lines(density(X, bw = 0.8), col = "blue", lwd = 2)


# InsectSprays – данные, полученных в ходе эксперимента по изучению эффективности шести видов инсектицидных средств. Каждым из этих средств обработаны по 12 растений, после чего подсчитано количество выживших на растениях насекомых.
# В InsectSprays имеются два столбца: count, содержащий результаты подсчета насекомых, и spray , содержащий коды инсектицидных средств (от А до F )

data(InsectSprays)
head(InsectSprays)
attach(InsectSprays)

# Установка пакета sm («smoothing methods»), если его нет в системе
#install.packages("sm")

# Если библиотека уже есть
library(sm)

# Сравнение всех групп по кривым ядерной плотности:
sm.density.compare(count, spray, lwd = 2, xlab = "Число насекомых", ylab = "Плотности")
# Специальная команда для заголовка
title(main = "Кривые ядерной плотности")
# Составляем вектор с кодами использованных цветов
Colfill <- c(2:(2 + length(levels(spray))))
# добавляем легенду туда, куда кликнем мышью:
legend(locator(1), levels(spray), fill = Colfill)


#  Двухмерное распределение, Indometh – еще один встроенный набор данных
data(Indometh)
attach(Indometh)
library(MASS)
f <- kde2d(time, conc) 
#Создание окрашенной прямоугольной сетки
image(f, xlab="Время выведения", ylab="Концентрация индометацина")
# Добавляем на график изолинии
contour(f, add = TRUE)

#  Примеры использования функции cdplot
library(HSAUR2)
data(plasma)
summary(plasma)
layout(matrix(1:2, ncol = 2))

cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), data = plasma)

# Сравните:
cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), bw = 0.9, data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), bw = 0.9, data = plasma)

#  Примеры "ящиков с усами" (box plot)
data(InsectSprays)
head(InsectSprays)
attach(InsectSprays)
# Слева от знака ~ указывается зависимая переменная, справа – предикторы
boxplot(count ~ spray,
        xlab = "Инсектициды",
        ylab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", data = InsectSprays)
# Горизонтальное расположение "ящиков":
boxplot(count ~ spray,
        ylab = "Инсектициды",
        xlab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", horizontal = TRUE,
        data = InsectSprays)

#  Пример двухмерного "мешка с усами"
data(Indometh); attach(Indometh)
library(aplpack)
bagplot(time, conc, xlab = "Время выведения",
        ylab = "Концентрация индометацина", main = "Мешок с усами")


#Не забываем проследующую команду (при необходимости)
par(mfrow = c(1,1))

# Примеры столбчатых диаграмм:
data(InsectSprays)
InsectSprays
attach(InsectSprays)
# Пример применения tapply
Means <- tapply(count, spray, mean)
Means

barplot(Means, col = "steelblue",
        xlab = "Инсектицид",
        ylab = "Количество выживших насекомых",
        border = "red", width = sqrt(Means))

#Горизонтальная версия 
barplot(Means, density = 20, angle = -45, space = 2,
        col = "red", horiz = TRUE, las = 1,
        ylab = "Инсектицид", 
        xlab = "Количество выживших насекомых")

# Столбчатые диаграммы для сгруппированных данных:
library(MASS)
data(genotype)
head(genotype)
means = with(genotype, tapply(Wt, list(Litter, Mother), mean))
means

barplot(means, beside = TRUE,
       col = topo.colors(4),
       legend.text = rownames(means),
       xlab = "Выводок", ylab = "Вес, г",
       ylim = c(0, 100))

barplot(means, beside = FALSE,
        col = topo.colors(4),
        xlab = "Выводок", ylab = "Вес, г")

#Вычисление стандартных отклонений
sds = with(genotype, tapply(Wt, list(Litter, Mother), sd))
sds

b <- barplot(means, ylim = c(min(pretty(means-sds)),
             max(pretty(means+sds))),
             col = topo.colors(4),
             beside = TRUE, xpd = FALSE,
             ylab = "Вес, г", xlab = "Выводок",
             legend.text=rownames(means))



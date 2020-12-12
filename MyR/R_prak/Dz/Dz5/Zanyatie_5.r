# Оценка выборочных характеристик:
data(mtcars)
head(mtcars)

# Среднее арифметическое:
mean(mtcars$mpg)

# Медиана:
median(mtcars$mpg)

# Дисперсия:
var(mtcars$mpg)

# Стандартное отклонение:
sd(mtcars$mpg)

# Минимальное значение:
min(mtcars$mpg)

# Максимальное значение:
max(mtcars$mpg)
max(mtcars$disp)

# Стандартная ошибка среднего:
SEmpg = sd(mtcars$mpg)/sqrt(length(mtcars$mpg))

# Квантили:
quantile(mtcars$mpg)
quantile(mtcars$mpg, p = seq(0, 1, 0.1))

# Интерквантильный размах:
IQR(mtcars$mpg)

# Отсутствующие значения:
mtcars$mpg[3] <- NA

# Просмотрим результат:
head(mtcars$mpg)
mean(mtcars$mpg)
mean(mtcars$mpg, na.rm = TRUE)
length(mtcars$mpg)
sum(!is.na(mtcars$mpg))

# Использование функции which:
which.min(mtcars$mpg)
which.max(mtcars$mpg)
rownames(mtcars)[which.min(mtcars$mpg)]
rownames(mtcars)[which.max(mtcars$mpg)]

# Использование функции apply:  средний объем двигателя у моделей с автоматической (0)
#и ручной (1) коробками передач 
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = mean)
# tapply(X = mtcars$disp, INDEX = list(mtcars$am, mtcars$vs), FUN = mean)
SE <- function(x) {sd(x)/sqrt(length(x))}
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = SE)

# Использование функции summary():
summary(mtcars)
summary(mtcars$mpg)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

# Проверим, удалась ли конвертация:
is.factor(mtcars$vs)
is.factor(mtcars$am)
summary(mtcars)

# Использование функций других пакетов
library(moments)  #загрузка пакета moments
kurtosis(mtcars$mpg, na.rm = TRUE)
skewness(mtcars$mpg, na.rm = TRUE)

# Пакет Hmisc, функция describe():
library(Hmisc)
describe(mtcars)

# Пакет pastecs, функция stat.desc():
library(pastecs)
stat.desc(mtcars)

# Пакет psych, функция describe.by() - расчет параметров
# описательной статистики для каждого уровня некоторого фактора:
library(psych)
describe.by(mtcars, mtcars$am)

# Пакет doBy, функция summaryBy():
library(doBy)
summaryBy(mpg + wt ~ cyl + vs, data = mtcars,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )


Sparrows = read.table("SparrowsElphick.txt", sep = "\t", header = T)
dotchart(Sparrows$wingcrd, xlab = "Длина крыла (мм)",
         ylab = "Порядковый номер", lcolor = NA)

	
# Заполнение пропусков:
library(VIM)
data(sleep, package = "VIM")
head(sleep)

#Результаты наблюдений за процессом сна у 62 млекопитающих разных видов
#Зависимые переменные: продолжительность сна со сновидениями (Dream),
#сна без сновидений (NonD) и их сумму (sleep)
#Таксономические переменные: масса тела (BodyWgt), вес мозга (BrainWgt),
#продолжительность жизни (Span) и время беременности (Gest).
#Экологические переменные: 5-бальные оценки степени хищничества животных (Pred), меры защищенности их места для сна (Exp): от глубокой норы до полно  стью открытого пространства, и показателя риска (Danger) на основе  логической комбинации Pred и Exp

# Список строк, в которых нет пропущенных значений:
sleep[complete.cases(sleep), ]

# Список строк, в которых есть хотя бы одно пропущенное значение:
sleep[!complete.cases(sleep), ]
sum(is.na(sleep$Dream))

#Дополнительная статистическая информация
library(mice)
#Нули в таблице соответствуют недостающим значениям: в первой строке пропусков нет,
# последующие упорядочены по числу их появления
#Первый столбец указывает число случаев в каждой строке исходных данных
#Последний столбец – число переменных с отсутствующими значениями в каждой строке
md.pattern(sleep)

#Числовые данные масштабируются к интервалу [0, 1] и представлены уровнями яркости:
# более темными цветами показаны большие значения
#Недостающие значения представлены красным цветом
matrixplot(sleep)
aggr(sleep)


#Определение корреляции между пропусками
# Формируем матрицу со значениями 1 в местах пропусков:
x <- as.data.frame (abs (is.na(sleep)))
y <- x[, which(colSums(x) > 0)]
print(cor(y),4)
cor(sleep, y, use = "pairwise.complete.obs")

#Заполнение пропущенных значений, Multivariate Imputation by Chained Equations
imp <- mice(sleep, seed = 1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

# Заполнение пропусков и сохранение результата для использования
# в последующих примерах:
sleep_imp3 <- complete(imp, action = 3)
head(sleep_imp3)
sleep[!complete.cases(sleep_imp3), ]
save(sleep_imp3, file = "sleep_imp.Rdata")


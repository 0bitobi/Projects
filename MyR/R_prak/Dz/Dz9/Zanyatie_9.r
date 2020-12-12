# Критерий хи-квадрат
qchisq(p = 0.95, df = 1) #"ручное" вычисление значения статистики

mice <- matrix(c(13, 44, 25, 29), nrow = 2, byrow = TRUE)
mice # просмотр содержимого матрицы
chisq.test(mice) # встроенный тест хи-квадрат

light <- c(12, 40, 45)
dark <- c(87, 34, 75)
very.dark <- c(3, 8, 2)
color.data <- matrix(c(light, dark, very.dark), nrow = 3,
                     dimnames = list(c("Pop1", "Pop2", "Pop3"),
                                     c("Light", "Dark", "Very dark")))
color.data
#Возникет предупреждение о некорректности аппроксимации, поскольку в very.dark 
#два значения меньше 5
chisq.test(color.data)


# Точный тест Фишера
(X <- matrix(c(1, 10, 8, 4), ncol = 2))
fisher.test(X)

# Критерий МакНемара (McNemar)
#Данные в "длинном"формате
data <- read.table(header = TRUE, text = '
 subject time result
       1  pre      0
       1 post      1
       2  pre      1
       2 post      1
       3  pre      0
       3 post      1
       4  pre      1
       4 post      0
       5  pre      1
       5 post      1
       6  pre      0
       6 post      1
       7  pre      0
       7 post      1
       8  pre      0
       8 post      1
       9  pre      0
       9 post      1
      10  pre      1
      10 post      1
      11  pre      0
      11 post      0
      12  pre      1
      12 post      1
      13  pre      0
      13 post      1
      14  pre      0
      14 post      0
      15  pre      0
      15 post      1
')

library(reshape2)

# Преобразуем данные в "широкий" формат
data.wide <- dcast(data, subject ~ time, value.var = "result")
data.wide
ct <- table(data.wide[, c("pre","post")])
ct
mcnemar.test(ct)
mcnemar.test(ct, correct = FALSE) #Отключение поправки Эдвардса 
table(data[,c("time", "result")])

# Критерий Кохрана-Мантеля-Хензеля (Cochran-Mantel-Haenszel)
drug <-
  array(c(11, 10, 25, 27,
          16, 22, 4, 10,
          14, 7, 5, 12,
          2, 1, 14, 16,
          6, 0, 11, 12,
          1, 0, 10, 10,
          1, 1, 4, 8,
          4, 6, 2, 1),
        dim = c(2, 2, 8),
        dimnames = list(
          Group = c("Drug", "Control"),
          Response = c("Success", "Failure"),
          Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug

mantelhaen.test(drug)
library(reshape) # для функции melt()

drug.df <- data.frame( melt(drug,
                            id=c("Center", "Group", "Response")))

library(ggplot2)
p <- ggplot(data = drug.df, aes(x = Center, y = value, fill = Response)) +
  ylab("%")
p + geom_bar(stat = "identity", position = "fill") + 
  facet_grid(Group ~ .)

# Мощность теста при сравнении долей
votes <- matrix(c(28, 72, 20, 80), ncol = 2, byrow = T)
votes
res <- chisq.test(votes)
res

#Изучить возможности power.prop.test

obs <- res$observed
exptd <- res$expected
obs <- obs/200 # здесь и ниже, 200 - общее число опрошенных
sqrt(sum((exptd - obs)^2/exptd))

#Используем дополнительную библиотеку
library(pwr)
ES.w2(obs)

pwr.chisq.test(w = ES.w2(obs), df = 1, N = 200)

#Определение необходимого числа респондентов для заданной мощности
pwr.chisq.test(w = 0.15, N = NULL, df = 1, 
               sig.level = 0.05, power = 0.8)

# Оценка мощности теста методом имитаций               
prop.power <- function(n1, n2, p1, p2) {
  twobytwo=matrix(NA, nrow = 10000, ncol = 4)
  twobytwo[,1] = rbinom(n = 10000, size = n1, prob = p1)
  twobytwo[,2] = n1-twobytwo[,1]
  twobytwo[,3] = rbinom(n = 10000, size = n2, prob = p2)
  twobytwo[,4] = n1 - twobytwo[, 3]
  p = rep(NA, 10000)
  chisq.test.v = function(x) 
    as.numeric(chisq.test(matrix(x, ncol = 2), correct = FALSE)[3])
  p=apply(twobytwo, 1, chisq.test.v)
  power=sum(ifelse(p < 0.05, 1, 0))/10000
  return(power)
}

prop.power(100, 100, 0.28, 0.20)


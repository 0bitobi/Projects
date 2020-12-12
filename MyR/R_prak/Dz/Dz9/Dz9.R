library(reshape2)
library(pwr)
library(dplyr)

BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)
glimpse(BD)
str(BD)
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
attach(BD)
#2
#                                 Хи-квадрат
chisq.test(n)

#                             Точный тест Фишера(2x2)
test_Fisher <- matrix(c(1, 2, 3, 4), ncol = 2)
fisher.test(test_Fisher)
fisher.test(walk, floor)

#                                   МакНемара
mcnemar.test(walk, floor, correct = TRUE)

#                           Кохрана-Мантеля-Хензеля
mantelhaen.test(code, brick, floor)

# library(ggplot2)
# p <- ggplot(data = BD, aes(x = floor, y = brick, fill = Response)) +
#   ylab("%")
# p + geom_bar(stat = "identity", position = "fill") + 
#   facet_grid(Group ~ .)
#3
power.prop.test(n = 20000, p1 = 0.73, p2 = 0.5, sig.level = 0.05,
                power = NULL,
                alternative = c("two.sided", "one.sided"),
                strict = FALSE, tol = .Machine$double.eps^0.25)

pwr.p.test(h = ES.h(p1 = 0.65, p2 = 0.50),
           sig.level = 0.05,
           power = 0.80)
#разобраться с вычислением статистической мощности при сравнении частот.

votes <- matrix(c(35, 71, 17, 90), ncol = 2, byrow = T)
votes
chisq.test(votes)

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

prop.power(100, 100, 0.30, 0.20)


# Одновыборочный t-критерий  
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
mean(d.intake)
t.test(d.intake, mu = 7725)

# Сравнение двух независимых выборок:
library(ISwR)
data(energy)
attach(energy)
head(energy)
tapply(expend, stature, mean)

t.test(expend ~ stature)
t.test(expend ~ stature, var.equal = TRUE)

# Сравнение двух зависимых выборок:
data(intake) # из пакета ISwR
attach(intake)
head(intake)
post - pre
mean(post - pre)

t.test(pre, post, paired = TRUE)


# Ранговый критерий Уилкоксона-Манна-Уитни

#   Одновыборочный критерий:
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
wilcox.test(d.intake, mu = 7725)

# Сравнение двух независимых выборок:
library(ISwR)
data(energy)
attach(energy)

wilcox.test(expend ~ stature, paired = FALSE)

# Сравнение двух зависимых выборок:
data(intake) # из пакета ISwR
attach(intake)
wilcox.test(pre, post, paired = TRUE)
wilcox.test(pre, post, paired = TRUE, conf.int = TRUE)


#  Оценка однородности дисперсий в двух группах:
data(energy, package = "ISwR")
attach(energy)
var.test(expend ~ stature)

#  Оценка однородности дисперсий в нескольких группах:
data(InsectSprays)
library(car)
leveneTest(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays, center = mean)
bartlett.test(count ~ spray, data = InsectSprays)
fligner.test(count ~ spray, data = InsectSprays)

# ������������� ������������� ������

# �������� ������� � �������:
tomato <- data.frame(weight =
                       c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
                         1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
                         1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))

# ������� �� �������� ����������:
Means <- data.frame(weight = as.numeric(tapply(tomato$weight,
                                               tomato$trt, mean)),
                    trt = rep("Means", 3))

# ��������� ������� Means � ������� tomato:
tomato <- rbind(tomato, Means)

# �������� ������� ������� ������� trt �� Water:
tomato$trt <- relevel(tomato$trt, ref = "Water")

# ������ �������� ��������� (��� ����� �� ������ Means ����� ��� ����
# ������������� ������ ������ ������):
stripchart(weight ~ trt, data = tomato, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "�������", xlab = "��� (��)")

# ��������� ������ ����� �� ������ Means ����� ������� �����
# (��� ����� ����� ����� ���������� (�������_1, 4), (�������_2, 4)
# � (�������_3, 4); �� ���� ����������� "�������"):
points(x = Means$weight, y = c(1, 1, 1), pch = 19,
       col = c("blue", "red", "black"))

# ���������� ������
tomato2 <-
  data.frame(weight =
               c(1.25, 1.9, 1.3, 1.5, 2.3, 1.45, # water
                 2.1, 2.2, 2.4, 2.9, 2.8, 3.1, # nutrient
                 0.9, 1.2, 0.8, 1.3, 0.7, 1.4), # nutrient+24D
             trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                       c(6, 6, 6)))

# ������� �� �������� ����������:
Means2 <- data.frame(weight = as.numeric(tapply(tomato2$weight,
                                                tomato2$trt, mean)),
                     trt = rep("Means", 3))

# ��������� ������� Means � ������� tomato:
tomato2 <- rbind(tomato2, Means2)

# �������� ������� ������� ������� trt �� Water:
tomato2$trt <- relevel(tomato2$trt, ref = "Water")

# ������ �������� ��������� (��� ����� �� ������ Means ����� ��� ����
# ������������ ������ ������ ������):
stripchart(weight ~ trt, data = tomato2, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "�������", xlab = "��� (��)")

# ��������� ������ ����� �� ������ Means ����� ������� �����
# (��� ����� ����� ����� ���������� (�������_1, 4), (�������_2, 4)
# � (�������_3, 4); �� ���� ����������� "�������"):
points(x = Means2$weight, y = c(1, 1, 1), pch = 19,
       col = c("red", "black", "blue"))

# ��� ������������� ������ ��� �������� �������� �������
x = seq(0, 10, 0.1)
plot(x, df(x, 2, 15), type = "l")
abline(v = qf(0.95, 2, 15), lty = 2)

#������ tomato ����� ��� ����� Means
# ���������� �������������� �������������� �������, aov() ��� lm():
summary(aov(weight ~ trt, data = tomato[1:18,1:2]))


# ������������� ������������� ������
library(HSAUR2)
data(weightgain)
str(weightgain)

library(ggplot2)
ggplot(data = weightgain, aes(x = type, y = weightgain)) + 
  geom_boxplot(aes(fill = source))

require(doBy)
summaryBy(weightgain ~ type + source, data = weightgain,
          FUN = c(mean, sd, length))

#���� ������������
plot.design(weightgain) 

# ������ ��������������
with(weightgain, interaction.plot(x.factor = type,
                                  trace.factor = source,
                                  response = weightgain))

# ������������� ������������� ������:
M1 <- aov(weightgain ~ source + type + source:type, 
          data = weightgain)
# M1 <- aov(weightgain ~ source*type, 
#           data = weightgain)
summary(M1)


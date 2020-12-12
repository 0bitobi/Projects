#��������� ��������������� ����� (100 ����) �� ����������� ������������� � ���������������� �����������
X <- rnorm(n = 100, mean = 15, sd  = 5)

#����� freq = FALSE ��������� �������� ����������� � �������������� ��������� (�������=1)
hist(X, breaks = 20, freq = FALSE, col = "lightblue")
#������� density ������������ ��� ��������� (�������) ������ ��������� #������� ���������!
plot(density(X))
plot(density(X, bw = 0.8))
hist(X, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "���������� X",
     ylab = "��������� �����������",
     main = "�����������, ����������� � ������ ���������")
lines(density(X), col = "red", lwd = 2)
lines(density(X, bw = 0.8), col = "blue", lwd = 2)


# InsectSprays � ������, ���������� � ���� ������������ �� �������� ������������� ����� ����� ������������� �������. ������ �� ���� ������� ���������� �� 12 ��������, ����� ���� ���������� ���������� �������� �� ��������� ���������.
# � InsectSprays ������� ��� �������: count, ���������� ���������� �������� ���������, � spray , ���������� ���� ������������� ������� (�� � �� F )

data(InsectSprays)
head(InsectSprays)
attach(InsectSprays)

# ��������� ������ sm (�smoothing methods�), ���� ��� ��� � �������
#install.packages("sm")

# ���� ���������� ��� ����
library(sm)

# ��������� ���� ����� �� ������ ������� ���������:
sm.density.compare(count, spray, lwd = 2, xlab = "����� ���������", ylab = "���������")
# ����������� ������� ��� ���������
title(main = "������ ������� ���������")
# ���������� ������ � ������ �������������� ������
Colfill <- c(2:(2 + length(levels(spray))))
# ��������� ������� ����, ���� ������� �����:
legend(locator(1), levels(spray), fill = Colfill)


#  ���������� �������������, Indometh � ��� ���� ���������� ����� ������
data(Indometh)
attach(Indometh)
library(MASS)
f <- kde2d(time, conc) 
#�������� ���������� ������������� �����
image(f, xlab="����� ���������", ylab="������������ ������������")
# ��������� �� ������ ��������
contour(f, add = TRUE)

#  ������� ������������� ������� cdplot
library(HSAUR2)
data(plasma)
summary(plasma)
layout(matrix(1:2, ncol = 2))

cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), data = plasma)

# ��������:
cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), bw = 0.9, data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), bw = 0.9, data = plasma)

#  ������� "������ � �����" (box plot)
data(InsectSprays)
head(InsectSprays)
attach(InsectSprays)
# ����� �� ����� ~ ����������� ��������� ����������, ������ � ����������
boxplot(count ~ spray,
        xlab = "�����������",
        ylab = "���������� �������� ���������",
        main = "������������� ������������",
        col = "coral", data = InsectSprays)
# �������������� ������������ "������":
boxplot(count ~ spray,
        ylab = "�����������",
        xlab = "���������� �������� ���������",
        main = "������������� ������������",
        col = "coral", horizontal = TRUE,
        data = InsectSprays)

#  ������ ����������� "����� � �����"
data(Indometh); attach(Indometh)
library(aplpack)
bagplot(time, conc, xlab = "����� ���������",
        ylab = "������������ ������������", main = "����� � �����")


#�� �������� ������������ ������� (��� �������������)
par(mfrow = c(1,1))

# ������� ���������� ��������:
data(InsectSprays)
InsectSprays
attach(InsectSprays)
# ������ ���������� tapply
Means <- tapply(count, spray, mean)
Means

barplot(Means, col = "steelblue",
        xlab = "����������",
        ylab = "���������� �������� ���������",
        border = "red", width = sqrt(Means))

#�������������� ������ 
barplot(Means, density = 20, angle = -45, space = 2,
        col = "red", horiz = TRUE, las = 1,
        ylab = "����������", 
        xlab = "���������� �������� ���������")

# ���������� ��������� ��� ��������������� ������:
library(MASS)
data(genotype)
head(genotype)
means = with(genotype, tapply(Wt, list(Litter, Mother), mean))
means

barplot(means, beside = TRUE,
       col = topo.colors(4),
       legend.text = rownames(means),
       xlab = "�������", ylab = "���, �",
       ylim = c(0, 100))

barplot(means, beside = FALSE,
        col = topo.colors(4),
        xlab = "�������", ylab = "���, �")

#���������� ����������� ����������
sds = with(genotype, tapply(Wt, list(Litter, Mother), sd))
sds

b <- barplot(means, ylim = c(min(pretty(means-sds)),
             max(pretty(means+sds))),
             col = topo.colors(4),
             beside = TRUE, xpd = FALSE,
             ylab = "���, �", xlab = "�������",
             legend.text=rownames(means))



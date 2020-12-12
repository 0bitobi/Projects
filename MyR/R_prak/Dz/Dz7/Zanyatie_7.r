# �������������� t-��������  
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
mean(d.intake)
t.test(d.intake, mu = 7725)

# ��������� ���� ����������� �������:
library(ISwR)
data(energy)
attach(energy)
head(energy)
tapply(expend, stature, mean)

t.test(expend ~ stature)
t.test(expend ~ stature, var.equal = TRUE)

# ��������� ���� ��������� �������:
data(intake) # �� ������ ISwR
attach(intake)
head(intake)
post - pre
mean(post - pre)

t.test(pre, post, paired = TRUE)


# �������� �������� ����������-�����-�����

#   �������������� ��������:
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
wilcox.test(d.intake, mu = 7725)

# ��������� ���� ����������� �������:
library(ISwR)
data(energy)
attach(energy)

wilcox.test(expend ~ stature, paired = FALSE)

# ��������� ���� ��������� �������:
data(intake) # �� ������ ISwR
attach(intake)
wilcox.test(pre, post, paired = TRUE)
wilcox.test(pre, post, paired = TRUE, conf.int = TRUE)


#  ������ ������������ ��������� � ���� �������:
data(energy, package = "ISwR")
attach(energy)
var.test(expend ~ stature)

#  ������ ������������ ��������� � ���������� �������:
data(InsectSprays)
library(car)
leveneTest(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays, center = mean)
bartlett.test(count ~ spray, data = InsectSprays)
fligner.test(count ~ spray, data = InsectSprays)

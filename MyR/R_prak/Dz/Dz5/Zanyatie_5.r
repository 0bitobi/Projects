# ������ ���������� �������������:
data(mtcars)
head(mtcars)

# ������� ��������������:
mean(mtcars$mpg)

# �������:
median(mtcars$mpg)

# ���������:
var(mtcars$mpg)

# ����������� ����������:
sd(mtcars$mpg)

# ����������� ��������:
min(mtcars$mpg)

# ������������ ��������:
max(mtcars$mpg)
max(mtcars$disp)

# ����������� ������ ��������:
SEmpg = sd(mtcars$mpg)/sqrt(length(mtcars$mpg))

# ��������:
quantile(mtcars$mpg)
quantile(mtcars$mpg, p = seq(0, 1, 0.1))

# ���������������� ������:
IQR(mtcars$mpg)

# ������������� ��������:
mtcars$mpg[3] <- NA

# ���������� ���������:
head(mtcars$mpg)
mean(mtcars$mpg)
mean(mtcars$mpg, na.rm = TRUE)
length(mtcars$mpg)
sum(!is.na(mtcars$mpg))

# ������������� ������� which:
which.min(mtcars$mpg)
which.max(mtcars$mpg)
rownames(mtcars)[which.min(mtcars$mpg)]
rownames(mtcars)[which.max(mtcars$mpg)]

# ������������� ������� apply:  ������� ����� ��������� � ������� � �������������� (0)
#� ������ (1) ��������� ������� 
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = mean)
# tapply(X = mtcars$disp, INDEX = list(mtcars$am, mtcars$vs), FUN = mean)
SE <- function(x) {sd(x)/sqrt(length(x))}
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = SE)

# ������������� ������� summary():
summary(mtcars)
summary(mtcars$mpg)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

# ��������, ������� �� �����������:
is.factor(mtcars$vs)
is.factor(mtcars$am)
summary(mtcars)

# ������������� ������� ������ �������
library(moments)  #�������� ������ moments
kurtosis(mtcars$mpg, na.rm = TRUE)
skewness(mtcars$mpg, na.rm = TRUE)

# ����� Hmisc, ������� describe():
library(Hmisc)
describe(mtcars)

# ����� pastecs, ������� stat.desc():
library(pastecs)
stat.desc(mtcars)

# ����� psych, ������� describe.by() - ������ ����������
# ������������ ���������� ��� ������� ������ ���������� �������:
library(psych)
describe.by(mtcars, mtcars$am)

# ����� doBy, ������� summaryBy():
library(doBy)
summaryBy(mpg + wt ~ cyl + vs, data = mtcars,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )


Sparrows = read.table("SparrowsElphick.txt", sep = "\t", header = T)
dotchart(Sparrows$wingcrd, xlab = "����� ����� (��)",
         ylab = "���������� �����", lcolor = NA)

	
# ���������� ���������:
library(VIM)
data(sleep, package = "VIM")
head(sleep)

#���������� ���������� �� ��������� ��� � 62 ������������� ������ �����
#��������� ����������: ����������������� ��� �� ������������ (Dream),
#��� ��� ���������� (NonD) � �� ����� (sleep)
#��������������� ����������: ����� ���� (BodyWgt), ��� ����� (BrainWgt),
#����������������� ����� (Span) � ����� ������������ (Gest).
#������������� ����������: 5-������� ������ ������� ����������� �������� (Pred), ���� ������������ �� ����� ��� ��� (Exp): �� �������� ���� �� �����  ���� ��������� ������������, � ���������� ����� (Danger) �� ������  ���������� ���������� Pred � Exp

# ������ �����, � ������� ��� ����������� ��������:
sleep[complete.cases(sleep), ]

# ������ �����, � ������� ���� ���� �� ���� ����������� ��������:
sleep[!complete.cases(sleep), ]
sum(is.na(sleep$Dream))

#�������������� �������������� ����������
library(mice)
#���� � ������� ������������� ����������� ���������: � ������ ������ ��������� ���,
# ����������� ����������� �� ����� �� ���������
#������ ������� ��������� ����� ������� � ������ ������ �������� ������
#��������� ������� � ����� ���������� � �������������� ���������� � ������ ������
md.pattern(sleep)

#�������� ������ �������������� � ��������� [0, 1] � ������������ �������� �������:
# ����� ������� ������� �������� ������� ��������
#����������� �������� ������������ ������� ������
matrixplot(sleep)
aggr(sleep)


#����������� ���������� ����� ����������
# ��������� ������� �� ���������� 1 � ������ ���������:
x <- as.data.frame (abs (is.na(sleep)))
y <- x[, which(colSums(x) > 0)]
print(cor(y),4)
cor(sleep, y, use = "pairwise.complete.obs")

#���������� ����������� ��������, Multivariate Imputation by Chained Equations
imp <- mice(sleep, seed = 1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

# ���������� ��������� � ���������� ���������� ��� �������������
# � ����������� ��������:
sleep_imp3 <- complete(imp, action = 3)
head(sleep_imp3)
sleep[!complete.cases(sleep_imp3), ]
save(sleep_imp3, file = "sleep_imp.Rdata")


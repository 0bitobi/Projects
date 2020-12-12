# ������� � ������� ��� ���������� ������:
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

c(mean(y), sd(y)) # ������� �������� � �����. ����������

shapiro.test(y) # ���� �� ������������ �������������

library(ggplot2)
ggplot(data = data.frame(y), aes(x = y)) + geom_histogram() + 
  ylab("�������") + xlab("��������, �� ��. ��.")


# ������� �������� ������

set.seed(101) # ��� ����������������� ����������
y.new.1 <- rnorm(n = 100, mean = 135.16, sd = 16.96)

set.seed(101)
y.new.2 <- 135.16 + rnorm(n = 100, mean = 0, sd = 16.96)

# ��������, ��������� �� ��� �������?
all(y.new.1 == y.new.2)

# ������ ���������� �������� ������:
y.lm <- lm(y ~ 1) # ������� ��� ������ ������ ���������� �����
summary(y.lm)

# ��������� ���������� ���������� �������� ������:
library(arm)
set.seed(102) # ��� ����������������� ����������
y.sim <- sim(y.lm, 4)
# y.sim - ������ ������ S4, ������� ��������
# ����� coef (������������ ������) � sigma 
# (�����. ���������� �������� ������):
str(y.sim)

# ��������� �������������� ���������� �������� �� y.sim:
y.sim@coef

# ��������� �������������� ���������� ��.���������� ��������:
y.sim@sigma

# �������� 1000 ��������� ������:
y.sim <- sim(y.lm, 1000)

# ������������� ������ �������, � ������� �� ����� ���������
# ������, ��������������� �� ������ 1000 �������������� 
# ���������� ������:
y.rep <- array(NA, c(1000, 100))

# ��������� ������� y.rep �������������� �������:
for(s in 1:1000){
  y.rep[s, ] <- rnorm(100, y.sim@coef[s], y.sim@sigma[s])
}

# ����������� ���������� ������������� ������ 12 ���������� ������� ������
par(mfrow = c(5, 4), mar = c(2, 2, 1, 1))
for(s in 1: 12){ hist(y.rep[s, ], xlab = "", ylab = "", breaks = 20, main = "")}

# ������ ����������������� ������� (���) ��� ������� �� 1000 �������������
# ������������� �������� ��������� ��������:
test.IQR <- apply(y.rep, MARGIN = 1, FUN = IQR)

# ����������� �������� ��� ��� 1000 ��������:
hist(test.IQR, xlim = range(IQR(y), test.IQR),
     main = "���", xlab = "", ylab = "�������", breaks = 20)
lines(rep(IQR(y), 2), c(0, 100), col = "blue", lwd = 4)

# ��������� ���� ��������� - �������:
x <- rep(seq(16, 65, 1), each = 2)

# ���������� �������� �������� � �������� ����� � ���� �������:
Data <- data.frame(Age = x, BP = y)

ggplot(data = Data, aes(x = Age, BP)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_rug(color = "gray70", sides = "tr") + 
  ylab("��������") + xlab("�������, ���")

summary(lm(BP ~ Age, data = Data)) 
#�������� ������ ���� ������������� c ������� ������ ����
#y_i=97.078+0.949*Age_i+epsilon_i, ��� epsilon_i~N(0, 9.563)

#������ ��� ������ ���������� ������
# �������� ������� � ������� � �������� ������:
library(gamair)
data(hubble)
str(hubble)

M <- lm(y ~ x - 1, data = hubble)
summary(M)
#anova(M)

#������ ������� ���������
(hub.const <- 76.581/3.09e19)
(age <- 1/hub.const)
age/(60^2*24*365)

# ������ ������������� ���������� ���������:
CPI.df <- cbind(predict(M,interval ="conf"), predict(M,interval ="pred"))  
CPI.df <- CPI.df[,-4] 
colnames(CPI.df) <- c("Y_fit","CI_l","CI_u","PI_l","PI_u")
head(CPI.df)

par(mfrow = c(1, 1))
matplot(hubble$x, CPI.df, type = "l", 
        lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4),
        ylab = "��������,��/�",xlab="����������,���")
with(hubble, matpoints(x, y, pch = 20))

# ������ ������������� ���������� ���������:

#��������������� ������
beta <- summary(M)$coefficients[1]
SE <- summary(M)$coefficients[2]
#����������� ������
SE
ci.lower <- beta - qt(0.975, df = 23)*SE
ci.upper <- beta + qt(0.975, df = 23)*SE
c(ci.lower, ci.upper)
Uni.upper <- 1/(ci.lower*60^2*24*365.25/3.09e19)
Uni.lower <- 1/(ci.upper*60^2*24*365.25/3.09e19)
c(Uni.lower, Uni.upper)

#�������
library(gamair)
data(hubble)

boots = vector(mode="list", length=6)

for(i in 1:6){
  boots[[i]] = hubble[sample(1:24, 24, replace = TRUE), 2:3]  
}

boots = do.call(rbind.data.frame, boots)
boots$reps = rep(c("A", "B", "C", "D", "E", "F"), each = 24)

ggplot(boots, aes(x, y)) + geom_point() + facet_wrap(~reps)


# ������ ������������� ���������� ��������� ����������:
regr <- function(data, indices) {
  # ������ indices ����� ������������� �������� boot() 
  dat <- data[indices, ] 
  fit <- lm(y ~ -1 + x, data = dat)
  return(summary(fit)$coefficients[1])
}

library(boot)
results <- boot(data = hubble, statistic = regr, R = 1000)
#�������� ���������� �����������
results

plot(results)
quantile(results$t, c(0.025, 0.975))
U.lower <- 1/(85.73249*60^2*24*365.25/3.09e19)
U.upper <- 1/(67.07360*60^2*24*365.25/3.09e19)
U.lower
U.upper
#����� �������� ��������� ������
boot.ci(results, type = "bca")

# ������ ������������� ���������� ��������� ������� ��������:
library(arm)
simulations <- sim(M, 1000)
hist(simulations@coef, breaks = 30)
sd(simulations@coef)
quantile(simulations@coef, c(0.025, 0.975))


#�������������� ������������� ����� ����� ��������� TSS
#� ����� ��������� �������� RSS
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


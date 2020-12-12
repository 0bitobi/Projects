#������ ��������� ��������� ������
?RNGkind

# ����������������� ��������� �����
example = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2), rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

tapply(example$Variable, example$Factor, summary)

example1 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example2 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example3 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example4 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

#���������� ggplot2
library(ggplot2)

#aes: Construct aesthetic mappings
p1 = ggplot(example1, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p2 = ggplot(example2, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p3 = ggplot(example3, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p4 = ggplot(example4, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)

library(gridExtra) # ��� ������� grid.arrange()
grid.arrange(p1, p2, p3, p4, ncol = 2)

#��������� ��������� ����� ��� ���������� ��������������� �����
set.seed(1020)

example1fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example2fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example3fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example4fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

p1 = ggplot(example1, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p2 = ggplot(example2, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p3 = ggplot(example3, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p4 = ggplot(example4, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)

grid.arrange(p1, p2, p3, p4, ncol = 2)

#  ������ ������������� ������������, ������������� � R

#�������� ��� ������� ��� ������ � ���������������
#d- (�density� � ���������): ������� ��������� �����������
#p- (�probability� � �����������): ������� ������������� ������������
#q- (�quantile� � ��������): ������� ��� ���������� ���������
#r- (�random� � ���������): ������� ��� ��������� ��������� �����

dnorm(-1)
pnorm(-1)
qnorm(c(0.25, 0.75))
rnorm(10, mean = 0, sd = 1)

#�������� ������: VGAM, actuar, gamlss � ActuDistns

dstr = rnorm(5000)
polyCurve <- function(x, y, from, to, n = 50, miny,
                      col = "red", border = col) {
  drawPoly <- function(fun, from, to, n = 50, miny, col, border) {
    Sq <- seq(from = from, to = to, length = n)
    polygon(x = c(Sq[1], Sq, Sq[n]),
            y = c(miny, fun(Sq), miny),
            col = col, border = border)
  }
  lf <- length(from)
  stopifnot(identical(lf, length(to)))
  if(length(col) != lf)
    col <- rep(col, length.out = lf)
  if(length(border) != lf)
    border <- rep(border, length.out = lf)
  if(missing(miny))
    miny <- min(y)
  interp <- approxfun(x = x, y = y)
  mapply(drawPoly, from = from, to = to, col = col, border = border,
         MoreArgs = list(fun = interp, n = n, miny = miny))
  invisible()
}
plot(sort(dstr), (1:5000)/5000, type = "l", pch = 19, col = "blue",
     panel.first = polyCurve(sort(dstr), (1:5000)/5000, 
                             from = -3, to = -1,
                             col = "red", border = "black"),
     ylab = "P", xlab = "x")
abline(v = -1, lty = 2)
abline(h = pnorm(-1), lty = 2)


# ������ ������ � ���������� ������������� � R
library(MASS)
set.seed(0)
x.gam <- rgamma(200, rate = 0.5, shape = 3.5) 

# ����� �������� 
med.gam <- mean(x.gam)                  # ���������� ������� 
var.gam<-var(x.gam)                     # ���������� ��������� 
(l.est <- med.gam/var.gam)              ## ������ ��������� �������� rate
(g.est <- ((med.gam)^2)/var.gam) 	## -������ ��������� ����� shape

# ����� ������� ������� ���������
library(rootSolve)
f1 <- function(x){c(F1 = x[1]/x[2] - med.gam, F2 = x[1]/x[2]^2 - var.gam)}
multiroot(f1, c(3, 0.6))

# ������������� �������� �� ��������� ���������� ��������
set.seed(1946)
x = sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
x  # ��������������� �������� 
summary(x)  # ���������� ��������������

hist(x, freq = FALSE, breaks = 15, col = "grey88", 
     main="����������� � ������� ���������")
lines(density(x), lwd = 2, col = "blue")



# ������� ��� ������ �������� ������������� � ������������ ������������ (���)
# � ������� ��������� (���):
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("������������ �� � ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("������������ ��������� � ", main_name))
  plot(density(x), lwd = 2, col = "blue", main = mn) 
  lines(x, pd, col = "red", lwd = 2)
  par(op)
}

# ������ ���������� ����������� �������������: 
(dof <- fitdistr(x,"normal"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x,pnorm, mean = ep1, sd = ep2)
graph_distr(x, pnorm(x, mean = ep1, sd = ep2),
            dnorm(x, mean = ep1, sd = ep2),
            "���������� �������������")

# ������ ���������� ���-����������� �������������:
(dof <- fitdistr(x,"log-normal")) 
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x, plnorm, meanlog = ep1, sdlog = ep2)
graph_distr(x, plnorm(x, meanlog = ep1, sdlog = ep2),
            dlnorm(x, meanlog = ep1, sdlog = ep2),
            "������������� �������������")


# �������� �� ������������ ������������� ������� ���������
set.seed(1946)
x <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(x), sd(x))

# ��������-����������� ������
qqnorm(x); qqline(x)

# ���������� � ������ ���������
z <- (x - mean(x))/sqrt(var(x))  #  �������������� �������
x.qq <- qqnorm(z, plot.it = FALSE)
x.qq <- lapply(x.qq, sort)

library(boot)
# ��������� 999 ��������-������� (�.�. ��������� ������� �� 
#  ����������� ������������� � ����������� ������� z)
x.gen <- function(dat, mle) rnorm(length(dat))
x.qqboot <- boot(z, sort, R = 999, 
                 sim = "parametric",ran.gen = x.gen)
sapply(1:999,function(i) lines(x.qq$x, x.qqboot$t[i,],
                               type = "l", col = "grey"))
points (x.qq, pch = 20)
lines(c(-3, 3), c(-3, 3), col = "red", lwd = 2)


library(car)
qqPlot(x, dist = "norm", col = palette()[1], pch = 19,
       xlab="�������� ����������� �������������", 
       ylab="����������� ��������", 
       main="��������� ��������� �� � ��")

library(sm)
sm.density(x, model = "Normal", xlab = "������������� �������",
           ylab = "������� ��������� �������������")

# ����� �� ������������:
shapiro.test(x)
library(nortest)
ad.test(x)
cvm.test(x)
lillie.test(x)
sf.test(x)


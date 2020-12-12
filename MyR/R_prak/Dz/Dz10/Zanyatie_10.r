load(file = "sleep_imp.Rdata")

# ������������ �������������� �������
M <- cor(sleep_imp3)
library(corrplot)
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                           "#7FFF7F", "cyan", "#007FFF", "blue","#00007F")) 
corrplot(M, method = "color", col = col4(20), cl.length = 21,
         order = "AOE", addCoef.col = "green")


# ������ �������� ���������
library(car)
vif(lm(Sleep ~ BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
       data = sleep_imp3))


#  ��������� ��������� ���������
cars <- mtcars[, 1:7]
pairs(cars, panel = panel.smooth)

# ������� ��� ���������� ������� � ������������� ����������:
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep = "")
  # ��� ������� ��������� �������� ������ ������ 
  # � ������������ �� ��������� ������������ ����������:
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# ������ ��� ���� ������� �������
#pairs(cars, , panel = panel.smooth, lower.panel = panel.cor)

# ������ ��� ���� ������� �������
library(lattice)
splom(cars)

# ������ ��� ���� ������� �������
library(GGally)
ggpairs(cars, 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"))

# ����������� ������� ������� (EDF)
library(mgcv)
summary(gam(mpg ~ s(hp) + s(qsec), data = cars))


# �������� �������� ������ �� �����
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
# �������� ����������� �������� ������: ������� � ������ ����� >= 65
I1 <- Sparrows$SpeciesCode == 1 & 
  Sparrows$Sex != "0" &
  Sparrows$wingcrd < 65
Wing1 <- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1 <- factor(Sparrows$Sex[I1])

# ��������� ����� � ��� ��� �������������� ����������
fMonth1 <- factor(Mon1, levels = c(5, 6, 7, 8, 9),
                  labels = c("���", "����",
                             "����", "������", "��������"))
fSex1 <- factor(Sex1, levels = c(4, 5),
                labels = c("�����", "�����"))

# ����� �������������� ���������
coplot(Wei1 ~ Wing1 | fMonth1 * fSex1,
       ylab = c("��� (�)", "���"),
       xlab = c("����� ����� (��)", "�����"),
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

#  ������� ������ �� ��� � ��������:
df <- data.frame(weight = Wei1, length = Wing1, sex = fSex1, month = fMonth1) 
df1 <- df[df$month != "May" & df$month != "Sep", ]

#  ������ �������� ������ � �������� ������������� �������:
M1 <- lm(weight ~ length*month*sex, data = df1)
DT <- anova(M1)

#  ������� ���������� �������� � ������� ������ Word:
library(stargazer)
stargazer(M1, type = "latex", out = "M1.doc")
stargazer(DT, type = "html", out = "DT.doc", summary = FALSE)


# ������� ������������������ ������� (���)

# �������� ������ � ����������� ��� �������:
Waders <- read.table(file = "wader.txt", header = TRUE)
Time <- seq(1, 25)

# ���������� ������� �������� � ����� ����
par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
plot(Time, Waders$C.fuscicolis, type = "l", 
     xlab = "����� (2 ������)", ylab = "C. fuscicollis abundance")
acf(Waders$C.fuscicolis, main = "C. fuscicollis ACF")
plot(Time, Waders$L.dominicanus, type = "l", 
     xlab = "����� (2 ������)", ylab = "L. dominicanus abundance")
acf(Waders$L.dominicanus, main = "L. dominicanus ACF")
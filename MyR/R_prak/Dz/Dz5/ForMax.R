library(ggplot2)
library(gganimate)
library(dplyr)
library(devtools)
library(sjPlot)
library(knitr)
library(sm)
library(HSAUR2)
library(aplpack)
##############
library(outliers)
library(Hmisc)
library(pan)
library(mix)
library(mitools)

# 1. � ������� ������� ������ outliers � ������� ���������� ��������� ��� ������������� �������� (������, �������, �����, Q-���� ������� � ��.) ��������� � ������, �������� �� ������������ ���������� ��������.
# 2. ��������������� ��������� mitools() , pan() , mix() �� ����������� �������, � ����� aregImpute() � transcan() �� ������ Hmisc ��� ���������� ��������� � ������.

BD <- read.csv2("C:/Users/Obito/Desktop/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)
glimpse(BD)
str(BD) 
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
#View(BD)
#������ ������� ����� �������� � ������� boxplot
#������ �������� ��� ������� (������� ��������)
#�� ��� � �������������� ����� � � ��� ����� ������ ��� 25% ������ 
#������� ��� 75% ���� ������ 
#��� ��������� ��� ������� ������� ������������� �������� (���������� ������ �� ������� ����� �����������)
boxplot(BD$price ~ BD$livesp, data=mtcars, subset=cyl %in% c(4,6))
########################################################################
# �� ��� ������ (��������� �� ����� ������������� ������)
hist(BD$livesp, breaks = 20, freq = FALSE, col = "brown1",
     xlab = "����� ������� ��������, ��.�",
     ylab = "��������� �����������",
     main = "�����������, ����������� � ������ ��������� ")
lines(density(BD$livesp), col = "blue", lwd = 2)
#������ �� ���������� ������������� (������ ���?) �� �� ����� � ��� �������� 
#1   
example <- function()
{
  MaxL <- max(BD$livesp)
  Vi2 <- outlier(BD$livesp, opposite = FALSE ,logical = FALSE)
  if(MaxL == Vi2)
  {
    print(max(BD$livesp))
    print("������������ ���������� ��������� ��������")
  }
}
example()

#                                        Q-���� �������

MinL <- min(BD$livesp)        # ����������� �������
MaxL <- max(BD$livesp)        # ������������ ������� 
R <- MaxL-MinL                #�������� ����� ���������� � ���������� ���������
S <- mean(BD$livesp)          # ������� ��������������
Dispersion <- var(BD$livesp)  #���������� �������������������� ����������(���������)

QTest <- function()
{
  sort(BD$livesp)
  Q_test <- abs(MinL-BD$livesp/R)
  SEmpg = sd(BD$livesp)/sqrt(length(BD$livesp))
  print("�� Q ����� �������")
  print(SEmpg)
}
QTest()
#����� ���������� � ���, ��� ����� ������� ��� ����� ��������� ������� 10. 
#����� ����� � �����.
#���� � ���, ��� ��� ������� �������� ������ �������� ���� ��������� ���������� Q-��������.


#                                             ������
Showena <- function()
{
  t <- abs( MaxL - S )/Dispersion
  P = (1 - t)*2
  print("�� ������")
  print(P)
  if(P<0.5)
    print("������������ �������� ������� ������ �������")
}
Showena()


#                                             ������� \������ )
Grabsa <- function()
{
  sum=0
  for ( i in BD$livesp ){
    sum= sum+(i - S)^2
  }
  so <- sqrt(sum/(2040 - 1))
  U <- abs( MaxL - S)/so
  print("�� �������")
  print(U)
}
Grabsa()  
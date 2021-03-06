library(ggplot2)
library(gganimate)
library(memisc)
library(lmtest)
library(lattice)
library(dplyr)
library(foreign)
library(vcd)
library(devtools)
library(hexbin)
library(pander)
library(sjPlot)
library(knitr)
library(HSAUR2)
library(sm)

BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)

glimpse(BD)
str(BD) 
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)

hist(BD$livesp, breaks = 20, freq = FALSE, col = "brown1", main = "���������� ����� ������� � ��������", xlab = "������� �� � ��.�", ylab = "����������")

#plot(density(BD$livesp))

qplot(data = BD, livesp, col = "brown1",main = 
        "�������� � ���(��� �������� ��� �����)", xlab = "������� �� � ��.�", ylab = "����������")

hist(BD$livesp, breaks = 20, freq = FALSE, col = "brown1",
     xlab = "����� ������� ��������, ��.�",
     ylab = "��������� �����������",
     main = "�����������, ����������� � ������ ��������� � ������� �������")
lines(density(BD$livesp), col = "green", lwd = 2)
lines(density(BD$livesp, bw = 0.8), col = "blue", lwd = 2)

legend("bottomleft", 
       legend = c("��������� ��� ������", "��������� ��� ������ ������
                  � ����� 0.8"), 
       col = c("green", "blue"), 
       pch = c(17,19), #������ � ����������� 
       bty = "n", # ��� ����
       pt.cex = 2, #������ ������ � ��������������
       cex = 1.5, # ������ ������
       text.col = "black", 
       horiz = F , 
       inset = c(0.4, 0.4)) # ��� ���������� �� �������� �� 0 �� 1

sm.density.compare(BD$livesp, BD$dist, lwd = 0, xlab = "xbckj", ylab = "���������")
sm.density.compare(BD$livesp, BD$code, lwd = 0, xlab = "xbckj", ylab = "���������")
#� ��� ������� ��� � �������� 
#qplot(data = BD, n, price, col = "brown1",main = 
#"�������� � ���(��� �������� ��� �����)", xlab = "���� � 1000$", ylab = "����������")

#cdplot � boxplot 
#cdplot
summary(BD)

cdplot(BD$livesp, BD$brick, col = c("firebrick2", "skyblue"),
       ylab = "�� ���� �������� ���", xlab = "����� ������� ��������, ��.�.",
       main = "��������� ����� ������� � ��������� �� ���� �������� ���")
legend("bottomleft", 
       legend = c("0 = ��������� ���", "1 = ��������������"), 
       #col = c("green", "firebrick2"), 
       pch = c(20,19), #������ � ����������� 
       bty = "n", # ��� ����
       pt.cex = 2, #������ ������ � ��������������
       cex = 1, # ������ ������
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.8)) # ��� ���������� �� �������� �� 0 �� 1

cdplot(BD$livesp, BD$code, col = c("deeppink","orangered1","yellow1","lawngreen","cyan1","darkblue","darkorchid3","red1"),
       ylab = "", xlab = "����� ������� ��������, ��.�.",  main ="������� ������� � ������ ������� ������")
#boxplot                  ��� � ������ ?

boxplot(BD$price ~ BD$livesp,
        xlab = "����� �������",
        ylab = "���� ��������",
        main = "����������� �� ������� �� � ����",
        col = "coral", data = BD)

boxplot(BD$price ~ BD$livesp, data=mtcars, subset=cyl %in% c(4,6))

#pie  ������ ������ ������ ��� ����� ������ � ������� ����� =(
summary(BD)

#par(mfrow = c(2,2))
pie(BD$price[BD$price>300], main = "������ �� ���� ��������", xlab = "������� ������� �������� ��� ���� �������")
pie(BD$livesp[BD$livesp>86], main = "������ �� ����� ������� ��������, ��.�.", xlab = "������� ������� �������� ��� ���� �������")
pie(BD$totsp[BD$totsp>150], main = "������ �� ����� ������� ��������, ��.�.", xlab = "������� ������� �������� ��� ���� �������")
#pie(BD$livesp)  ��� ��� ����� 

attach(BD)

Hentai <- c(0,0,0,0,0,0,0,0)
Loli <- levels(BD$code)
for (i in 1:length(BD$code)) {
        for(j in 1:length(levels(code)))
                if (as.character(code)[i] == Loli[j]) {Hentai[j] <- Hentai[j] + 1}

}
names(Hentai) <- Loli
pie(Hentai)


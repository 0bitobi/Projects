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

hist(BD$livesp, breaks = 20, freq = FALSE, col = "brown1", main = "Гистограма жилой площади в квартире", xlab = "Площадь кв в кв.м", ylab = "Количество")

#plot(density(BD$livesp))

qplot(data = BD, livesp, col = "brown1",main = 
        "Квартиры в МСК(Так выглядит без всего)", xlab = "Площадь кв в кв.м", ylab = "Количество")

hist(BD$livesp, breaks = 20, freq = FALSE, col = "brown1",
     xlab = "жилая площадь квартиры, кв.м",
     ylab = "Плотность вероятности",
     main = "Гистограмма, совмещенная с кривой плотности и ядерной оценкой")
lines(density(BD$livesp), col = "green", lwd = 2)
lines(density(BD$livesp, bw = 0.8), col = "blue", lwd = 2)

legend("bottomleft", 
       legend = c("учитывает все данные", "учитывает все данные только
                  с шагом 0.8"), 
       col = c("green", "blue"), 
       pch = c(17,19), #кружок и треугольник 
       bty = "n", # тип фона
       pt.cex = 2, #Размер кружка с треугольничком
       cex = 1.5, # размер шрифта
       text.col = "black", 
       horiz = F , 
       inset = c(0.4, 0.4)) # где находиться нв картинке от 0 до 1

sm.density.compare(BD$livesp, BD$dist, lwd = 0, xlab = "xbckj", ylab = "Плотности")
sm.density.compare(BD$livesp, BD$code, lwd = 0, xlab = "xbckj", ylab = "Плотности")
#а как сделать это с графиком 
#qplot(data = BD, n, price, col = "brown1",main = 
#"Квартиры в МСК(Так выглядит без всего)", xlab = "Цена в 1000$", ylab = "Количество")

#cdplot и boxplot 
#cdplot
summary(BD)

cdplot(BD$livesp, BD$brick, col = c("firebrick2", "skyblue"),
       ylab = "Из чего построен дом", xlab = "Жилая площадь квартиры, кв.м.",
       main = "Отношение жилой площади к материалу из чего построен дом")
legend("bottomleft", 
       legend = c("0 = Кирпичный дом", "1 = Железобетонный"), 
       #col = c("green", "firebrick2"), 
       pch = c(20,19), #кружок и треугольник 
       bty = "n", # тип фона
       pt.cex = 2, #Размер кружка с треугольничком
       cex = 1, # размер шрифта
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.8)) # где находиться нв картинке от 0 до 1

cdplot(BD$livesp, BD$code, col = c("deeppink","orangered1","yellow1","lawngreen","cyan1","darkblue","darkorchid3","red1"),
       ylab = "", xlab = "Жилая площадь квартиры, кв.м.",  main ="Площадь квартир в разных районах Москвы")
#boxplot                  что я сделал ?

boxplot(BD$price ~ BD$livesp,
        xlab = "Жилая площадь",
        ylab = "Цена квартиры",
        main = "Зависимость от плозади кв и цены",
        col = "coral", data = BD)

boxplot(BD$price ~ BD$livesp, data=mtcars, subset=cyl %in% c(4,6))

#pie  Почему урезал потому что много данных и выходит плохо =(
summary(BD)

#par(mfrow = c(2,2))
pie(BD$price[BD$price>300], main = "Данные по цене квартиры", xlab = "Столько квартир подходит под наше условие")
pie(BD$livesp[BD$livesp>86], main = "Данные по жилой площади квартиры, кв.м.", xlab = "Столько квартир подходит под наше условие")
pie(BD$totsp[BD$totsp>150], main = "Данные по общая площади квартиры, кв.м.", xlab = "Столько квартир подходит под наше условие")
#pie(BD$livesp)  тут все плохо 

attach(BD)

Hentai <- c(0,0,0,0,0,0,0,0)
Loli <- levels(BD$code)
for (i in 1:length(BD$code)) {
        for(j in 1:length(levels(code)))
                if (as.character(code)[i] == Loli[j]) {Hentai[j] <- Hentai[j] + 1}

}
names(Hentai) <- Loli
pie(Hentai)


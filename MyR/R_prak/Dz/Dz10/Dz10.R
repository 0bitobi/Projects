library("dplyr")
library("knitr")
library("ggplot2")
library("glmnet")
library("car") 
library("MASS") 
library("quantreg") 
library("foreign")
library("LinRegInteractive")
library("interplot")
library(corrplot)
library(ISwR)
library(psych)

BD <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/R_prak/Dz/Dz3/datasets/flats_moscow.txt",
                sep = "\t", dec = ".", header = TRUE)

glimpse(BD)
str(BD)
BD <- mutate_each(BD, "factor", walk, brick, floor, code)
glimpse(BD)
attach(BD)

#2
ggplot(data = BD, aes(x = totsp, y = dist)) + geom_point() +
  labs(x = "Площадь квартиры, кв.м.с",
       y = "Расстояние от центра в км",
       title = "")   

m.poly3 <- lm(livesp ~ kitsp + I(kitsp^2) + I(kitsp^3), data = BD)
summary(m.poly3)

X <- model.matrix(~ 0 + kitsp + I(kitsp^2) + I(kitsp^3), data = BD)
cor(X)

vif(m.poly3)
#    VIF но ручками
m.vif1 <- lm(kitsp ~ I(kitsp^2) + I(kitsp^3), data = BD)
r2.1 <- summary(m.vif1)$r.squared
vif1 <- 1 / (1 - r2.1)
vif1

#3, 4
m1 <- lm(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = BD)

summary(m1)

fxInteractive(m1)

fxInteractive(m1, preselect.var = 'metrdist', preselect.type = "effect",
              initial.values = list(livesp=mean(livesp), walk = 10, floor = 1))

plot(BD$metrdist, BD$price, type = 'n') # пустой график, чтобы точки не мешали

# проводим регрессионную прямую для каждого типа квартиры (для каждого значения floor)
# и для этого прогоняем модель на разных подвыборках - subset
# плюс при этом меняем цвет прямой

abline(lm(BD$price ~ BD$metrdist, subset = BD$floor==0), lty = 1, col = "red", lwd = 1)
abline(lm(BD$price ~ BD$metrdist, subset = BD$floor==1), lty = 1, col = "green", lwd = 1)
legend("topright", c("floor=0", "floor=1"), col = c("red", "green"), lty = c(1, 1))

interplot(m1, var1 = "metrdist", var2 = "floor") # модель, эффект чего смотрим (var1), в зависимости от значений какой переменной (var2)
interplot(m1, var1 = "floor", var2 = "metrdist")
interplot(m1, var1 = "floor", var2 = "metrdist") + 
  
  xlab("Расстояние до ближайшей станции метро") +
  
  ylab("Расчетный коэффициент для местоположения (floor)") +
  
  ggtitle("Предельное влияние \n расположения квартиры на ее цену")

#5
plot(Time, dist, type = "l", 
     xlab = "", ylab = "")
acf(dist, main = "Графики автокорреляционной функции")
plot(Time, kitsp, type = "l", 
     xlab = "", ylab = "")
acf(kitsp, main = "Графики автокорреляционной функции")
    
#Табличка
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(BD[,1:4], pch = 19,  cex = 0.5,
      col = my_cols[BD$livesp],
      lower.panel=NULL)

pairs(BD)

##############################3################################################
pairs.panels(BD[,-5], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#таблица корел
library(ISwR)
states <- as.data.frame(BD[,c("n", "price", "totsp", "livesp", "dist")])
M <- lm(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = states)
M <- cor(states)
col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#00007F"))
corrplot(M, method="color", col=col4(10), cl.length=11,
         order = "AOE", addCoef.col="green")

col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#00007F"))
corrplot(Corttt)
##########
corrplot(states,title = "Correlation Plot", method = "square", outline = T, addgrid.col = "darkgray",
         order="hclust", mar = c(4,0,4,0), addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b",
         tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5)
corrplot(M, method = "color")
###############################################################

states <- as.data.frame(BD[,c("n", "price", "totsp", "livesp", "dist")])
DD <- lm(price ~ livesp + metrdist + walk + floor + floor:metrdist, data = states)
M <- cor(states)
# corrplot(states)
corrplot(M, method="color", col=col4(10), cl.length=11,
         order = "AOE", addCoef.col="red")
#pi
corrplot(M, method="pi", col=col4(10), cl.length=11,
         order = "AOE", addCoef.col="red")
#shade
corrplot(M, method="shade", col=col4(10), cl.length=11,
         order = "AOE", addCoef.col="red")


library(ggplot2)
library(memisc)
library(lmtest)
library(dplyr)
library(foreign)
library(vcd)
library(devtools)
library(hexbin)
library(pander)
library(sjPlot)
library(knitr)

data <- c("01.08","02.08","03.08","04.08","05.08","06.08","07.08","08.08","09.08","10.08","11.08")
number <- c(73.4, 74.6 , 75.1, 74.9, 75.3, 75, 74.9, 75.2, 75.5, 75.7, 75)
RU_USD <- data.frame(DATA = data, NUMBER= number)
 print(RU_USD)
 
class(RU_USD$DATA)
#RU_USD$DATA <- strptime(RU_USD$DATA, format = "%d.%m.%g")
#class(RU_USD$DATA)

matplot(RU_USD$NUMBER, RU_USD$DATA, xlim = NULL,type = 'o', ylim=NULL, pch = 20, col = "Red",main = "Ruble/USD", ylab = "Äíè", xlab = "Ruble/USD")

qplot(data=RU_USD, NUMBER, xlab = "Ru_USD")
#if-else
if(number > 0){
  print("Non-negative number")
} else{
  print("Negative number")
}
#for
for (val in number) {
  if(val < 0)  
    print("Non-negative number")
}
print(number)
  
#while
while(TRUE)
{
  print("count")
}

#repeat

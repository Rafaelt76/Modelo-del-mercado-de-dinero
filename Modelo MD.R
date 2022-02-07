#Librer?as necesarias
library(readxl)
library(lmtest)
library(strucchange)
library(sandwich)
library(ggplot2)
library(car)
library(forecast)
library(tseries)
library(seasonal)
library(tibble)
library(tidyverse)
library(lubridate)
library(urca)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)
library(quantmod)
library(xts)
library(AER)
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
# datos
MLAFIN <- read_excel("MLAFIN.xlsx")
A=ts(MLAFIN$M2,start=2004,end=2021,frequency = 4)
B=ts(MLAFIN$PIBS1,start=2004,end=2021,frequency = 4)
C=ts(MLAFIN$PIBS2, start=2004,end=2021,frequency = 4)
D=ts(MLAFIN$PIBS3, start=2004,end=2021,frequency = 4)
E=ts(MLAFIN$CP, start=2004,end=2021,frequency = 4)
F=ts(MLAFIN$REM, start=2004,end=2021,frequency = 4)
PIB=ts(MLAFIN$PIB, start=2004,end=2021,frequency = 4)
#Graficos
theme_set(theme_dark())
ggplot(data=MLAFIN)+geom_line(size=1, mapping= aes(x=Año, y=M2, group=1), colour="green")+xlab("Año")+ylab("Instumentos de alta y media liquidez sin efectivo")
ggplot(data=MLAFIN)+geom_line( mapping= aes(x=Año, y=PIBS1, group=1, colour="blue"))
ggplot(data=MLAFIN)+geom_line( mapping= aes(x=Año, y=PIBS2, group=1, colour= "pourple"))
ggplot(data=MLAFIN)+geom_line( mapping= aes(x=Año, y=PIBS3, group=1, colour="orange"))
ggplot(data=MLAFIN)+geom_line( mapping= aes(x=Año, y=CP, group=1, colour="green"))
ggplot(data=MLAFIN)+geom_line( mapping= aes(x=Año, y=REM, group=1, colour="pourple"))
ggplot(data=MLAFIN)+geom_line( mapping= aes(x=Año, y=PIB, group=1, colour="red"))+xlab("Año")+ylab("Producto Interno Bruto")
#ModeloT (Es el modelo que explica la relaci?n del Pib, IPC, y nuestra BM calculada)
ModeloT=lm(A~B+C+D+log(E)+log(F))
summary(ModeloT)
residuales=ModeloT$residuals
plot(residuales)
#Prueba parcial de Fisher (Para saber si alguna de las variables es inecesaria)
LMFT1=lm(A~B)
LMFT2=lm(A~C)
LMFT3=lm(A~D)
LMFT4=lm(A~log(E))
LMFT5=lm(A~log(F))
anova(ModeloT,LMFT1)
anova(ModeloT,LMFT2)
anova(ModeloT,LMFT3)
anova(ModeloT,LMFT4)
anova(ModeloT,LMFT5)
######Pruebas 
#Prueba RESET ()
resettest(ModeloT)
#Prueba Chow break()
sctest(ModeloT,tipo="Chow",punto=10)
#Prueba Jarque Bera
jarque.bera.test(residuals(ModeloT))
#Beruch Pagan
bptest(ModeloT)
ncvTest(ModeloT)
#Dickey fuller 
DF=ur.df(residuales)
summary(DF)
#Durbin watson
dwtest(ModeloT)
#Causalidad BM PIB
grangertest(A~PIB, order=2)
grangertest(PIB~A, order=2)
#Coeficientes y porcentajes
BETAS=matrix(ModeloT$coefficients,nrow=1,ncol=6, byrow = FALSE)
b1=BETAS[1,2]
b2=BETAS[1,3]
b3=BETAS[1,4]
bPIB=b1+b2+b3
bPIB
#Porcentajes de aportación y Gráficos de pastel
#Nombramiento de datos
TEDG<- read_excel("MLAFIN.xlsx", sheet = "Aportaciones %TE")
EFDG<- read_excel("MLAFIN.xlsx", sheet = "Aportacion %EF")
SEDG<- read_excel("MLAFIN.xlsx", sheet = "Aportacion %SE")
#Graficas de porcentajes de aportación al PIB
#Por tamaño
ggplot(TEDG, aes(x=1, y=Porcentaje, fill=Tamaño)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(Porcentaje,1),"%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")
ggplot(EFDG, aes(x=Estado, y=Porcentaje)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  theme_classic() + coord_flip()+theme_set(theme_dark())
ggplot(SEDG, aes(x=Sector, y=Porcentaje)) + 
  geom_bar(stat="identity", fill="green") + 
  theme_classic() + coord_flip()+theme_set(theme_excel())



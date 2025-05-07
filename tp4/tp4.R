#install.packages('haven')
#install.packages('summarytools')
#install.packages("ggplot2") 
#install.packages("AER")
#install.packages("car")
#install.packages("xtable")
#install.packages("haven")
#limpio entorno
rm(list=ls()) 
#librerías necesarias


library(AER)
library(car)
library(xtable)
library(haven)
library(readr)
library(dplyr)
library(haven)
library(dplyr)
library(ggplot2)
library(stargazer)
library(plm)
library(summarytools)
library(rlang)
library(ggplot2)


setwd("C:/Users/sebib/Documents/GitHub/ECONOMETR-A/tp4")



#importo la base d datos
df <- read_dta("cuarto_trim_2019.dta")
head(df)


# Vemos quÃ© tipo de variables hay
str(df)



#--------------------------------------Punto A----------------------------------

# Proporción de deserción
table(df$deserta) / nrow(df)




#variables relevantes
#edad, mujer, jmujer, educjefe, mimebros, ch11 carac del estab educativo, estado de act

# Me quedo solo con las variables necesarias

df$privado <- ifelse(df$ch11 == 2, 1,
                     ifelse(df$ch11 == 1, 0, NA))

df <- df %>%
  select(deserta, edad, mujer, jmujer, educ_jefe, miembros, ch11, estado)
str(df)
df <- df %>%
  filter(estado != 0, estado != 4)
  


#Borramos valores faltantes de las variables que vamos a usar (antes eliminen las variables que no usen del dataset!)
df = na.omit(df)
summary(df)
df$estado <- factor(df$estado, labels = c("Ocupado", "Desocupado", "Inactivo"))


mydata$educ_jefe <- factor(mydata$educ_jefe, labels = c("Primaria", "Secundaria", "Universitaria"))



#--------------------------------------Punto B----------------------------------




#--------------------------------------Punto C----------------------------------


#--------------------------------------Punto D----------------------------------


#--------------------------------------Punto E----------------------------------


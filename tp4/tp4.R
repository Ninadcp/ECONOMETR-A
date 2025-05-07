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

#--------------------------------------Punto A----------------------------------


# Filtro la muestra: 13-19 años, solteros (CH07==1), vive con al menos un padre (PP07H==1 o 2),
# y primaria completa o más (NIVEL_ED >= 2)
df_filtrado <- df %>%
  filter(edad >= 13 & edad <= 19,
         CH07 == 1,
         PP07H %in% c(1, 2),
         NIVEL_ED >= 2)

# Ver proporción de deserción
table(df_filtrado$deserta) / nrow(df_filtrado)

# Estadísticas descriptivas generales
summary(select(df_filtrado, edad, mujer, hermanos, ingresos_familiares, jmujer, educ_jefe, miembros))

# Comparación por grupo: desertó vs no desertó
df_filtrado %>%
  group_by(deserta) %>%
  summarise(
    edad_prom = mean(edad, na.rm = TRUE),
    prop_mujer = mean(mujer, na.rm = TRUE),
    hermanos_prom = mean(hermanos, na.rm = TRUE),
    ingreso_prom = mean(ingresos_familiares, na.rm = TRUE),
    prop_jmujer = mean(jmujer, na.rm = TRUE),
    educ_jefe_prom = mean(educ_jefe, na.rm = TRUE),
    miembros_prom = mean(miembros, na.rm = TRUE)
  )


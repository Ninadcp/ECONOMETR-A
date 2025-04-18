#install.packages("readr")
#install.packages("stargazer")  # para exportar en latex fyi
#install.packages('fixest')
#install.packages("plm")
rm(list=ls())
#librerías necesarias
library(readr)
library(dplyr)
library(stargazer)
library(fixest)
library(plm)

#set directorio
setwd("/Users/ninadicostanzopereira/Desktop/ECONOMETR-A/tp2/")

#cargar datos
df <- read_csv("cornwell.csv")
#-----------------------------------a------------------------------------------
#ESTIMEN POR MCO
#agrego
#head(df)
modelo <- lm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity+ ltaxpc + lpctmin + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + lmix + lpctymle +  urban + central + west, data = df)

summary(modelo)
stargazer(modelo, type = "latex", out = "modelo2a.tex") #guarda en .txt para latex
#-----------------------------------c------------------------------------------
#use FE
pdata <- pdata.frame(df, index = c("county", "year"))

modelo_fe <- plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity+ ltaxpc + lpctmin + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + lmix + lpctymle , data = pdata, model = "within")

summary(modelo_fe)
stargazer(modelo, type = "latex", out = "modelo2fe.tex") #guarda en .txt para latex

#-----------------------------------d------------------------------------------



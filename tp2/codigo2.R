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
stargazer(modelo, type = "latex", out = "modelo.tex") #guarda en .txt para latex
#-----------------------------------c------------------------------------------
#use FE
pdata <- pdata.frame(df, index = c("county", "year"))

modelo_fe <- plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity+ ltaxpc + lpctmin + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + lmix + lpctymle , data = pdata, model = "within")

summary(modelo_fe)
stargazer(modelo, type = "latex", out = "modelofe.tex") #guarda en .txt para latex

#-----------------------------------d------------------------------------------
#restar medias, sacamos el promedio histórico x estado.
#con log 
#calcular las medias por estado:

#Agregar una columna q sea media de c/u.
vars <- list("lcrmrte", "lprbarr", "lprbconv", "lprbpris", "lavgsen", "lpolpc", 
          "ldensity", "ltaxpc", "lpctmin", "lwcon", "lwtuc", "lwtrd", "lwfir", 
          "lwser", "lwmfg", "lwfed", "lwsta", "lwloc", "lmix", "lpctymle", "west", "central", "urban" )


df <- df %>%
  mutate(nueva_col = west)

for (x in vars) {
  df <- df %>%
    group_by(county) %>%
    mutate(
      !!paste0(x, "_media") := mean(!!sym(x), na.rm = TRUE),           # Columna con las medias
      !!paste0(x, "_n") := !!sym(x) - mean(!!sym(x), na.rm = TRUE)      # Columna con valores sin las medias
    ) %>%
    ungroup()
}

modelofwl <- lm(lcrmrte_n ~ lprbarr_n + lprbconv_n + lprbpris_n + lavgsen_n + lpolpc_n + ldensity_n + ltaxpc_n + lpctmin_n + lwcon_n + lwtuc_n + lwtrd_n + lwfir_n + lwser_n + lwmfg_n + lwfed_n + lwsta_n + lwloc_n + lmix_n + lpctymle_n + urban_n + central_n + west_n - 1, data = df)
summary(modelofwl)
stargazer(modelo, type = "latex", out = "modelofwl.tex") #guarda en .txt para latex

#-----------------------------------e------------------------------------------
modelofwl2 <- lm(lcrmrte ~ lprbarr_n + lprbconv_n + lprbpris_n + lavgsen_n + lpolpc_n + ldensity_n + ltaxpc_n + lpctmin_n + lwcon_n + lwtuc_n + lwtrd_n + lwfir_n + lwser_n + lwmfg_n + lwfed_n + lwsta_n + lwloc_n + lmix_n + lpctymle_n + urban_n + central_n + west_n - 1, data = df)
summary(modelofwl2)
stargazer(modelo, type = "latex", out = "modelofwl2.tex") #guarda en .txt para latex



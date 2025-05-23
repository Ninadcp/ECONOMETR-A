#install.packages("readr")
#install.packages("stargazer")  # para exportar en latex fyi
#install.packages('fixest')
#install.packages("plm")
#install.packages("dplyr")
rm(list=ls())
#librerías necesarias
library(readr)
library(dplyr)
library(stargazer)
library(fixest)
library(plm)

#set directorio
setwd("/Users/CARLOS/OneDrive/Escritorio/R/TP2")

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
stargazer(modelo_fe, type = "latex", out = "modelofe.tex") #guarda en .txt para latex


hausman_test <- phtest(modelo_fe,modelo)
# H0: FE es consistente, MCO es consistente y eficiente
# HA: FE es consistente, MCO es inconsistente

hausman_table <- data.frame(
  Estadístico = round(hausman_test$statistic, 3),
  Grados_libertad = hausman_test$parameter,
  Valor_p = round(hausman_test$p.value, 4),
  row.names = "Test de Hausman"
)


stargazer(hausman_table,
          summary = FALSE,
          type = "latex",
          title = "Test de Hausman",
          label = "tab:hausman",
          out = "hausman_test.tex")  

#-----------------------------------d------------------------------------------
#restar medias, sacamos el promedio histórico x estado.
#con log
vars <- list("lcrmrte", "lprbarr", "lprbconv", "lprbpris", "lavgsen", "lpolpc", 
          "ldensity", "ltaxpc", "lpctmin", "lwcon", "lwtuc", "lwtrd", "lwfir", 
          "lwser", "lwmfg", "lwfed", "lwsta", "lwloc", "lmix", "lpctymle", "west", "central", "urban" )

for (x in vars) {
  df <- df %>%
    group_by(county) %>%
    mutate(
      !!paste0(x, "_media") := mean(!!sym(x), na.rm = TRUE),           # Columna con las medias
      !!paste0(x, "_n") := !!sym(x) - mean(!!sym(x), na.rm = TRUE)      # Columna con valores sin las medias
    ) %>%
    ungroup()
}
#regreso y* en x*, variables sin la media
modelofwl <- lm(lcrmrte_n ~ lprbarr_n + lprbconv_n + lprbpris_n + lavgsen_n + lpolpc_n + ldensity_n + ltaxpc_n + lpctmin_n + lwcon_n + lwtuc_n + lwtrd_n + lwfir_n + lwser_n + lwmfg_n + lwfed_n + lwsta_n + lwloc_n + lmix_n + lpctymle_n + urban_n + central_n + west_n - 1, data = df)
summary(modelofwl)
stargazer(modelofwl, type = "latex", out = "modelofwl.tex") #guarda en .txt para latex

#-----------------------------------e------------------------------------------
#regreso y en x*, variables sin la media en y sin restarle la media.
modelofwl2 <- lm(lcrmrte ~ lprbarr_n + lprbconv_n + lprbpris_n + lavgsen_n + lpolpc_n + ldensity_n + ltaxpc_n + lpctmin_n + lwcon_n + lwtuc_n + lwtrd_n + lwfir_n + lwser_n + lwmfg_n + lwfed_n + lwsta_n + lwloc_n + lmix_n + lpctymle_n + urban_n + central_n + west_n -1, data = df)

summary(modelofwl2)
stargazer(modelofwl2, type = "latex", out = "modelofwl2.tex") #guarda en .txt para latex



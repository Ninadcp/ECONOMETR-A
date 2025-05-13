#install.packages('haven')
#install.packages('summarytools')
#install.packages("ggplot2") 
#install.packages("AER")
#install.packages("car")
#install.packages("xtable")
#install.packages("haven")
#install.packages("mfx")
#install.packages("margins")
#limpio entorno
rm(list=ls()) 
#librerías necesarias


library(AER)
library(car)
library(xtable)
library(haven)

library(mfx) 
library(margins) 

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
library(dplyr)


setwd("C:/Users/CARLOS/OneDrive/Documentos/GitHub/ECONOMETR-A/tp4")



#importo la base d datos
df <- read_dta("cuarto_trim_2019.dta")
head(df)


# Vemos que tipo de variables hay
str(df)



#--------------------------------------Punto 1----------------------------------

# Proporción de deserción
table(df$deserta) / nrow(df)


#comparo los dos grupos veo solo log del ipcf y edad

df %>%
  group_by(deserta) %>%
  summarise(
    edad = mean(edad, na.rm = TRUE),
    miembros = mean(miembros, na.rm = TRUE)
  )





#variables relevantes
#ingreso_per_capita, edad, mujer, jmujer, educjefe, mimebros, ch11 carac del estab educativo, estado de act

# Transformamos la variable que indica si la escuela es pública o privada en una dummy

df$privado <- ifelse(df$ch11 == 2, 1,
                     ifelse(df$ch11 == 1, 0, NA))

#Sacamos las variables que no son relevantes
df <- df %>% dplyr::select(deserta, edad, mujer, jmujer, educ_jefe, miembros, ch11, estado,ingreso_per_capita)

str(df)
df <- df %>%
  filter(estado != 0, estado != 4)
  


#Borramos valores faltantes de las variables y agregamos labels
df = na.omit(df)
summary(df)

df$estado <- factor(df$estado)


df$educ_jefe <- factor(df$educ_jefe)





#--------------------------------------Punto 2----------------------------------

# Estimación del modelo probit 
modelo_probit <- glm(deserta ~ ingreso_per_capita + edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado,
                     family = binomial(link = "probit"), data = df)

# resumen
summary(modelo_probit)


# código latex

stargazer(modelo_probit, 
          type = "latex",
          title = "Modelo Probit para la probabilidad de deserción escolar",
          dep.var.labels = "Deserta el secundario",
          omit.stat = c("ll", "aic"),
          no.space = TRUE,
          digits = 3)

#--------------------------------------Punto 3----------------------------------
#Efectos marginales en las medias
marginal_media <- probitmfx(deserta ~ ingreso_per_capita + edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado,
                            data = df, atmean = TRUE, robust = TRUE)

marginal_media

#latex efectos marginales en las medias
library(stargazer)
stargazer(marginal_media$mfxest, type = "latex",
          title = "Efectos marginales en la media")

# Tomi hace esto: Average Marginal Effect: evalÃºa en todos los puntos y saca el promedio

#pero antes saco el nivel educativo del jefe =  jardín por que no hay obs ahí y me tira error
df <- droplevels(df)


marginal_promedio <- margins(modelo_probit)
summary(marginal_promedio)
#latex efectos promedio
library(stargazer)

stargazer(as.data.frame(summary(marginal_promedio)),
          type = "latex",
          title = "Efectos marginales promedio",
          digits = 4,
          summary = FALSE)


#--------------------------------------Punto 4----------------------------------
#modelo lineal de probabilidad - mco a la deserción

modelo_lineal <- lm(deserta ~ ingreso_per_capita + edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado, data = df)
summary(modelo_lineal)


#latex

stargazer(modelo_lineal,
          type = "latex",
          title = "Modelo Lineal de Probabilidad para deserción escolar",
          dep.var.labels = "Deserta el secundario",
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 3)


#--------------------------------------Punto 5----------------------------------

# creamos la variable, pero antes filtramos a los ingresos per capita familiar mayores a cero

df$ingreso_per_capita[df$ingreso_per_capita < 1] <- 1

df$ln_ing <- log(df$ingreso_per_capita)

#--------------------------------------Punto 6----------------------------------

#Reestimamos el modelo
probit_ipcf <- glm(deserta ~ ln_ing+edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado,
                          family = binomial(link = "probit"), data = df)

summary(probit_ipcf)

#latex

library(stargazer)

stargazer(probit_ipcf,
          type = "latex",
          title = "Modelo Probit con ln(ingreso per cápita)",
          dep.var.labels = "Deserta el secundario",
          omit.stat = c("ll", "aic"),
          digits = 3,
          no.space = TRUE)

#grafico?
newvalueshjh <- with(df, data.frame(edad=median(edad) , mujer=0 , jmujer=0 , educ_jefe="4"
                                 , miembros=median(miembros) , ch11=median(ch11) , estado="2", ln_ing=ln_ing))
newvalueshjm <- with(df, data.frame(edad=median(edad) , mujer=0 , jmujer=1 , educ_jefe="4"
                                  , miembros=median(miembros) , ch11=median(ch11) , estado="2", ln_ing=ln_ing))
newvaluesmjh <- with(df, data.frame(edad=median(edad) , mujer=1 , jmujer=0 , educ_jefe="4"
                                  , miembros=median(miembros) , ch11=median(ch11) , estado="2", ln_ing=ln_ing))
newvaluesmjm <- with(df, data.frame(edad=median(edad) , mujer=1 , jmujer=1 , educ_jefe="4"
                                  , miembros=median(miembros) , ch11=median(ch11) , estado="2", ln_ing=ln_ing))
df[, "predictionhjh"] <-predict(probit_ipcf, newvalueshjh, type = "response")
df[, "predictionhjm"] <-predict(probit_ipcf, newvalueshjm, type = "response")
df[, "predictionmjh"] <-predict(probit_ipcf, newvaluesmjh, type = "response")
df[, "predictionmjm"] <-predict(probit_ipcf, newvaluesmjm, type = "response")

ggplot(df) +
  geom_line( aes(x = ln_ing, y = predictionhjh,), size = 1.5)  +
  theme_bw() +  ylim(0.015, 0.035) +
  labs(x = "Logaritmo del ingreso per cápita", y = "Probabilidad predicha")

ggplot(df) +
  geom_line( aes(x = ln_ing, y = predictionhjm,), size = 1.5)  +
  theme_bw() +  ylim(0.015, 0.035) +
  labs(x = "Logaritmo del ingreso per cápita", y = "Probabilidad predicha")

ggplot(df) +
  geom_line( aes(x = ln_ing, y = predictionmjh,), size = 1.5)  +
  theme_bw() +  ylim(0.015, 0.035) +
  labs(x = "Logaritmo del ingreso per cápita", y = "Probabilidad predicha")

ggplot(df) +
  geom_line( aes(x = ln_ing, y = predictionmjm,), size = 1.5)  +
  theme_bw() +  ylim(0.015, 0.035) +
  labs(x = "Logaritmo del ingreso per cápita", y = "Probabilidad predicha")

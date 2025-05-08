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
#edad, mujer, jmujer, educjefe, mimebros, ch11 carac del estab educativo, estado de act

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
modelo_probit <- glm(deserta ~ edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado,
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
marginal_media <- probitmfx(deserta ~ edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado,
                            data = df, atmean = TRUE, robust = TRUE)

marginal_media

#latex
library(stargazer)
stargazer(marginal_media$mfxest, type = "latex",
          title = "Efectos marginales en la media")

#No entiendo esto que pide el tp: comparen con efectos marginales calculados en otros puntos que considere relevantes

#Tomi hace esto: Average Marginal Effect: evalÃºa en todos los puntos y saca el promedio
summary(margins(modelo_probit)) # ver "at"
?margins


#--------------------------------------Punto 4----------------------------------
#modelo lineal de probabilidad - mco a la deserción

modelo_lineal <- lm(deserta ~ edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado, data = df)
summary(modelo_lineal)


#latex

stargazer(modelo_lineal,
          type = "latex",
          title = "Modelo Lineal de Probabilidad para deserción escolar",
          dep.var.labels = "Deserta el secundario",
          covariate.labels = c("Edad", "Mujer", "Jefe mujer", "Educ. jefe", 
                               "Miembros hogar", "Colegio privado", "Condición de actividad"),
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 3)


#--------------------------------------Punto 5----------------------------------

# creamos la variable, pero antes filtramos a los ingresos per capita familiar mayores a cero

df$ingreso_per_capita[df$ingreso_per_capita < 1] <- 1

df$ln_ing <- log(df$ingreso_per_capita)

#--------------------------------------Punto 6----------------------------------

#Reestimamos el modelo
probit_ipcf <- glm(deserta ~ ln_ing + edad + mujer + jmujer + educ_jefe + miembros + ch11 + estado,
                          family = binomial(link = "probit"), data = df)

summary(probit_ipcf)

#latex

library(stargazer)

stargazer(probit_ipcf,
          type = "latex",
          title = "Modelo Probit con ln(ingreso per cápita)",
          dep.var.labels = "Deserta el secundario",
          covariate.labels = c("Log ingreso per cápita", "Edad", "Mujer", "Jefe mujer", 
                               "Educ. jefe", "Miembros hogar", "Colegio privado", 
                               "Condición de actividad"),
          omit.stat = c("ll", "aic"),
          digits = 3,
          no.space = TRUE)

#grafico?


#CHAT me tiró esto: que dicen? 

# Paso 1: Crear secuencia de ln_ing
ln_ing_seq <- seq(min(df$ln_ing, na.rm = TRUE),
                  max(df$ln_ing, na.rm = TRUE),
                  length.out = 100)

# Paso 2: Usamos una fila real del df para asegurar tipos y niveles correctos
tipo_base <- df[1, ]
tipo_varon <- tipo_base[rep(1, 100), ]

# Paso 3: Fijamos valores del individuo tipo
tipo_varon$ln_ing <- ln_ing_seq
tipo_varon$edad <- mean(df$edad, na.rm = TRUE)
tipo_varon$mujer <- factor("Varón", levels = levels(df$mujer))
tipo_varon$jmujer <- factor("Hombre", levels = levels(df$jmujer))
tipo_varon$educ_jefe <- factor("Secundario", levels = levels(df$educ_jefe))
tipo_varon$miembros <- mean(df$miembros, na.rm = TRUE)
tipo_varon$ch11 <- factor("Público", levels = levels(df$ch11))
tipo_varon$estado <- factor("Ocupado", levels = levels(df$estado))

# Paso 4: Creamos tipo_mujer cambiando solo el sexo
tipo_mujer <- tipo_varon
tipo_mujer$mujer <- factor("Mujer", levels = levels(df$mujer))

# Paso 5: Predecimos probabilidades
tipo_varon$prob <- predict(probit_ipcf, newdata = tipo_varon, type = "response")
tipo_mujer$prob <- predict(probit_ipcf, newdata = tipo_mujer, type = "response")

tipo_varon$sexo <- "Varón"
tipo_mujer$sexo <- "Mujer"
df_plot <- rbind(tipo_varon, tipo_mujer)

# Paso 7: Gráfico
ggplot(df_plot, aes(x = ln_ing, y = prob, color = sexo)) +
  geom_line(size = 1.2) +
  labs(title = "Probabilidad de deserción según ln(ingreso per cápita) y sexo",
       x = "Logaritmo del ingreso per cápita",
       y = "Probabilidad estimada de deserción") +
  theme_minimal()


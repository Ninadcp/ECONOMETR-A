#install.packages('haven')
#install.packages('summarytools')
#install.packages("ggplot2") 
#limpio entorno
rm(list=ls()) 
#librerías necesarias
library(haven)
library(readr)
library(dplyr)
library(stargazer)
library(fixest)
library(plm)
library(summarytools)
library(rlang)
library(ggplot2)

setwd("/Users/ninadicostanzopereira/Desktop/ECONOMETR-A/tp3/")

#importo la base d datos
df <- read_dta('QOB.dta')

educ_promedio_qob <- df %>%
  group_by(qob) %>%
  summarise(
    promedio_educ = mean(educ, na.rm = TRUE),
    n = n()
  )

ggplot(educ_promedio_qob, aes(x = factor(qob), y = promedio_educ)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(promedio_educ, 2)), vjust = -0.5) +
  labs(
    title = "Promedio de Años de Escolaridad por Trimestre de Nacimiento",
    x = "Trimestre de Nacimiento (qob)",
    y = "Años de escolaridad (promedio)"
  ) +
  coord_cartesian(ylim = c(11.5, 13.5)) +  # ajustá el rango según tus datos
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )
#estad descriptiva
summary(select(df, educ, lwage, age, ageq, ageqsq, race, married, smsa, qob))

# Seleccionar las variables relevantes
vars <- select(df, educ, lwage, age, ageq, ageqsq, race, married, smsa, qob)

# Exportar tabla en formato LaTeX
stargazer(vars, type = "latex", title = "Estadísticos descriptivos",
          summary.stat = c("mean", "sd", "min", "max"),
          out = "tabla_descriptivos.tex")

#varianza_muestral <- var(select(df, educ))
#---------------------b-----------
# Filtrar hombres nacidos entre 1930 y 1939
data_filtrado <- df %>%
  filter(yob >= 30, yob <= 39)

Modelo <- lm(lwage ~ educ + ageq + ageqsq + race + married + smsa, data = data_filtrado)

# Ver resultados
summary(Modelo)

stargazer(Modelo, type = "latex",
          title = "Regresión por MCO: Retornos a la educación",
          dep.var.labels = "Logaritmo del salario mensual",
          covariate.labels = c("Años de educación", "Edad (trimestres)", "Edad al cuadrado",
                               "Raza (dummy)", "Casado (dummy)", "Ciudad central (dummy)"),
          digits = 3,
          out = "mco_educacion.tex")

#---------------------------------------d---------------------------------------
data_filtrado <- data_filtrado %>%
  mutate(Z1 = ifelse(qob == 1, 1, 0),
         Z2 = ifelse(qob == 2, 1, 0),
         Z3 = ifelse(qob == 3, 1, 0))

#---------------------------------------e---------------------------------------

# Regresamos los instrumentos en educación 
educ_iv_reg <- lm(educ ~ Z1 + Z2 + Z3, data = data_filtrado)
summary(educ_iv_reg)

# Test F 
anova(educ_iv_reg)

stargazer(educ_iv_reg,
          type = "latex",
          title = "Regresión de educación sobre los instrumentos (trimestres de nacimiento)",
          dep.var.labels = "Años de educación",
          covariate.labels = c("1er Trimestre (Z1)", "2do Trimestre (Z2)", "3er Trimestre (Z3)"),
          digits = 3,
          out = "educ_sobre_instrumentos.tex")

#---------------------f--------------------------------------------------------------------
# Filtrar hombres nacidos entre 1930 y 1939
data_filtrado <- df %>%
  filter(yob >= 30, yob <= 39)

Modelo <- lm(educ ~ Z1+ Z2 + Z3 + ageq + ageqsq + race + married + smsa, data = data_filtrado)

# Ver resultados
summary(Modelo)

stargazer(Modelo, type = "latex",
          title = "Regresión por MCO: Retornos a la educación",
          dep.var.labels = "Logaritmo del salario mensual",
          covariate.labels = c("Años de educación", "Edad (trimestres)", "Edad al cuadrado",
                               "Raza (dummy)", "Casado (dummy)", "Ciudad central (dummy)"),
          digits = 3,
          out = "mco_educacion.tex")

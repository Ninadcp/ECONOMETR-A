# -------------------------- ECONOMETRIA AVANZADA  ----------------------------#
# --------------------------    MAE UdeSA 2025     ----------------------------#
# --------------------------           TP 1        ----------------------------#
#limpio el entorno
rm(list = ls())

# Set directory
getwd() 
#setwd("/Users/ninadicostanzopereira/Desktop/CORE/ECON/tp1") #mi computadora
setwd("/Users/ninadicostanzopereira/Desktop/ECONOMETR-A") #GIT
dir()
#install.packages("xtable")
#install.packages("openxlsx")
#install.packages("ggplot2")  
library(openxlsx)
library(xtable)
library(ggplot2)


#EJERCICIO 1

num_datasets <- 50
df_list <- vector("list", num_datasets)

for (i in 1:50) {
  set.seed(i)
  
  n <- 100
  #valores verdaderos
  b_0 <- 100
  b_1 <- 3
  b_2 <- 2
  b_3 <- -1
  
  
  x1 <- runif(n, min = 0, max = 200)
  x2 <- runif(n, min = 0, max = 200)
  x3 <- runif(n, min = 0, max = 200)
  
  u <- rnorm(n, mean = 0, sd = sqrt(1100))
  
  y <- b_0 + b_1 * x1 + b_2 * x2 + b_3 * x3 + u
  
  #creamos los df
  df_list[[i]] <- data.frame(y, x1, x2, x3, u)   
}  

head(df_list[[1]])

##correlación
cor_list <- vector("list", num_datasets)
for (i in 1:50)
{
  cor(df_list[[i]])
  cor_list[[i]] <- cor(df_list[[i]][, c("x1", "x2", "x3")])
}
#nos quedamos con la 22 y exportamos a .csv y latex:
corr_matrix <- cor(cor_list[[22]])
write.csv(corr_matrix, "correlaciontp1.csv", row.names = TRUE)

cor_table <- xtable(corr_matrix, caption = "Matriz de correlación entre x1, x2 y x3")

# Guardar en un archivo .tex
sink("correlaciontp1.tex")
print(cor_table, type = "latex", include.rownames = TRUE, include.colnames= TRUE)
sink()

# Estime β0, β1, β2 y β3 para las 50 muestras y guarde los datos en un Excel llamado
#primera_estimacion.xlsx. Tiene que quedarle una planilla con cuatro columnas y
#50 estimaciones distintas.

coef_mat <- matrix(nrow=50, ncol=4)
colnames(coef_mat) <- c("Beta_0", "Beta_1", "Beta_2", "Beta_3")  

for (i in 1:50)
{
  modelo <- lm(y ~ x1 + x2 + x3, data = df_list[[i]]) 
  coef_mat[i, ] <- coef(modelo)
  
}

df_coef <- as.data.frame(coef_mat)
# Guardar en un excel
write.xlsx(df_coef, "primera_estimacion.xlsx", overwrite = TRUE)

#______________________________4______________________________________
#cargamos el archivo
coef_df <- read.xlsx("primera_estimacion.xlsx")
head(coef_df)
#primer gráfico beta_1 beta_2

ggplot(df_coef, aes(x = Beta_1, y = Beta_2)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Estimaciones: β1 vs β2", x = "β1", y = "β2") + #NINACAMBIÁ ESTO
  theme_minimal()
ggsave("plot4_1.png")

#segundo gráfico beta_1 beta_3

ggplot(df_coef, aes(x = Beta_1, y = Beta_3)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Estimaciones: β1 vs β3", x = "β1", y = "β3") + #NINACAMBIÁ ESTO
  theme_minimal()
ggsave("plot4_2.png")

#_______________________-5_________________________________

df_list5 <- vector("list", num_datasets)

for (i in 1:50) {
  set.seed(i)
  
  n <- 100
  
  
  u <- rnorm(n, mean = 0, sd = sqrt(1100))
  
  y <- b_0 + b_1 * x1 + b_2 * x2 + b_3 * x3 + u
  
  #creamos los df
  df_list5[[i]] <- data.frame(y, x1, x2, x3, u)   
}  

head(df_list[[1]])




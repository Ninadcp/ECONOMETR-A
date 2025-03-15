# -------------------------- ECONOMETRIA AVANZADA  ----------------------------#
# --------------------------    MAE UdeSA 2025     ----------------------------#
# --------------------------           TP 1        ----------------------------#
#limpio el entorno
rm(list = ls())

# Set directory
getwd() 
#setwd("/Users/ninadicostanzopereira/Desktop/CORE/ECON/tp1") #mi computadora
#setwd("/Users/ninadicostanzopereira/Desktop/ECONOMETR-A") #GIT 
setwd("C:/Users/sebib/Documents/GitHub/ECONOMETR-A")



dir()
#install.packages("xtable")
#install.packages("openxlsx")
#install.packages("ggplot2")  
library(openxlsx)
library(xtable)
library(ggplot2)




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

#-----------------------------2------------------------correlación
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

# -----------3-Estime β0, β1, β2 y β3 para las 50 muestras y guarde los datos en un Excel llamado
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

#_______________________5_________________________________

df_list2 <- vector("list", num_datasets)



for (i in 1:50) {
  set.seed(i)
  
  n <- 100
  x1 <- runif(n, min = 0, max = 200)
  x2 <- scale(matrix( rnorm(100), ncol=1 ))
  xs <- cbind(scale(x1),x2)
  c1 <- var(xs)
  chol1 <- solve(chol(c1))
  newx <- xs
  newc <- matrix(
    c(1 , 0.987,
      0.987, 1 ), ncol=2)
  eigen(newc)
  chol2 <- chol(newc)
  xm2 <- newx%*% chol2 * sd(x1) + mean(x1)
  x2 <- xm2[, 2]
  
  x3 <- runif(n, min = 0, max = 200)
  
  
  u <- rnorm(n, mean = 0, sd = sqrt(1100))
  
  y <- b_0 + b_1 * x1 + b_2 * x2 + b_3 * x3 + u
  
  #creamos los df
  df_list2[[i]] <- data.frame(y, x1, x2, x3, u)   
}  

head(df_list2[[1]])

coef_mat <- matrix(nrow=50, ncol=4)
colnames(coef_mat) <- c("Beta_0", "Beta_1", "Beta_2", "Beta_3")  

for (i in 1:50)
{
  modelo <- lm(y ~ x1 + x2 + x3, data = df_list2[[i]]) 
  coef_mat[i, ] <- coef(modelo)
  
}

df_coef2 <- as.data.frame(coef_mat)
# Guardar en un excel
write.xlsx(df_coef2, "segunda_estimacion.xlsx", overwrite = TRUE)

##---6

#cargamos el archivo
coef_df2 <- read.xlsx("segunda_estimacion.xlsx")
head(coef_df2)
#primer gráfico beta_1 beta_2

ggplot(coef_df2, aes(x = Beta_1, y = Beta_2)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Estimaciones: β1 vs β2", x = "β1", y = "β2") + #NINACAMBIÁ ESTO
  theme_minimal()
ggsave("plot6_1.png")

#segundo gráfico beta_1 beta_3

ggplot(coef_df2, aes(x = Beta_1, y = Beta_3)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Estimaciones: β1 vs β3", x = "β1", y = "β3") + #NINACAMBIÁ ESTO
  theme_minimal()
ggsave("plot6_2.png")


#Ejercicio 2--------------------------------


num_datasets <- 50

df_list3 <- vector("list", num_datasets)


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
  df_list3[[i]] <- data.frame(y, x1, x2, x3, u)   
}  

head(df_list3[[1]])

coef_mat3 <- matrix(nrow=50, ncol=3)
colnames(coef_mat3) <- c("Beta_0", "Beta_1","Beta_3")  

for (i in 1:50)
{
  modelo <- lm(y ~ x1 + x3, data = df_list[[i]]) 
  coef_mat3[i, ] <- coef(modelo)
  
}

df_coef3 <- as.data.frame(coef_mat3)
# Guardar en un excel
write.xlsx(df_coef3, "tercera_estimacion.xlsx", overwrite = TRUE)

#______________________________Gráfico______________________________________
#cargamos el archivo
coef_df3 <- read.xlsx("tercera_estimacion.xlsx")
head(coef_df3)
#primer gráfico beta_1 beta_3

ggplot(coef_df3, aes(x = Beta_1, y = Beta_3)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Estimaciones: β1 vs β3", x = "β1", y = "β3") + #NINACAMBIÁ ESTO
  theme_minimal()
ggsave("plot21.png")


#----Ejercicio 2 b) 


df_list4 <- vector("list", num_datasets)


for (i in 1:50) {
  set.seed(i)
  
  n <- 100
  x1 <- runif(n, min = 0, max = 200)
  x2 <- scale(matrix( rnorm(100), ncol=1 ))
  xs <- cbind(scale(x1),x2)
  c1 <- var(xs)
  chol1 <- solve(chol(c1))
  newx <- xs
  newc <- matrix(
    c(1 , 0.987,
      0.987, 1 ), ncol=2)
  eigen(newc)
  chol2 <- chol(newc)
  xm2 <- newx%*% chol2 * sd(x1) + mean(x1)
  x2 <- xm2[, 2]
  
  x3 <- runif(n, min = 0, max = 200)
  
  
  u <- rnorm(n, mean = 0, sd = sqrt(1100))
  
  y <- b_0 + b_1 * x1 + b_2 * x2 + b_3 * x3 + u
  
  #creamos los df
  df_list4[[i]] <- data.frame(y, x1, x2, x3, u)   
}  

head(df_list4[[1]])

coef_mat4 <- matrix(nrow=50, ncol=3)
colnames(coef_mat4) <- c("Beta_0", "Beta_1", "Beta_3")  

for (i in 1:50)
{
  modelo <- lm(y ~ x1 + x3, data = df_list4[[i]]) 
  coef_mat4[i, ] <- coef(modelo)
  
}

df_coef4 <- as.data.frame(coef_mat4)
# Guardar en un excel
write.xlsx(df_coef4, "cuarta_estimacion.xlsx", overwrite = TRUE)

##---6

#cargamos el archivo
coef_df4 <- read.xlsx("cuarta_estimacion.xlsx")
head(coef_df4)

#segundo gráfico beta_1 beta_3

ggplot(coef_df4, aes(x = Beta_1, y = Beta_3)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Estimaciones: β1 vs β3", x = "β1", y = "β3") + #NINACAMBIÁ ESTO
  theme_minimal()
ggsave("plot2ult.png")


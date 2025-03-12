# -------------------------- ECONOMETRIA AVANZADA  ----------------------------#
# --------------------------    MAE UdeSA 2025     ----------------------------#
# --------------------------           TP 1        ----------------------------#
#limpio el entorno
rm(list = ls())

# Set directory
getwd() 
setwd("/Users/ninadicostanzopereira/Desktop/CORE/ECON/tp1")
dir()


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

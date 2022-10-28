if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")

library(dplyr) 

library(readxl) 

dados <- read_excel("C:\\Users\\rafal\\Downloads\\Dados_Transportadora.xls")

x <- dados %>% filter(Peso < 800)

hist(dados$Peso, main = "Dados_Transportadora", col="red", xlab = "Tamanho", ylab = "Peso")

hist(x$Peso, main=NULL, col="blue", add=T)


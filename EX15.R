if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
library(dplyr) 
library(ggplot2)
library(readxl) 

dados <- read_excel("C:\\Users\\rafal\\Downloads\\Dados_Transportadora.xls")

bom <- dados %>% filter(Opinião == "Bom")
excelente <- dados %>% filter(Opinião == "Excelente")




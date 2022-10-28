if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")

library(dplyr) 

library(readxl) 

dados <- read_excel("C:\\Users\\rafal\\Downloads\\Dados_Transportadora.xls")

rodov <- dados %>% filter(Modalidade == "Rodoviário")
ferrov <- dados %>% filter(Modalidade == "Ferroviário")
aereo <- dados %>% filter(Modalidade == "Aéreo")
mf <- dados %>% filter(Modalidade == "Marítimo/fluvial")
multi <- dados %>% filter(Modalidade == "Multimodal")

hist(dados$Peso, main = "Dados_Transportadora", col="red", xlab = "Quantidade", ylab = "Modalidade")

hist(x$Peso, main=NULL, col="blue", add=T)


## uSO ESTES VALORES COMO RELAÇÃO DE PORCENTAGEM AO TOTAL
porcent <- nrow(x)/nrow(dados) *100
pferro <- nrow(ferrov)/nrow(dados) *100
paereo <- nrow(aereo)/nrow(dados) *100
pmf <- nrow(mf)/nrow(dados) *100
pmulti <- nrow(multi)/nrow(dados) *100


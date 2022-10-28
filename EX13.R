if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")

library(readxl) 

dados <- read_excel("C:\\Users\\rafal\\Downloads\\Dados_Transportadora.xls")

x <- dados %>% filter(Tempo <= 80)

hist(dados$Tempo, main = "Dados_Transportadora", col="red", xlab = "Tamanho", ylab = "Peso")

hist(x$Tempo, main=NULL, col="blue", add=T)


sudestetotal <- dados %>% filter(Região == "Sudeste")
sudeste <- x %>% filter(Região == "Sudeste")

nortetotal <- dados %>% filter(Região == "Norte")
norte <- x %>% filter(Região == "Norte")

nodtotal<- dados %>% filter(Região == "Nordeste")
nod <- x %>% filter(Região == "Nordeste")

sultotal <- dados %>% filter(Região == "Sul")
sul <- x %>% filter(Região == "Sul")



cototal <- dados %>% filter(Região == "Centro-Oeste")
co <- x %>% filter(Região == "Centro-Oeste")

##aqui farei as proporções para determinar onde ta bom e onde ta ruim
propnorte <- nrow(norte)/nrow(nortetotal) * 100
propnod <- nrow(nod)/nrow(nodtotal) * 100
propco <- nrow(co)/nrow(cototal) * 100
propsul <- nrow(sul)/nrow(sultotal) * 100
propsud <- nrow(sudeste)/nrow(sudestetotal) * 100


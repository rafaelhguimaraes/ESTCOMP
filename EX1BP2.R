# 1)B - Funcao para teste de hipotese

teste_de_hipotese <- function(dados1, dados2, alfa=0.90, tipo_teste="b") {
  if (alfa <= 0) {
    return("Alfa nao pode ser <= 0")
  }
  if (!is.numeric(dados1) || !is.numeric(dados2)) {
    return("Alfa tem que ser numero")
  }
  if (is.double(tipo_teste)) {
    return("Tipo de teste tem   que ser numero decimal")
  }
  if (tipo_teste == "b" || tipo_teste == "B") {
    media_d1 <- mean(dados1) # media aritmetica 1 
    media_d2 <- mean(dados2) # media aritmetica 2 
    
    tamanho_dados1 <- length(dados1) # tamanho_dados 
    tamanho_dados2 <- length(dados2) # tamanho_dados
    
    desvio_padrao_d1 <- sd(dados1) # sd = standart desviation = desvio padrao
    desvio_padrao_d2 <- sd(dados2) # sd = standart desviation = desvio padrao
    
    m1menosm2 <- media_d1 - media_d2 # calcula-se a diferença das medias
    desvio_padrao <- desvio_padrao_d1/tamanho_dados1 + desvio_padrao_d2/tamanho_dados2
    
    alfa <- (1-alfa)/2 #alfa passa a valer menos
    
    zcalculado <- m1menosm2/sqrt(desvio_padrao)
    
    area_a_esquerda <- pnorm(zcalculado, m1menosm2, desvio_padrao)
    area_a_direita <- 1-pnorm(zcalculado, m1menosm2, desvio_padrao)
    
    area_central <- area_a_esquerda - area_a_direita
    
    return(area_central)
  }
  if (tipo_teste == "d" || tipo_teste == "D") {
    return("Sucesso")
  }
  if (tipo_teste == "c" || tipo_teste == "C") {
    return("Sucesso")
  }
  else {
    return("Erro")
  }
}

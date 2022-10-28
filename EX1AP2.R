# 1)A - Funcao para intervalo de confianca
intervaloconfianca <- function(dados1, dados2, alfa=0.96) {
  shapiro.test(dados1)
  shapiro.test(dados2)
  qqnorm()
  if (alfa <= 0) {
    return("Alfa nao pode ser <= 0")
  }
  if (!is.numeric(dados1) || !is.numeric(dados2)) {
    return("Alfa tem de ser numero")
  }
  else {
    media_arit_dados1 <- mean(dados1) # media aritmetica 1 
    media_arit_dados2 <- mean(dados2) # media aritmetica 2
    
    d1_sigma2 <- sd(dados1) # sd = standart desviation = desvio padrao
    d2_sigma2 <- sd(dados2) # sd = standart desviation = desvio padrao
    sigma_z <- d1_sigma2/length(dados1)+d2_sigma2/length(dados2)
    
    alfaZpor2 <- pnorm(1-alfa/2, mean=0, sd=1)
    
    limite_inf <- (media_arit_dados1 - media_arit_dados2)-alfaZpor2*sqrt(sigma_z)
    limite_sup <- (media_arit_dados1 - media_arit_dados2)+alfaZpor2*sqrt(sigma_z)
    
    return(c(limite_inf, limite_sup))
  }
}

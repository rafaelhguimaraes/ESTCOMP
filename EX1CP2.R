# C) Intervalo de Confiança com T desconhecido
intervalo_de_confianca <- function(dados1, dados2, alpha=0.02) {
  if (alpha <= 0) {return("Alfa não pode ser <= a zero")}
  if (!is.numeric(dados1) || !is.numeric(dados2)) { return("Erro nos dados. Tem que ser numeros") }
  else {
    
    media1 <- mean(dados1)# media aritmetica 1 
    media2 <- mean(dados2)# media aritmetica 2
    
    var1 <- round(var(dados1),2) # calculo da variancia 1 
    var2 <- round(var(dados2),2) # calculo da variancia 2
    
    Saoquadr <- round(abs((var1/length(dados1) - 1) + (var2/length(dados2) - 1)),2)
    S <- round((sqrt(Saoquadr)),2) # Calculo do desvio padrão amostral
    
    lib <- (length(dados1) + length(dados2) - 2)
    
    #diferenca de medias
    m1menosm2 <- media1 - media2
    
    #teste t
    t <- abs(round(((media1 - media2)) / sqrt(S + (length(dados1) + length(dados2))),2))
    
    #limite_inf <- m1menosm2 + (Z_alfa_dividido_por_2*S)
    LI <- round(((media1 - t * S/sqrt(length(dados1))) + (media2 - t * S/sqrt(length(dados2)))/2),2)
    
    #limite_sup <- m1menosm2 - (Z_alfa_dividido_por_2*S)
    LS <- round(((media1 + t * S/sqrt(length(dados1))) + (media2 + t * S/sqrt(length(dados2)))/2),2)
    
    #return(c(X1_sigma2,X2_sigma2, Sigma_Z2, sqrt(Sigma_Z2), Z_alfa_dividido_por_2, diferenca_das_medias))
    return(cat("[", round(LI, 5), ",", round(LS, 5), "]"))
  }
}


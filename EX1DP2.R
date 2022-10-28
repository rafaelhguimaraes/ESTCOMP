# D) Calcular um teste de hipotese com T desconhecido, podendo
# alterar seu tipo sendo eles:
# "b" = bilateral "d" = UNILATERAL a Direita "e" = Unilateral a esquerda
# Teste bilateral, por isso colocar tipo "b"
teste_de_hipotese <- function(dados1, dados2, alpha=0.90, tipo_teste="b") {
  if (alpha <= 0) {
    return("Alfa não pode ser <= a zero")
  }
  if (!is.numeric(dados1) || !is.numeric(dados2)) {
    return("Erro nos dados. Tem que ser numeros")
  }
  if (is.double(tipo_teste)) {
    return("ERRO - Tipo tem que ser b ou D")
  }
  if (tipo_teste == "b") {
    
    media1 <- mean(dados1) # media aritmetica 1 
    media2 <- mean(dados2) # media aritmetica 1 
    
    var_1 <- round(var(dados1),2) # calculo da variancia 1 
    var_2 <- round(var(dados2),2) # calculo da variancia 2
    
    Saoquadr <- round(abs((var_1/length(dados1) - 1) + (var_2/length(dados2) - 1)),2)
    S <- round(sqrt(Saoquadr),2)  # Calculo do desvio padrão amostral
    lib <- (length(dados1) + length(dados2) - 2) 
    # teste t
    t <- abs(round(((media1 - media2)) / sqrt(S + (length(dados1) + length(dados2))),2))
    
    # tipo = "b" alfa = alfa/2
    alfasobre2 <- 1 - alpha
    alfasobre2 <- alfasobre2/2
    
    
    mi1 <- sum(dados1)/length(dados1)   #média populacional 1 
    mi2 <- sum(dados2)/length(dados2)   #média populacional 2
    
    # H0: mi1 - mi2 = 0
    # H1: mi1 - mi2 !=(diferente) 0
    
    #t crítico
    t_critico <- round((qt(p = alfasobre2, df = lib)),2)
    t_critico <- abs(as.double(t_critico[1]))
    
    if (t <= -t_critico || t >= t_critico) {
      return("Rejeito H0 e Aceito H1")
    } else {
      return("Aceito H0 e Rejeio H1")
    }
  }
  # Tipo "d" = UNILATERAL a Direita
  if (tipo_teste == "d") {
    
    media1 <- mean(dados1) # media aritmetica 1 
    media2 <- mean(dados2) # media aritmetica 2
    
    var_1 <- var(dados1) # calculo da variancia 1 
    var_2 <- var(dados2) # calculo da variancia 2
    
    Saoquadr <- abs((var_1/length(dados1) - 1) + (var_2/length(dados2) - 1))
    S <- sqrt(Saoquadr)
    
    lib <- dados1 + dados2 - 2
    
    #teste t
    t <- media1 - media2 / sqrt(S + length(dados1 + dados2))
    
    # alpha em unilateral a direita = alpha
    novo_alfa <- 1-alpha
    
    mi1 <- sum(dados1)/length(dados1) # media populacional 1
    mi2 <- sum(dados2)/length(dados2) # media populacional 2
    
    
    # H0: mi1 - mi2 > 0
    # H1: mi1 - mi2 < 0
    
    #t crítico
    t_critico <- formatC(qt(p = novo_alfa, df = lib, lower.tail = FALSE))
    t_critico <- abs(as.double(t_critico[1]))
    
    if (t >= t_critico) {
      return("Rejeito H0 e Aceito H1")
    } else {
      return("Aceito H0 e Rejeio H1")
    }
  }
  # Tipo = "e" = Unilateral a esquerda
  if (tipo_teste == "e") {
    
    media1 <- mean(dados1) # media aritmetica 1 
    media2 <- mean(dados2) # media aritmetica 1 
    
    var_1 <- round(var(dados1),2) # calculo da variancia 1 
    var_2 <- round(var(dados2),2) # calculo da variancia 2
    
    Saoquadr <- abs((var_1/length(dados1) - 1) + (var_2/length(dados2) - 1))
    S <- sqrt(Saoquadr) 
    lib <- dados1 + dados2 - 2
    
    #teste t
    t <- abs(round(((media1 - media2)) / sqrt(S + (length(dados1) + length(dados2))),2))
    
    # alpha em unilateral a esquerda = alpha
    novo_alfa <- 1-alfa
    
    mi1 <- sum(dados1)/length(dados1)   #média populacional 1 
    mi2 <- sum(dados2)/length(dados2)   #média populacional 2
    
    # H0: mi1 - mi2 < 0
    # H1: mi1 - mi2 > 0
    
    # Cálculo do t crítico
    t_critico <- formatC(qt(p = novo_alfa, df = lib, lower.tail = TRUE))
    t_critico <- abs(as.double(t_critico[1]))
    
    if (t <= -t_critico) {
      return("Rejeito H0 e Aceito H1")
    } else {
      return("Aceito H0 e Rejeio H1")
    }
  }
  else {
    return("Erro")
  }
}  

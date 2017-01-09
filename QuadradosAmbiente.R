source("Estado.R")

## Classe e métodos para o problema dos 3 Missionários e 3 Canibais
QuadradoAmbiente <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("QuadradoAmbiente", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.QuadradoAmbiente = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.QuadradoAmbiente <- function(obj) {
  cat("(Q1 Q2 Q3 Q4 A): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.QuadradoAmbiente <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = M + C + B
  return(sum(atual$desc))
}

geraFilhos.QuadradoAmbiente <- function(obj) {
  custo <- c(2,1,1,3,3)
  filhos <- list()
  
  filhosDesc <- list()
  
  desc <- obj$desc
  bq1 <- as.numeric(desc[1])
  bq2 <- as.numeric(desc[2])
  bq3 <- as.numeric(desc[3])
  bq4 <- as.numeric(desc[4])
  
  bAtual <- as.numeric(desc[5])
  
  ## gera filhos usando todos os operadores  
  if(bAtual == 1){
    
    filhosQuadrados<- list(c(0 ,bq2,bq3, bq4, bAtual), #limpar
                           c(bq1 ,bq2,bq3, bq4, 3), #direita
                           c(bq1 ,bq2,bq3, bq4, 2), #pra baixo
                           c(bq1 ,bq2,bq3, bq4, bAtual), #pra esquerda
                           c(bq1 ,bq2,bq3, bq4, bAtual)) #pra cima
    
  }
  if(bAtual == 2){
    
    filhosQuadrados<- list(c(bq1 , 0, bq3, bq4, bAtual), #limpar
                           c(bq1 ,bq2,bq3, bq4, 4), #direita
                           c(bq1 ,bq2,bq3, bq4, bAtual), #pra baixo
                           c(bq1 ,bq2,bq3, bq4, bAtual), #pra esquerda
                           c(bq1 ,bq2,bq3, bq4, 1)) #pra cima
    
  }
  if(bAtual == 3){
    
    filhosQuadrados<- list(c(bq1 ,bq2, 0, bq4, bAtual), #limpar
                           c(bq1 ,bq2,bq3, bq4, bAtual), #direita
                           c(bq1 ,bq2,bq3, bq4, 4), #pra baixo
                           c(bq1 ,bq2,bq3, bq4, 1), #pra esquerda
                           c(bq1 ,bq2,bq3, bq4, bAtual)) #pra cima
    
  }
  if(bAtual == 4){
    
    filhosQuadrados<- list(c(bq1 ,bq2,bq3, 0, bAtual), #limpar
                           c(bq1 ,bq2,bq3, bq4, bAtual), #direita
                           c(bq1 ,bq2,bq3, bq4, bAtual), #pra baixo
                           c(bq1 ,bq2,bq3, bq4, 2), #pra esquerda
                           c(bq1 ,bq2,bq3, bq4, 3)) #pra cima
    
  }
  
  
  ## gera os objetos Canibais para os filhos
  for(filhoDesc in filhosDesc){
    filho <- QuadradoAmbiente(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}
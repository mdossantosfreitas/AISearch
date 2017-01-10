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
  #  return(all(obj1$desc$Q1 == obj2$desc$Q1, obj1$desc$Q2 == obj2$desc$Q2, obj1$desc$Q3 == obj2$desc$Q3,
   #            obj1$desc$Q4 == obj2$desc$Q4))
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
  ## h(obj) = Q1 + Q2 + Q3 + Q4
  return(sum(atual$desc) - as.numeric(atual$desc[5]))
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
    
    filhosQuadrados<- list(c(Q1 = 0 ,Q2 = bq2,Q3 = bq3, Q4 = bq4, A = bAtual), #limpar
                           c(Q1 = bq1 ,Q2 = bq2,Q3 = bq3, Q4 = bq4, A = 3), #direita
                           c(Q1 = bq1 ,Q2 = bq2,Q3 = bq3, Q4 = bq4, A = 2), #pra baixo
                           c(Q1 = bq1 ,Q2 = bq2,Q3 = bq3, Q4 = bq4, A=bAtual), #pra esquerda
                           c(Q1 = bq1 , Q2 = bq2,Q3 = bq3, Q4 = bq4, A=bAtual)) #pra cima
    
  }
  if(bAtual == 2){
    
    filhosQuadrados<- list(c(Q1 = bq1 , Q2 = 0, Q3 = bq3, Q4 =bq4, A =bAtual), #limpar
                           c(Q1 = bq1 ,Q2 =bq2,Q3 = bq3, Q4 =bq4, A = 4), #direita
                           c(Q1 = bq1 ,Q2 =bq2,Q3 = bq3, Q4 =bq4, A = bAtual), #pra baixo
                           c(Q1 = bq1 ,Q2 =bq2,Q3 = bq3, Q4 = bq4, A = bAtual), #pra esquerda
                           c(Q1 = bq1 ,Q2 =bq2,Q3 = bq3, Q4 =bq4, A = 1)) #pra cima
    
  }
  if(bAtual == 3){
    
    filhosQuadrados<- list(c(Q1 =bq1 ,Q2 = bq2, Q3 =0, Q4=bq4, A=bAtual), #limpar
                           c(Q1 =bq1 ,Q2= bq2,Q3 =bq3, Q4=bq4, A=bAtual), #direita
                           c(Q1 =bq1 ,Q2 =bq2,Q3 =bq3, Q4 =bq4, A=4), #pra baixo
                           c(Q1 =bq1 ,Q2 =bq2,Q3=bq3, Q4 =bq4, A=1), #pra esquerda
                           c(Q1 =bq1 ,Q2 =bq2,Q3 =bq3, Q4 =bq4, A=bAtual)) #pra cima
    
  }
  if(bAtual == 4){
    
    filhosQuadrados<- list(c(Q1=bq1 ,Q2=bq2,Q3=bq3, Q4=0, A=bAtual), #limpar
                           c(Q1=bq1 ,Q2=bq2,Q3=bq3, Q4=bq4, A=bAtual), #direita
                           c(Q1=bq1 ,Q2=bq2,Q3=bq3, Q4=bq4, A=bAtual), #pra baixo
                           c(Q1=bq1 ,Q2=bq2,Q3=bq3, Q4=bq4, A=2), #pra esquerda
                           c(Q1=bq1 ,Q2=bq2,Q3=bq3, Q4=bq4, A=3)) #pra cima
    
  }
  
  filhosDesc <- filhosQuadrados
  ## gera os objetos Canibais para os filhos
  for(filhoDesc in filhosDesc){
    filho <- QuadradoAmbiente(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}
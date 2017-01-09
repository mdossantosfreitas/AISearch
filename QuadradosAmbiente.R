source("Estado.R")

## Classe e métodos para o problema do aspirador de pó
QuadradosAmbiente <- function(quadrado = NULL, quad_atual = NULL, pai = NULL){
  
  e <- environment()
  
  #0 se limpo 
  #1 se sujo
  assign("quadrado", quadrado, envir = e)
  #qual quadrado atual
  assign("quad_atual", quad_atual, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("QuadradosAmbiente", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.QuadradosAmbiente = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$quad_atual == obj2$quad_atual, obj1$quadrado == obj2$quadrado))
  }
}

## Sobrecarga da função genérica "print" do R
print.QuadradosAmbiente <- function(obj) {
  cat("(0.0, 0.1, 1.0, 1.1): (", obj$quadrado, ")\n")
  cat("Quadrado Atual:", obj$quad_atual , "\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.QuadradosAmbiente <- function(atual){
  
  if(is.null(atual$quadrado))
    return(Inf)
  ## h(obj) = quadrado_1 + quadrado_2 + quadrado_3 + quadrado_4
  return(sum(atual$quadrado))
}

geraFilhos.QuadradosAmbiente <- function(obj) {
  
  custo <- list(2,1,1,3,3)
  filhos <- list()
  
  filhosDesc <- list()
  
  quadrado <- obj$quadrado
  
  #pega os 4 quadrados
  bq1 <- as.numeric(quadrado[1])
  bq2 <- as.numeric(quadrado[2])
  bq3 <- as.numeric(quadrado[3])
  bq4 <- as.numeric(quadrado[4])
  
  bAtual <- obj$quad_atual
 
  
  ## gera filhos usando todos os operadores  
  if(bAtual == 1){
    
    filhosQuadrados<- list(c(c(0 ,bq2,bq3, bq4), bAtual), #limpar
                       c(c(bq1 ,bq2,bq3, bq4), 3), #direita
                       c(c(bq1 ,bq2,bq3, bq4), 2), #pra baixo
                       c(c(bq1 ,bq2,bq3, bq4), bAtual), #pra esquerda
                       c(c(bq1 ,bq2,bq3, bq4), bAtual)) #pra cima
    
  }
  if(bAtual == 2){
    
    filhosQuadrados<- list(d(c(bq1 , 0, bq3, bq4), bAtual), #limpar
                           d(c(bq1 ,bq2,bq3, bq4), 4), #direita
                           d(c(bq1 ,bq2,bq3, bq4), bAtual), #pra baixo
                           d(c(bq1 ,bq2,bq3, bq4), bAtual), #pra esquerda
                           d(c(bq1 ,bq2,bq3, bq4), 1)) #pra cima
    
  }
  if(bAtual == 3){
    
    filhosQuadrados<- list(d(c(bq1 ,bq2, 0, bq4), bAtual), #limpar
                           d(c(bq1 ,bq2,bq3, bq4), bAtual), #direita
                           d(c(bq1 ,bq2,bq3, bq4), 4), #pra baixo
                           d(c(bq1 ,bq2,bq3, bq4), 1), #pra esquerda
                           d(c(bq1 ,bq2,bq3, bq4), bAtual)) #pra cima
    
  }
  if(bAtual == 4){
    
    filhosQuadrados<- list(d(c(bq1 ,bq2,bq3, 0), bAtual), #limpar
                           d(c(bq1 ,bq2,bq3, bq4), bAtual), #direita
                           d(c(bq1 ,bq2,bq3, bq4), bAtual), #pra baixo
                           d(c(bq1 ,bq2,bq3, bq4), 2), #pra esquerda
                           d(c(bq1 ,bq2,bq3, bq4), 3)) #pra cima
    
  }
  
  ## verifica estados filhos incompatíveis com o problema  
#  incompativeis <- sapply(1:length(filhosQuadrados),
#                          function(i) {
#                            fDesc <- filhosQuadrados[[i]]
#                            if((fDesc['C'] > fDesc['M']) || ## Se #Canibais > #Missionários OU
#                               (any(fDesc[1:2] > 3)) ||     ##    #Canibais ou #Missionários > 3 OU
#                               (any(fDesc[1:2] < 0)))       ##    #Canibais ou #Missionarios < 0 então
#                              i ## é incompatível: retorna índice
#                            else
#                              0 ## senão é compatível
#                          })
  
  ## mantém no vetor apenas os que são incompatíveis
#  incompativeis <- incompativeis[incompativeis != 0]
  
  ## remove estados filhos incompatíveis
 # filhosDesc <- filhosDesc[-incompativeis]
  
  ## gera os objetos Canibais para os filhos
  i <- 0
  for(filhoDesc in filhosQuadrados){
    filho <- QuadradosAmbiente(quadrado = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + as.numeric(custo[i])
    filhos <- c(filhos, list(filho))
    i<- i + 1
  }
  
  return(filhos)
}
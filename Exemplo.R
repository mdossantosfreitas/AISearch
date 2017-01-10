debugSource("QuadradosAmbiente.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- QuadradoAmbiente(desc = c(Q1 = 1, Q2 = 1, Q3 = 1, Q4 = 1, A = 1))

objetivo1 <- QuadradoAmbiente()
objetivo1$desc <- c(Q1 = 0, Q2 = 0, Q3 = 0, Q4 = 0, A = 1)
objetivo2 <- QuadradoAmbiente()
objetivo2$desc <- c(Q1 = 0, Q2 = 0, Q3 = 0, Q4 = 0, A = 2)
objetivo3 <- QuadradoAmbiente()
objetivo3$desc <- c(Q1 = 0, Q2 = 0, Q3 = 0, Q4 = 0, A = 3)
objetivo4 <- QuadradoAmbiente()
objetivo4$desc <- c(Q1 = 0, Q2 = 0, Q3 = 0, Q4 = 0, A = 4)

Exemplo.buscaEmLarguraObjetivo <- function() {
  cat("====\tBusca em Largura\t====\n")
  cat("====\tObjetivo 1\t====\n")
  print(unlist(buscaEmLargura(inicial, objetivo1)))
  cat("====\tObjetivo 2\t====\n")
  print(unlist(buscaEmLargura(inicial, objetivo2)))
  cat("====\tObjetivo 3\t====\n")
  print(unlist(buscaEmLargura(inicial, objetivo3)))
  cat("====\tObjetivo 4\t====\n")
  print(unlist(buscaEmLargura(inicial, objetivo4)))

}

Exemplo.buscaEmProfundidadeObjetivo <- function() {
  cat("====\tBusca em Profundidade\t====\n")
  cat("====\tObjetivo 1\t====\n")
  print(buscaEmProfundidade(inicial, objetivo1))
  cat("====\tObjetivo 2\t====\n")
  print(buscaEmProfundidade(inicial, objetivo2))
  cat("====\tObjetivo 3\t====\n")
  print(buscaEmProfundidade(inicial, objetivo3))
  cat("====\tObjetivo 4\t====\n")
  print(buscaEmProfundidade(inicial, objetivo4))
  
}

Exemplo.buscaCustoUniformeObjetivo <- function() {
  cat("====\tBusca em Profundidade\t====\n")
  cat("====\tObjetivo 1\t====\n")
  print(buscaCustoUniforme(inicial, objetivo1))
  cat("====\tObjetivo 2\t====\n")
  print(buscaCustoUniforme(inicial, objetivo2))
  cat("====\tObjetivo 3\t====\n")
  print(buscaCustoUniforme(inicial, objetivo3))
  cat("====\tObjetivo 4\t====\n")
  print(buscaCustoUniforme(inicial, objetivo4))  
}


Exemplo.buscaGulosaObjetivo <- function() {
  cat("====\tBusca em Profundidade\t====\n")
  cat("====\tObjetivo 1\t====\n")
  print(buscaBestFirst(inicial, objetivo1, "Gulosa"))
  cat("====\tObjetivo 2\t====\n")
  print(buscaBestFirst(inicial, objetivo2, "Gulosa"))
  cat("====\tObjetivo 3\t====\n")
  print(buscaBestFirst(inicial, objetivo3, "Gulosa"))
  cat("====\tObjetivo 4\t====\n")
  print(buscaBestFirst(inicial, objetivo4, "Gulosa"))
}

Exemplo.buscaAEstrelaObjetivo <- function() {
  cat("====\tBusca em Profundidade\t====\n")
  cat("====\tObjetivo 1\t====\n")
  print(buscaBestFirst(inicial, objetivo1, "AEstrela"))
  cat("====\tObjetivo 2\t====\n")
  print(buscaBestFirst(inicial, objetivo2, "AEstrela"))
  cat("====\tObjetivo 3\t====\n")
  print(buscaBestFirst(inicial, objetivo3, "AEstrela"))
  cat("====\tObjetivo 4\t====\n")
  print(buscaBestFirst(inicial, objetivo4, "AEstrela"))
}

Exemplo.buscaEmLarguraObjetivo()
Exemplo.buscaEmProfundidadeObjetivo()
Exemplo.buscaCustoUniformeObjetivo()
Exemplo.buscaGulosaObjetivo()
Exemplo.buscaAEstrelaObjetivo()

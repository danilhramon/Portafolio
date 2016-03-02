
aleatoriosCorelacionados<-function(Sigmas,NumeroAleatorios){
  C=chol(Sigmas)
  Z=matrix(rnorm(NumeroAleatorios*length(Sigmas[,1])),nrow = NumeroAleatorios, ncol = length(Sigmas[,1]))
  X=Z%*%C
  return(X)
}
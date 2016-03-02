ProbabilidadPerdida<-function(TiempoFinal,Composicion,portafolio){
  t<-0
  Tiempo<-TiempoFinal
  composicionP<-Composicion
  n<-100000
  portafolio<-as.matrix(portafolio)
  z<-length(portafolio[,1])
  
  rendimientos<-portafolio[(2:z),]/portafolio[(1:(z-1)),]-1
  mediasR<-apply(rendimientos,2,mean)
  varianzaR<-apply(rendimientos,2,var)
  
  simulaciones<-matrix(0,n,length(portafolio[1,]))
  Zt<-aleatoriosCorelacionados(cov(rendimientos),length(simulaciones[,1]))
  for(i in 1:length(portafolio[1,])){
    simulaciones[,i]<-portafolio[length(portafolio[,1]),i]*exp((mediasR[i]-(varianzaR[i])/2)*(Tiempo-t)+(sqrt(varianzaR[i])*sqrt(Tiempo-t)*Zt[,i]))
  }
  
  valoractualporta<-sum(portafolio[length(portafolio[,2]),]*composicionP)
  ValorportafolioT<-apply(simulaciones*composicionP,1,sum)
  med<-composicionP%*%mediasR
  var<-sqrt((composicionP%*%cov(rendimientos))%*%composicionP)
  ProbabilidadPerdida<-length(subset(ValorportafolioT-valoractualporta,(ValorportafolioT-valoractualporta)<0))/length(ValorportafolioT)
  ProbabilidadPerdida<-list("Probabilidad de perdida"=ProbabilidadPerdida,"Medias"=med,"Desviacion"=sqrt(var))
  return(ProbabilidadPerdida)
}
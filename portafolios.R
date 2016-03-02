portafolios<-function(portafolio,numeroPortafolio,activos){
  Pkg <- c("base","fBasics","grid","httr","lubridate","PerformanceAnalytics",
           "quantmod","xts","zoo","quadprog","quantmod","ggplot2","timeDate","fPortfolio")
  inst <- Pkg %in% installed.packages()
  if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
  instpackages <- lapply(Pkg, library, character.only=TRUE)
  
  rendimientosln<-matrix(0,length(portafolio[,1]),length(portafolio[1,]))
  rendimientosln<-na.omit(diff(log(portafolio)))
  
  matrizcov<-cov(rendimientosln)
  Medias<-apply(rendimientosln,2,mean)
  
  por<-numeroPortafolio#nuemro de portafolios
  composicion<-matrix(runif(length(portafolio[1,])*por),por,length(portafolio[1,]))
  composicion<-composicion/rowSums(composicion)
  
  PortafoliosVeVar<-matrix(0,por,2)
  PortafoliosVeVar[,1]<-  composicion %*% Medias
  PortafoliosVeVar[,2]<-sqrt(rowSums((composicion %*% matrizcov) * composicion))
  PortafoliosVeVar<-cbind(composicion,PortafoliosVeVar)
  colnames(PortafoliosVeVar)<-c(activos,"ValoresEsperados","Desviacion")
  grafica = qplot(x=PortafoliosVeVar[,length(PortafoliosVeVar[1,])], y=PortafoliosVeVar[,length(PortafoliosVeVar[1,])-1], 
                  xlab = "Desviacion Estandar",ylab = "Rendimientos", main = "Portafolios")
  Sale = list("Portafolios" = PortafoliosVeVar, "Grafica" = grafica)
  
  return(Sale)
}
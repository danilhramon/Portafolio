
AnaliticoCall<-function(r,sigma,k,St,ti,t,n){
  Zt <- rnorm(n)
    #Call europeo
    
    ST <- St*exp((r-sigma^2/2)*(ti-t)+sigma*sqrt(ti-t)*Zt)-k 
    ST[ST<0] <- 0 
    Ctanalitico <- exp(-r*(ti-t))*mean(ST)
   
}

AnaliticoPut<-function(r,sigma,k,St,ti,t,n){
  Zt <- rnorm(n)
  #Put europeo
    ST <- k-St*exp((r-sigma^2/2)*(ti-t)+sigma*sqrt(ti-t)*Zt)
    ST[ST<0] <- 0
    Ptanalitico <- exp(-r*(ti-t))*mean(ST)
    
  }



#montecarlo 

simulacionCall<-function(s0,ti,r,sigma,n,k){
  z<-c(rnorm(n,mean=0,sd=1))
  Sti<-s0*exp((r-(sigma^2)/2)*(ti)+ sigma*sqrt(ti)*z)
  resta<-c
  maximo<-pmax(resta, 0)*exp(-r*(ti-t))
  callsim1<-mean(maximo)
  #Reduciendo varianza
  Sti_esp<-s0*exp(r*ti)
  Sti_var<-s0^2*exp(2*r*ti)*(exp(sigma^2*ti)-1)
  alpha<--cov(maximo,Sti)/Sti_var
  ST_VR<-maximo+alpha*(Sti-Sti_esp)
  callsim_VR1<-mean(ST_VR)
  return(callsim_VR1)
}
#put
#Genero el n´umero aleatorio para z, y calculo el precio
simulacionPut<-function(s0,ti,r,sigma,n,k){
  z<-c(rnorm(n,mean=0,sd=1))
  Sti<-s0*exp((r-(sigma^2)/2)*(ti)+ sigma*sqrt(ti)*z)
  resta<-p
  maximo<-pmax(resta, 0)*exp(-r*(ti-t))
  putsim1<-mean(maximo)
  #Reduciendo varianza
  Sti_esp<-s0*exp(r*ti)
  Sti_var<-s0^2*exp(2*r*ti)*(exp(sigma^2*ti)-1)
  alpha<--cov(maximo,Sti)/Sti_var
  ST_VR<-maximo+alpha*(Sti-Sti_esp)
  putsim_VR1<-mean(ST_VR)
  return(putsim_VR1)
}
#COMPROBACIÓN

d1 <- (log(St/k)+(r+(sigma^2/2))*(ti-t))/(sigma*sqrt(ti-t))
d2 <- d1-sigma*sqrt(ti-t)

Ct<- St*pnorm(d1,0,1)-k*exp(-r*(ti-t))*pnorm(d2,0,1)
Pt<- k*exp(-r*(ti-t))*pnorm(-d2)-St*pnorm(-d1)

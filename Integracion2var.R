#INTEGRACION
#DOS DIMENSIONES
ax = 0
bx = 4
ay = 0
by = 4
n = 100000
f = function(x,y){x^2-3*y^2} #21.3

#Trapecios
IntegracionTrapecios = function(f, ax, bx, ay, by, n){
  hx = abs((bx-ax)/n)
  hy = abs((by-ay)/n)
  hxy = (hx+hy)/2
  i = seq(1:(n-1))
  x = ax + (i*hx)
  y = ay + (i*hy)
  sumatrapecios = 2*f(x,y)
  sumatrapecios = c(f(ax,ay),sumatrapecios,f(bx,by))
  sumatrapecios = (sum(sumatrapecios)*hx*hy*n)/4
  return(sumatrapecios)
}
IntTrapecios1var<-IntegracionTrapecios(f,ax,bx,ay,by,n)

#MONTE CARLO
IntegracionMonteCarlo = function(f, ax, bx, ay, by, n){
  hx = abs((bx-ax)/n)
  hy = abs((by-ay)/n)
  x = ax + runif(n)*(bx-ax)
  y = ay + runif(n)*(by-ay)
  sumamontecarlo = f(x,y)
  sumamontecarlo = sum(sumamontecarlo)*hx*hy*n
  return(sumamontecarlo)
}
IntMonteCarlo<-IntegracionMonteCarlo(f,ax,bx,ay,by,n)
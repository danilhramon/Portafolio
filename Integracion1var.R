#INTEGRACION
#UNA DIMANESION
a = 1
b = 6
n = 1000
f = function(x){x^2 + 2*x}


#TRAPECIOS
IntegracionTrapecios = function(f, a, b, n){
  h = abs((b-a)/n)
  i = seq(1:(n-1))
  x = a + (i*h)
  sumatrapecios = 2*f(x)
  sumatrapecios = c(f(a),sumatrapecios,f(b))
  sumatrapecios = sum(sumatrapecios)*(h/2)
  return(sumatrapecios)
}

#SIMPSON
IntegracionSimpson = function(f, a, b, n){
  h = abs((b-a)/n)
  x1 = c(a, b)
  x2 = seq(a+h,b-h,2*h)
  x3 = seq(a+(2*h),b-(2*h),2*h)
  y1 = f(x1)
  y2 = f(x2)
  y3 = f(x3)
  sumasimpson = (sum(y1) + 4*sum(y2) + 2*sum(y3)) * (h/3)
  return(sumasimpson)
}

#MONTE CARLO
IntegracionMonteCarlo = function(f, a, b, n){
  h = abs((b-a)/n)
  x = a + runif(n)*(b-a)
  sumamontecarlo = f(x)
  sumamontecarlo = sum(sumamontecarlo)*(b-a)/(n-1)
  return(sumamontecarlo)
}


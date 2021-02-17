# Problema 3 
# Subpunctul a)

func <- function(t,z){
  
  r = (t**(z-1))*exp(1)**(-t)
  return(r)
}

fgam <- function(n){
  if(n == 1){
    return(1)
  }
  if(n == 1/2){
    return(sqrt(pi))
  }
  if(n>0 && identical(round(n), n)){
    return(factorial(n-1))
  }
  if(n>1.0){
    return((n-1)*fgam(n-1))
  } 
  if(n<1 && n>=0){
    o  = integrate(func,0,Inf,z=n)
    return(o$value)
  }
}
fbet <- function(a,b){
  if(a>0 && b>0 && (a+b == 1)){
   return(pi/sin(a*pi))
  }else{
    
    return((fgam(a)*fgam(b))/fgam(a+b))
  }
}

F1 <- function(a,b,t){1/(b^a*(fgam(a)))*(t^(a-1))*exp(-t/b)}
F2 <- function(a,b,t){1/fbet(a,b)*(t^(a-1))*((1-t)^(b-1))}
F3 <- function(a,b,t,z0){(1/(b^a*(fgam(a)))*(t^(a-1))*exp(-t/b)) * (1/fbet(a,b)*((z0-t)^(a-1))*((1-z0+t)^(b-1)))}
#                  x z
fprobgammanr <-function(a1,b1){
  a <- a1
  b <- b1
  if(x<=0){
    return(0)
  }else{
    return(integrate(F1, 0, x, a=a1,b=b1)$value)
  }
}

fprobbetanr <-function(a2,b2){
  a <- a2
  b <- b2
  if(y<=0 || y>=1 ){
    return(0)
  }else{
    return(integrate(F2, 0, 1,a = a2,b = b2)$value)
  }
  
}
fprobnr <- function(an,bn)
{
  x <- 3
  x1 <- fprobgammanr(an,bn)
  x <- 5
  aux1 <- fprobgammanr(an,bn)
  x <- 2
  aux2 <- fprobgammanr(an,bn)
  x2 <- aux1 - aux2
  x <- 4
  aux1 <- fprobgammanr(an,bn)
  x <- 3
  aux3 <- fprobgammanr(an,bn)
  x3 <- (aux1 - aux3)/(1 - aux2)
  y <- 2
  y4 <- 1 - fprobbetanr(an,bn)
  x <- 6
  aux1 <- fprobgammanr(an,bn)
  x <- 4
  aux2 <- fprobgammanr(an,bn)
  x5 <- aux1 - aux2
  x <- 1
  aux1 <- fprobgammanr(an,bn)
  x <- 0
  aux2 <- fprobgammanr(an,bn)
  x <- 7
  aux3 <- fprobgammanr(an,bn)
  x6 <- (aux1 - aux2)/aux3
 
  f6 <-function(z,x){
    z1 <- integrate(F3, 0,Inf, a=an, b=bn, z0=5)
    return(z1$value)}
  z1r <- integrate(f6, 0,5,z=5)
  
}
d<-fprobnr(1,2)
mat <-c(0,0,0,0,0,0,0,0,0,0,0,0)
mat[1] <-pgamma(q = 3, shape = 1, scale = 2)
mat[3] <-pgamma(q = 5, shape = 1, scale = 2)-pgamma(q = 2, shape = 1, scale = 2)
mat[5] <-(pgamma(q = 4, shape = 1, scale = 2)-pgamma(q = 3, shape = 1, scale = 2))/pgamma(q = 2, shape = 1, scale = 2)
mat[7] <-1-pbeta(q = 2, 1, 2)
mat[9] <-pgamma(q = 6, shape = 1, scale = 2)-pgamma(q = 4, shape = 1, scale = 2)
mat[11] <-(pgamma(q = 1, shape = 1, scale = 2)-pgamma(q = 0, shape = 1, scale = 2))/pgamma(q = 7, shape = 1, scale = 2)


smoke<- matrix(mat,ncol=2,byrow=TRUE)

 colnames(smoke) <- c("Repartitia Gamma","Ce am facut noi")
 rownames(smoke) <- c("P(X<3)","P(2<X<5)","P(3<x<4|X>2)","P(Y>2)","P(4<X<6)","P(0<X<1|X<7)")
 smoke <- as.table(smoke)
smoke


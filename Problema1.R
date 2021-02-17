Eb <- 0
Eb2 <- 0
Vb <- 0
Ep <- 0
Ep2 <- 0
Vp <- 0
func <- function(t,z){
  
  r = (z*exp(-z*x))
  return(r)
}
func2 <- function(u1,t1){
  
  r = (1/(sqrt(2*pi)*sqrt(t1))*exp(-(x-u1)/(2*t1))))
  return(r)
}
b1 <- function(n,p){
  for (i in 1:1000) {
   Eb <- Eb+x1[i]*(factorial(n)/(factorial(x1[i])*factorial(n-x1[i])))*(p^x1[i])*((1-p)^(n-x1[i]))
   Eb2 <- Eb2+(x1[i]^2)*(factorial(n)/(factorial(x1[i])*factorial(n-x1[i])))*(p^x1[i])*((1-p)^(n-x1[i]))
  }
  Vb <- Eb2 - Eb^2
}
pois <- function(l){
  for (i in 1:1000) {
    Ep <- Ep+x1[i]*exp(-l)*(l^x1[i])*factorial(x1[i])
    Ep2 <- Ep2+(x1[i]^2)*exp(-l)*(l^x1[i])*factorial(x1[i])
  }
  Vp <- Ep2 - Ep^2
}
Exp <-function(li){
  Expp <- integrate(func,0,1000,z=li)$value
  Vexpp  <- (integrate(func,0,1000,z=li)$value)- Expp^2
}
N <-function(u,t){
  Expp <- integrate(func2,0,1000,u1=u,t1=t)$value
  Vexpp  <- (integrate(func2,0,1000,u1=u,t1=t)$value)- Expp^2
}
x1 <- sample(1:20, 1000, replace=T)
E <- sample(0:0, 1000, replace=T)
a <-b1(2,2)
p <-pois(4)
g <- Exp(4)


a <- seq(0,40,by = 2)
b <- dbinom(a,40,0.4)
plot(a,b,type="l",col="blue")
par(new=TRUE)
a1 <- seq(0,40,by = 4)
b1 <- dbinom(a1,40,0.7)
plot(a1,b1,col="purple",type="l")
par(new=TRUE)
a2 <- seq(0,40,by = 2)
b2 <- dbinom(a2,40,0.9)
plot(a2,b2,col="red",type="l")
par(new=TRUE)
a3 <- seq(0,40,by = 2)
b3 <- dbinom(a3,40,0.1)
plot(a3,b3,col="green",type="l")
par(new=TRUE)
a4 <- seq(0,40,by = 2)
b4 <- dbinom(a4,40,0.5)
plot(a4,b4,col="darkgreen",type="l")
par(new=FALSE)

plot( dpois( x=0:10, lambda=6 ),col="green",type="l")
par(new=TRUE)
plot( dpois( x=0:10, lambda=9 ),col="red",type="l")
par(new=TRUE)
plot( dpois( x=0:10, lambda=2 ),col="blue",type="l")
par(new=TRUE)
plot( dpois( x=0:10, lambda=3 ),col="purple",type="l")
par(new=TRUE)
plot( dpois( x=0:10, lambda=7 ),col="black",type="l")

x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2.5, sd = 0.5)
plot(x,y,type="l",col="blue")
par(new=TRUE)
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2, sd = 0.5)
plot(x,y,type="l",col="red")
par(new=TRUE)
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2.5, sd = 1)
plot(x,y,type="l",col="purple")
par(new=TRUE)
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 5, sd = 0.5)
plot(x,y,type="l",col="green")
par(new=TRUE)
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 1, sd = 1.5)
plot(x,y,type="l",col="black")


x <- seq(0, 20, length.out=1000)
plot(dexp(x, rate=0.65),type="l",col="black")
par(new=TRUE)
x <- seq(0, 20, length.out=1000)
plot(dexp(x, rate=0.50),type="l",col="blue")
par(new=TRUE)
x <- seq(0, 20, length.out=1000)
plot(dexp(x, rate=0.25),type="l",col="purple")
par(new=TRUE)
x <- seq(0, 20, length.out=1000)
plot(dexp(x, rate=0.35),type="l",col="green")
par(new=TRUE)
x <- seq(0, 20, length.out=1000)
plot(dexp(x, rate=0.75),type="l",col="pink")


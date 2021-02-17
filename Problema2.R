#install.packages("plotrix")
library("plotrix")
par(pty="s")
plot(seq(-1,1,length=10),seq(-1,1,length=10),type="n",xlab="",ylab="")
#draw.circle(2,4,c(1,0.66,0.33),border="purple",
#            col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
draw.circle(0,0,radius = 1,border="black",lty=1,lwd=1)
x=runif(1000,-1,1)
y=runif(1000,-1,1)
ma <-0
nra <-0
nrr <-0
for (i in 1:1000) {
  if(sqrt(x[i]^2+y[i]^2) <=1){
    points(x[i],y[i],col="blue")
    nra <- nra+1}
  else{
    points(x[i],y[i],col="red")
    nra <- nrr+1
  }
  ma <- ma + sqrt(x[i]^2+y[i]^2)
}
mt<-0
ma <- ma/1000
for (i in 1:1000) {
    if(sqrt(x[i]^2+y[i]^2) <=1){
      mt <- mt+(nra/1000)*sqrt(x[i]^2+y[i]^2)
      }
    else{
      mt <- mt+(nrr/1000)*sqrt(x[i]^2+y[i]^2)
    }
    
  }
#points(x[2],y[2],col="darkgreen")
  Funct5 <-function(r,t){
  return(1/pi*(1/(r*cos(t)*sqrt(1-(r^2)*(cos(t)^2)))+1/(r*sin(t)*sqrt(1-(r^2)*(sin(t)^2)))))
    
  }
 R <-function(r1){
    
      return(integrate(Funct5, 0, 2*pi,r=r1,subdivisions=2000)$value)
    }
 Te <-function(t1){
   
   return(integrate(Funct5, 0, Inf,t=t1)$value)
 }
 ttttt<-Funct5(1/sqrt(2),-pi/4)
 teta<-Te(pi/4)
 rrr<-R(1/sqrt(2))
  
 
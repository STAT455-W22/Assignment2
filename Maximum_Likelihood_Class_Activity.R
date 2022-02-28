library(tidyverse)

# Maximize with respect to lambda directly

L=seq(1,6,length=1001)
lik=L^5*exp((-1)*L)*L^1*exp((-1)*L)*L^3*exp((-1)*L)*L^6*exp((-1)*L)     
df <- data.frame(L,lik)

plot<- ggplot(data=df,aes(x=L, y=lik)) + 
  geom_line(color="blue", size=2) + 
  xlab("possible values of Lambda") + ylab("Likelihood") + 
  labs(title="Likelihood function for Park Visitors") 
plot + xlim(c(1, 6))

which.max(lik)
df[551,]


Lik.f <- function(Vis1,Vis2, Vis3, Vis4, nGrid){
  L=seq(min(c(Vis1, Vis2, Vis3, Vis4)),max(c(Vis1, Vis2, Vis3, Vis4)),length=nGrid)
  lik=L^Vis1*exp((-1)*L)*L^Vis2*exp((-1)*L)*L^Vis3*exp((-1)*L)*L^Vis4*exp((-1)*L)     
  return(L[lik==max(lik)])             # find and return the value of p that maximizes the likelihood function    
}

Lik.f(Vis1=5, Vis2=10, Vis3=3, Vis4=6, nGrid=10000)

#-------------------------------------------------------------

# Maximize with respect to b0 directly

b0=seq(0,log(6),length=10000)
lik=exp(b0)^5*exp((-1)*exp(b0))*exp(b0)^1*exp((-1)*exp(b0))*exp(b0)^3*exp((-1)*exp(b0))*exp(b0)^6*exp((-1)*exp(b0))     
df <- data.frame(b0,lik)

plot<- ggplot(data=df,aes(x=b0, y=lik)) + 
  geom_line(color="blue", size=2) + 
  xlab("possible values of Lambda") + ylab("Likelihood") + 
  labs(title="Likelihood function for Park Visitors") 
plot + xlim(c(0, log(6)))

which.max(lik)
df[7377,]

Evening <- c(0,0,1,1)
Visitors <- c(5,1,3,6)
Park <- data.frame(Evening, Visitors)


M <- glm(data=Park, Visitors~1, family="poisson")
summary(M)

#---------------------------------------------------------

# maximize wrt b0 and b1

Lik.f_Poisson2 <- function(Vis1, Vis2, Vis3, Vis4,nGrid){
  b0 <- seq(-3, 3, length = nGrid)  # values of b0 
  b1 <- seq(-1, 1, length=nGrid)  # values of b1
  B <- expand.grid(b0, b1)  # create all combinations of b0 and b1
  names(B) <- c("b0", "b1")  # give B the right names
  B <- B %>% mutate(Lik = exp(b0+b1*0)^Vis1*exp(-1*exp((b0+b1*0)))*
                      exp(b0+b1*0)^Vis2*exp(-1*exp((b0+b1*0)))*
                      exp(b0+b1*1)^Vis3*exp(-1*exp((b0+b1*1)))*
                      exp(b0+b1*1)^Vis4*exp(-1*exp((b0+b1*1))))
  #evaluate function
  return(B[B$Lik==max(B$Lik),]) # find and return combination of b0 and b1 that maximize B.     
}

Lik.f_Poisson2(Vis1=5,Vis2=1,Vis3=3,Vis4=6,nGrid=1000)

Evening <- c(0,0,1,1)
Visitors <- c(5,1,3,6)
Park <- data.frame(Evening, Visitors)

M <- glm(data=Park, Visitors~Evening, family="poisson")
summary(M)

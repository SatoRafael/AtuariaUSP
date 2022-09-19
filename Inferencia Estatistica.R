amostra = c(155,155,136,154,172,160,159,165,159,137,152,178,138,144,167,126,149,150)


#INTERVALO DE CONFIANCA - VARIANCIA DESCONHECIDA
ic_vd <- function(conf,amostra){
w <- conf #confiança dada 
o <-  var(amostra)
n <- length(amostra)
m <- mean(amostra) #média
t = qt(((1-w)/2), df = (n-1))
liminf <- m + t*sqrt(o/n)
limsup <- m - t*sqrt(o/n)
print(c(liminf,limsup))
}
ic_vd(0.9,amostra)

#INTERVALO DE CONFIANÇA PARA SIGMA
ic_sig <- function(conf,amostra){
  w <- conf
  o <- var((amostra))
  n <- length(amostra)
  cauda <- ((1-conf)/2)
  limsup <- ((n-1)*o)/qchisq(cauda, df = n-1)
  liminf <- ((n-1)*o)/qchisq((cauda + conf), df = n-1)
  print(c(liminf,limsup))
}
ic_sig(0.77,amostra)

#P-valor para média desconhecida 
xbarra <- mean(amostra)
n <- length(amostra)
s <- sqrt(var(amostra))
h0 <- 173.5
T0 <- -1*((sqrt(n)*abs(xbarra-h0))/s)
pt(T0, df = (n-1))


### hist
amostra <- c(111,123,124,128,138,139,141,142,147,152,154,156,157,161,163,165,168,172,174,174)
hist(amostra, prob = TRUE, right = FALSE, breaks = c(111,132,153,174))

install.packages('ggplot2')
library('ggplot2')

#Bernoulli
L = function(theta) theta^sum(x)*(1-theta)^(n-sum(x))
x = c(0,1,0,1,0,1,1,1,0,1,0,0,0,1,0)
n = length(x) 
plot(L)
abline(v=mean(x))

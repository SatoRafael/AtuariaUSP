######### Atividade 2 
ex2 <- read_excel('C:/Users/Lenovo/OneDrive/Área de Trabalho/Faculdade/Análise Regressiva/EAC 0355_Atividade 02_bd.xlsx')

modelo <- lm(ex2$PETR3 ~ ex2$Ibovespa, ex2)

plot(ex2$Ibovespa, ex2$PETR3)
abline(modelo)

###teste de kolmogorov-smirnov para normalidade no resíduo
install.packages('dgof')
library('dgof')
u <- modelo$residuals
ks.test(u, 'pnorm', mean(u), sd(u))

#teste de shapiro
shapiro.test(u)

#função para utilizar os valores
ex_2 <- function(x){
  return(-0.003987 + 1.543933*x)
}

ex_2(5)

#teste de White
install.packages('skedastic')
library('skedastic')  
white_lm(modelo)

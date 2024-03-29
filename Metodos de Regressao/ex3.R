###################### Atividade 3
library('readxl')
bd <- read_excel('C:/Users/Lenovo/OneDrive/�rea de Trabalho/Faculdade/An�lise Regressiva/EAC 0355_Atividade 03_bd.xlsx')

modelo <- lm(bd$Receita ~ bd$Preco + bd$Horas + bd$Invest + bd$Juros + bd$Cambio, data = bd)
summary(modelo)

#Modelo com vari�veis cujo p-valor > 5% foram removidas
modelo2 <- lm(bd$Receita ~ bd$Preco + bd$Horas + bd$Invest + bd$Juros, data = bd)
summary(modelo2)

modelo3 <- lm(bd$Receita ~ bd$Preco + bd$Horas + bd$Invest, data = bd)
summary(modelo3)
#Fun��o para obter valores
ex3 <- function(x,y,z){
  return(623.2045 + -1.6893*x + 4.2178*y + 8.8395*z)
}

#quest�o 2
print(ex3(10,-4,-5)-623.2045)

#quest�o 3
print(ex3(0,0,1)-623.20)

#quest�o 4
##extraindo o res�duo
u <- modelo2$residuals

##teste de kolmogorov-smirnov e shapiro-wilk para medir normalidade no res�duo
#install.packages('dgof')
library('dgof')

ks.test(u, 'pnorm', mean(u), sd(u))

shapiro.test(u)

##teste de homocedasticidade de White
#install.packages('skedastic')
library('skedastic')

white_lm(modelo3)

#quest�o 5
##importando package para estimar colinearidade (se VIF > 5, pode haver problemas de colinearidade)
#install.packages('car')
library('car')

vif(modelo)

#quest�o 8 
ex3(240,50,20)



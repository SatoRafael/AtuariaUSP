###################Atividade 8

######Bernoulli
library(readxl)

bd <- read_excel('C:/Users/Lenovo/OneDrive/�rea de Trabalho/Faculdade/An�lise Regressiva/EAC 0355_Atividade 08_bd_bernoulli.xlsx')

bd$Default <- ifelse(bd$Default == 'Sim', 1, 0)

bd$Sexo <- ifelse(bd$Sexo == 'masculino', 1, 0)

modelo <- glm(bd$Default ~ ., data = bd,  family = binomial(link = "logit"))

summary(modelo)

modelo$coefficients

ex8_bin <- function(Idade, Sexo, Renda){
  return(exp(2.975 - 0.024*Idade + 0.741*Sexo - 0.256*Renda))
}

####Raz�o de chance: utilizar apenas o exponencial da varia��o do beta - 1
exp(0.741) - 1

#Probabilidade: p = exp(Y)/1 + exp(Y)
ex8_bin(37, 0, 6.85)/(1 + ex8_bin(37, 0, 6.85))

######Poisson
bd <- read_excel('C:/Users/Lenovo/OneDrive/�rea de Trabalho/Faculdade/An�lise Regressiva/EAC 0355_Atividade 08_bd_poisson.xlsx')

bd$Sexo <- ifelse(bd$Sexo == 'masculino', 1, 0)

modelo <- glm(bd$QTsinistro ~ ., data = bd,  family = poisson(link = "log"))

modelo$coefficients

ex8_p <- function(Idade, Sexo, Renda){
  return(exp(0.040*Idade + 0.198*Sexo + 0.199*Renda))
}

#Qual o valor m�dio do n�mero de sinistros de indiv�duos com o seguinte perfil: sexo feminino, com 37 anos e com renda mensal de R$6850,00?  (resposta deve ser um n�mero inteiro)?
ex8_p(37, 0, 6.85)

#Em quanto o valor m�dio de sinistros varia (resposta em percentual) ao se aumentar a idade de uma unidade (mantidas as demais condi��es constantes)?
exp(0.040) - 1

#Qual a probabilidade (resposta em percentual) de em determinado m�s ocorrer 20 sinistros associados a indiv�duos com o seguinte perfil: sexo feminino, com 37 anos e com renda mensal de R$6850,00?
(exp(-ex8_p(37,0,6.85))*ex8_p(37,0,6.85)^20)/factorial(20) #A resposta est� dando uma diferen�a decimal, averiguar

#Em quanto o valor m�dio de sinistros associados aos indiv�duos masculinos varia (resposta em percentual) quando comparado ao valor m�dio associado aos indiv�duos do sexo femininos (mantidas as demais condi��es constantes)?
exp(0.198) - 1

#######Normal
bd <- read_excel('C:/Users/Lenovo/OneDrive/�rea de Trabalho/Faculdade/An�lise Regressiva/EAC 0355_Atividade 08_bd_normal.xlsx')

modelo <- lm(bd$Receita ~ bd$Preco + bd$Horas + bd$Invest, data = bd)

summary(modelo)

modelo$coefficients

ex8 <- function(Preco, Horas, Invest){
  return(623.204 -1.689*Preco + 4.218*Horas + 8.839*Invest )
}

#Qual seria a receita esperada se em determinado m�s ocorrer o seguinte cen�rio: pre�o do produto igual a 240, investimento em propaganda igual a 50 e 20 horas de treinamento?]
ex8(240,20,50)

#Qual seria a probabilidade da receita superar o valor de R$800 se em determinado m�s ocorrer o seguinte cen�rio: pre�o do produto igual a 240, investimento em propaganda igual a 50 e 20 horas de treinamento? (assumir que o desvio padr�o da receita � igual a R$125,00)
(800 - 744.15)/125 #A resposta n�o bate

#Qual seria a varia��o do valor da receita se o pre�o do produto subir 10, ocorrer uma redu��o de 5 no investimento em propagada e uma redu��o de 4 em horas de treinamento (mantendo os demais fatores fixos)?
-1.689*10 + 4.218*-4 + 8.839*-5 

#Qual a varia��o da receita m�dia para cada unidade de investimento em propaganda?
modelo$coefficients #Extrair o investimento

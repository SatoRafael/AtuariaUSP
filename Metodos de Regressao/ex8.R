###################Atividade 8

######Bernoulli
library(readxl)

bd <- read_excel('C:/Users/Lenovo/OneDrive/Área de Trabalho/Faculdade/Análise Regressiva/EAC 0355_Atividade 08_bd_bernoulli.xlsx')

bd$Default <- ifelse(bd$Default == 'Sim', 1, 0)

bd$Sexo <- ifelse(bd$Sexo == 'masculino', 1, 0)

modelo <- glm(bd$Default ~ ., data = bd,  family = binomial(link = "logit"))

summary(modelo)

modelo$coefficients

ex8_bin <- function(Idade, Sexo, Renda){
  return(exp(2.975 - 0.024*Idade + 0.741*Sexo - 0.256*Renda))
}

####Razão de chance: utilizar apenas o exponencial da variação do beta - 1
exp(0.741) - 1

#Probabilidade: p = exp(Y)/1 + exp(Y)
ex8_bin(37, 0, 6.85)/(1 + ex8_bin(37, 0, 6.85))

######Poisson
bd <- read_excel('C:/Users/Lenovo/OneDrive/Área de Trabalho/Faculdade/Análise Regressiva/EAC 0355_Atividade 08_bd_poisson.xlsx')

bd$Sexo <- ifelse(bd$Sexo == 'masculino', 1, 0)

modelo <- glm(bd$QTsinistro ~ ., data = bd,  family = poisson(link = "log"))

modelo$coefficients

ex8_p <- function(Idade, Sexo, Renda){
  return(exp(0.040*Idade + 0.198*Sexo + 0.199*Renda))
}

#Qual o valor médio do número de sinistros de indivíduos com o seguinte perfil: sexo feminino, com 37 anos e com renda mensal de R$6850,00?  (resposta deve ser um número inteiro)?
ex8_p(37, 0, 6.85)

#Em quanto o valor médio de sinistros varia (resposta em percentual) ao se aumentar a idade de uma unidade (mantidas as demais condições constantes)?
exp(0.040) - 1

#Qual a probabilidade (resposta em percentual) de em determinado mês ocorrer 20 sinistros associados a indivíduos com o seguinte perfil: sexo feminino, com 37 anos e com renda mensal de R$6850,00?
(exp(-ex8_p(37,0,6.85))*ex8_p(37,0,6.85)^20)/factorial(20) #A resposta está dando uma diferença decimal, averiguar

#Em quanto o valor médio de sinistros associados aos indivíduos masculinos varia (resposta em percentual) quando comparado ao valor médio associado aos indivíduos do sexo femininos (mantidas as demais condições constantes)?
exp(0.198) - 1

#######Normal
bd <- read_excel('C:/Users/Lenovo/OneDrive/Área de Trabalho/Faculdade/Análise Regressiva/EAC 0355_Atividade 08_bd_normal.xlsx')

modelo <- lm(bd$Receita ~ bd$Preco + bd$Horas + bd$Invest, data = bd)

summary(modelo)

modelo$coefficients

ex8 <- function(Preco, Horas, Invest){
  return(623.204 -1.689*Preco + 4.218*Horas + 8.839*Invest )
}

#Qual seria a receita esperada se em determinado mês ocorrer o seguinte cenário: preço do produto igual a 240, investimento em propaganda igual a 50 e 20 horas de treinamento?]
ex8(240,20,50)

#Qual seria a probabilidade da receita superar o valor de R$800 se em determinado mês ocorrer o seguinte cenário: preço do produto igual a 240, investimento em propaganda igual a 50 e 20 horas de treinamento? (assumir que o desvio padrão da receita é igual a R$125,00)
(800 - 744.15)/125 #A resposta não bate

#Qual seria a variação do valor da receita se o preço do produto subir 10, ocorrer uma redução de 5 no investimento em propagada e uma redução de 4 em horas de treinamento (mantendo os demais fatores fixos)?
-1.689*10 + 4.218*-4 + 8.839*-5 

#Qual a variação da receita média para cada unidade de investimento em propaganda?
modelo$coefficients #Extrair o investimento

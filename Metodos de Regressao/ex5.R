#Extraindo o banco de dados
install.packages('readxl')
library('readxl')

bd <- read_excel('C:/Users/Lenovo/OneDrive/Área de Trabalho/Faculdade/Análise Regressiva/EAC 0355_Atividade 05_bd.xlsx')

head(bd)

#Criando as dummies. Referências (0): BUS, sexo feminino e área A
install.packages('fastDummies')
library(fastDummies)

bd <- dummy_cols(bd, select_columns = c('TipoVeiculo', 'GeneroCondutor', 'AreaResidencial'))

bd <- subset(bd_final, select = -c(TipoVeiculo_BUS, GeneroCondutor_F, ID, AreaResidencial_A,TipoVeiculo, GeneroCondutor, AreaResidencial))

#Criando o modelo linear
modelo <- lm(ValorPago ~ ., bd)
summary(modelo)

#Importando library para fazer stepwise regression
library(MASS)
modelo_final <- stepAIC(modelo, direction = 'backward', trace = TRUE)

#Verificando os coeficientes do modelo linear
modelo_final$coefficients

#Teste de normalidade - Se p-valor < 0.05 rejeitamos a hipótese
#install.packages('dgof')
library('dgof')

u<- modelo_final$residuals

ks.test(u, 'pnorm', mean(u), sd(u))

shapiro.test(u)

#Teste de homocedasticidade - Se p-valor < 0.05 rejeitamos a hipótese
library('skedastic')

white_lm(modelo_final)

#Teste de Colinearidade - se VIF > 5, há possibilidade de problemas de colinearidade
library('car')
vif(modelo_final)

modelo_final$coefficients

#Criando função para estimar os valores do exercício
ex5 <- function(){
  x1 <- as.integer(readline(prompt = 'Idade do condutor: '))
  d2 <- as.integer(readline(prompt = 'COUPE? (1/0): '))
  d3 <- as.integer(readline(prompt = 'HBACK? (1/0): '))
  d4 <- as.integer(readline(prompt = 'HDTOP? (1/0): '))
  d5 <- as.integer(readline(prompt = 'RDSTR? (1/0): '))
  d6 <- as.integer(readline(prompt = 'TRUCK? (1/0): '))
  d7 <- as.integer(readline(prompt = 'Homem? (1/0): '))
  d8 <- as.integer(readline(prompt = 'E? (1/0): '))
  d9 <- as.integer(readline(prompt = 'F? (1/0): '))
  return(2248.86859-144.47860*x1+719.00330*d2-45.33843*d3-158.33577*d4
         +946.14406*d5-134.42392*d6+660.79492*d7+351.22826*d8+895.46502*d9)
}
ex5()


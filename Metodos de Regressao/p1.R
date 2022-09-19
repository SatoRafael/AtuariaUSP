
#Criando variáveis dummy
library('fastDummies')

bd <- dummy_cols(bd, select_columns = c('NivelEscolaridade', 'TipoTrabalho'))

bd <- subset(bd, select = -c(NivelEscolaridade, TipoTrabalho, NivelEscolaridade_Superior, TipoTrabalho_Informal))

#Criando o modelo linear
modelo <- lm(Perdas ~ ., bd)
summary(modelo)

modelo <- lm(Perdas ~ bd$Idade + bd$NoDependentes + bd$TempoEmprego + bd$TempoEndereco + bd$Endividamento +
               bd$DividaCartao + bd$OutrasDividas + bd$NivelEscolaridade_Fundamental + bd$`NivelEscolaridade_Segundo Grau` +
               bd$NivelEscolaridade_Tecnico + bd$TipoTrabalho_Formal, bd)

modelo <- lm(Perdas ~ bd$Idade + bd$NoDependentes + bd$TempoEmprego + bd$TempoEndereco +
               bd$DividaCartao + bd$OutrasDividas + bd$NivelEscolaridade_Fundamental + bd$`NivelEscolaridade_Segundo Grau` +
               bd$NivelEscolaridade_Tecnico + bd$TipoTrabalho_Formal, bd)

modelo <- lm(Perdas ~ bd$Idade + bd$NoDependentes + bd$TempoEmprego + bd$TempoEndereco +
               bd$DividaCartao + bd$NivelEscolaridade_Fundamental + bd$`NivelEscolaridade_Segundo Grau` +
               bd$NivelEscolaridade_Tecnico + bd$TipoTrabalho_Formal, bd)

modelo <- lm(Perdas ~ bd$Idade + bd$NoDependentes + bd$TempoEmprego + bd$TempoEndereco +
               bd$DividaCartao + bd$NivelEscolaridade_Fundamental + bd$`NivelEscolaridade_Segundo Grau` +
               bd$NivelEscolaridade_Tecnico, bd)

modelo <- lm(Perdas ~ bd$Idade + bd$NoDependentes + bd$TempoEmprego + bd$TempoEndereco +
               bd$DividaCartao + bd$NivelEscolaridade_Fundamental + bd$`NivelEscolaridade_Segundo Grau`, bd)

#Teste de normalidade - Se p-valor < 0.05 rejeitamos a hipótese
#install.packages('dgof')
library('dgof')

u<- modelo$residuals

ks.test(u, 'pnorm', mean(u), sd(u))

shapiro.test(u)

#Teste de homocedasticidade - Se p-valor < 0.05 rejeitamos a hipótese
library('skedastic')

white_lm(modelo)

#Teste de Colinearidade - se VIF > 5, há possibilidade de problemas de colinearidade
library('car')
vif(modelo)

modelo$coefficients

p1 <- function(){
  x1 <- as.integer(readline(prompt = 'Idade: '))
  x2 <- as.integer(readline(prompt = 'Dependentes: '))
  x3 <- as.integer(readline(prompt = 'Tempo no Emprego: '))
  x4 <- as.integer(readline(prompt = 'Tempo no Endereço: '))
  x5 <- as.integer(readline(prompt = 'Dívida no Cartão: '))
  d6 <- as.integer(readline(prompt = 'Ensino Fundamental?(1/0): '))
  d7 <- as.integer(readline(prompt = 'Segundo Grau?(1/0): '))
  return(2860.97756-63.16608*x1+679.11789*x2-141.16867*x3-89.30938*x4+712.13904*x5+1140.76960*d6+2524.35459*d7)
}


install.packages('readxl')
library('readxl')

base <- read_excel('C:/Users/Lenovo/OneDrive/�rea de Trabalho/Faculdade/An�lise Regressiva/EAC 0355_Atividade 01_bd.xlsx')

modelo <- lm(base$`Taxa de crescimento PIB` ~ base$`Investimento em educa��o`, base)
summary(modelo)

predict.lm(modelo)
plot(base$`Investimento em educa��o`, base$`Taxa de crescimento PIB`,
     main = 'Rela��o entre o investimento em educa��o e o PIB',
     xlab = 'Investimento em educa��o',
     ylab = 'Taxa de Crescimento do PIB')
abline(modelo)

#Intervalo de Confian�a
confint(modelo)
install.packages('readxl')
library('readxl')

base <- read_excel('C:/Users/Lenovo/OneDrive/Área de Trabalho/Faculdade/Análise Regressiva/EAC 0355_Atividade 01_bd.xlsx')

modelo <- lm(base$`Taxa de crescimento PIB` ~ base$`Investimento em educação`, base)
summary(modelo)

predict.lm(modelo)
plot(base$`Investimento em educação`, base$`Taxa de crescimento PIB`,
     main = 'Relação entre o investimento em educação e o PIB',
     xlab = 'Investimento em educação',
     ylab = 'Taxa de Crescimento do PIB')
abline(modelo)

#Intervalo de Confiança
confint(modelo)
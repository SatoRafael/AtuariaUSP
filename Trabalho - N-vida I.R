### Importando pacote para visualização dos dados
#install.packages("ggplot2")
library(ggplot2)

#Gerando seed para igualar os resultados
set.seed(0465)

####### Perspectiva individual

B <- 10000 #Nº de Simulações que o loop fará
prob <- .02 #Probabilidade de Sinistro
valor <- 10000 #Valor médio dos Sinistros
N <- 10000 #Carteira
###Observação: o código exige muito processamento e memória RAM, para que não trave o computador, reduza o nº de simulações
LMI <- 20000
franquia <- 1000
Freq_i <- matrix(nrow = B, ncol = B) #Matriz vazia de frequência
Sev_i_exp <- matrix(nrow = B, ncol = B) #Matriz vazia de X ~ Exp
Sev_i_n <- matrix(nrow = B, ncol = B) #Matriz vazia de X ~ N


Sinistros <- vector() #Vetor vazio que irá receber a frequência de sinistros de cada simulação
SAg_exp <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ Exp)
SAg_N <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ N)

for(i in 1:B){ #Irá iterar o número de simulações que queremos (linhas) 
  Freq_i[i,] <- rbinom(N, 1, prob) #Gera 10000 valores binários pseudoaleatórios que atendem a uma Distribuição binomial (utilizamos a propriedade np = λ para o caso da Poisson)
  for(j in 1:B){#Itera todas as colunas (clientes) para gerar a severidade do sinistro
      if(Freq_i[i,j] == 1) { #Se j = 1, houve sinistro
        Sev_i_exp[i,j] <- rweibull(1,1,valor)
        Sev_i_n[i,j] <- rweibull(1,3.6,valor)#Gera um valor aleatório que atende à Distribuição de Weibull
      } else{ #Caso contrário, não houve sinistro
        Sev_i_exp[i,j] <- 0
        Sev_i_n[i,j] <- 0
      }
    }
  Sinistros[i] <- sum(Freq_i[i,]) #Varredura da frequência de sinistros em cada simulação
  SAg_exp[i] <- sum(Sev_i_exp[i,]) #Varredura dos Sinistros Agregados (X ~ Exp)
  SAg_N[i] <- sum(Sev_i_n[i,]) #Varredura dos Sinistros Agregados (X ~ N)
}
 
Z_exp <- SAg_exp - mean(SAg_exp)/sd(SAg_exp) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ Exp)
Z_N <- SAg_N - mean(SAg_N)/sd(SAg_N) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ N)

provisoes_ind <- data.frame(Distribuição = c("Exponencial", "Normal"), #Criando dataframe com os valores das provisões
                      PE = c(mean(SAg_exp), mean(SAg_N)), #E[SAg]
                      PP = c(quantile(SAg_exp, probs = .75), quantile(SAg_N, probs = .75)), #Carregamento
                      DP = c(sd(SAg_exp), sd(SAg_N)), #Desvio-padrão das distribuições
                      U99.5 = c(quantile(SAg_exp, probs = .995), quantile(SAg_N, probs = .995)),
                      U99.97 = c(quantile(SAg_exp, probs = .9997), quantile(SAg_N, probs = .9997)),
                      CABR99.5 = c(quantile(SAg_exp, probs = .995) - quantile(SAg_exp, probs = .75), quantile(SAg_N, probs = .995) - quantile(SAg_N, probs = .75)), #Capital de Solvência para alpha = 0.05  
                      CABR99.97 = c(quantile(SAg_exp, probs = .9997) - quantile(SAg_exp, probs = .75), quantile(SAg_N, probs = .9997) - quantile(SAg_N, probs = .75))) #Capital de Solvência para alpha = 0.003

#Dataframe para utilizar nas plotagens
compara_ind <- data.frame(Distribuição = c(rep("Exponencial", each = B), rep("Normal", each= B)),
                      Severidades = c(SAg_exp, SAg_N)) 

#Visualizando os sinistros agregados juntos
sag_v <- ggplot(compara_ind, aes(x = Severidades,  fill = Distribuição))+
  geom_histogram(position = 'identity', alpha = .5, bins = 50)+
  geom_vline(data = provisoes_ind, aes(xintercept = PE, color= Distribuição))+
  xlim(1100000,3000000)+
  ylim(0, 1300)+
  labs(title = "Distribuição dos Sinistros Agregados",
       subtitle = 'Caso Individual',
       x = 'Sinistros Agregados',
       y = 'Frequência') 

#Visualizando os sinistros agregados pelas provisões
provisoes_plot <- ggplot(compara_ind, aes(x = Severidades))+
  geom_histogram(alpha = .75, bins = 100)+
  geom_vline(data = provisoes_col, aes(xintercept = PE), colour = 'red')+
  geom_vline(data = provisoes_col, aes(xintercept = PP), colour = 'blue')+
  geom_vline(data = provisoes_col, aes(xintercept = U99.5), colour = 'green')+
  geom_vline(data = provisoes_col, aes(xintercept = U99.97), colour = 'dark green')+
  xlim(1000000,2800000)+
  labs(title = "Distribuição dos Sinistros Agregados",
       subtitle = 'Caso Individual',
       x = 'Sinistros Agregados',
       y = 'Frequência')+
  facet_grid(Distribuição~.) 

#Visualizando a dispersão das duas distribuições
 dispersao_ind <- ggplot(compara_ind, aes(y = Severidades, x = Distribuição, fill = Distribuição))+ 
  geom_boxplot()+
  labs(title = "Dispersão dos Sinistros Agregados",
       subtitle = "Caso Individual",
       x = 'Dispersão',
       colour = 'Distribuição') 

######## Caso Coletivo

SAg_exp_c <- vector()
SAg_norm_c <- vector()
X_Exp_c <- vector()
X_Norm_c <- vector()
Freq_c <- vector()

for(i in 1:B){ #Irá iterar o número de simulações que queremos (linhas) 
  N_c <- rpois(1, 200) #Gera 10000 valores binários pseudoaleatórios que atendem a uma Distribuição de Poisson
  X_Exp_c <- rweibull(N_c, 1, valor) #Gera n valores de X~Exp
  X_Norm_c <- rweibull(N_c, 3.6, valor) #Gera n valores de X~Normal
  SAg_exp_c[i] <- sum(X_Exp_c) #Varredura da severidade dos sinistros 
  SAg_norm_c[i] <- sum(X_Norm_c)
}

Z_exp_c <- SAg_exp - mean(SAg_exp_c)/sd(SAg_exp_c) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ Exp)
Z_N_c <- SAg_N - mean(SAg_norm_c)/sd(SAg_norm_c) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ N)

#Dataframe para plotagens
compara_col <- data.frame(Distribuição = c(rep("Exponencial", each = B), rep("Normal", each= B)),
                          Severidades = c(SAg_exp_c, SAg_norm_c)) 

#Criando dataframe com os valores das provisões
provisoes_col <- data.frame(Distribuição = c("Exponencial", "Normal"), 
                          PE = c(mean(SAg_exp_c), mean(SAg_norm_c)), #E[SAg]
                          PP = c(quantile(SAg_exp_c, probs = .75), quantile(SAg_norm_c, probs = .75)), #Carregamento
                          DP = c(sd(SAg_exp), sd(SAg_N)), #Desvio-padrão das distribuições
                          U99.5 = c(quantile(SAg_exp_c, probs = .995), quantile(SAg_norm_c, probs = .995)), #Solvência para 99,5% dos Sinistros
                          U99.97 = c(quantile(SAg_exp_c, probs = .9997), quantile(SAg_norm_c, probs = .9997)), #Solvência para 99,97% dos Sinistros
                          CABR99.5 = c(quantile(SAg_exp_c, probs = .995) - quantile(SAg_exp_c, probs = .75), quantile(SAg_norm_c, probs = .995) - quantile(SAg_norm_c, probs = .75)), #Capital de Solvência para alpha = 0.05  
                          CABR99.97 = c(quantile(SAg_exp_c, probs = .9997) - quantile(SAg_exp_c, probs = .75), quantile(SAg_norm_c, probs = .9997) - quantile(SAg_norm_c, probs = .75))) #Capital de Solvência para alpha = 0.003

#Visualizando os sinistros juntos
sag_col <- ggplot(compara_col, aes(x = Severidades,  fill = Distribuição))+
  geom_histogram(position = 'identity', alpha = .5, bins = 50)+
  geom_vline(data = provisoes_col, aes(xintercept = PE, color= Distribuição))+
  #ylim(0,1000)+
  labs(title = "Distribuição dos Sinistros Agregados",
       subtitle = 'Caso Coletivo',
       x = 'Sinistros Agregados',
       y = 'Frequência') 

#Visualizando as provisões técnicas de cada distribuição
provisoes_col_plot <- ggplot(compara_col, aes(x = Severidades))+
  geom_histogram(alpha = .75, bins = 100)+
  geom_vline(data = provisoes_col, aes(xintercept = PE), colour = 'red')+
  geom_vline(data = provisoes_col, aes(xintercept = PP), colour = 'blue')+
  geom_vline(data = provisoes_col, aes(xintercept = U99.5), colour = 'green')+
  geom_vline(data = provisoes_col, aes(xintercept = U99.97), colour = 'dark green')+
  xlim(1000000,2800000)+
  labs(title = "Distribuição dos Sinistros Agregados",
       subtitle = 'Caso Coletivo',
       x = 'Sinistros Agregados',
       y = 'Frequência')+
  facet_grid(Distribuição~.) 

#Dispersão das distribuições
dispersao_col <- ggplot(compara_col, aes(y = Severidades, x = Distribuição, fill = Distribuição))+
  geom_boxplot()+
  labs(title = "Dispersão dos Sinistros Agregados",
       subtitle = "Caso Coletivo",
       x = 'Dispersão',
       colour = 'Distribuição')  

####### Franquias e Limites máximos

#Vetores que receberão cada cenário
X_Exp_lmi <- vector() 
X_Exp_frq <- vector()
X_Exp_lmi_frq <- vector()
X_norm_lmi <- vector()
X_norm_frq <- vector()
X_norm_lmi_frq <- vector()


#Vetores que receberão os Sinistros Agregados
Sinistros_frq <- vector() #Vetor vazio que irá receber a frequência de sinistros de cada simulação
SAg_exp_frq <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ Exp)
SAg_norm_frq <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ N)
SAg_exp_lmi <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ Exp)
SAg_norm_lmi <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ N)
SAg_exp_lmi_frq <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ Exp)
SAg_norm_lmi_frq <- vector() #Vetor vazio que irá receber os sinistros agregados de cada simulação (X ~ N)


#Funções para aplicar LMI e Franquia 
lmi <- function(X, n = 20000){
  ifelse(X >= n, X <- n, X <- X)
}

franquia <- function(X, n = 1000){
  ifelse(X <= n, x <- 0, X <- X - n)
}

#Loop para aplicar LMI e Franquia
for(i in 1:B){
  N_fq <- rpois(1, 200)      
  X_n <- rweibull(N_fq, 3.6, 10000)
  X_exp <- rweibull(N_fq, 1, 10000)
  
  ###Aplicando para o caso da normal
  X_norm_lmi <- sapply(X_n, lmi) #A função sapply() consegue replicar uma função, similar a um loop, mas com eficiência maior
  X_norm_frq <- sapply(X_n, franquia)
  X_norm_lmi_frq <- sapply(X_norm_frq, lmi)
  
  ###Aplicando para o caso da exponencial
  X_exp_lmi <- sapply(X_exp, lmi)
  X_exp_frq <- sapply(X_exp, franquia)
  X_exp_lmi_frq <- sapply(X_exp_frq, lmi)
  
  #Fazendo a varredura dos cenários para estimar os Sinistros Agregados
  SAg_norm_lmi[i] <- sum(X_norm_lmi)
  SAg_norm_frq[i] <- sum(X_norm_frq)
  SAg_norm_lmi_frq[i] <- sum(X_norm_lmi_frq)
  
  #Fazendo a varredura dos cenários para estimar os Sinistros Agregados
  SAg_exp_lmi[i] <- sum(X_exp_lmi)
  SAg_exp_frq[i] <- sum(X_exp_frq)
  SAg_exp_lmi_frq[i] <- sum(X_exp_lmi_frq)
} 

#Fazendo aproximações da Normal 
Z_exp_frq <- SAg_exp_frq - mean(SAg_exp_frq)/sd(SAg_exp_frq) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ Exp)
Z_N_frq <- SAg_norm_frq - mean(SAg_norm_frq)/sd(SAg_norm_frq) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ N)

Z_exp_lmi <- SAg_exp_lmi - mean(SAg_exp_lmi)/sd(SAg_exp_lmi) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados
Z_exp_lmi <- SAg_norm_lmi - mean(SAg_norm_lmi)/sd(SAg_norm_lmi) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados

Z_exp_lmi_frq <- SAg_exp_lmi_frq - mean(SAg_exp_lmi_frq)/sd(SAg_exp_lmi_frq) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ Exp)
Z_N_lmi_frq <- SAg_norm_lmi_frq - mean(SAg_norm_lmi_frq)/sd(SAg_norm_lmi_frq) #Normalizando (Média = 0, DP = 1) os Sinistros Agregados (X ~ N)

#Calculando as provisões dos cenários
provisoes_frq <- data.frame(Distribuição = c("Exponencial", "Normal"), #Criando dataframe com os valores das provisões
                          Tipo = c("C/ Franquia", "C/ Franquia"),
                          PE = c(mean(SAg_exp_frq), mean(SAg_norm_frq)), #E[SAg]
                          PP = c(quantile(SAg_exp_frq, probs = .75), quantile(SAg_norm_frq, probs = .75)), #Carregamento
                          DP = c(sd(SAg_exp_frq), sd(SAg_norm_frq)), #Desvio-padrão das distribuições
                          U99.5 = c(quantile(SAg_exp_frq, probs = .995), quantile(SAg_norm_frq, probs = .995)),
                          U99.97 = c(quantile(SAg_exp_frq, probs = .9997), quantile(SAg_norm_frq, probs = .9997)),
                          CABR99.5 = c(quantile(SAg_exp_frq, probs = .995) - quantile(SAg_exp_frq, probs = .75), quantile(SAg_norm_frq, probs = .995) - quantile(SAg_norm_frq, probs = .75)), #Capital de Solvência para alpha = 0.05  
                          CABR99.97 = c(quantile(SAg_exp_frq, probs = .9997) - quantile(SAg_exp_frq, probs = .75), quantile(SAg_norm_frq, probs = .9997) - quantile(SAg_norm_frq, probs = .75))) #Capital de Solvência para alpha = 0.003

provisoes_lmi <- data.frame(Distribuição = c("Exponencial", "Normal"), #Criando dataframe com os valores das provisões
                            Tipo = c("C/ LMI", "C/ LMI"),
                            PE = c(mean(SAg_exp_lmi), mean(SAg_norm_lmi)), #E[SAg]
                            PP = c(quantile(SAg_exp_lmi, probs = .75), quantile(SAg_norm_lmi, probs = .75)), #Carregamento
                            DP = c(sd(SAg_exp_lmi), sd(SAg_norm_lmi)), #Desvio-padrão das distribuições
                            U99.5 = c(quantile(SAg_exp_lmi, probs = .995), quantile(SAg_norm_lmi, probs = .995)),
                            U99.97 = c(quantile(SAg_exp_lmi, probs = .9997), quantile(SAg_norm_lmi, probs = .9997)),
                            CABR99.5 = c(quantile(SAg_exp_frq, probs = .995) - quantile(SAg_exp_frq, probs = .75), quantile(SAg_norm_frq, probs = .995) - quantile(SAg_norm_frq, probs = .75)), #Capital de Solvência para alpha = 0.05  
                            CABR99.97 = c(quantile(SAg_exp_frq, probs = .9997) - quantile(SAg_exp_frq, probs = .75), quantile(SAg_norm_frq, probs = .9997) - quantile(SAg_norm_frq, probs = .75))) #Capital de Solvência para alpha = 0.003

provisoes_lmi_frq <- data.frame(Distribuição = c("Exponencial", "Normal"),#Criando dataframe com os valores das provisões
                            Tipo = c("C/ Ambos", "C/ Ambos"),
                            PE = c(mean(SAg_exp_lmi_frq), mean(SAg_norm_lmi_frq)), #E[SAg]
                             PP = c(quantile(SAg_exp_lmi_frq, probs = .75), quantile(SAg_norm_lmi_frq, probs = .75)), #Carregamento
                            DP = c(sd(SAg_exp_lmi_frq), sd(SAg_norm_lmi_frq)), #Desvio-padrão das distribuições
                            U99.5 = c(quantile(SAg_exp_lmi_frq, probs = .995), quantile(SAg_norm_lmi_frq, probs = .995)),
                            U99.97 = c(quantile(SAg_exp_lmi_frq, probs = .9997), quantile(SAg_norm_lmi_frq, probs = .9997)),
                            CABR99.5 = c(quantile(SAg_exp_frq, probs = .995) - quantile(SAg_exp_frq, probs = .75), quantile(SAg_norm_lmi_frq, probs = .995) - quantile(SAg_norm_lmi_frq, probs = .75)), #Capital de Solvência para alpha = 0.05  
                            CABR99.97 = c(quantile(SAg_exp_frq, probs = .9997) - quantile(SAg_exp_frq, probs = .75), quantile(SAg_norm_lmi_frq, probs = .9997) - quantile(SAg_norm_lmi_frq, probs = .75))) #Capital de Solvência para alpha = 0.003

provisoes_finais <- rbind(provisoes_frq, provisoes_lmi)
provisoes_finais <- rbind(provisoes_finais, provisoes_lmi_frq)
#Dataframe para plotagens
compara_frq <- data.frame(Distribuição = c(rep("Exponencial", each = B*3), rep("Normal", each= B*3)),
                          Tipo = c(rep("C/ Franquia", each = B), rep("C/ LMI", each = B), 
                                   rep("C/ Ambos", each = B), rep("C/ Franquia", each = B), rep("C/ LMI", each = B), 
                                   rep("C/ Ambos", each = B)),
                          Severidades = c(SAg_exp_frq, SAg_exp_lmi, SAg_exp_lmi_frq,
                                          SAg_norm_frq, SAg_norm_lmi, SAg_exp_lmi_frq)) 

#Visualizando os sinistros juntos
sag_frq <- ggplot(compara_frq, aes(x = Severidades,  fill = Distribuição))+
  geom_histogram(position = 'identity', alpha = .5, bins = 70)+
  xlim(1100000,2500000)+
  ylim(0, 1000)+
  geom_vline(data = provisoes_finais, aes(xintercept = PE, color= Distribuição))+
  labs(title = "Distribuição dos Sinistros Agregados",
       x = 'Sinistros Agregados',
       y = 'Frequência')+ 
  facet_grid(Tipo~.)
  

#Visualizando os sinistros com provisões
premios_frq_plot <- ggplot(compara_frq, aes(x = Severidades))+
  geom_histogram(alpha = 1, bins = 70)+
    geom_vline(data = provisoes_finais, aes(xintercept = PE), colour = 'red')+
  geom_vline(data = provisoes_finais, aes(xintercept = PP), colour = 'blue')+
  geom_vline(data = provisoes_finais, aes(xintercept = U99.5), colour = 'green')+
  geom_vline(data = provisoes_finais, aes(xintercept = U99.97), colour = 'dark green')+
  labs(title = "Distribuição dos Sinistros Agregados",
       subtitle = 'Com Franquia e Limite',
       x = 'Sinistros Agregados',
       y = 'Frequência')+
  facet_grid(Distribuição~Tipo)+ #Visualizando as provisões técnicas de cada distribuição
  scale_x_discrete(guide = guide_axis(angle = 45))

dispersao_frq <- ggplot(compara_frq, aes(y = Severidades, x = Tipo, fill = Distribuição))+
  geom_boxplot()+
  labs(title = "Dispersão dos Sinistros Agregados",
       x = 'Dispersão',
       colour = 'Distribuição') #Dispersão das distribuições 





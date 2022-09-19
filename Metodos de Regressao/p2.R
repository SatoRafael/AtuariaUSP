#################### P2

#Bernoulli | Logit | 3.10 - 0.03x1 - 0.32x2 + 0.52x3
#Problema: probabilidade
exp(3.10 - 0.03*42 - 0.32*4.7 + 0.52*1)/(1 + exp(3.10 - 0.03*42 - 0.32*4.7 + 0.52*1))

#Poisson | Log | 0.2 + 0.05x1 + 0.25x2 - 0.3x3
#Problema: variação 
exp(-0.3) - 1

#Gama | Log | 4 + 0.0015x1 + 0.0035x2 + 0.008x3
#Problema: variação
exp(0.08) - 1

#Normal | Log | 5.5 + 0.002x1 + 0.004x2 + 0.01x3
#Problema: estimativa
exp(5.5 + 0.002*230 + 0.004*40 + 0.01*30)

#Normal Inversa | Log | 5.40 + 0.0025x1 + 0.0042x2 + 0.012x3
#Problema: variação
exp(0.12) - 1
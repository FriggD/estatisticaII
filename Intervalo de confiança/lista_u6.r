# IC PARA UMA VARIANCIA
# Considere como população todos os alunos de uma turma. Dessa população
# foram selecionados 10 alunos aleatoriamente e suas alturas foram medidas em cm.
# Essas medidas devem ser consideradas com comportamento semelhante a uma distribuição normal. 
# A partir dessa amostra, construa um IC com 90% de confiança para a variabilidade dessa turma
#

#c significa concatenar
x=c(170,171,157,168,177,183,190,172,176,169)

library(TeachingDemos) #executar a biblioteca antes

sigma.test(x, conf.level = 0.9) #42.79809 217.76705: tenho 90% de confiança de que o intervalo entre 42.79809 e 217.76705
# abrange o verdadeiro valor da variancia populacional

#-------------------------------------------------------------------------------
# IC DUAS VARIANCIAS
# Após notificação da ANP, o proprietario de uma rede de postos de combustiveis
# deseja saber se dois de seus postos estão com a mesma variabilidade em torno do valor 
# médio permitido, das massas especifcas (a 20°C) da mistura AEHC, metanol e gasolina em kg/m2. Com os dados abaixo 
# e tendo em vista a normalidade, estime o IC da razão de variancia das massas especificas com
# nivel de confiança de 90%.

p1=c(801.0,809.5,806.2,814.4,799.0,800.6,800.2,799.1,812.0)
p2=c(810.8,806.5,798.6,801.5,808.0,794.3,806.4,799.0,805.2,803.5)

v1=var(p1)
v2=var(p2)

var.test(p1,p2,conf.level = 0.9)
# 90 percent confidence interval:
# 0.441600 4.832096

#-------------------------------------------------------------------------------
# IC UMA MÉDIA COM VARIANCIA CONHECIDA
# Em uma amostra de tamanho 100, o tempo médio de processamento, em milisegundos(ms)
# num "sistema fortemente acoplado" foi de 40ms.
# Construir o intervalo de confiança para a media sabendo-se que
# desvio padrão = 10ms 
# Use alfa = 1%
# 

n=100
m=40
dp=10

library(BSDA)
zsum.test(m,dp,n,conf.level = 0.99)
# 99 percent confidence interval:
# 37.42417 42.57583

#-------------------------------------------------------------------------------
# IC PARA UMA MEDIA COM AMOSTRA PEQUENA E DP ESTIMADO NA AMOSTRA
# Deseja-se avaliar a dureza esperada μ do aço produzido sob um novo processo de
# têmpera. Uma amostra de dez corpos de prova do aço produziu os resultados de dureza, em HRc,
# apresentados abaixo. Construir um IC para a dureza média com 95% de confiança.

a=c(36.4, 35.7, 37.2, 36.5, 34.9, 35.2, 36.3, 35.8, 36.6, 36.9)

t.test(x=a)
# 95 percent confidence interval:
# 35.62405 36.67595

#-------------------------------------------------------------------------------
# Duas marcas de monitores para computador A e B, foram submetidas a um teste de
# durabilidade, com aferição na unidade dia. Sabe-se que o desvio padrão da durabilidade desses
# monitores é de 250 e 300, respectivamente. Com os dados abaixo, determine o IC (95%) e
# verifique se as duas marcas têm a mesma durabilidade.

mA=c(3412,3246,3183,3014,3531,3534,2738,3071,3546,3488,3395,3414,3429,3254,3683,2907,3410)
mB=c(2360,3090,2658,3297,3225,2754,2999,2788,2634,3232,3392,3505,3520,3322,3158,3382,3029)

library(BSDA)
z.test(mA,mB,sigma.x = 250, sigma.y = 300)
# 95 percent confidence interval:
# 44.36556 415.63444

#-------------------------------------------------------------------------------
#Duas marcas de HD para computador, A e B, foram submetidas a um teste de
#durabilidade, com aferição na unidade meses. Com os dados abaixo, determine o IC(95%) para
#a diferença entre as médias, das duas marcas de HD.

HD_a=c(42, 36, 48, 61, 53, 45, 53, 71, 46, 59, 67, 56, 62)
HD_b=c(51, 49, 65, 57, 55, 54, 49, 58, 63)

t.test(HD_a,HD_b,var.equal = T)








# 1. Em Illinois, uma amostra aleatória de 81 alunos da oitava série tem nota média de 272
# pontos com desvio padrão de 35 pontos, em um teste nacional de matemática. A secretaria da
# educação espera que a média de pontos dos alunos da seja de 290. Com 90% de confiança,
# aplique um teste de hipóteses para verificar se a média dos alunos está dentro do esperado
# pela secretaria da educação.

a = 81
media = 272
dp = 35
mediaEsp = 290
conf = 0.9

# tsum.test(mean.x, s.x = NULL, n.x = NULL, mean.y = NULL, s.y = NULL,
#   n.y = NULL, alternative = "two.sided", mu = 0, var.equal = FALSE,
#   conf.level = 0.95)

library(BSDA)
# tsum.test(272,35,81,mu=290,conf.level=0.9)

# amostra grande -> teste Z

zsum.test(272,35,81,alternative = "l",mu=290,conf.level=0.9)
# p-value = 3.682e-06

# 272 não é igual 290

# -----------------------------------------------------------------------------------------------

# 2. O reitor de uma universidade estima que o número médio de aulas dadas por professores de
# um curso integral todas as semanas seja de 22. Como membro do conselho estudantil, você
# quer testar essa afirmação. Uma amostra aleatória do número de horas em sala para 8
# professores do curso integral em uma semana é listada a seguir. Com = 5%, você pode
# concordar com a afirmação do reitor?
# 21,8 18,6 12,6 17,9 16,4 20,4 23,6 19,1
media = 22

qtdade = 8
horas = c(21.8, 18.6, 12.6, 17.9, 16.4, 20.4, 23.6, 19.1)

# média
mean(horas)

# tsum.test(mean.x, s.x = NULL, n.x = NULL, mean.y = NULL, s.y = NULL,
#   n.y = NULL, alternative = "two.sided", mu = 0, var.equal = FALSE,
#   conf.level = 0.95)

# desvio padrão
sd(horas, na.rm=FALSE)
sigma.test(horas)
tsum.test(18.8,s.x=3.37,n.x=8,mu=22)

# p-value = 0.03128

# -----------------------------------------------------------------------------------------------

# 3. Você entrevista uma amostra aleatória de 80 adultos. Os resultados da sua pesquisa
# mostram que 48% deles disseram que são mais propensos a comprarem um produto quando
# há amostra grátis. Com alfa = 0,05 você pode rejeitar a afirmação de que no mínimo 52% dos adultos são
# mais propensos a comprarem um produto quando há amostras grátis?

a = 80
maisP = 0.48
rejeitar = 0.52

# H0: 0.48 = 0.52
# H0: 0.48 < 0.52 

# uma população
x=80*0.48 # número de pessoas que disse sim
prop.test(x,80,0.52,alternative="less")
# prop.test(número de pessoas que disse "sim", quantidade total, valor que quero comparar, alternativa no mínimo)


# p-value = 0.2728
# maior que o nivel de significancia 
# aceita H0
# estatisticamente igual



# -----------------------------------------------------------------------------------------------

# 4. Um representante de um hospital afirma que o desvio padrão do tempo de espera que os
# pacientes passam no departamento de emergência do hospital não é maior que 8 minutos. Uma
# amostra aleatória de 28 tempos de espera tem um desvio padrão de 9 minutos. Com = 5% você
# pode rejeitar a afirmação do representante do hospital? E se testar com = 10%?

dp = 8

a = 28
dpa = 9

# H0: var = 64
# H1: var > 64
#  *TESTE PARA UMAVARIANCIA*
# = ((tamanho da amostra - 1)*variancia1)/variancia2
x1 = (27*9^2)/(8^2)
x1

qchisq(0.95,27)
# não rejeita H0

# compara o valor do qchisq() = 40.11, com o valor de x1 = 34.1718, se qchisq for maior, não rejeita H0, não entrou na região de rejeição

qchisq(0.90,27)
# ainda não entrou na região de rejeição
# Aceita H0

pchisq(x1,27,lower.tail = F)
# pchisq(limite que eu quero, grau de liberdade, lower.tail = F)
# lower.tail = F por que foi colocado maior no h1
# 0.1611077 > 0.05
# continua com H0

# desvio padrão de 9min, nesse caso, é estatisticamente igual à desvio padrão de 8min

# -----------------------------------------------------------------------------------------------

# 5. Você quer comprar um forno de micro-ondas e escolherá o Modelo A se os custos de reparo
# forem mais baixos que os custos de reparo do Modelo B. Você pesquisa os custos de reparo de
# 41 fornos do Modelo A e 61 fornos do Modelo B. Os resultados da sua pesquisa são mostrados
# na tabela abaixo. Utilizando um nível de significância de 5%, você compraria o modelo B?

# Modelo A Modelo B
# Média R$ 90,00 R$ 85,00
# Desvio
# padrão

# R$ 12,50 R$ 19,00
a = 41
b = 61
mediaA = 90
dpA = 12.5
mediaB = 85
dpB = 19

library(BSDA)
# tsum.test(media1, sigma1, qtdade1, media2, sigma2, qtdade2, var.equal=T/F, conf.level=0.95)
tsum.test(90,12.5,41,85,19,61,var.equal=F)
# p-value = 0.1415


# -----------------------------------------------------------------------------------------------

# 6. Uma associação de restaurantes diz que famílias nos Estados Unidos chefiadas por pessoas
# com menos de 25 anos de idade gastam menos com comida fora de casa do que famílias
# chefiadas por pessoas com idade entre 55 e 64 anos. Em uma amostragem, a quantia média
# gasta por 40 famílias chefiadas por pessoas com menos de 25 anos é de R$ 2115,00 e o desvio
# padrão é de R$ 103,00. A quantia média gasta por 47 famílias chefiadas por pessoas com idade
# entre 55 e 64 anos é de R$ 2798,00 e o desvio padrão é de R$ 107,00. Utilizando = 0,05, você
# pode apoiar a afirmação da associação de restaurantes?

a25 = 40
media25 = 2115
dp25 = 103

a55 = 47
media55 = 2798
dp55 = 107

library(BSDA)
tsum.test(2115,103,40,2798,107,47,var.equal=F) #p-value < 2.2e-16

# -----------------------------------------------------------------------------------------------

# 7. Segundo as especificações de uma máquina Y empacotadora de café, a variabilidade no
# enchimento dos pacotes é 30 gramas. Já uma máquina X, possui um desvio padrão de 27
# gramas. O engenheiro de produção dessa empresa precisa verificar se o peso médio para o
# enchimento dos pacotes é o mesmo nas duas máquinas, para isso selecionou aleatoriamente 32
# pacotes da máquina Y e 28 da máquina X, nessas amostras obteve um peso médio de 521
# gramas e 497 gramas, respectivamente. Utilizando 95% de confiança, a que conclusão o
# engenheiro pode chegar?

dpY = 30
dpX = 27
aY = 32
aX = 28
mediaY = 521
mediaX = 497

library(BSDA)
tsum.test(521,30,32,497,27,27,var.equal=F) # p-value = 0.002225

# -----------------------------------------------------------------------------------------------

# 8. Em uma amostra de 465 adultos que moram em uma área urbana, 377 disseram que usam a
# internet. Em uma pesquisa de 305 adultos que moram em uma área rural, 119 disseram que
# usam a internet. Com uma = 10%, você pode apoiar a afirmação de que a proporção de adultos
# que usam a internet é maior para os que moram em uma área urbana do que para os adultos
# que moram em área rural?

aU = 465
internetU = 377
aR = 305
internetR = 119

 
prop.test(c(377,119),c(465,305),conf.level=0.90, correct = F) #p-value < 2.2e-16

# -----------------------------------------------------------------------------------------------

# 9. Uma das maneiras de medir o grau de satisfação dos empregados de uma mesma categoria
# quanto à política salarial é por meio do desvio padrão de seus salários. A fábrica A diz ser mais
# coerente na política salarial do que a fábrica B. Para verificar essa afirmação, sorteou-se uma
# amostra de 12 funcionários não especializados de A, e 17 de B, obtendo-se os desvios padrões
# S A = 1100 e S B = 1600 reais. Qual seria a sua conclusão, com de 5%?

a = 12
b = 17
dpA = 1100
dpB = 1600
varA = 1100^2
varB= 1600^2

# Teste F: Divisão de variancias
# Variancias iguais: estatística do teste = 1
# Maior/menor > 1
# Menor/maior < 1 

# duas populações
# H0 : Va = Vb                      (H0 é sempre igual)
# H1: Va < Vb

Fcal = varA/varB
Fcal

qf(0.05,11,16)
# saí da região de rejeição
# aceita H0

pf(Fcal,11,16)
# 0,10 é maior que alfa


# ------------------------------------
# H0 : Va = Vb
# H1: Vb < Va

Fcal=varB/varA
qf(0.05,16,11, lower.tail=F)
pf(Fcal,16,11, lower.tail = F)



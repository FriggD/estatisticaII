# Uma grande empresa de equipamentos estima que a variância na vida de seus equipamentos seja de 4 dias². Você trabalha para um grupo de defesa do consumidor e lhe é pedido para testar essa afirmação. Você coleta uma amostra aleatória de 28 equipamentos da empresa e encontra uma variância de 3,2 dias². Com nível de significância de 5%, a que conclusão você chegaria? *
 
# Não é possível chegar a uma conclusão.
# A variância encontrada na amostra é estatisticamente menor que a afirmada pela empresa.
# [x] A afirmação da empresa pode ser considerada verdadeira, com 95% de confiança.
# A afirmação da empresa é falsa.

# H0: var = 4
# H1: var < 4

var = 4

a = 28
varA = 3.2

x2=(27*3.2)/4
x2 #21.6

qchisq(0.95,27)
#  40.11327 > 21.6
pchisq(x2,27)
#  0.2425847 > 0,05

#-------------------------------------------------------------------------------------------------------------------------------------------

# Um pesquisador declara que 25% dos adultos nos Estados Unidos têm medo de voar. Você quer testar essa afirmação. Você descobre que, em uma amostra aleatória de 1112 adultos nos Estados Unidos, 299 têm medo de voar. Com alfa de 7%, qual a sua conclusão sobre isso? *
 
# [x] Com 93% de confiança, pode-se afirmar que as duas proporções são iguais.
# Não é possível chegar a nenhuma conclusão.
# Com 93% de confiança, pode-se afirmar que as duas proporções são diferentes.
# Com 7% de confiança, pode-se afirmar que as duas proporções são diferentes.

# H0: 25% = 299
# H1: 25% <> 299

# prop.test(total de sucesso, total, proporção que eu quero comparar, "two.sided", "less", "greater", conf level)
prop.test(299,1112,0.25, alternative = "two.sided",conf.level=0.93, correct = F) # p-value = 0.1459 > 0.07
prop.test(299,1112,0.25, alternative = "g",conf.level=0.93, correct = F) # p-value = 0.07293
prop.test(299,1112,0.25, alternative = "l",conf.level=0.93, correct = F) #  p-value = 0.9271


# aceita H0

#-------------------------------------------------------------------------------------------------------------------------------------------

# Um ambientalista estima que a média de lixo reciclado por adultos nos Estados Unidos seja maior que 1,5 libras por pessoa por dia. Você quer testar essa afirmação. Você descobre que a média de lixo reciclado por pessoa ao dia em uma amostra aleatória de 12 adultos nos EUA é de 1,86 libras e o desvio padrão é 0,42 libras. Com alfa de 5%, é possível afirmar que o peso médio de lixo reciclado seja maior que 1,5 libras? *
 
# [x] Com 90% de confiança é possível afirmar que a média de lixo reciclado é maior que 1,5 libras.
# A média de lixo reciclado é estatisticamente igual a 1,5 libras.
# Com 95% de confiança, 1,86 libras pode ser considerado estatisticamente maior que 1,5 libras.
# Não é possível verificar essa afirmação.

mediaL = 1.5
mediaA = 1.86
a = 12
dp = 0.42

# tsum.test(mean.x, s.x = NULL, n.x = NULL, mean.y = NULL, s.y = NULL,
#   n.y = NULL, alternative = "two.sided", mu = 0, var.equal = FALSE,
#   conf.level = 0.95)

library(BSDA)
tsum.test(1.86, 0.42, 12, mu=1.5, alternative ="g",conf.level=0.95)
# p-value = 0.006381 < 0,05


#-------------------------------------------------------------------------------------------------------------------------------------------

# Deseja-se comparar dois analistas quanto à precisão (variabilidade) na análise de uma certa substância que contém carbono. O analista A é experiente, e o B é novo no serviço, sendo portanto, de experiência desconhecida. O analista A, analisou 15 amostras e apresentou uma variância de 1,75, já o analista B verificou 17 amostras e obteve variância igual a 5,49. Com 95% de confiança, aplique um teste de hipóteses para verificar se o analista B possui variância maior nas suas análises e em seguida marque as alternativas corretas. *
 
# [x]O analista A é mais preciso que o analista B.
# Os dois analistas têm a mesma precisão.
# O analista B é mais preciso que o analista A.
# Não é possível fazer essa verificação.

nA=15
nB=17
vA=1.75
vB=5.49

# H0: vA = Vb
# H1: vB > vA

# primeiro calcula a divisão das variancias para o Teste F

# F =  vB/vA
F = vB/vA
F # 3.137143
# aplica o qf ou pf
pf(F,16,14,lower.tail = F)
pf(F,16,14)
#  0.9813435 


qf(0.05,16,14,lower.tail = F)
qf(0.05,16,14)
# 0.421351
# ####

pf(F,14,16,lower.tail = F)
pf(F,14,16)

qf(0.05,14,16,lower.tail = F)
qf(0.05,14,16)


#-------------------------------------------------------------------------------------------------------------------------------------------

# A variabilidade no levantamento de impurezas de uma certa substância depende da duração do processo usado. Um químico utiliza dois processo, A e B. Ele melhorou o processo B, esperando com isso reduzir a variabilidade na medição das impurezas. Levantaram-se duas amostras, uma utilizando o processo A e outra o processo B, de tamanhos 21 e 13, respectivamente, obtendo-se S²A = 1,04 e S²B = 0,51. Aplique um teste de hipóteses adequado e em seguida marque as alternativas que achar corretas. Adote alfa de 5%. *
 
# [x] A melhoria feita no processo B realmente reduziu a variabilidade na medição.
# A melhoria feita no processo B não reduziu a variabilidade na medição.
# estatística do teste = 6,72
# limite da região de rejeição = 2,28

# H0: pA = pB
# H1: pA > pB

a=21
b=13
varA=1.04
varB=0.51

Fcal = varA/varB
Fcal
qf(0.05,20,12)
#  0.4390624
pf(Fcal, 20,12)
# 0.8968978

#-------------------------------------------------------------------------------------------------------------------------------------------

# O desejo é estudar se a proporção de homens que lêem revistas, e lembram-se de determinado anúncio, pode ser considerada maior que a proporção de mulheres que também lêem essas revistas e lembram-se do anúncio. A tabela a seguir apresenta os resultados de amostras aleatórias independentes de homens e mulheres, onde x1 é o número de homens que se lembram do anúncio, e x2 é o correspondente número de mulheres. Admita alfa de 10%. Qual seria a hipótese alternativa mais adequada para esta situação? *
 
# Imagem sem legenda
# H1: p1 ≠ p2
# H1: p1 = p2
# H1: p1 < p2
# H1: p1 > p2

prop.test(c(70,50),c(200,200),alternative="two.sided", correct = F) # p-value = 0.03817
prop.test(c(70,50),c(200,200),alternative="g", correct = F) # p-value = 0.01908
prop.test(c(70,50),c(200,200),alternative="l", correct = F) # p-value = 0.9809
# p-value = 0.03817

#-------------------------------------------------------------------------------------------------------------------------------------------

# Estão em teste dois processos para fechar latas de comestíveis. Em uma sequência de 1000 latas, o processo 1 gera 50 rejeições, enquanto o processo 2 acusou 210 rejeições em uma sequência de 1050 latas. Ao nível de 5%, aplique o teste de hipóteses adequado e marque a opção correta. *
 
# Estatística do teste = -8,09
# As duas proporções podem ser consideradas iguais.
# Devemos aceitar H0.
# [x] A proporção de rejeição do processo 1 não pode ser considerada igual a do processo 2.

a1=1000
rej1=50
a2=1050
rej2=210

prop.test(c(50,210),c(1000,1050),alternative="g",conf.level=0.95, correct = F)
prop.test(c(50,210),c(1000,1050),alternative="l",conf.level=0.95, correct = F)
# p-value < 2.2e-16
#-------------------------------------------------------------------------------------------------------------------------------------------

# Um supermercado não sabe se deve comprar lâmpadas da marca A ou B, de mesmo preço. Retirou-se uma amostra de 121 lâmpadas, de cada marca, obtendo os dados abaixo, em horas de funcionamento até queimar. Considere que um teste já foi aplicado e temos o resultado de que as variâncias das duas amostras podem ser consideradas diferentes. Aplique o teste de hipóteses adequado par verificar se a durabilidade média da marca A é menor que a de B, em seguida marque apenas as alternativas corretas. Use alfa de 5%. *
 
# Imagem sem legenda
# [x] A durabilidade da marca A é estatisticamente menor que a marca B.
# Estatística do teste = 1,6577
# Deve-se aceitar H0.
# A durabilidade das duas médias é a mesma.

a=121
mediaA=1360
dpA=70

b=121
mediaB=1440
dpB=80

library(BSDA)
# média, dp,amostra

zsum.test(mediaA,dpA,a,mediaB,dpB,b,alternative="l")

# tsum.test(mediaA,dpA,a,mediaB,dpA,b,var.equal=F)
# p-value < 2.2e-16

#-------------------------------------------------------------------------------------------------------------------------------------------

# Um fabricante de pneus produz dois tipos de pneumáticos. Já são conhecidos os desvios padrão da durabilidade para o tipo A, 2700 milhas, e para o tipo B, 2950 milhas. Um táxi testou 40 pneus do tipo A e 50 do tipo B, obtendo uma durabilidade média de 27500 milhas e 25000 milhas, respectivamente. Com 5% de significância, qual seria a sua conclusão se você quisesse verificar se existe diferença entre as durabilidades das duas marcas? Marque apenas as alternativas corretas. *
 
# Não é possível chegar a nenhuma conclusão.
# A durabilidade média das duas marcas de pneus pode ser considerada igual.
# Deve-se aceitar H0.
# [x] A durabilidade média das duas marcas de pneus podem ser consideradas diferentes.

sa=2700
sb=2000
ta=50
tb=40
mA=22000
mB=20000

library(BSDA)
zsum.test(mA,sa,ta,mB,sb,tb)
# p-value = 5.483e-05


#-------------------------------------------------------------------------------------------------------------------------------------------

# Em Illinóis, uma amostra aleatória de 85 alunos da oitava série teve nota média de 292 com desvio padrão de 35, em um teste nacional de matemática. O diretor dessa escola informou ao administrador do teste nacional que a nota média dos seus alunos de oitava série é maior do que o mínimo estipulado pelo estado, 287 pontos. Com alfa = 0,04, é possível confirmar essa informação? *

# A média dos 85 alunos é estatisticamente menor que 287.
# O diretor está correto em sua afirmação.
# [x] A média alcançada pelos alunos é estatisticamente maior que 287
# A afirmação do diretor é falsa.

a=85
media=292
dp=35
mediaEsperada > 287  

# H0: media = mediaEsperada
# H1: media > 287


zsum.test(292,35,85,alternative = "greater",mu=287,conf.level=0.96)
# p-value = 0.09391

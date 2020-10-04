# UMA VARIANCIA

# Os dados abaixo se referem ao comprimento (mm) de um componente de
# computador fabricado pela empresa JC. Uma montadora de computadores só aceita comprar
# esse componente, se ele tiver média de comprimento igual a 801,8 mm e desvio padrão de 6 mm.
# Tendo em vista estas condições, a montadora retirou uma amostra de um lote de componentes
# da empresa JC, que está apresentada abaixo. Verifique com um nível de significância de 5%, se a
# variabilidade encontrada na amostra não difere da especificação exigida pela montadora.

# 801.0 809.5 806.2 814.4 799.0 800.6 800.2 799.1 812.0 790.2

jc = c(801.0, 809.5, 806.2, 814.4, 799.0, 800.6, 800.2, 799.1, 812.0, 790.2)
sig=6
var=36
library(TeachingDemos)

sigma.test(jc, 6, alternative = "greater")

# 0.1533 > nível de significância de 5% => é maior que a região de rejeição => aceita H0
# p-value = 0.1533

# p-valor > alfa aceita H0
# aceitar H0 significa que a variancia é igual a 36, e se a variancia é igual a 36, a montyadora pode comprar da empresa jc
# a empresa fornece componentes dentro da variação especificado


# quantil q-quadrado
qchisq(0.1533, 9, lower.tail = F) #fornece a área e retorna um numero

# probabilidade do q-quadrado
pchisq(13.21074,9, lower.tail = F) #fornece o ponto e retorna a área | calcular o p-valor

# -------------------------------------------------------------------------------------------------------------------------

# DUAS VARIANCIAS

# Uma empresa montadora de computadores deseja *comparar a variabilidade* do
# comprimento (mm) de um determinado componente, fornecido por duas empresas diferentes.
# Com os dados abaixo e tendo em vista a normalidade, qual a conclusão que se pode chegar ao
# nível α = 0,05?
# Fornecedor - 1: Empresa JC

# 801, 809.5, 806.2, 814.4, 799, 800.6, 800.2, 799.1, 812 790.2

# Fornecedor - 2: Empresa WF

# 810.8, 806.5, 798.6, 801.5, 808.0, 794.3, 806.4, 799.0, 805.2, 803.5

jc = c(801, 809.5, 806.2, 814.4, 799, 800.6, 800.2, 799.1, 812, 790.2)
wf = c(810.8, 806.5, 798.6, 801.5, 808.0, 794.3, 806.4, 799.0, 805.2, 803.5)


# precisa saber qual variancia é a maior
var(jc)
var(wf)

var.test(jc, wf, alternative = "greater")

# p-valor > alfa =>aceitar H0
# p-value = 0.1423

# probabilidade
# estatistica do teste, grau de liberdade_1, grau de liberdade _2, lower.tail =T calcula pro lado menor
pf(2.099,9,9,lower.tail = F)

qf(0.05,9,9,lower.tail = F)

# -------------------------------------------------------------------------------------------------------------------------
# Teste de Hipótese para Uma Média Em Amostra Grande Ou Com Variância Conhecida

# Deseja-se estudar o tempo de resposta num sistema de rede local. Estudos
# anteriores, afirmam que o tempo de resposta ideal para uma consulta no sistema, é de 12
# milissegundos (ms) com desvio-padrão de 1 milissegundo. Foram monitorados 5 clientes da
# rede, aleatoriamente escolhidos, obtendo-se os seguintes tempos médios (em ms) de resposta
# para uma consulta no sistema: 12.9, 13.6, 14.6, 13.9, 14.3. Pergunta-se: o sistema continua
# trabalhando dentro do esperado? Use alfa de 5%.

a=c(12.9, 13.6, 14.6, 13.9, 14.3)

library(BSDA)

z.test(a, mu=12,sigma.x=1)
# p-value = 3.195e-05
# valor muito menor que a região de rejeição
# então rejeita H0
# a média é diferente de 12
# mean of x = 13.86

# p-valor
pnorm(4.1591,lower.tail = F)*2

qnorm(0.025)

# -------------------------------------------------------------------------------------------------------------------------
# Teste de Hipótese Para Uma Média em Amostras Pequenas e Variâncias Desconhecidas

# Deseja-se estudar a influência do tempo de resposta num sistema de rede local.
# Estudos anteriores, afirmam que o tempo de resposta ideal para uma consulta no sistema, é de
# 12 milissegundos (ms). Foram monitorados 5 clientes da rede, aleatoriamente escolhidos,
# obtendo-se os seguintes tempos médios (em ms) de resposta para uma consulta no sistema: 12.9,
# 13.6, 14.6, 13.9, 14.3. O sistema continua trabalhando dentro do esperado? Use alfa de 5%.

a=c(12.9, 13.6, 14.6, 13.9, 14.3)

# estimar a variancia na amostra
# amostra pequena < 30

library(stats)
# mu é o valor a ser comparado
t.test(a,mu=12)

# p-value = 0.003206 < região de significancia
# rejeita a igualdade
# a média é diferente de 12

pt(6.3205,4,lower.tail=F)*2
qt(0.025,4)

# -------------------------------------------------------------------------------------------------------------------------

# Teste de Hipóteses Para Duas Médias Quando as Variâncias São Conhecidas

# Duas marcas de monitores para computador, A e B, foram submetidas a um teste de
# durabilidade. Em estudos anteriores, as duas marcas verificaram que o desvio padrão da
# durabilidade é de 250 dias e 300 dias, respectivamente. Com os dados abaixo, testar ao nível de
# 5% de significância, se o monitor A tem durabilidade superior ao monitor B.

# conjunto de dados grande demais

library(BSDA)
z.test(a,b,altyernative = "g", sugma.x=250, sigma.y=300)

# p-valor 0,009527 < alfa
# rejeita H0
# a média a é maior que a média B
# a durabilidade do monitor A é maior que a durabilidade do monitor B

# -------------------------------------------------------------------------------------------------------------------------

# Teste de Hipóteses Para Duas Médias Quando as Variâncias São Desconhecidas e Consideradas Iguais

# Duas marcas de HD para computador, A e B, foram submetidas a um teste de
# durabilidade, que foi medida em meses. Com os dados abaixo, testar ao nível alfa = 5% se existe
# superioridade entre as marcas dos HD’s.

hdA = c(42,36,48,61,53,45,53,71,46,59,67,56,62)
hdB = c(51,49,65,57,55,54,49,58,63)

var.test(hdA, hdB)

# p-value = 0.1089 > alfa
# aceita H0
# variancias iguais

library(BSDA)

t.test(hdA, hdB,var.equal = T)
# médias iguais:
#  -9.763006  5.968134

# -------------------------------------------------------------------------------------------------------------------------
# Teste de Hipóteses Para Duas Médias Com Variâncias Desconhecidas e Consideradas Diferentes

# Duas marcas de HD para computador, X e Y, foram submetidas a um teste de
# durabilidade, que foi medida em meses. Com os dados abaixo, testar ao nível alfa = 5% se existe
# superioridade entre as marcas dos HD’s.

hdA = c(42,36,28,31,53,35,33,71,36,49,67,56,62)
hdB = c(61,59,65,57,55,54,49,58,63)

var.test(hdA,hdB, alternative="g")
# p-value = 0.002275
# p-valor<alfa
# rejeita H0
# variancias diferentes

t.test(hdA,hdB,var.equal=F)
# p-value = 0.01548 < alfa
# as médias são diferentes

# -------------------------------------------------------------------------------------------------------------------------
# Teste de Hipóteses Para Uma Proporção

# Num experimento para testar a eficiência do sistema inteligente “KNOW”, na
# aquisição de conhecimento sobre determinado assunto, elaboraram-se 60 questões do tipo
# certo-errado. Tendo em vista que a proporção mínima para que um sistema computacional de
# aquisição de conhecimento seja considerado eficiente é de 70%, o que se pode concluir ao nível
# de 1% de significância sobre o “KNOW”, uma vez que acertou 40 questões?

prop.test(40,60,0.7,"less",0.99, F)
# p-value = 0.2866 > alfa
# aceita H0

# -------------------------------------------------------------------------------------------------------------------------

# Teste de Hipótese Para Duas Proporções

# Numa pesquisa de opinião pública, realizada em uma rede social sobre a utilização
# dos atuais navegadores, foram entrevistadas 2660 pessoas que responderam a seguinte
# pergunta: “Você acha que os atuais navegadores para internet atendem satisfatoriamente as
# necessidades do usuário para uma navegação segura?”. Diante do fato de que 1250 dos
# entrevistados eram do sexo masculino, dos quais 670 responderam “SIM” e que 1410 eram do
# sexo feminino, dos quais 770 também responderam afirmativamente, com 99% de certeza, o que
# se poderia concluir sobre as opiniões de homens e mulheres sobre a segurança dos navegadores?

prop.test(c(670,770),c(1250,1410),conf.level=0.99, correct = F)
# p-value = 0.6019 > alfa, aceita H0
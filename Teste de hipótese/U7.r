# Uma grande empresa de equipamentos estima que a variância na vida de seus equipamentos seja de 4 dias². Você trabalha para um grupo de defesa do consumidor e lhe é pedido para testar essa afirmação. Você coleta uma amostra aleatória de 28 equipamentos da empresa e encontra uma variância de 3,2 dias². Com nível de significância de 5%, a que conclusão você chegaria? *
 
# Não é possível chegar a uma conclusão.
# A variância encontrada na amostra é estatisticamente menor que a afirmada pela empresa.
# A afirmação da empresa pode ser considerada verdadeira, com 95% de confiança.
# A afirmação da empresa é falsa.

var = 4
a = 28
varA = 3.2
nivelSig = 0.05

library(BSDA)

zsum.test(a,mu=varA)
#-------------------------------------------------------------------------------------------------------------------------------------------

# Um pesquisador declara que 25% dos adultos nos Estados Unidos têm medo de voar. Você quer testar essa afirmação. Você descobre que, em uma amostra aleatória de 1112 adultos nos Estados Unidos, 299 têm medo de voar. Com alfa de 7%, qual a sua conclusão sobre isso? *
 
# Com 93% de confiança, pode-se afirmar que as duas proporções são iguais.
# Não é possível chegar a nenhuma conclusão.
# Com 93% de confiança, pode-se afirmar que as duas proporções são diferentes.
# Com 7% de confiança, pode-se afirmar que as duas proporções são diferentes.

#-------------------------------------------------------------------------------------------------------------------------------------------

# Um ambientalista estima que a média de lixo reciclado por adultos nos Estados Unidos seja maior que 1,5 libras por pessoa por dia. Você quer testar essa afirmação. Você descobre que a média de lixo reciclado por pessoa ao dia em uma amostra aleatória de 12 adultos nos EUA é de 1,86 libras e o desvio padrão é 0,42 libras. Com alfa de 5%, é possível afirmar que o peso médio de lixo reciclado seja maior que 1,5 libras? *
 
# Com 90% de confiança é possível afirmar que a média de lixo reciclado é maior que 1,5 libras.
# A média de lixo reciclado é estatisticamente igual a 1,5 libras.
# Com 95% de confiança, 1,86 libras pode ser considerado estatisticamente maior que 1,5 libras.
# Não é possível verificar essa afirmação.

#-------------------------------------------------------------------------------------------------------------------------------------------

# Deseja-se comparar dois analistas quanto à precisão (variabilidade) na análise de uma certa substância que contém carbono. O analista A é experiente, e o B é novo no serviço, sendo portanto, de experiência desconhecida. O analista A, analisou 15 amostras e apresentou uma variância de 1,75, já o analista B verificou 17 amostras e obteve variância igual a 5,49. Com 95% de confiança, aplique um teste de hipóteses para verificar se o analista B possui variância maior nas suas análises e em seguida marque as alternativas corretas. *
 
# O analista A é mais preciso que o analista B.
# Os dois analistas têm a mesma precisão.
# O analista B é mais preciso que o analista A.
# Não é possível fazer essa verificação.

#-------------------------------------------------------------------------------------------------------------------------------------------

# A variabilidade no levantamento de impurezas de uma certa substância depende da duração do processo usado. Um químico utiliza dois processo, A e B. Ele melhorou o processo B, esperando com isso reduzir a variabilidade na medição das impurezas. Levantaram-se duas amostras, uma utilizando o processo A e outra o processo B, de tamanhos 21 e 13, respectivamente, obtendo-se S²A = 1,04 e S²B = 0,51. Aplique um teste de hipóteses adequado e em seguida marque as alternativas que achar corretas. Adote alfa de 5%. *
 
# A melhoria feita no processo B realmente reduziu a variabilidade na medição.
# A melhoria feita no processo B não reduziu a variabilidade na medição.
# estatística do teste = 6,72
# limite da região de rejeição = 2,28

#-------------------------------------------------------------------------------------------------------------------------------------------

# O desejo é estudar se a proporção de homens que lêem revistas, e lembram-se de determinado anúncio, pode ser considerada maior que a proporção de mulheres que também lêem essas revistas e lembram-se do anúncio. A tabela a seguir apresenta os resultados de amostras aleatórias independentes de homens e mulheres, onde x1 é o número de homens que se lembram do anúncio, e x2 é o correspondente número de mulheres. Admita alfa de 10%. Qual seria a hipótese alternativa mais adequada para esta situação? *
 
# Imagem sem legenda
# H1: p1 ≠ p2
# H1: p1 = p2
# H1: p1 < p2
# H1: p1 > p2

#-------------------------------------------------------------------------------------------------------------------------------------------

# Estão em teste dois processos para fechar latas de comestíveis. Em uma sequência de 1000 latas, o processo 1 gera 50 rejeições, enquanto o processo 2 acusou 210 rejeições em uma sequência de 1050 latas. Ao nível de 5%, aplique o teste de hipóteses adequado e marque a opção correta. *
 
# Estatística do teste = -8,09
# As duas proporções podem ser consideradas iguais.
# Devemos aceitar H0.
# A proporção de rejeição do processo 1 não pode ser considerada igual a do processo 2.

#-------------------------------------------------------------------------------------------------------------------------------------------

# Um supermercado não sabe se deve comprar lâmpadas da marca A ou B, de mesmo preço. Retirou-se uma amostra de 121 lâmpadas, de cada marca, obtendo os dados abaixo, em horas de funcionamento até queimar. Considere que um teste já foi aplicado e temos o resultado de que as variâncias das duas amostras podem ser consideradas diferentes. Aplique o teste de hipóteses adequado par verificar se a durabilidade média da marca A é menor que a de B, em seguida marque apenas as alternativas corretas. Use alfa de 5%. *
 
# Imagem sem legenda
# A durabilidade da marca A é estatisticamente menor que a marca B.
# Estatística do teste = 1,6577
# Deve-se aceitar H0.
# A durabilidade das duas médias é a mesma.

#-------------------------------------------------------------------------------------------------------------------------------------------

# Um fabricante de pneus produz dois tipos de pneumáticos. Já são conhecidos os desvios padrão da durabilidade para o tipo A, 2700 milhas, e para o tipo B, 2950 milhas. Um táxi testou 40 pneus do tipo A e 50 do tipo B, obtendo uma durabilidade média de 27500 milhas e 25000 milhas, respectivamente. Com 5% de significância, qual seria a sua conclusão se você quisesse verificar se existe diferença entre as durabilidades das duas marcas? Marque apenas as alternativas corretas. *
 
# Não é possível chegar a nenhuma conclusão.
# A durabilidade média das duas marcas de pneus pode ser considerada igual.
# Deve-se aceitar H0.
# A durabilidade média das duas marcas de pneus podem ser consideradas diferentes.

#-------------------------------------------------------------------------------------------------------------------------------------------

# Em Illinóis, uma amostra aleatória de 85 alunos da oitava série teve nota média de 292 com desvio padrão de 35, em um teste nacional de matemática. O diretor dessa escola informou ao administrador do teste nacional que a nota média dos seus alunos de oitava série é maior do que o mínimo estipulado pelo estado, 287 pontos. Com alfa = 0,04, é possível confirmar essa informação? *

# A média dos 85 alunos é estatisticamente menor que 287.
# O diretor está correto em sua afirmação.
# A média alcançada pelos alunos é estatisticamente maior que 287
# A afirmação do diretor é falsa.
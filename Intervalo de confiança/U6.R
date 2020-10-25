

# Usuários de uma rede de transmissão de energia elétrica têm reclamado da alta variação na tensão.
# A empresa encarregada da transmissão de energia elétrica na região instalou novos transformadores,
# e em seguida fez 30 observações, em locais aleatoriamente escolhidos. A partir dessa amostra,
# com 90% de confiança, foi determinado o seguinte IC para a variância da tensão: [59,76; 123,94].
# Sabendo que a variação na tensão deve ser no máximo de 50V²,
# assinale as afirmações que julgar verdadeiras. Escolha uma ou mais alternativas: 

#[59,76; 123,94]

# ---------------------------------------------------------------------------------------------------

# Deseja-se comparar dois analistas quanto à precisão (variabilidade) na análise de uma certa substância que contém carbono.
# O analista A é experiente, e o B é novo no serviço, sendo, portanto, de experiência desconhecida. 
# O analista A, analisou 15 amostras e apresentou uma variância de 1,75, 
# já o analista B verificou 17 amostras e obteve variância igual a 5,49. 
# A fim de verificar se os dois analistas têm a mesma experiência no trabalho,
# construa um IC com 90% de confiança

a = 15
b = 17
varA = 1.75
varB = 5.49

fi = qf(0.05,14,16)
fs = qf(0.05,14,16, lower.tail = F)
li = varA/(fs*varB)
ls = varA/(fi*varB)

# ---------------------------------------------------------------------------------------------------

# Um fabricante de pneus produz dois tipos de pneumáticos. Já são conhecidos os desvios padrões da durabilidade para o tipo A, 
# 2700 milhas, e para o tipo B, 2000 milhas. Um táxi testou 50 pneus do tipo A e 40 do tipo B, obtendo uma durabilidade média
# de 22000 milhas e 20000 milhas, respectivamente. Adotando um risco alfa de 4%,
# calcule o erro da estimativa da diferença entre as duas médias.


dpA = 2700^2
dpB = 2000^2
totA = 50
totB = 40
mA = 22000
mB = 20000

sp2 =((totA-1)*dpA)+((totB-1)*dpB)/(totA+totB-2)
sp=sqrt(sp2)
sp*sqrt((1/totA)+(1/totB))

library(BSDA)
zsum.test(mA, dpA,totA, mB, dpB, totB, conf.level = 0.96)

media = (981.7878 + 3018.2122)/2
dif = media - 981.7878
dif2 = 3018.2122 - media

# ---------------------------------------------------------------------------------------------------

# Um supermercado não sabe se deve comprar lâmpadas da marca A ou B, de mesmo preço.
# Retirou-se uma amostra de 100 lâmpadas, de cada marca, obtendo os dados abaixo,
# em horas de funcionamento até queimar. Considere que um teste já foi aplicado
# e temos o resultado de que as variâncias das duas amostras podem ser consideradas diferentes.
# Determine o IC, com 95% de confiança, para a diferença entre a durabilidade média das duas marcas de lâmpada.

mediaA = 1360
mediaB = 1440
dpA = 70
dpB = 80

library(BSDA)
tsum.test(mediaA,dpA,100,mediaB,dpB,100)

# -100.9651  -59.0349
# A média é diferente, pois não abrange o numero zero no intervalo

# ---------------------------------------------------------------------------------------------------

# Suponha que estejamos interessados em estimar a proporção de consumidores de um certo produto.
# Se a amostra de tamanho 300 forneceu 100 indivíduos que consomem o dado produto,
# determine o IC para "p" com 95% de confiança

tam = 300
cont = 100
prop.test(cont,tam, correct = F)

# 0.2823934 0.3884875

# ---------------------------------------------------------------------------------------------------

# Para estimar a porcentagem de alunos favoráveis à modificação do currículo escolar,
# tomou-se uma amostra de 100 alunos, dos quais 80 foram favoráveis.
# Com 96% de confiança encontrou-se o intervalo [71,8%; 88,2%]. A seguir assinale as alternativas que julgar correta.

tot = 100
cont = 80
 prop.test(cont, tot)

media =  (0.7056770+0.8707518)/2
dif = media - 0.7056770
# 0.0825374 => 8,2%
# ---------------------------------------------------------------------------------------------------

# Estão em teste dois processos para fechar latas de comestíveis.
# Em uma sequência de 1000 latas, o processo 1 gera 50 rejeições, enquanto o processo 2 acusa 200 rejeições. Ao nível de 5%,
# construa um IC a fim de verificar se os dois processos podem ser considerados diferentes.

tot = 1000
rej1 = 50
rej2 = 200

prop.test(c(50,200),c(1000,1000))

# -0.179233 -0.120767
# como o zero não está incluído, são estatisticamente diferentes os dois processos
# Sendo a primeira média menor do que a segunda média



# ---------------------------------------------------------------------------------------------------

# Deseja-se estudar a igualdade entre as proporções de homens e mulheres que lêem revistas e lembram-se de determinado anúncio.
# Para isso determinou-se um IC, com 95% de confiança, para a diferença entre as proporções, sendo ele: [0,04; 0,07].
# A seguir marque apenas as alternativas ERRADAS. Escolha uma ou mais alternativas: 

# São estatísticamente diferentes, pois não há 0 no intervalo

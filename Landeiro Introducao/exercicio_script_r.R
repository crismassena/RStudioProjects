# Vetor com 4 palavras aleatórias
words<-c('Blues', 'Beer', 'Guittar', 'Night')
# Criando amostragem com 1000 valores aleatorios
amostra<-sample(words, 1000, replace = TRUE)
amostra
# Descobrindo quantas vezes cada palavra foi sorteada
table(amostra)
# Gráfico simples com os resultados
barplot(table(amostra))
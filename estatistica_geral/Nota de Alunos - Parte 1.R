#-----Tabela de Frequencia para Gênero-----#
library(readxl)
Nota_Alunos <- read_excel("Nota de Alunos - Parte 1.xlsx")
freq_genero <- table(Nota_Alunos$Genero)
freq_genero
prop_genero <- prop.table(freq_genero)
perc_genero <- round(prop_genero*100, digits = 2)
coluna_freq <- c(freq_genero, sum(freq_genero))
coluna_perc <- c(perc_genero, sum(perc_genero))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq, coluna_perc)
tabela_freq

#-----Tabela de Frequencia para Conceito-----#
freq_conceito <- table(Nota_Alunos$Conceito)
freq_conceito
prop_conceito <- prop.table(freq_conceito)
perc_conceito <- round(prop_conceito*100, digits = 2)
coluna_freq <- c(freq_conceito, sum(freq_conceito))
coluna_perc <- c(perc_conceito, sum(perc_conceito))
names(coluna_freq)[length(coluna_freq)]<-"Total"
tabela_freq <- cbind(coluna_freq, coluna_perc)
tabela_freq

#-----Tabela de Frequencia para Nota final-----#
intervalos <- cut(Nota_Alunos$Nota_Final, breaks =0:10, right = F)
freq_notas <- table(intervalos)
freq_notas
prop_notas <- prop.table(freq_notas)
perc_notas <- round(prop_notas*100, digits = 2)
coluna_freq <- c(freq_notas, sum(freq_notas))
coluna_perc <- c(perc_notas, sum(perc_notas))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq, coluna_perc)
tabela_freq

#=======================================================#
#-----Gráfico de Pizza-----#
rotulos <- paste(perc_genero, "%", sep="")
pie(freq_genero,main="Gráfico de Pizza: Gênero dos Alunos", 
    labels= rotulos, col = rainbow(7))
legend(1,1,names(freq_genero), col= rainbow(7), pch = 15)

#-----Gráfico de Barras ou Colunas-----#
barplot(freq_conceito)
barplot(freq_conceito, horiz = T)
freq_cruzada <- table(Nota_Alunos$Genero, Nota_Alunos$Conceito)
#freq_cruzada
barplot(freq_cruzada, beside = T, main = "Conceito vs Gênero", 
        ylab = "Número de Aluno", col = c("darkblue", "red"))
legend(1, 30, rownames(freq_cruzada), col = c("darkblue", "red"), pch = 15)

#-----Histograma para Nota Final-----#
hist(Nota_Alunos$Nota_Final, breaks = 0:10, right = F, col = "green",
     xlab = "Notas", ylab = "Frequencia", main = "Distribuição de Notas")

#-----Gráfico de séries-----#
plot(Nota_Alunos$Prova_1, type = "l", xlab = "ID Aluno", ylab = "Nota")
lines(Nota_Alunos$Prova_2, col = "blue")
lines(Nota_Alunos$Prova_3, col = "red")

#-----Gráfico de Caixa-----#
boxplot(Nota_Alunos$Nota_Final ~ Nota_Alunos$Disciplina,
        main = "Nota final por Disciplina",
        xlab = "Disciplina", col = c("orange", "green"))

#=======================================================#
#-----Medidas de tendencia Central-----#
#-----Media Aritmetica-----#
media1<-mean(Nota_Alunos$Prova_1)
media1
media2<-mean(Nota_Alunos$Prova_2)
media2
media3<-mean(Nota_Alunos$Prova_3)
# Existe um valor nulo na coluna, esse resultado dessa forma vem NA
media3 
# na.rm=TRUE faz o Na ser desconsiderado no calculo da média
media3<-mean(Nota_Alunos$Prova_3, na.rm=TRUE)
media3
mediatotal<-mean(media1, media2, media3)
mediatotal

#-----Mediana-----#
# É o valor central do conjunto de dados quando os dados estão ordenados de maneira crescente ou decrescente.
# Deixa 50% dos dados abaixo dela e 50% dos dados acima dela.
# Não é afetado por dados extremos

median(Nota_Alunos$Prova_1)
median(Nota_Alunos$Prova_2)
median(Nota_Alunos$Prova_3, na.rm=TRUE)

#-----Moda-----#
# Valor mais frequente, Amostra Bimodal, Amostra multimodal
# Medida mais fraca e deve ser aplicada com cuidado

tabela_freq <- table(Nota_Alunos$Prova_1)
subset(tabela_freq,
       tabela_freq == max(tabela_freq))

#-----Média Ponderada-----#
# Cada elemento amostral tem seu peso específico, diferente da média aritmética
# Exemplo com notas de alunos e pesos, wt sendo os pesos e x as notas
 
wt <- c(5, 5, 4, 1)
x <- c(3.7, 3.3, 3.5, 2.8)
xm <- weighted.mean(x, wt)
m <- mean(x)
m
xm

#=======================================================#
#-----Medidas Separatrizes-----#
#-----Separatrizes-----#

# Divide o conjunto de dados ordenados em partes (menor >> maior)

# Percentil: divide a amostra em 100 partes
#-----Percentil 35-----#
quantile(Nota_Alunos$Prova_1, .35)

# Decil: divide a amotra em 10 partes
#-----Decil 2-----#
quantile(Nota_Alunos$Prova_1, .20)

# Quartil: divide a amostra em 4 partes
#------Quartil 3-----#
quantile(Nota_Alunos$Prova_1, .75)

#=======================================================#
#-----Medidas de Dispersão-----#
#-----Amplitude-----#
# Diferença entre o valor máximo e o mínimo da amostra
# Leva em consideração apenas as extremidades
# AMPLITUDE = XMÁXIMO-XMÍNIMO
# diff calcula a diferença entre duas amostras
# range retorna o mínimo e o máximo de uma amostra

diff(range(Nota_Alunos$Prova_1))

#-----Variância e Desvio-padrão-----#
# Variação dos valores em torno da média amostral
# Media de variabilidade mais usada
# Levam em consideração todos os dados

# variancia
var(Nota_Alunos$Prova_1)
# desvio padrão
sd(Nota_Alunos$Prova_1)

# variancia populacional
n <- length(Nota_Alunos$Prova_1)
n
dpd <- ((n-1)/n)*var(Nota_Alunos$Prova_1)
dpd
# desvio padrão populacional
sqrt(dpd)
     
#-----Coeficiente de Variação-----#
# Apesar do desvio padrão ser mais utilizado, ele mede a dispersão em termos absolutos.
# O Coeficiente de Variação (C.V) mede a dispersão em termos relativos.
# x_barra -> média / s -> desvio padrão

x_barra <- mean(Nota_Alunos$Prova_1)
s <- sd(Nota_Alunos$Prova_1)
CV <- s*100/x_barra
CV

#-----Sumarização-----#
# Função no R que traz as principais medidas estatística de um conjunto de dado ou dataset.

summary(Nota_Alunos$Prova_1)
summary(Nota_Alunos$Prova_2)
summary(Nota_Alunos$Prova_3)
summary(Nota_Alunos$Nota_Final)
summary(Nota_Alunos)

#-----Tabela de Frequencia para Gênero-----#
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


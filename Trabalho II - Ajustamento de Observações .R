# Universidade Federal do Pampa 
# Engenharia Florestal  

# Disciplina: Ajustamento de Observações Geodésicas 

# Trabalho 2 - Teoria dos Erros;
# Data: 16/10/2018

# definindo a pasta do arquivo; 

setwd('~/Faculdade/SEMESTRES/SEMESTRE 5 - TODOS ARQUIVOS/AJUSTAMENTO DE OBSERVAÇÕES  - TRABALHOS/Trabalho II - Ajustamento de Observações .R')
getwd()

# criando objetos (vetores) com as medidas obtidas entre os dois pontos; 

n1 <- c(2.378)

n2 <- c(2.334)

n3 <- c(2.299)

n4 <- c(5.300)

n5 <- c(2.301)

n6 <- c(2.323)

n7 <- c(2.331)

# obtendo a medida de todos pontos; 

vmp <- sum(n1,n2,n3,n4,n5,n6,n7)

vmp

# i) Valor mais provável;

N <- c(n1,n2,n3,n4,n5,n6,n7)
N
vmp <- mean(N)
vmp
summary(N)
  

## ii) Variância; 

variancia <- var(N)
variancia

## iii) Erro absoluto aparente de cada medida 

EAA1 <- abs(n1-vmp)
EAA1 
EAA2 <- abs(n2-vmp)
EAA2 
EAA3 <- abs(n3-vmp)
EAA3 
EAA4 <- abs(n4-vmp)
EAA4
EAA5 <- abs(n5-vmp)
EAA5
EAA6 <- abs(n6-vmp)
EAA6
EAA7 <- abs(n7-vmp)
EAA7

# iv) Erro verdadeiro aparente de cada medida
EvaN1 <- n1-vmp
EvaN2 <- n2-vmp
EvaN3 <- n3-vmp
EvaN4 <- n4-vmp
EvaN5 <- n5-vmp
EvaN6 <- n6-vmp
EvaN7 <- n7-vmp

# v) Desvio Padrão

sdN <- sd(N)
sdN
sdN == sqrt(variancia)


# Existe erro grosseiro? SIM.

erros_absolutos <- c(EAA1, EAA2, EAA3, EAA4, EAA5,EAA6, EAA7)
erros_absolutos
media_dos_erros <- mean(erros_absolutos)
media_dos_erros

desvio_padrao_erros <- sd(erros_absolutos)
desvio_padrao_erros
grosseiro <- desvio_padrao_erros*3
grosseiro

EAA1 > grosseiro # N1 não possui erros grosseiros 
# considerando somente erros
EAA2 > grosseiro # N2  não possui erros grosseiros 
# considerando somente erros
EAA3 > grosseiro # N3 não possui erros grosseiros 
# considerando somente erros
EAA4 > grosseiro # N4  possui erros grosseiros 
# considerando somente erros
EAA5 > grosseiro # N5 n possui erros grosseiros 
# considerando somente erros
EAA6 > grosseiro
EAA7 > grosseiro 

# Portanto, foram encontrados erros grosseiros em apenas uma das observações, no desnível 4. (n4). 

# plotando os erros; 

erros_em_ordem_crescente <- sort(erros_absolutos)

histograma <- hist(erros_em_ordem_crescente, 
                   main = "Histograma de distribuição dos Erros Absolutos",
                   xlab = "Erro (m)",
                   ylab = "Frequência de erros")

xfit <- seq(min(erros_em_ordem_crescente),
            max(erros_em_ordem_crescente))

yfit <- dnorm(xfit, mean=media_dos_erros, sd=desvio_padrao_erros)

yfit <- yfit*diff(histograma$mids[1:2])*length(erros_em_ordem_crescente)


lines(xfit, yfit, col="green", lwd=2)
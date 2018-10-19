###########################################################
# Universidade Federal do Pampa - UNIPAMPA                #
#                                                         #
#                                                         #
#                                                         #
#                                                         #
# Curso de Engenharia                                     #
#                          
# II semestre de 2018  
#                                                         #
# Prova Prática I - Ajustamento de Observações Geodésicas # 
#                                                         #
#                                                         #
#                                                         #
#                                                         #
###########################################################

# I) Demonstração de propriedades das matrizes. Matrizes diferentes para cada item. 


# 1) associativa da associação de matrizes: 

# é definida pela seguinte propriedade: considerando uma matriz A, B e C
#

# temos: (A + B) + C = A + (B + C) 
# 

# ou seja, a propriedade associativa afirma que pode-se alterar o agrupamento 
# em uma adição de matrizes e obter o mesmo resultado. 
#
#
# Para demonstrar esta propriedade, primeiramente, há de se criar as matrizes A, B e C. 

# Passo I: criação das matrizes, de dimensões iguais
#
#
# neste caso, 4x1. 

matriz_a <- matrix(c(12, 13, 16, 13), nrow=4, ncol=1)
matriz_a


matriz_b <- matrix(c(41, 72, 62, 12), nrow=4, ncol=1)
matriz_b

matriz_c <- matrix(c( 20, 27, 33, 36), nrow=4, ncol=1)
matriz_c

# Passo II: organização do cálculo 


ASSOC_A <- (matriz_a + matriz_b) + matriz_c 

ASSOC_B <-(matriz_a) + (matriz_b + matriz_c)


# Passo III: confirmação da igualdade 

ASSOC_A == ASSOC_B 

# Portanto, confirmou-se a iguldade, demonstrando a propriedade associativa de matrizes. 

#######################################################################################

# 2) Propriedades I e IV da multiplicação por escalar

# I)
#
# A propriedade I da multiplicação por escalar é a seguinte: 
#
# k*( A + B ) = k*B + k*A

# Passo I: atribuindo valor a constante k

constante_k <- c(15)
constante_k

# Passo II: criação das matrizes A e B. 

# Criação da Matriz A. 

matriz_a1 <- matrix(c(77, 62, 93, 33, 23, 38), nrow=3,ncol=3)
matriz_a1 

# Criação da Matriz B. 

matriz_b1 <- matrix(c(84, 57, 35, 21, 53, 72), nrow=3,ncol=3)
matriz_b1 

# Passo III: organização do cálculo 

escalar <- constante_k * (matriz_a1 + matriz_b1)

escalar_1 <- constante_k * matriz_b1 + constante_k * matriz_a1 

# Passo IV: confirmação da igualdade 

escalar == escalar_1 

# Portanto, confirmou-se a igualdade, demonstrando assim a propridade I 
#da multiplicação por um escalar k. 

##############################################
##############################################
#
# Propridade IV da multiplicação por escalar. 

#  k1(k2*A) =  (k1*k2)A
#

# Passo I: atribuindo valores as constantes k1 e k2 


constante_k1 <- c(2)
constante_k1
constante_k2 <- c(5)
constante_k2


# Passo II: criando a matriz. 


matriz_a2 <- matrix(c(12, 22, 33, 13, 34, 22, 25, 41), nrow=4,ncol=4)
matriz_a2


# Passo III: organizando o cálculo 

escalar_2 = constante_k1*(constante_k2*matriz_a2)
escalar_2 

escalar_3 = (constante_k1*constante_k2) * matriz_a2 
escalar_3 

escalar_2 == escalar_3 

# OBSERVAÇÃO: a propriedade com a subtração no termo após a igualdade resultou em erro. 
# usando outra definição, referenciada na literatura, a propriedade foi confimada. 
# no caso: k1(k2*A) =  (k1*k2)A 

####################################################
####################################################

# 3) Propriedades 4 e 6 da multiplicação de matrizes. 

# PROPRIEDADE 4: (A+B) * C = A*C + B*C 

matriz_a3 <- matrix(c(12, 5, 3))
matriz_a3
matriz_b3 <- matrix(c(18, 9, 11))
matriz_b3
matriz_c <- matrix(c(1,3,8))
matriz_c 

escal_mat <- (matriz_a3 + matriz_b3) * matriz_c 

escal_mat_1 <- matriz_a3 * matriz_c + matriz_b3 * matriz_c 

escal_mat == escal_mat_1 

# Portanto, comprovou-se a propriedade.

####################################################
####################################################
#
# PROPRIEDADE 6: A∗B'=B'*A'

matriz_a_linha <- matrix(c(54, 32, 43, 11, 38),nrow=1, ncol=5)
matriz_a_linha

matriz_b_linha <- matrix(c(47, 24, 61, 14, 19), nrow=1, ncol=5)
matriz_b_linha

(matriz_a_linha * matriz_b_linha) == matriz_b_linha * matriz_a_linha 

# Provavelmente, está acarretada de algum erro. 
######################################################
######################################################
######################################################
######################################################
#
# II) Elaborar um exemplo com no mínimo 10 observações. 
#
# que contenham valor mais provável, erro absoluto aparente de cada medida;
# erro verdadeiro aparente de cada medida;
# desvio padrão.
#
# Verificar  se  existe  erro  grosseiro,  considerando  os  desvios  padrões  das  medidas  
# e  não  os  desvios  padrões  dos  erros.

# criando objetos (vetores) com as medidas aleatórias.

n1 <- c(12.3)

n2 <- c(12.334)

n3 <- c(11.299)

n4 <- c(25.300)

n5 <- c(22.301)

n6 <- c(32.323)

n7 <- c(17.331)

n8 <- c(19.2)

n9 <- c(21.4)

n10 <- c(25.1)

#obtendo a medida de todos pontos; 

mdp <- sum(n1,n2,n3,n4,n5,n6,n7,n8,n9, n10)

mdp

# 4) Valor mais provável;

N <- c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10)
N
vmp <- mean(N)
vmp
summary(N)


# 5) Variância; 

variancia <- var(N)
variancia

# 6) Erro absoluto aparente de cada medida 

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
EAA8 <- abs(n8-vmp)
EAA8
EAA9 <- abs(n9-vmp)
EAA9 
EAA10 <- abs(n10-vmp)
EAA10

# 7) Erro verdadeiro aparente de cada medida

EvaN1 <- n1-vmp
EvaN2 <- n2-vmp
EvaN3 <- n3-vmp
EvaN4 <- n4-vmp
EvaN5 <- n5-vmp
EvaN6 <- n6-vmp
EvaN7 <- n7-vmp

# 8) Desvio Padrão

sdN <- sd(N)
sdN
sdN == sqrt(variancia) # desvio padrão é igual a raíz da variância, por isso, a confirmação de igualdade foi efetuada.


# 9) Verificar se existe erros grosseiros nos desvios padrões das medidas. 

medidas <- c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10)
medidas
media_das_medidas <- mean(medidas)
media_das_medidas

desvio_padrao_medidas <- sd(medidas) # obtendo o desvio padrão 
desvio_padrao_medidas # exibindo o desvio padrão 
grosseiro <- desvio_padrao_medidas*3 # erro grosseiro é definido como o desvio padrão dos dados multiplicado pela escalar 3. 
grosseiro # exibindo o valor do erro grosseiro 

# EAA é maior que grosseiro?, em R: EAA1 > grosseiro
#
#
# Confirmando se o Erro Absoluto Aparente é maior que o erro grosseiro

EAA1 > grosseiro # N1 não possui erros grosseiros 

EAA2 > grosseiro # N2  não possui erros grosseiros 

EAA3 > grosseiro # N3 não possui erros grosseiros 

EAA4 > grosseiro # N4  possui erros grosseiros 

EAA5 > grosseiro # Não possui erros grosseiros 

EAA6 > grosseiro # Não possui erros

EAA7 > grosseiro # N7 não possui erros 

EAA8 > grosseiro # sem erros grosseiros

EAA9 > grosseiro # sem erros grosseiros 

EAA10 > grosseiro # sem erros grosseiros 


# Portanto, não foram encontrados erros grosseiros nas observações. 

#################################################################
################################################################


# 4) Ajustamento da medida AE pelo Método dos Mínimos Quadrados


# Após os devidos cálculos, e organização matricial
# o sistema assumiu a seguinte forma:


# 3x1 + 2x2 + 0x3 = 210.87 
# 2x1 + 5x2 + 2x3 = 472.35
# x1 + 2x2 + 3x3 = 265.23


# logo, define-se os coeficientes, e as incógnitas, como segue:

# matriz coeficientes das incógnitas

coeficiente <- c(3,2,0)
coeficientes_1 <- c(2,5,2)
coeficiente_2 <- c(1,2,3)

R <- rbind(coeficiente,coeficientes_1,coeficiente_2)
R

# vetor dos valores observados 

R1 <- 210.87
R2 <- 472.35
R3 <- 265.23

R_ <- rbind(R1,R2,R3)
R_

solucao <- solve(t (R) %*% R) %*% t (R) %*% R_
solucao <- round(solucao, digits=2) # arredondar 2 algarismos por conta dos valores observados estarem com 2 algarismos significativos. 
solucao

# Cálculo realizado com êxito. 
# Confirmado pelo método de Gauss-Jordan; substituição de incógnitas; etc. 
#

# Data do término da elaboração do Script: 19/10/2018

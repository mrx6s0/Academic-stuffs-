###########################################################
# Universidade Federal do Pampa - UNIPAMPA                #
#                                                         #
#                                                         #
#                                                         #
# Curso de Engenharia Florestal                           #
#                                                         #
# II semestre de 2018                                     #
#                                                         #
# Prova Prática I - Ajustamento de Observações Geodésicas # 
#                                                         #
#                                                         #
#                                                         #
###########################################################

# I) Demonstração de propriedades das matrizes. Observação: matrizes diferentes para cada item. 

# 1) associativa da associação de matrizes: 

# é definida pela seguinte propriedade: considerando uma matriz A, B e C
#

# temos: (A + B) + C = A + (B + C) 
# 

# ou seja, a propriedade associativa afirma que pode-se alterar o agrupamento 
# em uma adição de matrizes e obter o mesmo resultado. 
#
#
# Primeiramente, há de se criar as matrizes A, B e C. 

# Passo I: criação das matrizes, de dimensões iguais
#
#
# neste caso, 2x2.

matriz_a <- matrix(c(12, 13, 16, 13), nrow=2, ncol=2)
matriz_a


matriz_b <- matrix(c(41, 72, 62, 12), nrow=2, ncol=2)
matriz_b

matriz_c <- matrix(c( 20, 27, 33, 36), nrow=2, ncol=2)
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
# da multiplicação por um escalar k. 

#######################################################################
#######################################################################
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

matriz_a3 <- matrix(c(12, 5, 3, 7), nrow=2,ncol=2)
matriz_a3
matriz_b3 <- matrix(c(18, 9, 11, 14), nrow=2, ncol=2)
matriz_b3
matriz_c <- matrix(c(1,3,8, 11), nrow=2, ncol=2)
matriz_c 

# Organizando o cálculo 
escal_mat <- (matriz_a3 + matriz_b3) %*% matriz_c 
escal_mat
escal_mat_1 <- matriz_a3 %*% matriz_c + matriz_b3 %*% matriz_c 
escal_mat_1
escal_mat == escal_mat_1 

# Portanto, comprovou-se a propriedade.

####################################################
####################################################
#
# PROPRIEDADE 6: A∗B'=B'*A'

matriz_a_transp <- matrix(c(54, 32, 43, 11),nrow=2, ncol=2)
matriz_a_transp
matriz_a1_transp <- t(matriz_a_transp)

matriz_b_transp <- matrix(c(47, 24, 61, 14), nrow=2, ncol=2)
matriz_b_transp
matriz_b1_transp <- t(matriz_b_transp)

equal <- t(matriz_a_transp %*% matriz_b_transp) 
equal_1 <- matriz_b1_transp %*% matriz_a1_transp

equal == equal_1

# Portanto, comprovou-se a propriedade. 

######################################################
######################################################
######################################################
######################################################
#
# II) Elaborar um exemplo com no mínimo 10 observações. 
#
# que contenham valor mais provável, variância, erro absoluto aparente de cada medida;
# erro verdadeiro aparente de cada medida;
# desvio padrão.
#
# Verificar  se  existe  erro  grosseiro,  considerando  os  desvios  padrões  das  medidas  
# e  não  os  desvios  padrões  dos  erros.

# Obtendo cada medida. 

mdp1 <- c(12.2)
mdp1
mdp2 <- c(12.4)
mdp2
mdp3<- c(13.5)
mdp3
mdp4 <- c(16.7)
mdp4
mdp5 <- c(11.5)
mdp5
mdp6 <- c(19.6)
mdp6
mdp7 <- c(37.4)
mdp7 
mdp8 <- c(41.1)
mdp8 
mdp9 <- c(21.3)
mdp9
mdp10 <- c(12.5)
mdp10

# 4) Valor mais provável;

N <- c(mdp1, mdp2, mdp3, mdp4, mdp5, mdp6, mdp7, mdp8, mdp9, mdp10)
N

vmp <- mean(N)
vmp
summary(N) # 'resumo estatístico', 

# 5) Variância

variancia <- var(N)
variancia

# 6) Erro absoluto aparente de cada medida 

EAA1 <- abs(mdp1-vmp)
EAA1 
EAA2 <- abs(mdp2-vmp)
EAA2 
EAA3 <- abs(mdp3-vmp)
EAA3 
EAA4 <- abs(mdp4-vmp)
EAA4
EAA5 <- abs(mdp5-vmp)
EAA5
EAA6 <- abs(mdp6-vmp)
EAA6
EAA7 <- abs(mdp7-vmp)
EAA7
EAA8 <- abs(mdp8-vmp)
EAA8
EAA9 <- abs(mdp9-vmp)
EAA9 
EAA10 <- abs(mdp10-vmp)
EAA10

# 7) Erro verdadeiro aparente de cada medida

EvaN1 <- mdp1-vmp
EvaN2 <- mdp2-vmp
EvaN3 <- mdp3-vmp
EvaN4 <- mdp4-vmp
EvaN5 <- mdp5-vmp
EvaN6 <- mdp6-vmp
EvaN7 <- mdp7-vmp
EvaN8 <- mdp8-vmp
EvaN9 <- mdp9-vmp
EvaN10 <- mdp10-vmp

# 8) Desvio Padrão

sdN <- sd(N)
sdN
sdN == sqrt(variancia) # desvio padrão é igual a raíz da variância, por isso, a confirmação de igualdade foi efetuada.


# 9) Verificar se existe erros grosseiros nos desvios padrões das medidas. 

medidas <- c(mdp1, mdp2, mdp3, mdp4, mdp5, mdp6, mdp7, mdp8, mdp9, mdp10)
medidas
media_das_medidas <- mean(medidas)
media_das_medidas

desvio_padrao_medidas <- sd(medidas) # obtendo o desvio padrão 
desvio_padrao_medidas # exibindo o desvio padrão 

grosseiro <- desvio_padrao_medidas * 3 # erro grosseiro é definido como o desvio padrão dos dados multiplicado# pelo escalar 3. 

grosseiro # exibindo o valor do erro grosseiro 

#
# Verificando se há erros grosseiros 

EAA1 > grosseiro
EAA1
EAA2 > grosseiro
EAA2
EAA3 > grosseiro
EAA3 
EAA4 > grosseiro 
EAA4
EAA5 > grosseiro
EAA5
EAA6 > grosseiro
EAA6
EAA7 > grosseiro 
EAA7 
EAA8 > grosseiro
EAA8 
EAA9 > grosseiro 
EAA10 > grosseiro 

# Portanto, não foram encontrados erros grosseiros nas observações. 

#################################################################
#################################################################
#################################################################
#################################################################

# 4) Ajustamento da medida AE pelo Método dos Mínimos Quadrados
#
#
# Após a devida análise e organização matricial
# o sistema assumiu a seguinte forma:

# -1 * x1a - 0 * x2a - 0 * x3a = -50.63
# -0 * x1a -1 * x2a - 0 * x3a = -76.63 
# -0 * x1a - 0 * x2a - 1 * x3a = -29.75 
# -1 * x1a - 1 * x2a - 0 * x3a = -63.61
# -0 * x1a - 1 * x2a - 1 * x3a = -127.85
# -1 * x1a - 1 * x2a - 0 * x3a = -96.63
# -0 * x1a - 1 * x2a - 1 * x3a = -107.63

# logo, define-se os coeficientes das incógnitas; 

# Matriz coeficientes das incógnitas.

coeficiente <- c(-1,0,0)
coeficiente_1 <- c(0,-1,0)
coeficiente_2 <- c(0,0,-1)
coeficiente_3 <- c(-1,-1,0)
coeficiente_4 <- c(0,-1,-1)
coeficiente_5 <- c(-1,-1,0)
coeficiente_6 <- c(0,-1,-1)

R <- rbind(coeficiente,coeficiente_1,coeficiente_2,coeficiente_3, coeficiente_4, coeficiente_5, coeficiente_6)
R

# Vetor dos valores observados. 

R1 <- -50.63
R2 <- -76.63
R3 <- -29.75
R4 <- -63.61 
R5 <- -127.85 
R6 <- -96.63   
R7 <- -107.63

R_ <- rbind(R1,R2,R3,R4,R5,R6,R7)
R_

solucao_0 <- solve(t (R) %*% R) %*% t (R) %*% R_
solucao_0

solucao_0 <- round(solucao_0, digits=2) # arredondar 2 algarismos por conta dos valores observados estarem com 2 algarismos significativos. 
solucao_0

# Distância AE ajustada. 

D_AE <- sum(solucao_0)
D_AE 

# Cálculo realizado com êxito. 

######################################################################
######################################################################
######################################################################
#
#
# Com a derivação encontra-se resultados igualmente fiduciais,
# como é demonstrado a seguir.
#
# O sistema assume a seguinte forma:

# 3x1 + 2x2 + x3 = 210.87 
# 2x1 + 5x2 + 2x3 = 472.35
# 1x1 + 2x2 + 3x3 = 265.23

# logo, define-se os coeficientes, e as constantes, como segue:

# Matriz coeficientes das incógnitas.

coeficiente_ <- c(3,2,0)
coeficientes_1 <- c(2,5,2)
coeficientes_2 <- c(1,2,3)

J <- rbind(coeficiente_,coeficientes_1,coeficientes_2)
J

# Vetor dos valores observados. 

J1 <- 210.87
J2 <- 472.35
J3 <- 265.23

J_ <- rbind(J1,J2,J3)
J_

solucao_1 <- solve(t (J) %*% J) %*% t (J) %*% J_
solucao_1
solucao_1 <- round(solucao_1, digits=2) 
solucao_1

# Distância AE com equações derivadas

D_AE_DERIVADA <- sum(solucao_1)
D_AE_DERIVADA

## diferença entre os resultados.

diferenca <- D_AE - D_AE_DERIVADA
diferenca

# Confirmando a aproximação dos valores de x1, x2 e x3 encontrados, 
# em ambos os sistemas. 

colnames(solucao_0) <- c("Incógnitas Ajustadas (m)")
solucao_0 <-round(solucao_0, digits=2)
rownames(solucao_0) <- c("x1a", "x2a", "x3a")

solucao_0

colnames(solucao_1) <- c("Incógnitas Ajustadas (m)")
solucao_1 <-round(solucao_1, digits=2)
rownames(solucao_1) <- c("x1a", "x2a", "x3a")

solucao_1 
solucao_0 > solucao_1 # solução sem equações derivadas tem um valor maior que a 
## solução com equações derivadas. Ou seja, há uma maior acurácia utilizando-se 
## de derivadas. 

summary(solucao_0) # resumo do resultado sem derivação.
summary(solucao_1) # resumo do resultado com derivação.

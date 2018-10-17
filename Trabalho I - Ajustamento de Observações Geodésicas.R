# Trabalho I de Ajustamento de Observações Geodésicas

# Engenharia florestal - UNIPAMPA 

# 1) 

# criação das matrizes dadas, por meio da função matrix. 

matriz_A <- matrix(c(3, 1, -1, 0, 1, 2, -1, -2, 0), nrow = 3, ncol = 3) 
matriz_B <- matrix(c(1, 2, 3, -4, 2, 4, 6, 8, 3, 6, -1, 2, 4, 8, 2, 2), nrow = 4, ncol = 4)
matriz_l <- matrix(c(9, 18, 27), nrow = 3, ncol=1) 

# A) 

# calcular [A][B] 
# é impossível pois são duas matrizes de proporções diferentes,
# o número de linhas e colunas deve ser igual. 

#A = matriz_A %*% matriz_B
#print(A)

# B)

# calcular [A][l] 

B = matriz_A %*% matriz_l
print(B)

# C)

# calcular [A]^t[A]

C = t(matriz_A) %*% matriz_A
print(C)

# D)

# calcular [B][l][B]^t 

# não é possível realizar o cálculo. 

#D = matriz_B %*% matriz_l %*% t(matriz_B)

# E) 

# calcular l por l transposta 

E = matriz_l %*% t(matriz_l)
print(E)

# F) 

# calcular l transposta por l 

F = t(matriz_l) %*% matriz_l 
print(F)

# G)  calcular B inversa 

G = solve(matriz_A) # erro inesperado!!!
G
# H) [A][x] = [l] 

H = solve(matriz_A, matriz_l)
print(H)

## 2) 

# 

vet_c <- matrix(c(1, -2, 3, 5), nrow = 4, ncol = 1) 
matriz_P <- matrix(c(2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 3), nrow = 4, ncol = 4) 
matriz_P

vet_c_transp = t(vet_c)
vet_c
t(vet_c)

fqf = t(vet_c) %*% matriz_P %*% vet_c
print(fqf)

# 3) 

# ([X][Y][Z])^t = [Z]^t[Y]^t[X]^t 

# criando as matrizes

matriz_x <- matrix(c(1, 9, 3, 5), nrow = 4, ncol = 1)
matriz_y <- matrix(c(0, -4, 7, 11), nrow = 4, ncol = 1)
matriz_z <- matrix(c(4, 7, 2, 3), nrow = 4, ncol = 1)

## criando as transpostas

exemp = t(matriz_z) 
exemp_1 = t(matriz_y)                   
exemp_2 = t(matriz_x)


# calculando a transposta das matrizes separadamente. 

res_ = t(matriz_x * matriz_y * matriz_z)
res

## calculando as matrizes previamente transposta 

res = exemp_2 * exemp_1 * exemp 
print(res)

# confirma-se a igualdade

# 4

# a) 

matriz_N <- matrix(c(3, 2, 6, 5), nrow = 2, ncol = 2)
matriz_M <- matrix(c(1, 5, 3, 12), nrow = 2, ncol = 2)
traco = diag(matriz_M + matriz_N)

print(traco)

traco_1 = diag(matriz_M) + diag(matriz_N)

print(traco_1)

# b) 

c <- c(33)
b = diag(c * matriz_M)
print(b)

comprov = c * diag(matriz_M)

#diag(matriz_M)

# c) 

c_ = diag(matriz_M * matriz_N)
c_1 = diag(matriz_N * matriz_M)

c_ == c_1

#true

# d) 

# nao fechou... 

d = solve(matriz_M)

d1 = d * matriz_N * matriz_M    

print(d1)

n = diag(matriz_N)

d1 == n

print(n)
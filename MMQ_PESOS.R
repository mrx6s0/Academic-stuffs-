### Uso dos pesos no MMQ
### 25/10/2018

# Distancias em km

d1 <- 207.99/1000
d2 <- 209.96/1000
d3 <- 201.01/1000
d4 <- 222.51/1000
d5 <- 213.08/1000
d6 <- 207.84/1000
d7 <- 275.19/1000
d8 <- 284.83/1000
d9 <- 291.68/1000

# Vetor das distancias

D <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9)

# Matriz peso das distâncias usando Schmidtz

P2 <-  diag(1/(sqrt(4*100*D)))

# Matriz peso usando identidade ou seja, peso igual
# para todas as observações

P1 <- diag( rep(1,9) )

#Matriz A

eq1 <- c(0,0,0,1)
eq2 <-c(-1,0,0,1)
eq3 <- c(1,0,0,0)
eq4 <- c(0,1,0,0)
eq5 <- c(0,-1,1,0)
eq6 <- c(0,0,1,0)
eq7 <- c(0,0,1,-1)
eq8 <- c(0,0,0,1)
eq9 <- c(0,0,1,0)

A <- rbind(eq1, eq2, eq3, eq4, eq5, eq6, eq7, eq8, eq9)
A

# Vetor das observações

L <- c(162.279, 28.050, 190.340, 193.885, 24.118, 169.773, 7.489,
       192.431, 209.446)
L
# Vetor das incógnitas usando identidade
X1 <- solve( t(A) %*% P1 %*% A  ) %*% t(A) %*% P1 %*% L
X1
# Vetor das incógnitas usando distâncias
X2 <- solve( t(A) %*% P2 %*% A  ) %*% t(A) %*% P2 %*% L
X2
# Difrença entre os dois pesos usados
X1
X2
Diferenca <- abs(X1 - X2)
Diferenca 

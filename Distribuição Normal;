### Distribuição Normal
plot(dnorm, -3, 3)
abline(v = 0)
abline(v =-1)
abline(v =-2)
abline(v =-3)
abline(v = 1)
abline(v = 2)
abline(v = 3)

### Exemplo quanto MAIOR o desvio padrão, 
# MENOR será a precisão do levantamento e mais achatada 
# será a curva.
par(mfrow=c(1,3))
x <- seq(5, 15, length=1000)
y <- dnorm(x, mean=10, sd=1)
plot(x, y, type="l", lwd=1, main = " 1 Desvio Padrão",
     ylim = c(0,0.4), xlab = "Valores")

y2 <- dnorm(x, mean=10, sd=2)
plot(x, y2, type="l", lwd=2 , main = " 2 Desvios Padrão",
     ylim = c(0,0.4), xlab = "Valores")

y3 <- dnorm(x, mean=10, sd=3)
plot(x, y3, type="l", lwd=3, , main = " 3 Desvios Padrão",
     ylim = c(0,0.4), xlab = "Valores")

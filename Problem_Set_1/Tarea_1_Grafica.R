
#GRAFICA 1

# Parámetros
p1 <- 2
p2 <- 2
alpha <- 0.3
beta  <- 0.6

# Funciones
r1 <- function(w){
  ((p1*(1-alpha)^(1-alpha)*alpha^alpha)/(w^(1-alpha)))^(1/alpha)
}

r2 <- function(w){
  ((p2*(1-beta)^(1-beta)*beta^beta)/(w^(1-beta)))^(1/beta)
}

# Dominio
w_vals <- seq(0.1, 5, length.out = 500)

# Ajustar márgenes (más espacio izquierda y abajo)
par(mar = c(5.5, 5.5, 4, 2))

# Graficar
plot(w_vals, r1(w_vals),
     type = "l",
     col = "blue",
     lwd = 2,
     xlab = "Salario (w)",
     ylab = "Renta del capital (r)",
     cex.lab = 1.3,
     cex.axis = 1.2,
     ylim = c(0, 5))

lines(w_vals, r2(w_vals),
      col = "red",
      lwd = 2)

legend("topright",
       legend = c("Bien 1", "Bien 2"),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n",
       cex = 1.1)

#GRAFICA 2


# Parámetros
p1 <- 2
p2 <- 2
alpha <- 0.3
beta  <- 0.6

# Progreso tecnológico Hicks-neutral en bien 2
A <- 1.3                 # A > 1
p2_eff <- A * p2          # equivalente a "precio efectivo" del bien 2

# Funciones r(w)
r1 <- function(w){
  ((p1*(1-alpha)^(1-alpha)*alpha^alpha)/(w^(1-alpha)))^(1/alpha)
}

r2 <- function(w){
  ((p2*(1-beta)^(1-beta)*beta^beta)/(w^(1-beta)))^(1/beta)
}

r2_new <- function(w){
  ((p2_eff*(1-beta)^(1-beta)*beta^beta)/(w^(1-beta)))^(1/beta)
}

# Dominio (w > 0)
w_vals <- seq(0.1, 5, length.out = 500)

# Ajustar márgenes para que no se peguen labels
par(mar = c(5.5, 5.5, 4, 2))

# Calcular valores
y1  <- r1(w_vals)
y2  <- r2(w_vals)
y2n <- r2_new(w_vals)

# Limites (para que entren las 3 curvas)
ymax <- max(y1, y2, y2n, na.rm = TRUE)
ymax <- min(ymax, 10)   # cap opcional para evitar escalas gigantes

# Graficar curvas originales
plot(w_vals, y1,
     type = "l",
     col = "blue",
     lwd = 2,
     xlab = "Salario (w)",
     ylab = "Renta del capital (r)",
     cex.lab = 1.3,
     cex.axis = 1.2,
     ylim = c(0, ymax))

lines(w_vals, y2,
      col = "red",
      lwd = 2)

# Nueva curva del bien 2 (shock tecnológico) en verde
lines(w_vals, y2n,
      col = "darkgreen",
      lwd = 2,
      lty = 2)

# Leyenda
legend("topright",
       legend = c("Bien 1 (original)", "Bien 2 (original)", "Bien 2 (A>1)"),
       col    = c("blue", "red", "darkgreen"),
       lwd    = 2,
       lty    = c(1, 1, 2),
       bty    = "n",
       cex    = 1.1)





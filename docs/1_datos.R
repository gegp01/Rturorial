# Datos normales


# EJEMPLO. 
# Los hombres mexicanos miden en promedio 1.70 m de altura, sus esposas son 12 cm más bajas y miden sólo 1.57 m. En España los hombres miden 1.76 m y las mujeres 1.62 m.
# REF. https://www.datosmundial.com/estatura-promedio.php

# Para 1000 observaciones:

# Generar 1000 datos al azar de una distribución normal 
masc_gaussian_sd1 = rnorm(1000, 176, sd = 1)
masc_gaussian_sd3 = rnorm(1000, 176, sd = 3)

fem_gaussian_sd1 = rnorm(1000, 157, sd = 1)
fem_gaussian_sd3 = rnorm(1000, 157, sd = 3)

# Datos Poisson (conteos, enteros)
d_poisson = rpois(1000, 10)
plot(cumsum(d_poisson))

 # Datos binarios
grupo = sample(c(0,1), 1000, replace = T)


# Series de tiempo aleatorias (random walk) centradas a 0 con 3 desviaciones estandar.
par(mfrow = c(1,2), font.main = 1)

z1 = cumsum(rnorm(1000, 0, sd = 3)) 
plot(z1, ylim = c(-170, 170), ylab = "valor", main = "Caminata aleatoria centrada a 0 y desv. est. = 3 ", type = "l")

z2 = cumsum(rnorm(1000, 0, sd = 3))
points(z2, col="red")

z3 = cumsum(rnorm(1000, 0, sd = 3))
lines(z3, col="blue")

# Series de tiempo aleatorias (random walk), centradas a 0 con 1 desviación estandar.
z4 = cumsum(rnorm(1000, 0, sd = 1)) 
plot(z4, ylim = c(-170, 170), ylab = "valor", main = "Caminata aleatoria centrada a 0 y desv. est. = 1 ", type = "l")

z5 = cumsum(rnorm(1000, 0, sd = 1))
points(z5, col="red")

z6 = cumsum(rnorm(1000, 0, sd = 1))
lines(z6, col="blue")








# Compilar todo en una base de datos en formato csv







# Series de tiempo
sigmoid = function(x) {
   1 / (1 + exp(-x))
}


x <- seq(-5, 5, 0.01)


# Graficas. Histograma.
hist()
qqnorm()
plot(

# Pruebas de normalidad






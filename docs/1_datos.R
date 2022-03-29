# Con este tutorial, se espera aprender a manipular y graficar datos en R y describirlos estadisticamente.
# En particular nos enfocaremos a cuatro temas:
# 1. Comparar las distribuciones normal y poisson (histogramas, qqplots, y prueba de Shaphiro-Wilk.
# 2. Tablas de Contingencia y prubas de Chi²
# 3. Series de tiempo (random walk y periodicas)  
# 4. Leer y guardar datos en un archivo csv.
##################################
#
# EJEMPLO. 
# Los hombres mexicanos miden en promedio 1.70 m de altura, sus esposas son 12 cm más bajas y miden sólo 1.57 m. En España los hombres miden 1.76 m y las mujeres 1.62 m.
# REF. https://www.datosmundial.com/estatura-promedio.php

# 1. COMPARAR LAS DISTRIBUCIONES NORMAL Y POISSON
# Para 1000 observaciones:
# Generar 1000 datos al azar de una distribución normal con distinta varianza (sd) 
masc_gaussian_sd1 = rnorm(1000, 176, sd = 1)
masc_gaussian_sd3 = rnorm(1000, 176, sd = 3)

# Generar 1000 datos al azar de una distribución normal con distinta varianza (sd)
fem_gaussian_sd1 = rnorm(1000, 157, sd = 1)
fem_gaussian_sd3 = rnorm(1000, 157, sd = 3)

# Histograma: Gráfica de la frecuencia de los datos clasificados en intervalos del mismo tamaño.
cubeta <- as.integer(cut(masc_gaussian_sd1, c(172:180), include.lowest=TRUE))
barplot(table(cubeta))

# Grafica Historgamas
par(mfrow=c(2,2))
hist(masc_gaussian_sd1, col="lightblue")
hist(masc_gaussian_sd3, col="lightblue")
hist(fem_gaussian_sd1, col="lightblue")
hist(fem_gaussian_sd3, col="lightblue")

# Datos Poisson (conteos, enteros con distintas medias)
d_poisson1 = rpois(1000, 1)
d_poisson5 = rpois(1000, 5)
d_poisson10 = rpois(1000, 10)
d_poisson50 = rpois(1000, 50)

# Grafica Histogramas
par(mfrow=c(2,2))
hist(d_poisson1)
hist(d_poisson5)
hist(d_poisson10)
hist(d_poisson50)

# QQ plots
# Quantiles. Es una forma de clasificar los datos en proporciones iguales.

y = quantile(masc_gaussian_sd1, probs=0:10/10), include.lowest=TRUE)
x = quantile(rnorm(1000), probs=0:10/10), include.lowest=TRUE)
plot(x,y)

qqnorm(masc_gaussian_sd1)
qqline(masc_gaussian_sd1)

qqnorm(d_poisson1)
qqline(d_poisson1)

#  decil <- as.integer(cut(masc_gaussian_sd1, quantile(masc_gaussian_sd1, probs=0:10/10), include.lowest=TRUE))
# barplot(table(decil))
# plot(decil, masc_gaussian_sd1)

# PRUEBAS DE NORMALIDAD
# Shapiro-wilk tests
shapiro.test(masc_gaussian_sd3)
shapiro.test(fem_gaussian_sd3)

shapiro.test(d_poisson1)
shapiro.test(d_poisson10)

# 2. TABLAS DE CONTINGENCIA. CORRELACIÓN ENTRE CLASES.
clase_1 = sample(c(0,1), 1000, replace = T)
clase_2 = sample(c(0,1), 1000, replace = T)

# Observados
X = table(clase_1, clase_2)
CHI = chisq.test(table(clase_1, clase_2)
CHI
CHI$expected

# 3. SERIES DE TIEMPO

# Random walk
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


                 
# Series de tiempo periodicas
xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)

par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3

wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)

plot(wave.1, type="l", col="darkslategrey")
lines(wave.2, col="grey", lty = "dashed")
lines(wave.3, col="red")
                                                                                         
wave.4 <- wave.3
# Encontrar picos en la onda                                            
wave.4[wave.3>0.5] <- 0.5 # acotar los datos
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)                                              
                                              
boxplot(wave.1, wave.2, wave.3
       , col=c("transparent", "transparent", "red")
       , ylab = "variación", labels = c("", "", ""))
                                                                                                               
# 4. MANIPULACION DE DATOS
DATOS = data.frame(masc_gaussian_sd1, masc_gaussian_sd3, masc_gaussian_sd1, masc_gaussian_sd3, d_poisson, clase_1, clase_2)
head(DATOS)

# Seleccionar los datos con grupo = 1                                              
DATOS[grep(1, DATOS$clase_1),]
DATOS[DATOS$clase_1==1,]
                                              
# Resumen descriptivo                                              
summary(DATOS[DATOS$clase_1==1,])                       
summary(DATOS[DATOS$clase_1==0,])
par(mfrow = c(1,2))                                              
plot(DATOS[DATOS$clase_1==1,])
plot(DATOS[DATOS$clase_1==0,])
                                              
# GUARDAR LOS DATOS EN csv
write.csv(DATOS, "datos.csv")
q = read.csv("datos.csv")
                                              
head(q)
str(q)
names(q)





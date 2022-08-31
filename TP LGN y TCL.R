#a)  
set.seed(71818)
u <-runif(1000,0,1)
hist(u, col = 'lightblue',main = 'Histograma variable aleatoria uniforme ',freq = FALSE, xlab = "x")
#El histograma se asemeja a una densidad uniforme.

#b) X1 y X2 va ~ U(0,1). 
set.seed(71818)
X1 <- runif(1000,0,1)
X2 <- runif(1000,0,1)
promedio <- (X1+X2)/2
hist(promedio,col = 'salmon',main = 'Histograma los promedios de 2 v.a.',probability = TRUE)
equis2 <- seq(min(promedio), max(promedio), by=0.01)
lines(equis2,dnorm(equis2, mean = mean(promedio),sd =sqrt(var(promedio))),col = 2,lwd = 2)
#Caracteristicas del histograma: es salmon, se asemeja a una campana.

#c) Aumentar a 5 las variables independientes.

X3 <- runif(1000,0,1)
X4 <- runif(1000,0,1)
X5 <- runif(1000,0,1)
promedio_5 <- (1/5)*(X1+X2+X3+X4+X5) #promedio_5 me da un vector con mil promedios (cada uno promediando las 5 Xi)
hist(promedio_5,col = 'khaki',main = 'Histograma de prom 5 variables',freq = FALSE)
#Observo que ahora es khaki y tiene forma de campana más definida.

#d) 
set.seed(71818)
Xi_total30 <- c()
for (i in 1:30){
  Xi <- runif(1000,0,1)
  Xi_total30 <- cbind(Xi_total30,Xi)
}
promedio_30 <- rowMeans(Xi_total30)
hist(promedio_30,col = 'lightgreen',main = 'Histograma prom de 30 variables',freq = FALSE)
#ahora es light green y tiene m?s forma de campana jajaja

#e) para 500
set.seed(71818)
Xi_total500 <- c()
for (i in 1:500){
  Xi <- runif(1000,0,1)
  Xi_total500 <- cbind(Xi_total500,Xi)#cada columna son 1000 repeticiones de una uniforme 
  }
promedio_500 <- rowMeans(Xi_total500)
hist(promedio_500,col = 'salmon',main = 'Histograma prom de 500 variables',freq = FALSE)

#f) para 1200
Xi_total1200 <- c()
for (i in 1:1200){
  Xi <- runif(1000,0,1)
  Xi_total1200 <- cbind(Xi_total1200,Xi)#cada columna son 1000 repeticiones de una uniforme 
}
promedio_1200 <- rowMeans(Xi_total1200)
hist(promedio_1200,col = 'orange',main = 'Histograma prom de 1200 variables',freq = FALSE)

#chequear todos los hist tengan la misma cantidad de intervalos y la misma escala en densidad.

#boxplot con los 6 sets de valores
#todos_los_datos <- c(u,X1,X2,X3,X4,X5,Xi_total30,Xi_total500,Xi_total1200) #1736000 valores
#boxplot(todos_los_datos)

#hay que hacer 6 boxplots en el mismo gr?fico, no uno solo

par(mfrow=c(1,1))
boxplot(u,promedio,promedio_5,promedio_30,promedio_500,promedio_1200) #poner labels y colores
#los valores se van concentrando al rededor de la mediana y la media (aporx 0,5)

#algunas medidas de posici?n y dispersi?n (falta dividir por n!!)

media_u <- mean(u)
var_u <-var(u)

media_prom <- mean(promedio)
var_prom <- var(promedio)

media_prom5 <- mean(promedio_5)
var_prom5 <- var(promedio_5)

media_prom30 <- mean(promedio_30)
var_prom30 <- var(promedio_30)

media_prom500 <- mean(promedio_500)
var_prom500 <- var(promedio_500)

media_prom1200 <- mean(promedio_1200)
var_prom1200 <- var(promedio_1200)

medias <- c(media_u,media_prom,media_prom5,media_prom30,media_prom500,media_prom1200)
varianzas <-c(var_u,var_prom,var_prom5,var_prom30,var_prom500,var_prom1200)

#la LGN me dice que: el promedio tiende a la esperanza (en este caso la esperanza te?rica es 0,5)
#la LGN me dice que: la varianza disminuye al aumentar n (la var te?rica es (b-a)^2/12=1/12), entonces deber?a disminuir como (1/12)/n???
#los valores dan bastante bien, habr?a que escribir esto bien detallado en overleaf

#qqplots que en realidad son qqnorms

par(mfrow=c(2,3)) #poner los graficos en 2 filas y 3 columnas
dev.off()
qqnorm(u) #es una muestra de variables aleatorias uniformes, no promedios
qqline(u,col='2')
qqnorm (promedio)
qqline(promedio,col='2')
qqnorm (promedio_5)
qqline(promedio_5,col='2')
qqnorm(promedio_30) #tiene pinta de normal
qqline(promedio_30,col='2')
qqnorm(promedio_500)
qqline(promedio_500,col='2')
qqnorm(promedio_1200)
qqline(promedio_1200,col='2')

#transformacion de los promedios para el TCL
norm <- (u-0.5)/sqrt(1/12) #estos no son promedios porque hay 1 sola va pero bueno

norm2 <- (promedio - 0.5)/sqrt((1/12)/2) #estandarizo mil datos para cada set 

norm5 <- (promedio_5-0.5)/sqrt((1/12)/5)

norm30 <- (promedio_30-0.5)/sqrt((1/12)/30)

norm500 <- (promedio_500-0.5)/sqrt((1/12)/500)

norm1200 <- (promedio_1200-0.5)/sqrt((1/12)/1200)

par(mfrow=c(2,6))
dev.off()
hist(norm,col = 'orange',main = 'Histograma de promedios',freq = FALSE)
hist(norm2,col = 'orange',main = 'Histograma de promedios 2',freq = FALSE)
hist(norm5,col = 'orange',main = 'Histograma de promedios 5',freq = FALSE)
hist(norm30,col = 'orange',main = 'Histograma de promedios 30',freq = FALSE)
hist(norm500,col = 'orange',main = 'Histograma de promedios 500',freq = FALSE)
hist(norm1200,col = 'orange',main = 'Histograma de promedios 1200',freq = FALSE)
boxplot(norm)
boxplot(norm2)
boxplot(norm5)
boxplot(norm30)
boxplot(norm500)
boxplot(norm1200)


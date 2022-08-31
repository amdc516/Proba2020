calcular_proba_mismo_cumple <- function(m){
  1 - prod(365:(365-m+1))/365^m
  }
calcular_proba_mismo_cumple(20)

emes <- seq(20,100)
proba_mismo_cumple <- sapply(emes, calcular_proba_mismo_cumple)
#par(mar = c(0.5, 0.5, 0.5, 0.5)) #puedo usar este comando para corregir error de márgenes
dev.off() 
plot(emes, proba_mismo_cumple)
plot(emes,proba_mismo_cumple, type = "l")
 sample(1:6, 2, TRUE)
 replicate(5, sample(1:6,2,TRUE))

 resultado <- matrix(0,nrow = 2,ncol = 5)
 resultado <- matrix(0,nrow = 2,ncol = 5)
 for(i in 1:5){
   resultado[,i] <- sample(1:6,2,TRUE)
 }
sum(sample(1:6,2,TRUE))
replicate(100, sum(sample(1:6,2,TRUE)))
replicate(100, sum(sample(1:6,2,TRUE)))==8
mean(replicate(100, sum(sample(1:6,2,TRUE)))==8)


sample(1:6,2,TRUE)
sum(sample(1:6,2,TRUE))
'_R_USE_PIPEBIND_'=TRUE #uso este comando para que reconozca >=
replicate (1000, sum(sample(1:6,2,TRUE)))
replicate (1000, sum(sample(1:6,2,TRUE)))=>5
replicate (1000, sum(sample(1:6,2,TRUE)))>=5


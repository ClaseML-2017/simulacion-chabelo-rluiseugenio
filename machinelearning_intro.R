numsim = 100000 
puertas = 1:3

#Funciones
#Abro la puerta no ganadora
abrirPuerta = function(x) { 
  if (x[1]==x[2])
    return(sample(puertas[-c(x[1])], 1)) 
  else return(puertas[-c(x[1],x[2])])
}

#Cambio la puerta 
cambiarPuerta = function(x) { return(puertas[-c(x[1], x[2])]) }

#Primero se esoge al ganador
ganador = sample(puertas, numsim, replace=TRUE)
#Después se escoge una puerta
escoger = sample(puertas, numsim, replace=TRUE)
#Abro la puerta no correcta.
abrir = apply(cbind(ganador, escoger), 1, abrirPuerta)
#Asigno la puerta si cambio
NuevoEscoger = apply(cbind(abrir, escoger), 1, cambiarPuerta)
#
cat("sin cambiar de puerta, ganó un ",
    round(sum(ganador==escoger)/numsim*100,1)," 
    porcentaje de las veces. \n", sep="")
cat("cambiando de puerta, ganó un ",
    round(sum(ganador==NuevoEscoger)/numsim*100,1)," 
    porcentaje de las veces.\n", sep="")
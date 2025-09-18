## Elaborado por **Julieta González Herrera**

#Usar max, R0 calculado junto con el matriz de deSolve
    #En base a: t_max <- out$time[which_max(out$I)]
                    #       datos[renglón, columna]
colnames(out)
#which.max(out[ , "I"]) #137 -> da los renglones
#t_max <- out[137, 1] #de la columna 3, "time"
#t_max ### maximo de infectados en t_max

#Funciona:
#t_max <- out[      renglon         , columna]  
t_max <- out[which.max(out[, "I"]), "time"]  # out[which.max(out[, "I"]), 1]
t_max

#Graficar
abline(v = t_max, col = "orange", lty = 3)
text (x = max(tiempo) * 0.8, y = t_max + 10,    
      labels = paste("t max =", t_max), col = "orange")
## Maximo de infectados en t max ## Tiempo que ocurre pico de infección 

#####                     PARTE B                 ######
#which.max(out[, "I"]) #renglón de los infectados máximos
#datos[renglón, columna]
#I_max <- out[137,3] #de la columna I

colnames(out)
#Funciona:
t_max <- out[      renglon         , columna]  
t_max <- out[which.max(out[, "I"]), "I"]   # out[which.max(out[, "I"]), 3]
I_max ### MAXIMO DE INFECTADOS, pico de infección

#Graficar
abline(h = I_max, col = "purple", lty = 5)
text (x = max(tiempo)*0.8, y = I_max + 14,    
      labels = paste("I max =", round(I_max, 2)), col = "purple")
##      maximo de infectados  ##   Pico de infección

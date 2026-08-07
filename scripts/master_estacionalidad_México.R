lluvias<-read.csv("C:\\Users\\crist\\OneDrive\\Documentos\\Tesis\\datos tesis\\precipitacionMexico.csv")
lluvias
meses<-lluvias[c(2,3,4,5,6,7,8,9,10,11,12,13)]
	meses
	ppt<-meses[.2:13]
promedio_ppt<-colMeans(meses)
	promedio_ppt

#ajustasr un modelo polinómico para la precipitación promedio
me<-1:12#Meses
mod1<-lm(promedio_ppt~I(me)+I(me^2)+I(me^3)+I(me^4)+I(me^5))
mod1
plot(1:12,promedio_ppt)
points(1:12,fitted(mod1),type="l")


#generar una interpolación diaria
day<-seq(1,12,11/365)



pdiaria<--30.06264+102.86276*day-66.23385*day^2+16.48948*day^3-1.61498*day^4+0.05366*day^5
plot(pdiaria,type="l",lwd=2.5,col="darkblue",
xlab="Day of the Year",
ylab="Total Daily Precipitation (mm)",las=1,
cex.axis=1.2,cex.lab=1.5,
main="Average Rainy Season Dynamics Across Mexico (1974-2023)"
)

#points(seq(1,365,31),promedio_ppt,col=2)
	lines(c(0,365),c(52,52),col="lightgrey",lty=2)
	lines(c(138,138),c(0,52),col="lightgrey",lty=2)
		text(138,52,"may 18th", col="black",pos=3,cex=1.2)
		text(138,60,"rainy season onset", col="black",pos=3,cex=1.2)


			lines(c(317,317),c(0,52),col="lightgrey",lty=2)
				text(317,52,"november 14th", col="black",pos=3,cex=1.2)
				text(317,60,"rainy season offset", col="black",pos=3,cex=1.2)
				text(230,30,"179 days ", col="black",pos=3,cex=1.2)
				text(230,20,"Rainy Season Length",col="black",pos=3,cex=1.2)
			

plot(pdiaria,type="l",lwd=2.5,col="darkblue",
xlab="Day of the Year",
ylab="Total Daily Precipitation (mm)",las=1,
cex.axis=1.2,cex.lab=1.5,
main="Summary of methodology"
)
	points(which.max(pdiaria),max(pdiaria),col="red",pch=19,cex=1.5)
	text(which.max(pdiaria), max(pdiaria), labels = "Max", pos = 1, col = "red", cex = 1.2)
	points(which.min(pdiaria),min(pdiaria),col="blue",pch=19,cex=1.5)
	text(which.min(pdiaria), min(pdiaria), labels = "Min", pos = 3, col = "blue", cex = 1.2)
	lines(c(0,365),c(52,52),col="lightgrey",lty=2)
	lines(c(138,138),c(0,52),col="lightgrey",lty=2)
		text(138,52,"Onset", col="black",pos=3,cex=1.2)


			lines(c(317,317),c(0,52),col="lightgrey",lty=2)
				text(317,52,"Offset", col="black",pos=3,cex=1.2)
				text(230,30,"Lenght", col="black",pos=3,cex=1.2)
				#text(1, max(pdiaria), "Inflection = 0.2(Max - Min) + Min", col = "black", pos = 4, cex = 1.2)
				text(75,75,"Inflection=0.2(Max-Min)+Min",col="black",pos=3,cex=1)
				lines(c(75,75),c(75,52),col="darkgrey",lty=2,cex=1.2)

max(pdiaria)
which(pdiaria>=52)

# el 52 es el punto de inflexión donde inicia la temporada de lluvias y donde terminan
# el dia 138 donde inicia temp lluvias 18 de mayo
# el dia 317 donde termina la temp lluvias 14 de noviembre 

#-------------------------------------------------------
#vector para interpolación diaria
day<-seq(1,12,11/365)

#--------------------------------------------

#FITEAR L

	coeficientes_por_ano <- data.frame(Año = numeric(), Coef_0 = numeric(), Coef_1 = numeric(), Coef_2 = numeric(), 
                                   Coef_3 = numeric(), Coef_4 = numeric(), Coef_5 = numeric())

for (anio in 1901:2023) {
  datos_ano <- subset(lluvias, YEAR == anio)
  if (nrow(datos_ano) > 0) {
    promedio_ppt <- colMeans(datos_ano[, 2:13])
    me <- 1:12
    mod <- lm(promedio_ppt ~ I(me) + I(me^2) + I(me^3) + I(me^4) + I(me^5))
    coeficientes <- coef(mod)
    coeficientes_por_ano <- rbind(coeficientes_por_ano, data.frame(
      Año = anio,
      Coef_0 = coeficientes[1],  # Intercepto
      Coef_1 = coeficientes[2],  # Coeficiente de 'me'
      Coef_2 = coeficientes[3],  # Coeficiente de 'I(me^2)'
      Coef_3 = coeficientes[4],  # Coeficiente de 'I(me^3)'
      Coef_4 = coeficientes[5],  # Coeficiente de 'I(me^4)'
      Coef_5 = coeficientes[6]   # Coeficiente de 'I(me^5)'
    ))
  }
}
print(coeficientes_por_ano)


x <- 123  # O cualquier índice de año que desees analizar
#day <- 1:12  # Ahora day es un rango de meses
day<-seq(1,12,11/365)

fiteado <- coeficientes_por_ano[x, 2] + coeficientes_por_ano[x, 3] * day + coeficientes_por_ano[x, 4] * (day^2) +
  coeficientes_por_ano[x, 5] * (day^3) + coeficientes_por_ano[x, 6] * (day^4) + coeficientes_por_ano[x, 7] * (day^5)





#----------------------------------------------------

#VISUALIZACIÓN DATOS INICIO,FIN,DURACIÓN LLUVIAS EN TABLA


resultados <- data.frame(Año = numeric(),
                         Inicio_Lluvias = numeric(),
                         Fin_Lluvias = numeric(),
                         Duracion = numeric(),
                         stringsAsFactors = FALSE)

umbral <- 52  # Umbral para lluvias significativas

# Generar datos diarios (365 días) usando coeficientes ajustados
	for (anio in coeficientes_por_ano$Año) {
		# Obtener los coeficientes del año actual
  			coef_anio <- coeficientes_por_ano[coeficientes_por_ano$Año == anio, ]
  
  # Crear un vector de días en el año (1 a 365)
  				day <- seq(1, 12, length.out = 365)  # Dividir 12 meses en 365 días
  
  # Calcular los valores ajustados (fiteado) para cada día
  					fiteado <- coef_anio$Coef_0 + coef_anio$Coef_1 * day + coef_anio$Coef_2 * (day^2) +
   							 coef_anio$Coef_3 * (day^3) + coef_anio$Coef_4 * (day^4) + coef_anio$Coef_5 * (day^5)
  
 # Identificar los días con lluvias significativas
 			 dias_lluviosos <- which(fiteado >umbral & fiteado >10)
  
 				 if (length(dias_lluviosos) > 0) {
   						 # Día de inicio y fin de lluvias
   							inicio_lluvias <- dias_lluviosos[1]
    							fin_lluvias <- dias_lluviosos[length(dias_lluviosos)]
    
    								# Calcular duración
   								 duracion <- fin_lluvias - inicio_lluvias
    
   									 # Guardar resultados
    										resultados <- rbind(resultados, data.frame(
     											Año = anio,
     											Inicio_Lluvias = inicio_lluvias,
      										Fin_Lluvias = fin_lluvias,
      										Duracion = duracion
  											))
  											} else {
    												# Si no hay días lluviosos, guardar NA
    													resultados <- rbind(resultados, data.frame(
      													Año = anio,
      													Inicio_Lluvias = NA,
      													Fin_Lluvias = NA,
     														Duracion = NA
   														))
  														}
}


# Revisar resultados
print(resultados)

#------------------------------------------------------


#GRÁFICAS INICIO,FIN,DURACION LLUVIAS


par(mfrow = c(3, 1),oma=c(0,0,4,0), mar=c(2,4,1,2))

# Graficar los resultados
sd_plus<-151
sd_minus<-121

colores<-ifelse(resultados$Inicio_Lluvias>sd_plus,"blue",
		ifelse(resultados$Inicio_Lluvias<sd_minus,"red","black"))

plot(resultados$Año, resultados$Inicio_Lluvias, type = "b",
	ylim=c(110,160),xlim=c(1900,2025),
		pch=16,las=1,
			xlab="",ylab="ONSET",
			col=colores,
			xaxt="n")

				axis(1,at=seq(1900,2025,by=5))

					lines(c(1900,2025),c(136,136),col="lightgrey")#promedio
						text(2025,136,"mean",pos=3,col="lightgrey",cex=0.8)

					lines(c(1900,2025),c(151,151),col="lightgrey",lty=4)#limite superior
						text(2025,151,"mean + sd",pos=3,col="lightgrey",cex=0.8)
						
					lines(c(1900,2025),c(121,121),col="lightgrey",lty=4)
						text(2025,121,"mean - sd",pos=3,col="lightgrey",cex=0.8)#limite inferior
								
											


sd_plus1<-333
sd_minus1<-307

colores<-ifelse(resultados$Fin_Lluvias>sd_plus1,"blue",
		ifelse(resultados$Fin_Lluvias<sd_minus1,"red","black"))


plot(resultados$Año, resultados$Fin_Lluvias, type = "b",
	ylim=c(300,370),xlim=c(1900,2025),
		pch=16,las=1,
		xlab="",ylab="OFFSET",
		col=colores,
		xaxt="n")

			axis(1,at=seq(1900,2025,by=5))

				lines(c(1900,2025),c(320,320),col="lightgrey")
					text(2025,320,"mean",pos=3,col="lightgrey",cex=0.8)

				lines(c(1900,2025),c(333,333),col="lightgrey",lty=4)
					text(2025,333,"mean + sd",pos=3,col="lightgrey",cex=0.8)

				lines(c(1900,2025),c(307,307),col="lightgrey",lty=4)
					text(2025,307,"mean - sd",pos=3,col="lightgrey",cex=0.8)


sd_plus2<-204
sd_minus2<-163

colores<-ifelse(resultados$Duracion>sd_plus2,"blue",
		ifelse(resultados$Duracion<sd_minus2,"red","black"))

	
#par(mar=c(5, 5, 2, 2))  

			
plot(resultados$Año, resultados$Duracion, type = "b",  
     xlim=c(1900, 2025),      
     ylim=c(150,250),
     pch=16, las=1,            
     xlab="", ylab="LENGTH",
     col=colores,             
     xaxt="n") 

	axis(1,at=seq(1900,2025,by=5))

		lines(c(1900,2025),c(183,183),col="lightgrey")
					text(2025,183,"mean",pos=3,col="lightgrey",cex=0.8)



		lines(c(1900,2025),c(204,204),col="lightgrey",lty=4)
					text(2025,204,"mean + sd",pos=3,col="lightgrey",cex=0.8)

		lines(c(1900,2025),c(163,163),col="lightgrey",lty=4)
			text(2025,163,"mean - sd",pos=3,col="lightgrey",cex=0.8)



mtext("RAINY SEASON DYNAMICS (DAYS)",side=3,outer=TRUE,cex=2,front=2)
#--------------------------------------------------------------------

#TABLA DE REGRESIÓN LINEAL INICIO,FIN,DURACIÓN LLUVIAS

library(dplyr)
rs_regresion <- data.frame(
  Periodo = character(),
  Variable = character(),
  R2 = numeric(),
  p_valor = numeric(),
  stringsAsFactors = FALSE
)
	periodos <- c(100, 50, 20)
	max_year <- max(resultados$Año)

		for (p in periodos) {

		dts_filtrados<-subset(resultados, Año >= (max_year - p + 1))

			mod_inicio <- lm(Inicio_Lluvias ~ Año, data = dts_filtrados)
  			mod_fin <- lm(Fin_Lluvias ~ Año, data = dts_filtrados)
  			mod_duracion <- lm(Duracion ~ Año, data = dts_filtrados)


				rs_regresion <- rbind(rs_regresion,data.frame(
      			Periodo = rep(p, 3),
      			Variable = c("Inicio_Lluvias", "Fin_Lluvias", "Duracion"),
      			R2 = c(summary(mod_inicio)$r.squared, summary(mod_fin)$r.squared, summary(mod_duracion)$r.squared),
      			p_valor = c(summary(mod_inicio)$coefficients[2, 4], 
                  	summary(mod_fin)$coefficients[2, 4], 
                 		summary(mod_duracion)$coefficients[2, 4])
    )
  )
}

rs_regresion$Variable <- recode(rs_regresion$Variable,
				"Inicio_Lluvias"="ONSET",
				"Fin_Lluvias"="OFFSET",
				"Duracion"="LENGTH")
print(rs_regresion)
knitr::kable(rs_regresion, digits = 5)
#write.csv(rs_regresion, "tabla_regresion.csv", row.names = FALSE)


#---------------------------------------------------------------------
wetMex <- read.csv("C:\\Users\\crist\\OneDrive\\Documentos\\Tesis\\datos tesis\\wetMex.csv")


mod4<-lm(resultados$Duracion~wetMex$ANN)
dts_combinados<-merge(resultados, wetMex, by.x = "Año", by.y = "YEAR")
mod4 <- lm(ANN ~ Duracion, data = dts_combinados)
summary(mod4)


plot(dts_combinados$Duracion, dts_combinados$ANN,
     xlim = c(150, 250),
     las = 1,
     xlab = "LENGTH (DAYS)",
     ylab = "WET (DAYS)",
     pch = 21,
     bg = "lightblue",
     col = "black",
     main = "CORRELATION OF ANNUAL PRECIPITATION WITH LENGTH (DAYS)")

# Luego agregar la línea del modelo (suponiendo que mod4 ya está definido)
abline(mod4, col = "red", lwd = 2)

# Por último, agregar la leyenda o texto
legend("topleft", legend = expression(atop(R^2 == 0.28, P < 2.519e-10)),
       bty = "n", cex = 0.9, text.col = "black")

# Graficar los resultados
par(mfrow = c(3, 1),oma=c(0,0,4,0), mar=c(2,4,1,2))
sd_plus<-151
sd_minus<-121

colores<-ifelse(resultados$Inicio_Lluvias>sd_plus,"blue",
		ifelse(resultados$Inicio_Lluvias<sd_minus,"red","black"))

plot(resultados$Año[resultados$Año>=1974&resultados$Año<=2023],
resultados$Inicio_Lluvias[resultados$Año>=1974&resultados$Año<=2023],
type = "b",
ylim=c(110,160),xlim=c(1974,2024),
pch=16,las=1,
xlab="",ylab="ONSET",
col=colores)
#xaxt="n")
axis(1,at=seq(1974,2024,by=2))
	lines(c(1974,2025),c(136,136),col="lightgrey")#promedio
		text(2024,136,"mean",pos=3,col="lightgrey",cex=0.8)
	lines(c(1974,2025),c(151,151),col="lightgrey",lty=4)#limite superior
		text(2024,151,"mean + sd",pos=3,col="lightgrey",cex=0.8)	
	lines(c(1974,2025),c(121,121),col="lightgrey",lty=4)
		text(2024,121,"mean - sd",pos=3,col="lightgrey",cex=0.8)#limite inferior
sd_plus1<-333
sd_minus1<-307

colores<-ifelse(resultados$Fin_Lluvias>sd_plus1,"blue",
ifelse(resultados$Fin_Lluvias<sd_minus1,"red","black"))

plot(resultados$Año[resultados$Año>=1974&resultados$Año<=2023],
resultados$Fin_Lluvias[resultados$Año>=1974&resultados$Año<=2023],
 type = "b",
	ylim=c(300,370),xlim=c(1974,2024),
pch=16,las=1,
xlab="",ylab="OFFSET",
col=colores)
axis(1,at=seq(1974,2024,by=2))
	lines(c(1974,2025),c(320,320),col="lightgrey")
		text(2024,320,"mean",pos=3,col="lightgrey",cex=0.8)
	lines(c(1974,2025),c(333,333),col="lightgrey",lty=4)
		text(2024,333,"mean + sd",pos=3,col="lightgrey",cex=0.8)
	lines(c(1974,2025),c(307,307),col="lightgrey",lty=4)
		text(2024,307,"mean - sd",pos=3,col="lightgrey",cex=0.8)


sd_plus2<-204
sd_minus2<-163

colores<-ifelse(resultados$Duracion>sd_plus2,"blue",
	ifelse(resultados$Duracion<sd_minus2,"red","black"))
plot(resultados$Año[resultados$Año>=1974&resultados$Año<=2023],
resultados$Duracion[resultados$Año>=1974&resultados$Año<=2023],
	type = "b",  
     xlim=c(1974, 2024),      
     ylim=c(150,250),
     pch=16, las=1,            
     xlab="", ylab="LENGTH",
     col=colores) 

	axis(1,at=seq(1974,2024,by=2))

		lines(c(1974,2025),c(183,183),col="lightgrey")
					text(2024,183,"mean",pos=3,col="lightgrey",cex=0.8)
		lines(c(1974,2025),c(204,204),col="lightgrey",lty=4)
					text(2024,204,"mean + sd",pos=3,col="lightgrey",cex=0.8)
		lines(c(1974,2025),c(163,163),col="lightgrey",lty=4)
					text(2024,163,"mean - sd",pos=3,col="lightgrey",cex=0.8)
mtext("RAINY SEASON DYNAMICS (DAYS)",side=3,outer=TRUE,cex=2,front=2)



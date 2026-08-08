
#png("C:\\Users\\crist\\OneDrive\\Documentos\\Tesis\\avances\\imagenes\\test.png", width = 800, height = 320,)
par(mfrow = c(2, 3), mar=c(4,2,3,2), oma = c(3, 2, 5, 2),
#cex.axis=1.4,
#cex.lab=1.5,
cex.main=1.5
)
lat<- as.vector(replicate(71,seq(-89.75,89.75,0.5)[205:250]))
lon<- as.vector(t(replicate(46,seq(-179.75,179.75,0.5)[120:190])))
clr2<-colorRampPalette(c("midnightblue","blue","deepskyblue",
		"lightblue","yellow1","orangered1","white"))(1000)
cat<-inicio_mapa
	lim<-as.vector(t(cat))
 	rdf<-data.frame(cbind(lon,lat,lim))
 	names(rdf) <- c( "lon", "lat", "V3")
 	points_df <- rdf          # hicimos una copia para que no se borre
 	coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
 	pixel_df <- points_df
 	gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
 	rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
 		g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
 		#muestra el mapa
		#las=1 hace que las letras salen verticales
 		#axes=t que grafique los ejes
 		#horizontal, la leyenda sale arriba
 		#ylim y xlim son para las coordinadas del lugar que quiero
 			image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(10,35),xlim=c(-120,-85),main="Onset (DOY)",col=clr2,cex.axis=1.5,font.axis=1.5)
 			world(add=T,interior=T,lwd=3)





lat<- as.vector(replicate(71,seq(-89.75,89.75,0.5)[205:250]))
lon<- as.vector(t(replicate(46,seq(-179.75,179.75,0.5)[120:190])))
	clr2<-colorRampPalette(c("white","orangered1","yellow1","paleturquoise2","lightblue1","lightskyblue","deepskyblue","royalblue","midnightblue"))(5000)
	cat<-fin_mapa
	lim<-as.vector(t(cat))
 	rdf<-data.frame(cbind(lon,lat,lim))
 	names(rdf) <- c( "lon", "lat", "V3")
 	points_df <- rdf          # hicimos una copia para que no se borre
 	coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
 	pixel_df <- points_df
 	gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
 	rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
 		g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
 		#muestra el mapa
		#las=1 hace que las letras salen verticales
 		#axes=t que grafique los ejes
 		#horizontal, la leyenda sale arriba
 		#ylim y xlim son para las coordinadas del lugar que quiero
 			image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(10,35),xlim=c(-120,-85), zlim = c(240, 365),main="Offset (DOY)",col=clr2,cex.axis=1.5,font.axis=1.5)
 			world(add=T,interior=T,lwd=3)

lat<- as.vector(replicate(71,seq(-89.75,89.75,0.5)[205:250]))
lon<- as.vector(t(replicate(46,seq(-179.75,179.75,0.5)[120:190])))
	clr2<-colorRampPalette(c("white","orangered1","yellow1","paleturquoise2","lightblue1","lightskyblue","deepskyblue","royalblue","midnightblue"))(5000)
	cat<-duracion_mapa
	lim<-as.vector(t(cat))
 	rdf<-data.frame(cbind(lon,lat,lim))
 	names(rdf) <- c( "lon", "lat", "V3")
 	points_df <- rdf          # hicimos una copia para que no se borre
 	coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
 	pixel_df <- points_df
 	gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
 	rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
 		g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
 		#muestra el mapa
		#las=1 hace que las letras salen verticales
 		#axes=t que grafique los ejes
 		#horizontal, la leyenda sale arriba
 		#ylim y xlim son para las coordinadas del lugar que quiero
 			image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(10,35),xlim=c(-120,-85),main="Length (Days)",col=clr2,cex.axis=1.5,font.axis=1.5)
 			world(add=T,interior=T,lwd=3)
#---------------------------------------
lat<- as.vector(replicate(71,seq(-89.75,89.75,0.5)[205:250]))
lon<- as.vector(t(replicate(46,seq(-179.75,179.75,0.5)[120:190])))
cat<-inicio_tendencia
cat<-ifelse(cat>=1.5,1.5,cat)
cat<-ifelse(cat<=-1.5,-1.5,cat)
clr2 <-brewer.pal(11, "RdBu")
lim<-as.vector(t(cat))
 rdf<-data.frame(cbind(lon,lat,lim))
 names(rdf) <- c( "lon", "lat", "V3")
 points_df <- rdf          # hicimos una copia para que no se borre
 coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
 pixel_df <- points_df
 gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
 rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
 g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
 #muestra el mapa
 #las=1 hace que las letras salen verticales
 #axes=t que grafique los ejes
 #horizontal, la leyenda sale arriba
 #ylim y xlim son para las coordinadas del lugar que quiero
 image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(10,35),xlim=c(-120,-85),zlim = c(-1.5, 1.5),main="Onset Trend (Days/Years)",col=rev(clr2),cex.axis=1.5,font.axis=1.5)
 world(add=T,interior=T,lwd=3)


cat<-fin_tendencia
cat<-ifelse(cat>=1.5,1.5,cat)
cat<-ifelse(cat<=-1.5,-1.5,cat)
clr2 <- brewer.pal(11, "RdBu")
lim<-as.vector(t(cat))
 rdf<-data.frame(cbind(lon,lat,lim))
 names(rdf) <- c( "lon", "lat", "V3")
 points_df <- rdf          # hicimos una copia para que no se borre
 coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
 pixel_df <- points_df
 gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
 rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
 g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
 #muestra el mapa
 #las=1 hace que las letras salen verticales
 #axes=t que grafique los ejes
 #horizontal, la leyenda sale arriba
 #ylim y xlim son para las coordinadas del lugar que quiero
 image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(10,35),xlim=c(-120,-85),zlim = c(-1.5, 1.5),main="Offset Trend (Days/Years)",col=(clr2),cex.axis=1.5,font.axis=1.5)
 world(add=T,interior=T,lwd=3)


cat<-duracion_tendencia
cat<-ifelse(cat>=1.5,1.5,cat)
cat<-ifelse(cat<=-1.5,-1.5,cat)
clr2 <- brewer.pal(11, "RdBu")
lim<-as.vector(t(cat))
rdf<-data.frame(cbind(lon,lat,lim))
 names(rdf) <- c( "lon", "lat", "V3")
 points_df <- rdf          # hicimos una copia para que no se borre
 coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
 pixel_df <- points_df
 gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
 rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
 g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
 #muestra el mapa
 #las=1 hace que las letras salen verticales
 #axes=t que grafique los ejes
 #horizontal, la leyenda sale arriba
 #ylim y xlim son para las coordinadas del lugar que quiero
 image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(10,35),xlim=c(-120,-85),zlim = c(-1.5, 1.5),main="Length Trend (Days/Years) ",col=(clr2),cex.axis=1.5,font.axis=1.5)
 world(add=T,interior=T,lwd=3)
mtext("Rainy Season Dynamics 1974-2023",outer=TRUE,cex=1.8,line=0,font=2)
#dev.off()
#--------------------------------------------------
#cuadros de Northwest Mexico
#png("C:\\Users\\crist\\OneDrive\\Documentos\\Tesis\\avances\\imagenes\\test.png", width = 800, height = 320,)
#par(pty = "s")
image.plot(cat[6:40,21:43],col=clr2,main="Northwest Mexico")

cat<-duracion_tendencia
cat<-ifelse(cat>=1.5,1.5,cat)
cat<-ifelse(cat<=-1.5,-1.5,cat)
clr2 <- brewer.pal(11, "RdBu")
lim<-as.vector(t(cat))
rdf<-data.frame(cbind(lon,lat,lim))
 names(rdf) <- c( "lon", "lat", "V3")
 points_df <- rdf          # hicimos una copia para que no se borre
 coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
 pixel_df <- points_df
 gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
 rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
 g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
 #muestra el mapa
 #las=1 hace que las letras salen verticales
 #axes=t que grafique los ejes
 #horizontal, la leyenda sale arriba
 #ylim y xlim son para las coordinadas del lugar que quiero
 image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(20,34),xlim=c(-118,-105),zlim = c(-1.5, 1.5),main="Northwest Mexico (days/year)",col=(clr2),cex.axis=1.5,font.axis=1.5)
 world(add=T,interior=T,lwd=3)
mtext("Rainy Season Dynamics 1974-2023",outer=TRUE,cex=1.8,line=0,font=2)





#---------------------------------------------
#cuadro Southeast Mexico
#image.plot(cat[30:67,6:46])
cat<-duracion_tendencia
cat<-ifelse(cat>=1.5,1.5,cat)
cat<-ifelse(cat<=-1.5,-1.5,cat)
clr2 <- brewer.pal(11, "RdBu")
lim<-as.vector(t(cat))
rdf<-data.frame(cbind(lon,lat,lim))
names(rdf) <- c( "lon", "lat", "V3")
points_df <- rdf          # hicimos una copia para que no se borre
coordinates(points_df) = c("lon", "lat")     # convierte una tabla a coordenadas
pixel_df <- points_df
gridded(pixel_df) <- TRUE #transforma coordenadas a rejilla
rdf_sp = as(pixel_df, "SpatialGridDataFrame") #dice que es un mapa
g_plot <- as.image.SpatialGridDataFrame(rdf_sp["V3"]) #vuelve imagen nuestra rejilla (grid)
#muestra el mapa
#las=1 hace que las letras salen verticales
#axes=t que grafique los ejes
#horizontal, la leyenda sale arriba
#ylim y xlim son para las coordinadas del lugar que quiero
image.plot(g_plot,las=1, axes=T, horizontal=F,legend.lab="",ylim=c(14,23),xlim=c(-95,-86),zlim = c(-1.5, 1.5),main="Southeast Mexico (days/year)",col=(clr2),cex.axis=1.5,font.axis=1.5)
world(add=T,interior=T,lwd=3)
mtext("Rainy Season Dynamics 1974-2023",outer=TRUE,cex=1.8,line=0,font=2)


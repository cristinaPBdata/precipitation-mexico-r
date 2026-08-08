library(fields)
library(ncdf4)
library(sp)
library(sf)
library(RColorBrewer)
library(paletteer) 
b<-nc_open("C:\\Users\\crist\\OneDrive\\Documentos\\Tesis\\cru_ts4.08.1901.2023.pre.dat.nc")
pre<-ncvar_get(b,"pre")
a<-nc_open("C:\\Users\\crist\\OneDrive\\Documentos\\curso_netcdf\\new_landmask_0.5x0.5.nc")
mask<-ncvar_get(a)
mask2<-ifelse(mask==3,1,NaN)
pre_mask<-pre*replicate(1476,mask2)
mex_pre<-pre_mask[120:190,205:250,]
mex_pre[41,23,]
pixel1<-(mex_pre[41,23,])
me2<-1:1476
pixel2<-array(pixel1,dim=c(12,123))
mex_pre2<-array(mex_pre,dim=c(71,46,12,123))

inicio_mapa<-array(NaN,dim=c(71,46))
fin_mapa<-array(NaN,dim=c(71,46))
duracion_mapa<-array(NaN,dim=c(71,46))
inicio_tendencia<-array(NaN,dim=c(71,46))
fin_tendencia<-array(NaN,dim=c(71,46))
duracion_tendencia<-array(NaN,dim=c(71,46))
for(lati in 1:71){
	for(loni in 1:46){
		pixel3<-mex_pre2[lati,loni,,]
		if (is.na(pixel3[1,1])) {
    			next
 		}
INICIO<-array(NaN,dim=c(123))
		FIN<-array(NaN,dim=c(123))
		DURACION<-array(NaN,dim=c(123))
		for (anio in 1:123) {
    			coeficientes_por_ano <- data.frame(Coef_0 = numeric(), Coef_1 = numeric(), Coef_2 = numeric(), Coef_3 = numeric(), Coef_4 = numeric(), Coef_5 = numeric())
    			promedio_ppt<-pixel3[,anio]
    			me <- 1:12
    			mod <- lm(promedio_ppt ~ I(me) + I(me^2) + I(me^3) + I(me^4) + I(me^5))
    			coeficientes <- coef(mod)
   			coeficientes_por_ano <- rbind(coeficientes_por_ano, data.frame(
      			Coef_0 = coeficientes[1],  # Intercepto
      			Coef_1 = coeficientes[2],  # Coeficiente de 'me'
      			Coef_2 = coeficientes[3],  # Coeficiente de 'I(me^2)'
      			Coef_3 = coeficientes[4],  # Coeficiente de 'I(me^3)'
      			Coef_4 = coeficientes[5],  # Coeficiente de 'I(me^4)'
      			Coef_5 = coeficientes[6]   # Coeficiente de 'I(me^5)'
   		 ))
		day<-seq(1,12,11/365)
		CA<-as.vector(unlist(coeficientes_por_ano))
		fiteado <- CA[1] + CA[2] * day + CA[3] * (day^2) + CA[4] * (day^3) + CA[5] * (day^4) + CA[6] * (day^5)
		umbral<-52
		dias_lluviosos <- which(fiteado >umbral & fiteado >10)  
		if (length(dias_lluviosos) > 0) {
			inicio_lluvias <- dias_lluviosos[1]
			fin_lluvias <- dias_lluviosos[length(dias_lluviosos)]
			duracion <- fin_lluvias - inicio_lluvias
		} 	
		INICIO[anio]<-inicio_lluvias
		FIN[anio]<-fin_lluvias
		DURACION[anio]<-duracion
		}
a1<-mean(INICIO)
	a2<-mean(FIN)
	a3<-mean(DURACION)
	a4<-as.vector(coef(lm(INICIO[74:123]~c(1:50)))[2])
	a5<-as.vector(coef(lm(FIN[74:123]~c(1:50)))[2])
	a6<-as.vector(coef(lm(DURACION[74:123]~c(1:50)))[2])
	inicio_mapa[lati,loni]<-a1
	fin_mapa[lati,loni]<-a2
	duracion_mapa[lati,loni]<-a3
	inicio_tendencia[lati,loni]<-a4
	fin_tendencia[lati,loni]<-a5
	duracion_tendencia[lati,loni]<-a6
	}
}

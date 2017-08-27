####################################################################################################################
# Tratamiento de los datos de los consumos de energía activa y reactiva de los clientes no regulados de EPSA.
####################################################################################################################
#Se modifica el directorio de trabajo deacuerdo a donde se tengan los archivos de datos
setwd("C:/Users/Camilo Andres/Desktop/Variety/i2t/[code]")
#Se cargan ltodas las librerias y scripts necesarios 'functions_v1.R'
library(calibrate)
library(dplyr)#detach("package:MASS", unload=TRUE)
library(ggdendro)
library(ggfortify, cluster)
library(ggplot2)
library(msts)
library(reshape)
library(Rtsne)
library(tidyr)
library(tseries)
library(graphics)
library(forecast)
library(xts)
source("functions_v2.R")

#Se carga el dataset de lecturas diarias
bd_hourly<-read.table("C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/dataset_hourly_full/2015-2016.txt", header = T, sep="\t")
#bd_hourly<-load_multiple_datasets()
sectors_v1<-read.table("C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/market_sectors_v1.txt", header = T, sep="|")
clients_v1<-read.table("C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/name_clients_v1.txt", header = T, sep="|")[,c(1,2)]

#Se seleccionan las variables de interes (las cuales se van a usar para agrupar las lecturas posteriormente)
bd_h_1<-filter(bd_hourly, CLAS_DESCRIP %in% c('COMERCIAL','INDUSTRIAL'))
bd_h_1<-bd_h_1[,c(2,1,6:9)]
bd_h_1$CLAS_DESCRIP <- as.factor(as.character(bd_h_1$CLAS_DESCRIP))
colnames(bd_h_1)[1] <- "COD_CLI"
colnames(bd_h_1)[2] <- "ID_CONTRATO"
colnames(bd_h_1)[3] <- "CLASE_CLI"
colnames(bd_h_1)[6] <- "VALOR_LECTURA"
bd_h_1 <- verify_input_data_3(bd_hourly,bd_h_1)

#Se calcula el rango de fechas a partir del cual se van a filtrar las lecturas del consumo
lowerDate <- min(bd_h_1$FECHA_HORA) #"2015-01-01"
upperDate <- max(bd_h_1$FECHA_HORA) #"2016-12-31 23:00:00 COT"
#Se define una fecha limite que permita disminuir la cantidad de lecturas faltantes para los contratos
#lowerDate <- as.POSIXct('01/01/2013 00:00:00', format='%d/%m/%Y %H:%M:%S')
#upperDate <- as.POSIXct('31/12/2016 23:00:00', format='%d/%m/%Y %H:%M:%S')
bd_h_1 <- filter(bd_h_1, FECHA_HORA >= lowerDate & FECHA_HORA <= upperDate) #eliminar fechas que sobrepasen límite

####################################################################################################################
# Agrupamos las lecturas por contrato y canal para verificar que todos los     #
# contratos tienen lecturas ACTIVA y REACTIVA, y que no hay lecturas faltantes #
####################################################################################################################
#Se agrupan las lecturas por Contrato y canal, para verificar el conteo de lecturas
s1 <- summarize(group_by(bd_h_1, COD_CLI,ID_CONTRATO), count=n(), lowerDate=min(FECHA_HORA), upperDate=max(FECHA_HORA))
#Se calculan todos los periodos de tiempo en los que cada contrato, en cada canal, deberia tener asociada una lectura
allDates <- seq.POSIXt(lowerDate, upperDate, by = 'hour')

#Se eliminan las lecturas repetidas o duplicadas
bd_h_2 <- remove_duplicates_3(bd_h_1)#WAIT for 3 minutes
#Se calcula y aplica el umbral optimo que minimiza la presencia de lecturas faltantes
s2 <- compute_threshold_3(bd_h_2,allDates)
bad_cont <- unique(s2[s2$count<length(allDates)*limit,1])

bd_h_3 <- filter(bd_h_2, !ID_CONTRATO %in% bad_cont$ID_CONTRATO)
#Se adicionan las lecturas faltantes para cada contrato
bd_h_3 <- add_missing_3(bd_h_3,allDates)#WAIT for 5 minutes
s3 <- summarize(group_by(bd_h_3, COD_CLI,ID_CONTRATO), count=n(), lowerDate=min(FECHA_HORA), upperDate=max(FECHA_HORA))

##################
uni_cont <- unique(bd_h_3$ID_CONTRATO)
ta_1 <- data.frame(time=allDates)#row.names
#tr_1 <- data.frame(row.names = 1:first(s3$count))
#trate_1 <- data.frame(row.names = 1:first(s3$count))
for(c in uni_cont){
  t1<-bd_h_3[bd_h_3$ID_CONTRATO == c,5]
  ta_1<-cbind.data.frame(ta_1,t1)
  #t2<-bd_h_3[bd_h_3$ID_CONTRATO == c & bd_h_3$COD_CANAL=='REACTIVA',5]
  #tr_1<-cbind.data.frame(tr_1,t2)
  #trate_1<-cbind.data.frame(trate_1,(t2/t1))
}
# trate_1 <- do.call(data.frame,lapply(trate_1, function(x) replace(x, !is.finite(x),0)))
colnames(ta_1)[-1] <- as.character(uni_cont)
melt_v1 <- melt(ta_1, id.vars="time")
jpeg("C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/[1].jpg", width=5000, height=2000)
ggplot(data=melt_v1, aes(x=time, y=value, group=variable, color=variable)) + geom_line() + xlab('') + ylab('Energy Lecture')
dev.off()
###############
# http://robjhyndman.com/hyndsight/seasonal-periods/
# http://rpubs.com/sinhrks/plot_tsstats
# mts_1 <- msts(ta_1[,2], seasonal.periods=c(24,24*7))
# mts_1.fit <- tbats(mts_1)
# mts_1.fc <- forecast(mts_1.fit)
# plot(mts_1.fc)

acf(ta_1[,2],xaxp=c(0,length(allDates),length(allDates)))
pacf(ta_1[,2],xaxp=c(0,length(allDates),length(allDates)))

for(c in 2:ncol(ta_1)){
  Y <- ta_1[,c]
  # Dickey-Fuller test for variable and diff
  adf.test(Y, alternative="stationary", k=0)
  adf.test(diff(Y), alternative="stationary", k=0)
  # ACF and PACF
  acf(Y,xaxp=c(0,length(allDates),length(allDates)))
  pacf(Y,xaxp=c(0,length(allDates),length(allDates)))
  acf(diff(Y),xaxp=c(0,length(allDates),length(allDates)))
  pacf(diff(Y),xaxp=c(0,length(allDates),length(allDates)))
}

# autoplot(stl(ts(Y[10:(24*7)],frequency = 24), s.window = 'periodic'))+ggtitle("Decompose with frecuency = 24*7")
p1 <- autoplot(stl(ts(Y,frequency = 24*365), s.window = 'periodic'))+ggtitle("Decompose with frecuency = 24*7")
p1 <- autoplot(acf(Y, plot = F))+ggtitle("acf")
p2 <- autoplot(pacf(Y, plot = F))+ggtitle("pacf")
p3 <- autoplot(acf(diff(Y), plot = F))+ggtitle("acf(diff)")
p4 <- autoplot(pacf(diff(Y), plot = F))+ggtitle("pacf(diff)")
multiplot(p1, p2, p3, p4, cols=2)

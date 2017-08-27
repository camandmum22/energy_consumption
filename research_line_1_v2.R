####################################################################################################################
# Importe de librerias y carga inicial de datasets
####################################################################################################################

# Rutas de directorios contenedores. Modificar en caso de ser necesario
dir_data <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/"
dir_code <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/[code]/"
dir_plots <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/hourly/"
file_report <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/[hourly_v1].txt"

#Se modifica el directorio de trabajo deacuerdo a donde se tengan los archivos de datos
setwd(dir_code)
#Se cargan todas las librerias y scripts necesarios
library(arules)
library(ggfortify)
library(plyr)
library(ggbeeswarm)
library(calibrate)
library(dplyr)#detach("package:MASS", unload=TRUE)
library(ggdendro)
library(ggplot2)
library(reshape)
library(reshape2)
library(Rtsne)
library(tidyr)
#Se carga script con funciones auxiliares
source("aux_functions.R")

#Se carga el dataset de lecturas diarias
bd_daily<-read.table(paste(dir_data,"dataset_daily.txt",sep=''), header = T, sep="|",
                     colClasses=c("COD_ACTIVIDAD_ECO"="factor"))#,"COD_CLI"="factor","ID_CONTRATO"="factor"))
#Se carga el dataset de lecturas horarias
bd_hourly<-read.table(paste(dir_data,"dataset_hourly_full/2015-2016.txt",sep=""), header = T, sep="\t")
#Se carga el dataset auxiliar de sectores economicos
sectors_v1<-read.table(paste(dir_data,"market_sectors_v1.txt",sep=""), header = T, sep="|")
#Se carga el dataset auxiliar de clientes
clients_v1<-read.table(paste(dir_data,"name_clients_v1.txt",sep=""), header = T, sep="|")[,c(1,2)]
#Se carga el dataset auxiliar de clusters
clusters_v1<-read.table(paste(dir_data,"clustering_hourly_v1.txt",sep=''), header = T, sep="\t")
#Se carga el dataset auxiliar de categorizacion de clientes
categ_v1<-read.table(paste(dir_data,"categorization_v1.txt",sep=""), header = T, sep="\t")

text_size_v1 <- c(24,18,14,18,16)
# title, legend title, legend text, axis title, axis text 
text_size_v2 <- c(50,30,22,40,30,40,28)
# title, legend title, legend text, x-axis title, x-axis text, y-axis title, y-axis text 

#Se seleccionan solamente los clientes de los sectores 'COMERCIAL' e 'INDUSTRIAL'
bd_h_1<-filter(bd_hourly, CLAS_DESCRIP %in% c('COMERCIAL','INDUSTRIAL'))
#Se seleccionan las variables de interes (las cuales se van a usar para agrupar las lecturas posteriormente)
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

#######Test 6 meses#######
time <- as.POSIXct('07/01/2016 00:00:00', format='%d/%m/%Y %H:%M:%S')
t1 <- filter(bd_h_1, FECHA_HORA >= time & FECHA_HORA <= upperDate)
s1_v2 <- summarize(group_by(t1, COD_CLI,ID_CONTRATO), count=n(), lowerDate=min(FECHA_HORA), upperDate=max(FECHA_HORA))
allDates_v2 <- seq.POSIXt(time, upperDate, by = 'hour')
t2 <- remove_duplicates_3(t1)#WAIT for 3 minutes
#Se calcula y aplica el umbral optimo que minimiza la presencia de lecturas faltantes
s2_v2 <- compute_threshold_3(t2,allDates_v2)
bad_cont_v2 <- unique(s2_v2[s2_v2$count<length(allDates_v2)*limit,1])
#######Finding Gaps#######
t1 <- as.character(unique(bd_h_2$ID_CONTRATO))
for (t in t1){
  d1 <- bd_h_2[bd_h_2$ID_CONTRATO==t,4]
  d2 <- data.frame(FECHA_HORA=allDates)
  d2$value <- d2[match(d1$FECHA_HORA, d2$FECHA_HORA),1]
  #Records at the start of gaps 
  d1[diff(dtdta$V2)>1, ]
  #Records at the end of gaps 
  d1[c(1, diff(dtdta$V2))>1, ]
  #Gap dataframe 
  dfgaps <-data.frame( start= DF[c(1, diff(DF$V2))>1, ]$V2, end=   
                           DF[diff(DF$V2)>1, ]$V2) 
}

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
#Elimino contratos que no cumplen con la regla empirica
bd_h_3 <- filter(bd_h_2, !ID_CONTRATO %in% bad_cont$ID_CONTRATO)
#Elimino contrato con ID = 'null'
bd_h_3 <- bd_h_3[bd_h_3$ID_CONTRATO!='null',]
#Se adicionan las lecturas faltantes para cada contrato
bd_h_3 <- add_missing_3(bd_h_3,allDates)#WAIT for 5 minutes
s3 <- summarize(group_by(bd_h_3, COD_CLI,ID_CONTRATO), count=n(), lowerDate=min(FECHA_HORA), upperDate=max(FECHA_HORA))

####################################################################################################################
# Agrupacion de las lecturas con base a diversos criterios, para luego realizar una clusterizacion por cada agrupacion
####################################################################################################################
bd_h_3$HORA <- as.numeric(format(as.POSIXct(bd_h_3$FECHA_HORA), "%H"))
bd_h_3$DIA <- ifelse(as.numeric(format(as.POSIXct(bd_h_3$FECHA_HORA), "%w"))==0,7,as.numeric(format(as.POSIXct(bd_h_3$FECHA_HORA), "%w")))
bd_h_3$MES <- as.numeric(format(as.POSIXct(bd_h_3$FECHA_HORA), "%m"))
#Agrupacion de montos de consumo por hora del dia
s_hourday <- summarize(group_by(bd_h_3, COD_CLI,ID_CONTRATO, HORA), count=n(), VALOR_LECTURA=sum(VALOR_LECTURA))
#Agrupacion de montos de consumo por dia de la semana
s_weekday <- summarize(group_by(bd_h_3, COD_CLI,ID_CONTRATO, DIA), count=n(), VALOR_LECTURA=sum(VALOR_LECTURA))
#Agrupacion de montos de consumo por mes del año
s_month <- summarize(group_by(bd_h_3, COD_CLI,ID_CONTRATO,MES), count=n(), VALOR_LECTURA=sum(VALOR_LECTURA))
#Agrupacion de montos de consumo por dia de la semana y hora del dia
s_weekday_and_hour <- summarize(group_by(bd_h_3, COD_CLI,ID_CONTRATO, DIA,HORA), count=n(), VALOR_LECTURA=sum(VALOR_LECTURA))

uni_cont<-unique(s_hourday$ID_CONTRATO) #t<-cbind.data.frame(unique(s_hourday[,1:2]),s_weekday_v2[,-1])
s_weekday_v2 <- as.data.frame(setNames(replicate(7,numeric(0)), c(sprintf("d%d", 1:7))),stringsAsFactors=FALSE)
s_month_v2 <- as.data.frame(setNames(replicate(12,numeric(0)), c(sprintf("m%d", 1:12))),stringsAsFactors=FALSE)
s_hourday_v2 <- as.data.frame(setNames(replicate(24,numeric(0)), c(sprintf("h%d", 0:23))),stringsAsFactors=FALSE)
s_weekday_and_hour_v2 <- as.data.frame(setNames(replicate(7*24,numeric(0)), paste('d',rep(1:7,each = 24),'_h',0:23,sep = '')),stringsAsFactors=FALSE)

c = 1
while(c <= length(uni_cont)){
  t1 <- as.numeric(t(s_weekday[s_weekday$ID_CONTRATO==uni_cont[c],])[5,])
  t2 <- as.numeric(t(s_month[s_month$ID_CONTRATO==uni_cont[c],])[5,])
  t3 <- as.numeric(t(s_hourday[s_hourday$ID_CONTRATO==uni_cont[c],])[5,])
  t4 <- as.numeric(t(s_weekday_and_hour[s_weekday_and_hour$ID_CONTRATO==uni_cont[c],])[6,])
  s_weekday_v2[c,] <- t1
  s_month_v2[c,] <- t2
  s_hourday_v2[c,] <- t3
  s_weekday_and_hour_v2[c,] <- t4
  c<- c+1
}
s_weekday_v2<-cbind.data.frame(ID_CONTRATO=uni_cont,s_weekday_v2)
s_month_v2<-cbind.data.frame(ID_CONTRATO=uni_cont,s_month_v2)
s_hourday_v2<-cbind.data.frame(ID_CONTRATO=uni_cont,s_hourday_v2)
s_weekday_and_hour_v2<-cbind.data.frame(ID_CONTRATO=uni_cont,s_weekday_and_hour_v2)

################################################################################################
# franjas comerciales --> early morning (7h-9h)   morning (9h-13h)    early afternoon (13h-17h)   late afternoon (17h-21h)    night (21h-7h)
# franjas industriales v1 --> madrugada (0h-8h)   day (8h-18h)    night (18h-0h)
# franjas industriales v2 --> madrugada (22h-6h)   day (6h-14h)    night (14h-22h)
group_day_slots <- as.data.frame(t(mapply(cbind, ID_CONTRATO=as.character(uni_cont),h7a9=rowSums(s_hourday_v2[,8:9]), h9a13=rowSums(s_hourday_v2[,10:13]),
                                          h13a17=rowSums(s_hourday_v2[,14:17]),h17a21=rowSums(s_hourday_v2[,18:21]),h21a7=rowSums(s_hourday_v2[,c(22:25,2:7)]),
                                          #h0a8=rowSums(s_hourday_v2[,2:9]),h8a18=rowSums(s_hourday_v2[,10:19]),h18a0=rowSums(s_hourday_v2[,c(20:25)])
                                          h22a6=rowSums(s_hourday_v2[,c(24:25,2:7)]),h6a14=rowSums(s_hourday_v2[,8:15]),h14a22=rowSums(s_hourday_v2[,c(16:23)])
                                          )))
for(x in 2:ncol(group_day_slots)){
  group_day_slots[,x] <- as.numeric(as.character(group_day_slots[,x]))
}
colnames(group_day_slots) <- c('ID_CONTRATO',paste('fc',rep(1:5,each = 1),sep = ''),paste('fi',rep(1:3,each = 1),sep = ''))

group_v1 <- cbind.data.frame(ID_CONTRATO=uni_cont,
                             s_hourday_v2[,-1]/rowSums(s_hourday_v2[,-1]),
                             group_day_slots[,-1]/rowSums(group_day_slots[,-1]),
                             s_weekday_v2[,-1]/rowSums(s_weekday_v2[,-1]),
                             s_month_v2[,-1]/rowSums(s_month_v2[,-1]),
                             Total_Activa = rowSums(s_weekday_v2[,-1]))
out_v1<-group_v1[!is.finite(rowSums(group_v1[,-1])),]
group_v1 <-group_v1[is.finite(rowSums(group_v1[,-1])),]

# Clusterizacion por hora del dia, weekday, month, total consumo
group_v2 <- as.data.frame(scale(group_v1[,c(grep("^h0$", colnames(group_v1)):grep("^h23$", colnames(group_v1)),
                                            grep("^d1$", colnames(group_v1)):grep("^Total_Activa$", colnames(group_v1)))], scale = T))

fith_1 <- hclust(dist(group_v2, method = "euclidean"), method='ward.D')
for(x in c(2:30)){
  print(data.frame(table(as.factor(cutree(fith_1, x))),Percentage=as.data.frame(prop.table(table(as.factor(cutree(fith_1, x)))))$Freq),row.names = F)
  # png(paste(dir_plots,"hclust_dendo_k",x,'.png',sep = ''), width=8000, height=2000)
  # par(cex=1,font=1)
  # plot(fith_1, hang=-1, main="Dendrogram", label=group_v1$ID_CONTRATO)
  # rect.hclust(fith_1, k=x, border="red")
  # dev.off()
}
group_v2$cluster_h <- as.factor(cutree(fith_1, 10))
wss_11 <- (nrow(group_v2[,-c(ncol(group_v2))])-1)*sum(apply(group_v2,2,var))
for (k in 2:30){
  km <- kmeans(group_v2[,-c(ncol(group_v2))],centers=k,iter.max = 100,algorithm="MacQueen")
  wss_11[k] <- km$tot.withinss# algorithm=Lloyd,MacQueen
  print(data.frame(table(as.factor(km$cluster)),Percentage=as.data.frame(prop.table(table(as.factor(km$cluster))))$Freq),row.names = F)
}
# png(paste(dir_plots,"kmeans_codo.png",sep = ''), width=1200, height=700)
# plot(1:30, wss_11, type="b", xlab="Numero de Clusters",ylab="Withinss")
# dev.off()
group_v2$cluster_1 <- as.factor(kmeans(group_v2[,-c(ncol(group_v2))], 10,iter.max = 100,algorithm="MacQueen")$cluster)

group_v2$cluster_h <- as.factor(clusters_v1[match(group_v1$ID_CONTRATO, clusters_v1$ID_CONTRATO),2])
group_v2$cluster_1 <- as.factor(clusters_v1[match(group_v1$ID_CONTRATO, clusters_v1$ID_CONTRATO),3])
####################################################################################################################
# Analisis de Componenetes Principales
####################################################################################################################
prc_v1 <- princomp(group_v2[,-c(ncol(group_v2)-1,ncol(group_v2))], cor=TRUE, scores=TRUE)
# t1<- summary(prc_v1)
# write('------------------------------------------------------------------------------------------', file_report, append = F)
# write('--------------------------Analisis de Reduccion Multidimensional con PCA--------------------------', file_report, append = T)
# write('------------------------------------------------------------------------------------------', file_report, append = T)
# sink(file_report, append=TRUE, split=TRUE)
# print(t1)
# sink()

png(paste(dir_plots,"pca_hclust.png",sep = ''), width=1200, height=800)
autoplot(prc_v1, data=group_v2, colour = 'cluster_h', loadings = F, loadings.label = F)+ggtitle("Grafico de Reduccion Multidimensional usando PCA y Clusterizacion Jerarquica\n")+
  labs(x="\nComponente 1",y="Componente 2\n")+theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
    axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()
png(paste(dir_plots,"pca_kmeans.png",sep = ''), width=1200, height=800)
autoplot(prc_v1, data=group_v2, colour = 'cluster_1', loadings = F, loadings.label = F)+ggtitle("Grafico de Reduccion Multidimensional usando PCA y Clusterizacion con Kmeans\n")+
  labs(x="\nComponente 1",y="Componente 2\n")+theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
    axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()
####################################################################################################################
# Reduccion de dimensiones con tsn-e y Visualizacion http://distill.pub/2016/misread-tsne/
####################################################################################################################
#Rtsne v1 -> Plot in image
rtsne_v1 <- Rtsne(as.matrix(group_v2[,-c(ncol(group_v2)-1,ncol(group_v2))]), dims = 2, perplexity=30, pca=F, verbose=T, max_iter = 2000,check_duplicates=F,theta = 0.25)
t1 <- data.frame(x = rtsne_v1$Y[,1], y= rtsne_v1$Y[,2])
t1$cluster_h <- group_v2$cluster_h
t1$cluster_1 <- group_v2$cluster_1
png(paste(dir_plots,"tsne_hclust.png",sep = ''), width=1200, height=800)
ggplot(t1, aes(x, y, colour = cluster_h))+geom_point()+ggtitle("Grafico de Reduccion Multidimensional usando t-SNE y Clusterizacion Jerarquica\n")+labs(x="\nComponente 1",y="Componente 2\n")+
  theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
        axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()
png(paste(dir_plots,"tsne_kmeans.png",sep = ''), width=1200, height=800)
ggplot(t1, aes(x, y, colour = cluster_1))+geom_point()+ggtitle("Grafico de Reduccion Multidimensional usando t-SNE y Clusterizacion con Kmeans\n")+labs(x="\nComponente 1",y="Componente 2\n")+
  theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
        axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()
####################################################################################################################
# Contruccion y Generacion de Reportes
####################################################################################################################
group_v3 <- group_v2[,c(ncol(group_v2)-1,ncol(group_v2))]
group_v3$ID_CONTRATO <- group_v1$ID_CONTRATO
group_v3$COD_CLI <- bd_daily[match(group_v3$ID_CONTRATO, bd_daily$ID_CONTRATO),1]
group_v3$client <-  with(clients_v1[match(group_v3$COD_CLI, clients_v1$COD_CLI),], paste(COD_CLI, DES_CLI,sep = ' - '))
group_v3$sector <- as.factor(bd_daily[match(group_v3$ID_CONTRATO, bd_daily$ID_CONTRATO),13])
group_v3$ID_CONTRATO <- factor(group_v3$ID_CONTRATO, levels = group_v3$ID_CONTRATO[order(group_v3$sector)])
group_v3$sector_full <- paste(group_v3$sector,as.factor(sectors_v1[match(group_v3$sector, sectors_v1$COD_ACTIVIDAD_ECO),2]),sep = " - ")
group_v3$market <- as.factor(bd_hourly[match(group_v3$ID_CONTRATO, bd_hourly$SUMI_SGC_NIC),6])
  
group_v2_kmeans <- data.frame()
group_v2_hclust <- data.frame()
group_v2_kmeans_com <- data.frame()
group_v2_hclust_com <- data.frame()
group_v2_kmeans_ind <- data.frame()
group_v2_hclust_ind <- data.frame()
for(k in 1:length(unique(group_v2$cluster_1))){
  cont <- group_v1[group_v2$cluster_1 == k,1]
  cont_ind <- group_v1[group_v2$cluster_1 == k & group_v3$market=="INDUSTRIAL",1]
  cont_com <- group_v1[group_v2$cluster_1 == k & group_v3$market=="COMERCIAL",1]
  group_v2_kmeans <- rbind(group_v2_kmeans,data.frame(
    cluster=as.factor(k),
    size=length(cont),
    AVG_HOURLY=mean(s_hourday[s_hourday$ID_CONTRATO %in% cont,5]$VALOR_LECTURA),
    AVG_DAILY=mean(s_weekday[s_weekday$ID_CONTRATO %in% cont,5]$VALOR_LECTURA),
    AVG_MONTHLY=mean(s_month[s_month$ID_CONTRATO %in% cont,5]$VALOR_LECTURA),
    h0 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,2])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h1 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,3])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h2 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,4])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h3 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,5])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h4 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,6])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h5 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,7])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h6 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,8])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h7 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,9])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h8 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,10])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h9 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,11])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h10 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,12])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h11 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,13])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h12 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,14])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h13 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,15])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h14 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,16])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h15 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,17])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h16 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,18])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h17 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,19])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h18 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,20])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h19 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,21])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h20 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,22])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h21 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,23])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h22 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,24])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h23 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,25])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    fc1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,2])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,3])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,4])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc4 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,5])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc5 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,6])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fi1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,7])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(7:9)]),
    fi2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,8])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(7:9)]),
    fi3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,9])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(7:9)]),
    d1 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d1)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d2 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d2)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d3 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d3)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d4 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d4)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d5 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d5)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d6 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d6)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d7 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d7)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    m1 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m1)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m2 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m2)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m3 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m3)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m4 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m4)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m5 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m5)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m6 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m6)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m7 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m7)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m8 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m8)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m9 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m9)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m10 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m10)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m11 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m11)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m12 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m12)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1])
  ))
  if(length(cont_com)>0){
    group_v2_kmeans_com <- rbind(group_v2_kmeans_com,data.frame(
      cluster=as.factor(k),
      size=length(cont_com),
      AVG_HOURLY=mean(s_hourday[s_hourday$ID_CONTRATO %in% cont_com,5]$VALOR_LECTURA),
      AVG_DAILY=mean(s_weekday[s_weekday$ID_CONTRATO %in% cont_com,5]$VALOR_LECTURA),
      AVG_MONTHLY=mean(s_month[s_month$ID_CONTRATO %in% cont_com,5]$VALOR_LECTURA),
      h0 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,2])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h1 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,3])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h2 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,4])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h3 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,5])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h4 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,6])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h5 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,7])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h6 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,8])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h7 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,9])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h8 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,10])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h9 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,11])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h10 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,12])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h11 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,13])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h12 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,14])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h13 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,15])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h14 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,16])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h15 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,17])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h16 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,18])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h17 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,19])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h18 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,20])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h19 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,21])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h20 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,22])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h21 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,23])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h22 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,24])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h23 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,25])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      fc1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,2])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,3])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,4])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc4 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,5])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc5 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,6])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fi1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,7])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(7:9)]),
      fi2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,8])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(7:9)]),
      fi3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,9])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(7:9)]),
      d1 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d1)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d2 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d2)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d3 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d3)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d4 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d4)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d5 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d5)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d6 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d6)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d7 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d7)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      m1 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m1)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m2 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m2)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m3 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m3)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m4 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m4)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m5 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m5)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m6 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m6)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m7 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m7)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m8 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m8)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m9 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m9)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m10 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m10)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m11 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m11)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m12 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m12)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1])
    ))
  }
  group_v2_kmeans_ind <- rbind(group_v2_kmeans_ind,data.frame(
    cluster=as.factor(k),
    size=length(cont_ind),
    AVG_HOURLY=mean(s_hourday[s_hourday$ID_CONTRATO %in% cont_ind,5]$VALOR_LECTURA),
    AVG_DAILY=mean(s_weekday[s_weekday$ID_CONTRATO %in% cont_ind,5]$VALOR_LECTURA),
    AVG_MONTHLY=mean(s_month[s_month$ID_CONTRATO %in% cont_ind,5]$VALOR_LECTURA),
    h0 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,2])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h1 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,3])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h2 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,4])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h3 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,5])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h4 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,6])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h5 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,7])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h6 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,8])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h7 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,9])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h8 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,10])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h9 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,11])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h10 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,12])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h11 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,13])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h12 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,14])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h13 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,15])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h14 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,16])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h15 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,17])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h16 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,18])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h17 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,19])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h18 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,20])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h19 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,21])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h20 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,22])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h21 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,23])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h22 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,24])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h23 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,25])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    fc1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,2])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,3])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,4])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc4 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,5])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc5 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,6])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fi1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,7])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(7:9)]),
    fi2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,8])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(7:9)]),
    fi3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,9])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(7:9)]),
    d1 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d1)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d2 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d2)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d3 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d3)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d4 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d4)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d5 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d5)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d6 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d6)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d7 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d7)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    m1 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m1)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m2 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m2)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m3 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m3)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m4 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m4)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m5 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m5)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m6 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m6)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m7 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m7)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m8 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m8)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m9 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m9)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m10 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m10)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m11 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m11)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m12 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m12)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1])
  ))
}
for(k in 1:length(unique(group_v2$cluster_h))){
  cont <- group_v1[group_v2$cluster_h == k,1]
  cont_ind <- group_v1[group_v2$cluster_h == k & group_v3$market=="INDUSTRIAL",1]
  cont_com <- group_v1[group_v2$cluster_h == k & group_v3$market=="COMERCIAL",1]
  group_v2_hclust <- rbind(group_v2_hclust,data.frame(
    cluster=as.factor(k),
    size=length(cont),
    AVG_HOURLY=mean(s_hourday[s_hourday$ID_CONTRATO %in% cont,5]$VALOR_LECTURA),
    AVG_DAILY=mean(s_weekday[s_weekday$ID_CONTRATO %in% cont,5]$VALOR_LECTURA),
    AVG_MONTHLY=mean(s_month[s_month$ID_CONTRATO %in% cont,5]$VALOR_LECTURA),
    h0 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,2])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h1 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,3])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h2 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,4])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h3 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,5])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h4 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,6])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h5 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,7])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h6 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,8])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h7 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,9])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h8 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,10])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h9 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,11])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h10 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,12])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h11 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,13])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h12 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,14])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h13 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,15])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h14 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,16])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h15 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,17])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h16 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,18])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h17 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,19])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h18 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,20])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h19 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,21])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h20 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,22])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h21 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,23])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h22 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,24])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    h23 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,25])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont,-1]),
    fc1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,2])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,3])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,4])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc4 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,5])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fc5 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,6])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(2:6)]),
    fi1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,7])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(7:9)]),
    fi2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,8])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(7:9)]),
    fi3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,9])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont,c(7:9)]),
    d1 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d1)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d2 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d2)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d3 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d3)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d4 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d4)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d5 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d5)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d6 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d6)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    d7 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,]$d7)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont,-1]),
    m1 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m1)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m2 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m2)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m3 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m3)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m4 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m4)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m5 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m5)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m6 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m6)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m7 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m7)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m8 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m8)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m9 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m9)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m10 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m10)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m11 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m11)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1]),
    m12 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,]$m12)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont,-1])
  ))
  if(length(cont_com)>0){
    group_v2_hclust_com <- rbind(group_v2_hclust_com,data.frame(
      cluster=as.factor(k),
      size=length(cont_com),
      AVG_HOURLY=mean(s_hourday[s_hourday$ID_CONTRATO %in% cont_com,5]$VALOR_LECTURA),
      AVG_DAILY=mean(s_weekday[s_weekday$ID_CONTRATO %in% cont_com,5]$VALOR_LECTURA),
      AVG_MONTHLY=mean(s_month[s_month$ID_CONTRATO %in% cont_com,5]$VALOR_LECTURA),
      h0 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,2])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h1 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,3])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h2 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,4])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h3 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,5])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h4 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,6])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h5 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,7])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h6 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,8])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h7 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,9])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h8 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,10])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h9 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,11])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h10 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,12])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h11 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,13])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h12 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,14])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h13 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,15])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h14 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,16])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h15 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,17])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h16 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,18])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h17 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,19])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h18 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,20])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h19 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,21])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h20 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,22])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h21 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,23])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h22 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,24])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      h23 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,25])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_com,-1]),
      fc1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,2])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,3])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,4])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc4 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,5])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fc5 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,6])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(2:6)]),
      fi1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,7])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(7:9)]),
      fi2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,8])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(7:9)]),
      fi3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,9])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_com,c(7:9)]),
      d1 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d1)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d2 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d2)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d3 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d3)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d4 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d4)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d5 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d5)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d6 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d6)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      d7 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,]$d7)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_com,-1]),
      m1 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m1)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m2 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m2)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m3 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m3)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m4 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m4)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m5 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m5)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m6 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m6)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m7 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m7)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m8 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m8)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m9 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m9)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m10 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m10)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m11 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m11)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1]),
      m12 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,]$m12)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_com,-1])
    ))
  }
  group_v2_hclust_ind <- rbind(group_v2_hclust_ind,data.frame(
    cluster=as.factor(k),
    size=length(cont_ind),
    AVG_HOURLY=mean(s_hourday[s_hourday$ID_CONTRATO %in% cont_ind,5]$VALOR_LECTURA),
    AVG_DAILY=mean(s_weekday[s_weekday$ID_CONTRATO %in% cont_ind,5]$VALOR_LECTURA),
    AVG_MONTHLY=mean(s_month[s_month$ID_CONTRATO %in% cont_ind,5]$VALOR_LECTURA),
    h0 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,2])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h1 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,3])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h2 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,4])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h3 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,5])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h4 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,6])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h5 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,7])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h6 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,8])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h7 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,9])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h8 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,10])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h9 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,11])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h10 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,12])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h11 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,13])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h12 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,14])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h13 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,15])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h14 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,16])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h15 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,17])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h16 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,18])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h17 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,19])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h18 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,20])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h19 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,21])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h20 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,22])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h21 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,23])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h22 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,24])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    h23 = sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,25])/sum(s_hourday_v2[s_hourday_v2$ID_CONTRATO %in% cont_ind,-1]),
    fc1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,2])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,3])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,4])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc4 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,5])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fc5 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,6])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(2:6)]),
    fi1 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,7])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(7:9)]),
    fi2 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,8])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(7:9)]),
    fi3 = sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,9])/sum(group_day_slots[s_hourday_v2$ID_CONTRATO %in% cont_ind,c(7:9)]),
    d1 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d1)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d2 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d2)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d3 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d3)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d4 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d4)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d5 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d5)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d6 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d6)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    d7 = sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,]$d7)/sum(s_weekday_v2[s_weekday_v2$ID_CONTRATO %in% cont_ind,-1]),
    m1 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m1)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m2 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m2)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m3 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m3)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m4 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m4)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m5 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m5)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m6 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m6)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m7 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m7)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m8 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m8)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m9 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m9)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m10 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m10)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m11 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m11)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1]),
    m12 = sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,]$m12)/sum(s_month_v2[s_month_v2$ID_CONTRATO %in% cont_ind,-1])
  ))
}

t1 <- data.frame(group_v1,group_v3[,-c(3,4,6)])
options(scipen=999)
write('------------------------------------------------------------------------------------------', file_report, append = F)
write('---------------------------------Clusterizacion Jerarquica---------------------------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(group_v2_hclust, file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
for(k in 1:length(unique(group_v3$cluster_h))){
  cont <- group_v3[group_v3$cluster_h == k,3]
  write(paste("---------------------------------Cluster ",k,'---------------------------------',sep = ''), file_report, append = T)
  s1 <- summarize(group_by(group_v3[group_v3$cluster_h == k,], sector_full), count=n())
  s2 <- summarize(group_by(group_v3[group_v3$cluster_h == k,], client), count=n())
  write("-----------------Sectores-----------------", file_report, append = T)
  write.table(s1[order(-s1$count),c(2,1)], file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
  write("-----------------Clientes-----------------", file_report, append = T)
  write.table(s2[order(-s2$count),c(2,1)], file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
  write("-----------------Tabla Completa-----------------", file_report, append = T)
  write.table(format(t1[t1$ID_CONTRATO %in% cont,], digits=2), file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
}
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('---------------------------------Clusterizacion con Kmeans---------------------------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(group_v2_kmeans, file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
for(k in 1:length(unique(group_v3$cluster_1))){
  cont <- group_v3[group_v3$cluster_1 == k,3]
  write(paste("---------------------------------Cluster ",k,'---------------------------------',sep = ''), file_report, append = T)
  s1 <- summarize(group_by(group_v3[group_v3$cluster_h == k,], sector_full), count=n())
  s2 <- summarize(group_by(group_v3[group_v3$cluster_h == k,], client), count=n())
  write("-----------------Sectores-----------------", file_report, append = T)
  write.table(s1[order(-s1$count),c(2,1)], file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
  write("-----------------Clientes-----------------", file_report, append = T)
  write.table(s2[order(-s2$count),c(2,1)], file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
  write("-----------------Tabla Completa-----------------", file_report, append = T)
  write.table(format(t1[t1$ID_CONTRATO %in% cont,], digits=2), file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
}

group_v1$ID_CONTRATO <- as.factor(group_v1$ID_CONTRATO)
plot_dim <- c(2800,1500,2000,3000)
sizes <- c(5,2,NA,0.2,0.1,12)
#mean points size, data points size, tick y axis (total), tick y axis (percentage)

lab_hours <- c(sprintf("%d am", c(12,1:11)),sprintf("%d pm", c(12,1:11)))
lab_segments <- c('7h-9h','9h-13h','13h-17h','17h-21h','21h-7h')
lab_segments_v2 <- c('22h-6h','6h-14h','14h-22h')#c('0h-8h','8h-18h','18h-0h')
lab_days <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))
lab_months <-  months(x=as.Date(seq(length.out = 12,by = 31), origin="1950-01-01"))
lab_clust <- c(' (Usando Jerarquico)\n',' (Usando Kmeans)\n')

for(k in 1:length(unique(group_v2$cluster_h))){
  # melt_aux <- melt(group_v1[group_v2$cluster_h==as.character(k) & group_v3$market=='COMERCIAL',], id.vars=c("ID_CONTRATO"))
  # m1_segments <- melt_aux[substring(melt_aux$variable, 1, 2)=='fc',]
  # melt_aux <- melt(group_v1[group_v2$cluster_h==as.character(k) & group_v3$market=='INDUSTRIAL',], id.vars=c("ID_CONTRATO"))
  # m1_segments_v2 <- melt_aux[substring(melt_aux$variable, 1, 2)=='fi',]
  melt_v1 <- melt(group_v1[group_v2$cluster_h==as.character(k),], id.vars=c("ID_CONTRATO"))
  m1_segments <- melt_v1[substring(melt_v1$variable, 1, 2)=='fc',]
  m1_segments_v2 <- melt_v1[substring(melt_v1$variable, 1, 2)=='fi',]
  m1_days <- melt_v1[substring(melt_v1$variable, 1, 1)=='d',]
  m1_months <- melt_v1[substring(melt_v1$variable, 1, 1)=='m',]
  m1_total <- melt_v1[substring(melt_v1$variable, 1, 1)=='T',]
  m1_total <- m1_total[order(m1_total$value),]
  m1_hours <- melt_v1[substring(melt_v1$variable, 1, 1)=='h',]
  m1_segments$variable <- as.numeric(substring(as.character(m1_segments$variable), 3))
  m1_segments_v2$variable <- as.numeric(substring(as.character(m1_segments_v2$variable), 3))
  m1_days$variable <- as.numeric(substring(as.character(m1_days$variable), 2))
  m1_months$variable <- as.numeric(substring(as.character(m1_months$variable), 2))
  m1_hours$variable <- as.numeric(substring(as.character(m1_hours$variable), 2))+1
  
  # melt_aux <- melt(group_v1[group_v2$cluster_1==as.character(k) & group_v3$market=='COMERCIAL',], id.vars=c("ID_CONTRATO"))
  # m2_segments <- melt_aux[substring(melt_aux$variable, 1, 2)=='fc',]
  # melt_aux <- melt(group_v1[group_v2$cluster_1==as.character(k) & group_v3$market=='INDUSTRIAL',], id.vars=c("ID_CONTRATO"))
  # m2_segments_v2 <- melt_aux[substring(melt_aux$variable, 1, 2)=='fi',]
  melt_v2 <- melt(group_v1[group_v2$cluster_1==as.character(k),], id.vars=c("ID_CONTRATO"))
  m2_segments <- melt_v2[substring(melt_v2$variable, 1, 2)=='fc',]
  m2_segments_v2 <- melt_v2[substring(melt_v2$variable, 1, 2)=='fi',]
  m2_days <- melt_v2[substring(melt_v2$variable, 1, 1)=='d',]
  m2_months <- melt_v2[substring(melt_v2$variable, 1, 1)=='m',]
  m2_total <- melt_v2[substring(melt_v2$variable, 1, 1)=='T',]
  m2_total <- m2_total[order(m2_total$value),]
  m2_hours <- melt_v2[substring(melt_v2$variable, 1, 1)=='h',]
  m2_segments$variable <- as.numeric(substring(as.character(m2_segments$variable), 3))
  m2_segments_v2$variable <- as.numeric(substring(as.character(m2_segments_v2$variable), 3))
  m2_days$variable <- as.numeric(substring(as.character(m2_days$variable), 2))
  m2_months$variable <- as.numeric(substring(as.character(m2_months$variable), 2))
  m2_hours$variable <- as.numeric(substring(as.character(m2_hours$variable), 2))+1
  
  m1_ncol <- nrow(m1_total) %/% (plot_dim[2]/30) +if_else(nrow(m1_total) %% (plot_dim[2]/30) >0,1,0)
  m2_ncol <- nrow(m2_total) %/% (plot_dim[2]/30) +if_else(nrow(m2_total) %% (plot_dim[2]/30) >0,1,0)
  m1_scale <- -qt(c(0.025,0.05,0.1), df=nrow(m1_total)-1)*(sd(m1_total$value)/sqrt(nrow(m1_total)))
  m2_scale <- -qt(c(0.025,0.05,0.1), df=nrow(m2_total)-1)*(sd(m2_total$value)/sqrt(nrow(m2_total)))
  #geom_quasirandom(dodge.width=0.5)
  g1 <- ggplot(m1_segments, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
    scale_y_continuous(breaks = seq(min(m1_segments$value,0), max(m1_segments$value), by = round(max(m1_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
  g1_v2 <- ggplot(m1_segments_v2, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
    scale_y_continuous(breaks = seq(min(m1_segments_v2$value,0), max(m1_segments_v2$value), by = round(max(m1_segments_v2$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
  g2 <- ggplot(m1_days, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
    scale_y_continuous(breaks = seq(min(m1_days$value,0), max(m1_days$value), by = round(max(m1_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
  g3 <- ggplot(m1_months, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
    scale_y_continuous(breaks = seq(min(m1_months$value,0), max(m1_months$value), by = round(max(m1_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
  g5 <- ggplot(m1_hours, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
    scale_y_continuous(breaks = seq(min(m1_hours$value,0), max(m1_hours$value), by = round(max(m1_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
  g4 <- ggplot(m1_total, aes(x=1:nrow(m1_total), y=value))+ggtitle(paste("Consumos totales, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Contrato\n', y="\nValor Consumo")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(3, "cm"), legend.position=c(0.9, 0.3),legend.title = element_text(size=text_size_v2[2]*2),
          legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_blank(), axis.text.x = element_text(size=text_size_v2[6]))+
    geom_bar(stat='identity',aes(fill=value),width=.5)+coord_flip()+scale_y_continuous(breaks = seq(min(m1_total$value,0), max(m1_total$value),  by = round(max(m1_total$value)*sizes[5],0)))+
    annotate("rect", ymin = mean(m1_total$value)-m1_scale[1], ymax = mean(m1_total$value)+m1_scale[1], xmin = -Inf, xmax = Inf, fill = "gray60", alpha = 0.05)+
    annotate("rect", ymin = mean(m1_total$value)-m1_scale[2], ymax = mean(m1_total$value)+m1_scale[2], xmin = -Inf, xmax = Inf, fill = "gray40", alpha = 0.05)+
    annotate("rect", ymin = mean(m1_total$value)-m1_scale[3], ymax = mean(m1_total$value)+m1_scale[3], xmin = -Inf, xmax = Inf, fill = "gray20", alpha = 0.05)
  # g4 <- geom_bar(stat='identity',width=.5)+scale_x_discrete(limits=nrow(m1_total):1), geom_quasirandom(stat='identity', size=10)
  
  g6 <- ggplot(m2_segments, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
    scale_y_continuous(breaks = seq(min(m2_segments$value,0), max(m2_segments$value), by = round(max(m2_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
  g6_v2 <- ggplot(m2_segments_v2, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
    scale_y_continuous(breaks = seq(min(m2_segments_v2$value,0), max(m2_segments_v2$value), by = round(max(m2_segments_v2$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
  g7 <- ggplot(m2_days, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
    scale_y_continuous(breaks = seq(min(m2_days$value,0), max(m2_days$value), by = round(max(m2_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
  g8 <- ggplot(m2_months, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
    scale_y_continuous(breaks = seq(min(m2_months$value,0), max(m2_months$value), by = round(max(m2_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
  g10 <- ggplot(m2_hours, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_quasirandom(size= sizes[2])+ggtitle(paste("Patrones de Consumo, Cluster ",k,lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
    scale_y_continuous(breaks = seq(min(m2_hours$value,0), max(m2_hours$value), by = round(max(m2_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
  g9 <- ggplot(m2_total, aes(x=1:nrow(m2_total), y=value))+ggtitle(paste("Consumos totales, Cluster ",k,lab_clust[2],sep = ""))+labs(x='Contrato\n', y="\nValor Consumo")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(3, "cm"), legend.position=c(0.9, 0.3),legend.title = element_text(size=text_size_v2[2]*2),
          legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_blank(), axis.text.x = element_text(size=text_size_v2[6]))+
    geom_bar(stat='identity',aes(fill=value),width=.5)+coord_flip()+scale_y_continuous(breaks = seq(min(m2_total$value,0), max(m2_total$value),  by = round(max(m2_total$value)*sizes[5],0)))+
    annotate("rect", ymin = mean(m2_total$value)-m2_scale[1], ymax = mean(m2_total$value)+m2_scale[1], xmin = -Inf, xmax = Inf, fill = "gray60", alpha = 0.05)+
    annotate("rect", ymin = mean(m2_total$value)-m2_scale[2], ymax = mean(m2_total$value)+m2_scale[2], xmin = -Inf, xmax = Inf, fill = "gray40", alpha = 0.05)+
    annotate("rect", ymin = mean(m2_total$value)-m2_scale[3], ymax = mean(m2_total$value)+m2_scale[3], xmin = -Inf, xmax = Inf, fill = "gray20", alpha = 0.05)
  # g9 <- geom_bar(stat='identity', aes(fill=ID_CONTRATO),width=.5)+scale_x_discrete(limits=nrow(m2_total):1), geom_quasirandom(stat='identity', size=10)
  
  png(paste(dir_plots,"hclust_k",k,'_segments_comercial.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g1)
  dev.off()
  png(paste(dir_plots,"hclust_k",k,'_daily.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g2)
  dev.off()
  png(paste(dir_plots,"hclust_k",k,'_monthly.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g3)
  dev.off()
  png(paste(dir_plots,"hclust_k",k,'_total.png',sep = ""), width=plot_dim[1], height=plot_dim[2])#width=plot_dim[3], height=plot_dim[4])
  plot(g4)
  dev.off()
  png(paste(dir_plots,"hclust_k",k,'_hourly.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g5)
  dev.off()
  png(paste(dir_plots,"kmeans_k",k,'_segments_comercial.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g6)
  dev.off()
  png(paste(dir_plots,"kmeans_k",k,'_daily.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g7)
  dev.off()
  png(paste(dir_plots,"kmeans_k",k,'_monthly.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g8)
  dev.off()
  png(paste(dir_plots,"kmeans_k",k,'_total.png',sep = ""), width=plot_dim[1], height=plot_dim[2])#width=plot_dim[3], height=plot_dim[4])
  plot(g9)
  dev.off()
  png(paste(dir_plots,"kmeans_k",k,'_hourly.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g10)
  dev.off()
  png(paste(dir_plots,"hclust_k",k,'_segments_industrial.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g1_v2)
  dev.off()
  png(paste(dir_plots,"kmeans_k",k,'_segments_industrial.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g6_v2)
  dev.off()
}

melt_v1 <- melt(group_v2_hclust, id.vars=c("cluster"))
m1_segments <- melt_v1[substring(melt_v1$variable, 1, 2)=='fc',]
m1_segments_v2 <- melt_v1[substring(melt_v1$variable, 1, 2)=='fi',]
m1_days <- melt_v1[substring(melt_v1$variable, 1, 1)=='d',]
m1_months <- melt_v1[substring(melt_v1$variable, 1, 1)=='m',]
m1_hours <- melt_v1[substring(melt_v1$variable, 1, 1)=='h',]
m1_segments$variable <- as.numeric(substring(as.character(m1_segments$variable), 3))
m1_segments_v2$variable <- as.numeric(substring(as.character(m1_segments_v2$variable), 3))
m1_days$variable <- as.numeric(substring(as.character(m1_days$variable), 2))
m1_months$variable <- as.numeric(substring(as.character(m1_months$variable), 2))
m1_hours$variable <- as.numeric(substring(as.character(m1_hours$variable), 2))+1

melt_v1_com <- melt(group_v2_hclust_com, id.vars=c("cluster"))
m1_com_segments <- melt_v1_com[substring(melt_v1_com$variable, 1, 2)=='fc',]
m1_com_segments_v2 <- melt_v1_com[substring(melt_v1_com$variable, 1, 2)=='fi',]
m1_com_days <- melt_v1_com[substring(melt_v1_com$variable, 1, 1)=='d',]
m1_com_months <- melt_v1_com[substring(melt_v1_com$variable, 1, 1)=='m',]
m1_com_hours <- melt_v1_com[substring(melt_v1_com$variable, 1, 1)=='h',]
m1_com_segments$variable <- as.numeric(substring(as.character(m1_com_segments$variable), 3))
m1_com_segments_v2$variable <- as.numeric(substring(as.character(m1_com_segments_v2$variable), 3))
m1_com_days$variable <- as.numeric(substring(as.character(m1_com_days$variable), 2))
m1_com_months$variable <- as.numeric(substring(as.character(m1_com_months$variable), 2))
m1_com_hours$variable <- as.numeric(substring(as.character(m1_com_hours$variable), 2))+1

melt_v1_ind <- melt(group_v2_hclust_ind, id.vars=c("cluster"))
m1_ind_segments <- melt_v1_ind[substring(melt_v1_ind$variable, 1, 2)=='fc',]
m1_ind_segments_v2 <- melt_v1_ind[substring(melt_v1_ind$variable, 1, 2)=='fi',]
m1_ind_days <- melt_v1_ind[substring(melt_v1_ind$variable, 1, 1)=='d',]
m1_ind_months <- melt_v1_ind[substring(melt_v1_ind$variable, 1, 1)=='m',]
m1_ind_hours <- melt_v1_ind[substring(melt_v1_ind$variable, 1, 1)=='h',]
m1_ind_segments$variable <- as.numeric(substring(as.character(m1_ind_segments$variable), 3))
m1_ind_segments_v2$variable <- as.numeric(substring(as.character(m1_ind_segments_v2$variable), 3))
m1_ind_days$variable <- as.numeric(substring(as.character(m1_ind_days$variable), 2))
m1_ind_months$variable <- as.numeric(substring(as.character(m1_ind_months$variable), 2))
m1_ind_hours$variable <- as.numeric(substring(as.character(m1_ind_hours$variable), 2))+1

melt_v2 <- melt(group_v2_kmeans, id.vars=c("cluster"))
m2_segments <- melt_v2[substring(melt_v2$variable, 1, 2)=='fc',]
m2_segments_v2 <- melt_v2[substring(melt_v2$variable, 1, 2)=='fi',]
m2_days <- melt_v2[substring(melt_v2$variable, 1, 1)=='d',]
m2_months <- melt_v2[substring(melt_v2$variable, 1, 1)=='m',]
m2_hours <- melt_v2[substring(melt_v2$variable, 1, 1)=='h',]
m2_segments$variable <- as.numeric(substring(as.character(m2_segments$variable), 3))
m2_segments_v2$variable <- as.numeric(substring(as.character(m2_segments_v2$variable), 3))
m2_days$variable <- as.numeric(substring(as.character(m2_days$variable), 2))
m2_months$variable <- as.numeric(substring(as.character(m2_months$variable), 2))
m2_hours$variable <- as.numeric(substring(as.character(m2_hours$variable), 2))+1

melt_v2_com <- melt(group_v2_kmeans_com, id.vars=c("cluster"))
m2_com_segments <- melt_v2_com[substring(melt_v2_com$variable, 1, 2)=='fc',]
m2_com_segments_v2 <- melt_v2_com[substring(melt_v2_com$variable, 1, 2)=='fi',]
m2_com_days <- melt_v2_com[substring(melt_v2_com$variable, 1, 1)=='d',]
m2_com_months <- melt_v2_com[substring(melt_v2_com$variable, 1, 1)=='m',]
m2_com_hours <- melt_v2_com[substring(melt_v2_com$variable, 1, 1)=='h',]
m2_com_segments$variable <- as.numeric(substring(as.character(m2_com_segments$variable), 3))
m2_com_segments_v2$variable <- as.numeric(substring(as.character(m2_com_segments_v2$variable), 3))
m2_com_days$variable <- as.numeric(substring(as.character(m2_com_days$variable), 2))
m2_com_months$variable <- as.numeric(substring(as.character(m2_com_months$variable), 2))
m2_com_hours$variable <- as.numeric(substring(as.character(m2_com_hours$variable), 2))+1

melt_v2_ind <- melt(group_v2_kmeans_ind, id.vars=c("cluster"))
m2_ind_segments <- melt_v2_ind[substring(melt_v2_ind$variable, 1, 2)=='fc',]
m2_ind_segments_v2 <- melt_v2_ind[substring(melt_v2_ind$variable, 1, 2)=='fi',]
m2_ind_days <- melt_v2_ind[substring(melt_v2_ind$variable, 1, 1)=='d',]
m2_ind_months <- melt_v2_ind[substring(melt_v2_ind$variable, 1, 1)=='m',]
m2_ind_hours <- melt_v2_ind[substring(melt_v2_ind$variable, 1, 1)=='h',]
m2_ind_segments$variable <- as.numeric(substring(as.character(m2_ind_segments$variable), 3))
m2_ind_segments_v2$variable <- as.numeric(substring(as.character(m2_ind_segments_v2$variable), 3))
m2_ind_days$variable <- as.numeric(substring(as.character(m2_ind_days$variable), 2))
m2_ind_months$variable <- as.numeric(substring(as.character(m2_ind_months$variable), 2))
m2_ind_hours$variable <- as.numeric(substring(as.character(m2_ind_hours$variable), 2))+1

m1_ncol <- length(unique(melt_v1$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v1$cluster)) %% (plot_dim[2]/30) >0,1,0)
m2_ncol <- length(unique(melt_v2$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v2$cluster)) %% (plot_dim[2]/30) >0,1,0)
m1_com_ncol <- length(unique(melt_v1_com$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v1_com$cluster)) %% (plot_dim[2]/30) >0,1,0)
m2_com_ncol <- length(unique(melt_v2_com$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v2_com$cluster)) %% (plot_dim[2]/30) >0,1,0)
m1_ind_ncol <- length(unique(melt_v1_ind$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v1_ind$cluster)) %% (plot_dim[2]/30) >0,1,0)
m2_ind_ncol <- length(unique(melt_v2_ind$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v2_ind$cluster)) %% (plot_dim[2]/30) >0,1,0)
#geom_quasirandom(dodge.width=0.5)
m1_s1 <- ggplot(m1_segments, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
  scale_y_continuous(breaks = seq(min(m1_segments$value,0), max(m1_segments$value), by = round(max(m1_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
m1_s2 <- ggplot(m1_segments_v2, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
  scale_y_continuous(breaks = seq(min(m1_segments$value,0), max(m1_segments$value), by = round(max(m1_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
m1_d <- ggplot(m1_days, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m1_days$value,0), max(m1_days$value), by = round(max(m1_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
m1_m <- ggplot(m1_months, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m1_months$value,0), max(m1_months$value), by = round(max(m1_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
m1_h <- ggplot(m1_hours, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
  scale_y_continuous(breaks = seq(min(m1_hours$value,0), max(m1_hours$value), by = round(max(m1_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))

m1_com_s1 <- ggplot(m1_com_segments, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
  scale_y_continuous(breaks = seq(min(m1_com_segments$value,0), max(m1_com_segments$value), by = round(max(m1_com_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_com_ncol))
m1_com_s2 <- ggplot(m1_com_segments_v2, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
  scale_y_continuous(breaks = seq(min(m1_com_segments$value,0), max(m1_com_segments$value), by = round(max(m1_com_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_com_ncol))
m1_com_d <- ggplot(m1_com_days, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m1_com_days$value,0), max(m1_com_days$value), by = round(max(m1_com_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_com_ncol))
m1_com_m <- ggplot(m1_com_months, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m1_com_months$value,0), max(m1_com_months$value), by = round(max(m1_com_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_com_ncol))
m1_com_h <- ggplot(m1_com_hours, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
  scale_y_continuous(breaks = seq(min(m1_com_hours$value,0), max(m1_com_hours$value), by = round(max(m1_com_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_com_ncol))

m1_ind_s1 <- ggplot(m1_ind_segments, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
  scale_y_continuous(breaks = seq(min(m1_ind_segments$value,0), max(m1_ind_segments$value), by = round(max(m1_ind_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ind_ncol))
m1_ind_s2 <- ggplot(m1_ind_segments_v2, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
  scale_y_continuous(breaks = seq(min(m1_ind_segments$value,0), max(m1_ind_segments$value), by = round(max(m1_ind_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ind_ncol))
m1_ind_d <- ggplot(m1_ind_days, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m1_ind_days$value,0), max(m1_ind_days$value), by = round(max(m1_ind_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ind_ncol))
m1_ind_m <- ggplot(m1_ind_months, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m1_ind_months$value,0), max(m1_ind_months$value), by = round(max(m1_ind_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ind_ncol))
m1_ind_h <- ggplot(m1_ind_hours, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[1],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
  scale_y_continuous(breaks = seq(min(m1_ind_hours$value,0), max(m1_ind_hours$value), by = round(max(m1_ind_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ind_ncol))

m2_s1 <- ggplot(m2_segments, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
  scale_y_continuous(breaks = seq(min(m2_segments$value,0), max(m2_segments$value), by = round(max(m2_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
m2_s2 <- ggplot(m2_segments_v2, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
  scale_y_continuous(breaks = seq(min(m2_segments$value,0), max(m2_segments$value), by = round(max(m2_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
m2_d <- ggplot(m2_days, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m2_days$value,0), max(m2_days$value), by = round(max(m2_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
m2_m <- ggplot(m2_months, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m2_months$value,0), max(m2_months$value), by = round(max(m2_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))
m2_h <- ggplot(m2_hours, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
  scale_y_continuous(breaks = seq(min(m2_hours$value,0), max(m2_hours$value), by = round(max(m2_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ncol))

m2_com_s1 <- ggplot(m2_com_segments, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
  scale_y_continuous(breaks = seq(min(m2_com_segments$value,0), max(m2_com_segments$value), by = round(max(m2_com_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_com_ncol))
m2_com_s2 <- ggplot(m2_com_segments_v2, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
  scale_y_continuous(breaks = seq(min(m2_com_segments$value,0), max(m2_com_segments$value), by = round(max(m2_com_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_com_ncol))
m2_com_d <- ggplot(m2_com_days, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m2_com_days$value,0), max(m2_com_days$value), by = round(max(m2_com_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_com_ncol))
m2_com_m <- ggplot(m2_com_months, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m2_com_months$value,0), max(m2_com_months$value), by = round(max(m2_com_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_com_ncol))
m2_com_h <- ggplot(m2_com_hours, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Comerciales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
  scale_y_continuous(breaks = seq(min(m2_com_hours$value,0), max(m2_com_hours$value), by = round(max(m2_com_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_com_ncol))

m2_ind_s1 <- ggplot(m2_ind_segments, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments)+
  scale_y_continuous(breaks = seq(min(m2_ind_segments$value,0), max(m2_ind_segments$value), by = round(max(m2_ind_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ind_ncol))
m2_ind_s2 <- ggplot(m2_ind_segments_v2, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_segments_v2)+
  scale_y_continuous(breaks = seq(min(m2_ind_segments$value,0), max(m2_ind_segments$value), by = round(max(m2_ind_segments$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ind_ncol))
m2_ind_d <- ggplot(m2_ind_days, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m2_ind_days$value,0), max(m2_ind_days$value), by = round(max(m2_ind_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ind_ncol))
m2_ind_m <- ggplot(m2_ind_months, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m2_ind_months$value,0), max(m2_ind_months$value), by = round(max(m2_ind_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ind_ncol))
m2_ind_h <- ggplot(m2_ind_hours, aes(x=variable, y=value, colour=cluster)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Panorama General de Patrones de Consumo, Clientes Industriales ',lab_clust[2],sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "line", color = "orange", size = sizes[2])+scale_x_discrete(limits=lab_hours)+
  scale_y_continuous(breaks = seq(min(m2_ind_hours$value,0), max(m2_ind_hours$value), by = round(max(m2_ind_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m2_ind_ncol))


png(paste(dir_plots,"global_hclust_segments_comercial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_s1)
dev.off()
png(paste(dir_plots,"global_hclust_segments_industrial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_s2)
dev.off()
png(paste(dir_plots,"global_hclust_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_d)
dev.off()
png(paste(dir_plots,"global_hclust_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_m)
dev.off()
png(paste(dir_plots,"global_hclust_hourly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_h)
dev.off()

png(paste(dir_plots,"global_comercial_hclust_segments.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_com_s1)
dev.off()
# png(paste(dir_plots,"global_comercial_hclust_segments_industrial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
# plot(m1_com_s2)
# dev.off()
png(paste(dir_plots,"global_comercial_hclust_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_com_d)
dev.off()
png(paste(dir_plots,"global_comercial_hclust_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_com_m)
dev.off()
png(paste(dir_plots,"global_comercial_hclust_hourly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_com_h)
dev.off()

# png(paste(dir_plots,"global_industrial_hclust_segments_comercial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
# plot(m1_ind_s1)
# dev.off()
png(paste(dir_plots,"global_industrial_hclust_segments.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_ind_s2)
dev.off()
png(paste(dir_plots,"global_industrial_hclust_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_ind_d)
dev.off()
png(paste(dir_plots,"global_industrial_hclust_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_ind_m)
dev.off()
png(paste(dir_plots,"global_industrial_hclust_hourly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m1_ind_h)
dev.off()
#------------------------#
png(paste(dir_plots,"global_kmeans_segments_comercial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_s1)
dev.off()
png(paste(dir_plots,"global_kmeans_segments_industrial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_s2)
dev.off()
png(paste(dir_plots,"global_kmeans_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_d)
dev.off()
png(paste(dir_plots,"global_kmeans_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_m)
dev.off()
png(paste(dir_plots,"global_kmeans_hourly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_h)
dev.off()

png(paste(dir_plots,"global_comercial_kmeans_segments.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_com_s1)
dev.off()
# png(paste(dir_plots,"global_comercial_kmeans_segments_industrial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
# plot(m2_com_s2)
# dev.off()
png(paste(dir_plots,"global_comercial_kmeans_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_com_d)
dev.off()
png(paste(dir_plots,"global_comercial_kmeans_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_com_m)
dev.off()
png(paste(dir_plots,"global_comercial_kmeans_hourly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_com_h)
dev.off()

# png(paste(dir_plots,"global_industrial_kmeans_segments_comercial.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
# plot(m2_ind_s1)
# dev.off()
png(paste(dir_plots,"global_industrial_kmeans_segments.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_ind_s2)
dev.off()
png(paste(dir_plots,"global_industrial_kmeans_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_ind_d)
dev.off()
png(paste(dir_plots,"global_industrial_kmeans_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_ind_m)
dev.off()
png(paste(dir_plots,"global_industrial_kmeans_hourly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(m2_ind_h)
dev.off()
####################################################################################################################
# Mapeo con sectores y clientes
####################################################################################################################
clients_v1 <- clients_v1[order(clients_v1$COD_CLI),]
sectors_v1 <- sectors_v1[order(sectors_v1$COD_ACTIVIDAD_ECO),]
clients_v1$full_name <- paste(clients_v1$COD_CLI,' - ',clients_v1$DES_CLI,sep='')
sectors_v1$full_name <- paste(sectors_v1$COD_ACTIVIDAD_ECO,' - ',sectors_v1$DES_ACTIVIDAD_ECO,sep='')
sectors_v2_hclust <- data.frame(matrix(ncol = length(unique(group_v3$cluster_h)), nrow = nrow(sectors_v1)))
clients_v2_hclust <- data.frame(matrix(ncol = length(unique(group_v3$cluster_h)), nrow = nrow(clients_v1)))
sectors_v2_kmeans <- data.frame(matrix(ncol = length(unique(group_v3$cluster_1)), nrow = nrow(sectors_v1)))
clients_v2_kmeans <- data.frame(matrix(ncol = length(unique(group_v3$cluster_1)), nrow = nrow(clients_v1)))

for(k in 1:length(unique(group_v3$cluster_h))){
  cont <- group_v3[group_v3$cluster_h == k,]
  sec <- unique(cont$sector)
  cli <- unique(cont$COD_CLI)
  t1 <- as.data.frame(table(as.character(cont$sector)))
  t2 <- as.data.frame(table(as.character(cont$COD_CLI)))
  sectors_v2_hclust[sectors_v1$COD_ACTIVIDAD_ECO %in% sec,k] <- t1[order(t1$Var1),2]
  clients_v2_hclust[clients_v1$COD_CLI %in% cli,k] <- t2[order(t2$Var1),2]
}
for(k in 1:length(unique(group_v3$cluster_1))){
  cont <- group_v3[group_v3$cluster_1 == k,]
  sec <- unique(cont$sector)
  cli <- unique(cont$COD_CLI)
  t1 <- as.data.frame(table(as.character(cont$sector)))
  t2 <- as.data.frame(table(as.character(cont$COD_CLI)))
  sectors_v2_kmeans[sectors_v1$COD_ACTIVIDAD_ECO %in% sec,k] <- t1[order(t1$Var1),2]
  clients_v2_kmeans[clients_v1$COD_CLI %in% cli,k] <- t2[order(t2$Var1),2]
}

sectors_v2_hclust[is.na(sectors_v2_hclust)] <- 0
clients_v2_hclust[is.na(clients_v2_hclust)] <- 0
sectors_v2_kmeans[is.na(sectors_v2_kmeans)] <- 0
clients_v2_kmeans[is.na(clients_v2_kmeans)] <- 0
sectors_v2_hclust$sum <- rowSums(sectors_v2_hclust)
clients_v2_hclust$sum <- rowSums(clients_v2_hclust)
sectors_v2_kmeans$sum <- rowSums(sectors_v2_kmeans)
clients_v2_kmeans$sum <- rowSums(clients_v2_kmeans)

colnames(sectors_v2_hclust)[-ncol(sectors_v2_hclust)] <- c(sprintf("cluster %d", 1:length(unique(group_v3$cluster_h))))
colnames(clients_v2_hclust)[-ncol(clients_v2_hclust)] <- c(sprintf("cluster %d", 1:length(unique(group_v3$cluster_h))))
colnames(sectors_v2_kmeans)[-ncol(sectors_v2_kmeans)] <- c(sprintf("cluster %d", 1:length(unique(group_v3$cluster_1))))
colnames(clients_v2_kmeans)[-ncol(clients_v2_kmeans)] <- c(sprintf("cluster %d", 1:length(unique(group_v3$cluster_1))))
row.names(sectors_v2_hclust) <- sectors_v1$full_name
row.names(clients_v2_hclust) <- clients_v1$full_name
row.names(sectors_v2_kmeans) <- sectors_v1$full_name
row.names(clients_v2_kmeans) <- clients_v1$full_name

write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Sectores (Jerarquica)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(sectors_v2_hclust[order(-sectors_v2_hclust$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Sectores (Kmeans)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(sectors_v2_kmeans[order(-sectors_v2_kmeans$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Clientes (Jerarquica)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(clients_v2_hclust[order(-clients_v2_hclust$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Clientes (Kmeans)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(clients_v2_kmeans[order(-clients_v2_kmeans$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')

for(k in 1:length(unique(group_v3$cluster_h))){
  m1_total <- data.frame(value = sectors_v2_hclust[,k],rnames=row.names(sectors_v2_hclust))
  m1_total <- m1_total[m1_total$value>2,]
  m1_total <- m1_total[order(m1_total$value),]
  m2_total <- data.frame(value = sectors_v2_kmeans[,k],rnames=row.names(sectors_v2_kmeans))
  m2_total <- m2_total[m2_total$value>2,]
  m2_total <- m2_total[order(m2_total$value),]
  m3_total <- data.frame(value = clients_v2_hclust[,k],rnames=row.names(clients_v2_hclust))
  m3_total <- m3_total[m3_total$value>2,]
  m3_total <- m3_total[order(m3_total$value),]
  m4_total <- data.frame(value = clients_v2_kmeans[,k],rnames=row.names(clients_v2_kmeans))
  m4_total <- m4_total[m4_total$value>2,]
  m4_total <- m4_total[order(m4_total$value),]
  
  if(nrow(m1_total)>0){
    g1 <- ggplot(m1_total, aes(x=1:nrow(m1_total), y=value)) + geom_bar(stat='identity', aes(fill=value),width=.5)+scale_y_continuous(breaks = round(seq(0, max(m1_total$value), by = max(m1_total$value)%/%20 + 1),1))+
      ggtitle(paste("Sectores mas Frecuentes, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Sectores economicos\n', y="\nFrecuencia dentro del cluster")+
      theme(legend.position=c(0.9, 0.2),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
            legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))+coord_flip()+scale_x_discrete(limits=substr(m1_total$rnames,1,50))#substr(m1_total$rnames,1,20)
    png(paste(dir_plots,"hclust_heat_sectors_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
    plot(g1)
    dev.off()
  }
  if(nrow(m2_total)>0){
    g1 <- ggplot(m2_total, aes(x=1:nrow(m2_total), y=value)) + geom_bar(stat='identity', aes(fill=value),width=.5)+scale_y_continuous(breaks = round(seq(0, max(m2_total$value), by = max(m2_total$value)%/%20 + 1),1))+
      ggtitle(paste("Sectores mas Frecuentes, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Sectores economicos\n', y="\nFrecuencia dentro del cluster")+
      theme(legend.position=c(0.9, 0.2),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
            legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))+coord_flip()+scale_x_discrete(limits=substr(m2_total$rnames,1,50))#substr(m2_total$rnames,1,20)
    png(paste(dir_plots,"kmeans_heat_sectors_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
    plot(g1)
    dev.off()
  }
  if(nrow(m3_total)>0){
    g1 <- ggplot(m3_total, aes(x=1:nrow(m3_total), y=value)) + geom_bar(stat='identity', aes(fill=value),width=.5)+scale_y_continuous(breaks = round(seq(0, max(m3_total$value), by = max(m3_total$value)%/%20 + 1),1))+
      ggtitle(paste("Clientes mas Frecuentes, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Clientes\n', y="\nFrecuencia dentro del cluster")+
      theme(legend.position=c(0.9, 0.2),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
            legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))+coord_flip()+scale_x_discrete(limits=substr(m3_total$rnames,1,50))#substr(m3_total$rnames,1,20)
    png(paste(dir_plots,"hclust_heat_clients_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
    plot(g1)
    dev.off()
  }
  if(nrow(m4_total)>0){
    g1 <- ggplot(m4_total, aes(x=1:nrow(m4_total), y=value)) + geom_bar(stat='identity', aes(fill=value),width=.5)+scale_y_continuous(breaks = round(seq(0, max(m4_total$value), by = max(m4_total$value)%/%20 + 1),1))+
      ggtitle(paste("Clientes mas Frecuentes, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Clientes\n', y="\nFrecuencia dentro del cluster")+
      theme(legend.position=c(0.9, 0.2),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
            legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))+coord_flip()+scale_x_discrete(limits=substr(m4_total$rnames,1,50))#substr(m4_total$rnames,1,20)
    png(paste(dir_plots,"kmeans_heat_clients_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
    plot(g1)
    dev.off()
  }
}
m1_total <- data.frame(sectors_v2_hclust,max=do.call(pmax, sectors_v2_hclust),rnames=substr(row.names(sectors_v2_hclust),1,50))
m1_total <- m1_total[m1_total$max>8,]
m1_total <- m1_total[order(-m1_total$sum),]
m1_total$Name <- with(m1_total, reorder(rnames, sum))
m1_total.m <- melt(m1_total[,-c(ncol(m1_total)-3,ncol(m1_total)-2)])#melt(m1_total)
m1_total.m <- ddply(m1_total.m, .(variable), transform)
g1 <- ggplot(m1_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Sectores",lab_clust[1],sep = ''))+labs(y='Sectores economicos\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))

m2_total <- data.frame(sectors_v2_kmeans,max=do.call(pmax, sectors_v2_kmeans),rnames=substr(row.names(sectors_v2_kmeans),1,50))
m2_total <- m2_total[m2_total$max>8,]
m2_total <- m2_total[order(-m2_total$sum),]
m2_total$Name <- with(m2_total, reorder(rnames, sum))
m2_total.m <- melt(m2_total[,-c(ncol(m2_total)-3,ncol(m2_total)-2)])#melt(m2_total)
m2_total.m <- ddply(m2_total.m, .(variable), transform)
g2 <- ggplot(m2_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Sectores",lab_clust[2],sep = ''))+labs(y='Sectores economicos\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))

m3_total <- data.frame(clients_v2_hclust,max=do.call(pmax, clients_v2_hclust),rnames=substr(row.names(clients_v2_hclust),1,50))
m3_total <- m3_total[m3_total$max>2,]
m3_total <- m3_total[order(-m3_total$sum),]
m3_total$Name <- with(m3_total, reorder(rnames, sum))
m3_total.m <- melt(m3_total[,-c(ncol(m3_total)-3,ncol(m3_total)-2)])#melt(m3_total)
m3_total.m <- ddply(m3_total.m, .(variable), transform)
g3 <- ggplot(m3_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Clientes",lab_clust[1],sep = ''))+labs(y='Clientes\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))

m4_total <- data.frame(clients_v2_kmeans,max=do.call(pmax, clients_v2_kmeans),rnames=substr(row.names(clients_v2_kmeans),1,50))
m4_total <- m4_total[m4_total$max>2,]
m4_total <- m4_total[order(-m4_total$sum),]
m4_total$Name <- with(m4_total, reorder(rnames, sum))
m4_total.m <- melt(m4_total[,-c(ncol(m4_total)-3,ncol(m4_total)-2)])#melt(m4_total)
m4_total.m <- ddply(m4_total.m, .(variable), transform)
g4 <- ggplot(m4_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Clientes",lab_clust[2],sep = ''))+labs(y='Clientes\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))

png(paste(dir_plots,"global_hclust_heat_sectors",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g1)
dev.off()
png(paste(dir_plots,"global_kmeans_heat_sectors",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g2)
dev.off()
png(paste(dir_plots,"global_hclust_heat_clients",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g3)
dev.off()
png(paste(dir_plots,"global_kmeans_heat_clients",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g4)
dev.off()

#######Categorization#######
t1 <- summarize(group_by(categ_v1, ID), count=n(), categoria=paste(categoria,collapse=' '), categoria_num=paste(resultado,collapse=' '), respaldo=paste(respaldo,collapse=' '))
categ_v2 <- t1[t1$count==1 & ! t1$ID %in% grep('E', categ_v1$ID,value = T),-c(2)]
#group_v3 <- group_v3[,c(1:8)]
group_v3 <- cbind.data.frame(group_v3, categ_v2[match(group_v3$ID_CONTRATO, categ_v2$ID),2])
group_v3 <- cbind.data.frame(group_v3, categ_v2[match(group_v3$ID_CONTRATO, categ_v2$ID),3])
group_v3 <- cbind.data.frame(group_v3, categ_v2[match(group_v3$ID_CONTRATO, categ_v2$ID),4])
group_v4 <- group_v3[!is.na(group_v3$categoria),]

categ_lab_hclust <- data.frame(matrix(ncol = length(unique(group_v4$cluster_h)), nrow = length(unique(group_v4$categoria))))
categ_lab_kmeans <- data.frame(matrix(ncol = length(unique(group_v4$cluster_1)), nrow = length(unique(group_v4$categoria))))
categ_num_hclust <- data.frame(matrix(ncol = length(unique(group_v4$cluster_h)), nrow = length(unique(group_v4$categoria_num))))
categ_num_kmeans <- data.frame(matrix(ncol = length(unique(group_v4$cluster_1)), nrow = length(unique(group_v4$categoria_num))))
categ_resp_hclust <- data.frame(matrix(ncol = length(unique(group_v4$cluster_h)), nrow = length(unique(group_v4$respaldo))))
categ_resp_kmeans <- data.frame(matrix(ncol = length(unique(group_v4$cluster_1)), nrow = length(unique(group_v4$respaldo))))
keys <- data.frame(key=c(LETTERS[1:5],'0','BAJO','MEDIO','ALTO'),value=c(1:5,1:4))

for(k in 1:length(unique(group_v4$cluster_h))){
  cont <- group_v4[group_v4$cluster_h == k,]
  t1 <- as.data.frame(table(as.character(cont$categoria)))
  t2 <- as.data.frame(table(as.character(cont$categoria_num)))
  t3 <- as.data.frame(table(as.character(cont$respaldo)))
  t1$Var1 <- factor(t1$Var1, levels = LETTERS[1:5])
  t2$Var1 <- factor(t2$Var1, levels = 1:9)
  t3$Var1 <- factor(t3$Var1, levels = c('0','BAJO','MEDIO','ALTO'))
  
  categ_lab_hclust[keys[t1$Var1,2], k] <- t1[order(t1$Var1),2]
  categ_num_hclust[as.numeric(t2$Var1), k] <- t2[order(t2$Var1),2]
  categ_resp_hclust[keys[t3$Var1,2], k] <- t3[order(t3$Var1),2]
}
for(k in 1:length(unique(group_v4$cluster_1))){
  cont <- group_v4[group_v4$cluster_1 == k,]
  t1 <- as.data.frame(table(as.character(cont$categoria)))
  t2 <- as.data.frame(table(as.character(cont$categoria_num)))
  t3 <- as.data.frame(table(as.character(cont$respaldo)))
  t1$Var1 <- factor(t1$Var1, levels = LETTERS[1:5])
  t2$Var1 <- factor(t2$Var1, levels = 1:9)
  t3$Var1 <- factor(t3$Var1, levels = c('0','BAJO','MEDIO','ALTO'))
  
  categ_lab_kmeans[keys[t1$Var1,2], k] <- t1[order(t1$Var1),2]
  categ_num_kmeans[as.numeric(t2$Var1), k] <- t2[order(t2$Var1),2]
  categ_resp_kmeans[keys[t3$Var1,2], k] <- t3[order(t3$Var1),2]
}

categ_lab_hclust[is.na(categ_lab_hclust)] <- 0
categ_num_hclust[is.na(categ_num_hclust)] <- 0
categ_resp_hclust[is.na(categ_resp_hclust)] <- 0
categ_lab_kmeans[is.na(categ_lab_kmeans)] <- 0
categ_num_kmeans[is.na(categ_num_kmeans)] <- 0
categ_resp_kmeans[is.na(categ_resp_kmeans)] <- 0
categ_lab_hclust$sum <- rowSums(categ_lab_hclust)
categ_num_hclust$sum <- rowSums(categ_num_hclust)
categ_resp_hclust$sum <- rowSums(categ_resp_hclust)
categ_lab_kmeans$sum <- rowSums(categ_lab_kmeans)
categ_num_kmeans$sum <- rowSums(categ_num_kmeans)
categ_resp_kmeans$sum <- rowSums(categ_resp_kmeans)

colnames(categ_lab_hclust)[-ncol(categ_lab_hclust)] <- c(sprintf("Cluster %d", 1:length(unique(group_v4$cluster_h))))
colnames(categ_lab_kmeans)[-ncol(categ_lab_kmeans)] <- c(sprintf("Cluster %d", 1:length(unique(group_v4$cluster_1))))
colnames(categ_num_hclust)[-ncol(categ_num_hclust)] <- c(sprintf("Cluster %d", 1:length(unique(group_v4$cluster_h))))
colnames(categ_num_kmeans)[-ncol(categ_num_kmeans)] <- c(sprintf("Cluster %d", 1:length(unique(group_v4$cluster_1))))
colnames(categ_resp_hclust)[-ncol(categ_resp_hclust)] <- c(sprintf("Cluster %d", 1:length(unique(group_v4$cluster_h))))
colnames(categ_resp_kmeans)[-ncol(categ_resp_kmeans)] <- c(sprintf("Cluster %d", 1:length(unique(group_v4$cluster_1))))
row.names(categ_lab_hclust) <- c(sprintf("Categoria %s", LETTERS[1:nrow(categ_lab_hclust)]))
row.names(categ_lab_kmeans) <- c(sprintf("Categoria %s", LETTERS[1:nrow(categ_lab_kmeans)]))
row.names(categ_num_hclust) <- c(sprintf("Categoria %d", 1:nrow(categ_num_hclust)))
row.names(categ_num_kmeans) <- c(sprintf("Categoria %d", 1:nrow(categ_num_kmeans)))
row.names(categ_resp_hclust) <- c(sprintf("Categoria %s", c('0','BAJO','MEDIO','ALTO')))
row.names(categ_resp_kmeans) <- c(sprintf("Categoria %s", c('0','BAJO','MEDIO','ALTO')))

write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Categorias Alfabeticas (Jerarquica)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(categ_lab_hclust[order(-categ_lab_hclust$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Categorias Alfabeticas (Kmeans)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(categ_lab_kmeans[order(-categ_lab_kmeans$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Categorias Numericas (Jerarquica)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(categ_num_hclust[order(-categ_num_hclust$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Categorias Numericas (Kmeans)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(categ_num_kmeans[order(-categ_num_kmeans$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Niveles de Potencia de Respaldo (Jerarquica)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(categ_resp_hclust[order(-categ_resp_hclust$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Tabla de Correspondencia: Clusters Vs Niveles de Potencia de Respaldo (Kmeans)-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(categ_num_hclust[order(-categ_num_hclust$sum),], file_report, append = T,col.names = T,row.names = T, quote = F,sep = '\t')

m1_total <- categ_lab_hclust
m1_total$Name <- row.names(m1_total)
m1_total.m <- melt(m1_total[,-c(ncol(m1_total)-1)])#melt(m1_total)
m1_total.m <- ddply(m1_total.m, .(variable), transform)
m1_total.m <- m1_total.m[order(-m1_total.m$value),]

m2_total <- categ_lab_kmeans
m2_total$Name <- row.names(m2_total)
m2_total.m <- melt(m2_total[,-c(ncol(m2_total)-1)])#melt(m2_total)
m2_total.m <- ddply(m2_total.m, .(variable), transform)
m2_total.m <- m2_total.m[order(-m2_total.m$value),]

m3_total <- categ_num_hclust
m3_total$Name <- row.names(m3_total)
m3_total.m <- melt(m3_total[,-c(ncol(m3_total)-1)])#melt(m3_total)
m3_total.m <- ddply(m3_total.m, .(variable), transform)
m3_total.m <- m3_total.m[order(-m3_total.m$value),]

m4_total <- categ_num_kmeans
m4_total$Name <- row.names(m4_total)
m4_total.m <- melt(m4_total[,-c(ncol(m4_total)-1)])#melt(m4_total)
m4_total.m <- ddply(m4_total.m, .(variable), transform)
m4_total.m <- m4_total.m[order(-m4_total.m$value),]

m5_total <- categ_resp_hclust
m5_total$Name <- row.names(m5_total)
m5_total.m <- melt(m5_total[,-c(ncol(m5_total)-1)])#melt(m5_total)
m5_total.m <- ddply(m5_total.m, .(variable), transform)
m5_total.m <- m5_total.m[order(-m5_total.m$value),]

m6_total <- categ_resp_kmeans
m6_total$Name <- row.names(m6_total)
m6_total.m <- melt(m6_total[,-c(ncol(m6_total)-1)])#melt(m6_total)
m6_total.m <- ddply(m6_total.m, .(variable), transform)
m6_total.m <- m6_total.m[order(-m6_total.m$value),]

#reorder factors
m1_total.m$Name <- factor(m1_total.m$Name,levels=rownames(categ_lab_hclust[order(categ_lab_hclust$sum),]))
m2_total.m$Name <- factor(m2_total.m$Name,levels=rownames(categ_lab_kmeans[order(categ_lab_kmeans$sum),]))
m3_total.m$Name <- factor(m3_total.m$Name,levels=rownames(categ_num_hclust[order(categ_num_hclust$sum),]))
m4_total.m$Name <- factor(m4_total.m$Name,levels=rownames(categ_num_kmeans[order(categ_num_kmeans$sum),]))
m5_total.m$Name <- factor(m5_total.m$Name,levels=rownames(categ_resp_hclust[order(categ_resp_hclust$sum),]))
m6_total.m$Name <- factor(m6_total.m$Name,levels=rownames(categ_resp_kmeans[order(categ_resp_kmeans$sum),]))

g1 <- ggplot(m1_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Categorias Alfabeticas",lab_clust[1],sep = ''))+labs(y='Categorias\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))
g2 <- ggplot(m2_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Categorias Alfabeticas",lab_clust[2],sep = ''))+labs(y='Categorias\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))
g3 <- ggplot(m3_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Categorias Numericas",lab_clust[1],sep = ''))+labs(y='Categorias\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))
g4 <- ggplot(m4_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Categorias Numericas",lab_clust[2],sep = ''))+labs(y='Categorias\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))
g5 <- ggplot(m5_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Niveles de Potencia de Respaldo",lab_clust[1],sep = ''))+labs(y='Niveles\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))
g6 <- ggplot(m6_total.m, aes(variable, Name)) + geom_tile(aes(fill = value), colour = "white") +scale_fill_gradient(low = "white",high = "steelblue")+
  ggtitle(paste("Analisis de Correspondencia Clusters Vs Niveles de Potencia de Respaldo",lab_clust[2],sep = ''))+labs(y='Niveles\n', x="\nClusters")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
        legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))

png(paste(dir_plots,"global_hclust_heat_categ_lab",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g1)
dev.off()
png(paste(dir_plots,"global_kmeans_heat_categ_lab",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g2)
dev.off()
png(paste(dir_plots,"global_hclust_heat_categ_num",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g3)
dev.off()
png(paste(dir_plots,"global_kmeans_heat_categ_num",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g4)
dev.off()
png(paste(dir_plots,"global_hclust_heat_categ_resp",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g5)
dev.off()
png(paste(dir_plots,"global_kmeans_heat_categ_resp",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g6)
dev.off()

####################################################################################################################
# Contratos Filtrados/Eliminados
####################################################################################################################
bd_hourly$COD_CLI <- bd_daily[match(bd_hourly$SUMI_SGC_NIC, bd_daily$ID_CONTRATO),1]
s_pre <-  as.data.frame(summarize(group_by(bd_hourly, COD_CLI), count=length(unique(SUMI_SGC_NIC)), contratos=paste(sort(unique(SUMI_SGC_NIC)),collapse=" ")))
s_post <- as.data.frame(summarize(group_by(group_v3, COD_CLI), count=length(unique(ID_CONTRATO)), contratos=paste(sort(unique(ID_CONTRATO)),collapse=" ")))
s_post[is.na(s_post$COD_CLI),1]<-'NULL'
s_pre[is.na(s_pre$COD_CLI),1]<-'NULL'
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('-----------------Contratos NO Preservados Luego de Limpieza-----------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
for(x in 1:nrow(s_pre)){
  client <- as.character(s_pre$COD_CLI[x])
  cont_1 <- s_pre$contratos[x]
  cont_2 <- s_post[s_post$COD_CLI==client,3]
  
  if(!identical(cont_1,cont_2)){
    write(paste('-----Cliente -> ',client,' - ',clients_v1[clients_v1$COD_CLI==client,2],sep=''), file_report, append = T,sep = '\t')
    write(paste('Contratos pre: ',paste(cont_1,collapse = ' '),sep=''), file_report, append = T,sep = '\t')
    if(length(cont_2) >0){
      write(paste('Contratos post: ',paste(cont_2,collapse = ' '),sep=''), file_report, append = T,sep = '\t')
    }
  }
}

####################################################################################################################
# Casos de Interes
##############################################################################################################################
t1 <- group_v3[group_v3$cluster_1=='5' & group_v3$sector=='5511',]
t1$Departamento <- bd_daily[match(t1$ID_CONTRATO, bd_daily$ID_CONTRATO),4]
t1$Municipio <- bd_daily[match(t1$ID_CONTRATO, bd_daily$ID_CONTRATO),5]

t1 <- group_v3[group_v3$cluster_h=='7' & group_v3$sector=='0124',]
t1 <- t1[!is.na(t1$COD_CLI),]
t1$Departamento <- bd_daily[match(t1$ID_CONTRATO, bd_daily$ID_CONTRATO),4]
t1$Municipio <- bd_daily[match(t1$ID_CONTRATO, bd_daily$ID_CONTRATO),5]

t2 <- t1[,c(3,5,9,10)]
case <- 2
melt_v1 <- melt(group_v1[group_v1$ID_CONTRATO%in% t1$ID_CONTRATO,], id.vars=c("ID_CONTRATO"))
m1_days <- melt_v1[substring(melt_v1$variable, 1, 1)=='d',]
m1_months <- melt_v1[substring(melt_v1$variable, 1, 1)=='m',]
m1_hours <- melt_v1[substring(melt_v1$variable, 1, 1)=='h',]
m1_days$variable <- as.numeric(substring(as.character(m1_days$variable), 2))
m1_months$variable <- as.numeric(substring(as.character(m1_months$variable), 2))
m1_hours$variable <- as.numeric(substring(as.character(m1_hours$variable), 2))+1

m1_ncol <- case#length(unique(melt_v1$ID_CONTRATO)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v1$ID_CONTRATO)) %% (plot_dim[2]/30) >0,1,0)

g1 <- ggplot(m1_hours, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Caso de Interes ',case,'\n',sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_hours)+
  scale_y_continuous(breaks = seq(min(m1_hours$value,0), max(m1_hours$value), by = round(max(m1_hours$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
g2 <- ggplot(m1_days, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Caso de Interes ',case,'\n',sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m1_days$value,0), max(m1_days$value), by = round(max(m1_days$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))
g3 <- ggplot(m1_months, aes(x=variable, y=value, colour=ID_CONTRATO)) +geom_line(size= sizes[2]*.5)+ggtitle(paste('Caso de Interes ',case,'\n',sep = ""))+labs(x="\nPeriodo",y="% Consumo\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[5]), axis.text.y = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median",  geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m1_months$value,0), max(m1_months$value), by = round(max(m1_months$value)*sizes[4],2)))+guides(colour=guide_legend(ncol=m1_ncol))

png(paste(dir_plots,'case_',case,'_hourly.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g1)
dev.off()
png(paste(dir_plots,'case_',case,'_daily.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g2)
dev.off()
png(paste(dir_plots,'case_',case,'_monthly.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g3)
dev.off()

t1 <- sectors_v1[grepl('EDUCACION',sectors_v1$full_name)==T,]
t2 <- unique(bd_daily[bd_daily$COD_ACTIVIDAD_ECO %in% t1$COD_ACTIVIDAD_ECO,c(1,13)])
t2$client <-  with(clients_v1[match(t2$COD_CLI, clients_v1$COD_CLI),], paste(COD_CLI, DES_CLI,sep = ' - '))
t2$sector_full <- with(sectors_v1[match(t2$COD_ACTIVIDAD_ECO, sectors_v1$COD_ACTIVIDAD_ECO),], paste(COD_ACTIVIDAD_ECO, DES_ACTIVIDAD_ECO,sep = ' - '))
t3 <- t2[,c(3,4)]

####################################################################################################################
# Importe de librerias y carga inicial de datasets
####################################################################################################################
# Rutas de directorios contenedores. Modificar en caso de ser necesario
dir_data <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/"
dir_code <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/[code]/"
dir_plots <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/daily/"
file_report <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/[daily_v1].txt"

#Se modifica el directorio de trabajo deacuerdo a donde se tengan los archivos de datos
setwd(dir_code)
#Se cargan ltodas las librerias y scripts necesarios 'functions_v1.R'
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
source("aux_functions.R")

#Se carga el dataset de lecturas diarias
bd_daily<-read.table(paste(dir_data,"dataset_daily.txt",sep=''), header = T, sep="|",
                     colClasses=c("COD_ACTIVIDAD_ECO"="factor"))#,"COD_CLI"="factor","ID_CONTRATO"="factor"))
sectors_v1<-read.table(paste(dir_data,"market_sectors_v1.txt",sep=''), header = T, sep="|")
clients_v1<-read.table(paste(dir_data,"name_clients_v1.txt",sep=''), header = T, sep="|")[,c(1,2)]
clusters_v1<-read.table(paste(dir_data,"clustering_daily_v1.txt",sep=''), header = T, sep="\t")
text_size_v1 <- c(24,18,14,18,16)
# title, legend title, legend text, axis title, axis text 
text_size_v2 <- c(50,30,22,40,30,40,28)
# title, legend title, legend text, x-axis title, x-axis text, y-axis title, y-axis text 

#Se seleccionan las variables de interes (las cuales se van a usar para agrupar las lecturas posteriormente)
bd_d_1 <- bd_daily[,-c(8:10,12)]#select(bd_daily, COD_CLI, ID_CONTRATO, COD_CANAL, FECHA, VALOR_LECTURA, ID_MEDIDOR)
bd_d_1 <- verify_input_data_2(bd_daily,bd_d_1)

#Se calcula el rango de fechas a partir del cual se van a filtrar las lecturas del consumo
lowerDate <- min(bd_d_1$FECHA) #"2015-01-01"
upperDate <- max(bd_d_1$FECHA) #"2017-01-13"
#Se define una fecha limite que permita disminuir la cantidad de lecturas faltantes para los contratos
lowerDate <- as.Date("01/01/2016", format='%d/%m/%Y')
upperDate <- as.Date("30/11/2016", format='%d/%m/%Y ')
bd_d_1 <- filter(bd_d_1, FECHA >= lowerDate & FECHA <= upperDate) #eliminar fechas que sobrepasen limite

####################################################################################################################
# Agrupamos las lecturas por contrato y canal para verificar que todos los     #
# contratos tienen lecturas ACTIVA y REACTIVA, y que no hay lecturas faltantes #
####################################################################################################################
#Se agrupan las lecturas por Contrato y canal, para verificar el conteo de lecturas
s1 <- summarize(group_by(bd_d_1, ID_CONTRATO, COD_CANAL), count=n(), lowerDate=min(FECHA), upperDate=max(FECHA))
#Se calculan todos los periodos de tiempo en los que cada contrato, en cada canal, deberia tener asociada una lectura
allDates<- seq.Date(lowerDate, upperDate, by = 'day')

#Se eliminan las lecturas repetidas o duplicadas
bd_d_2 <- remove_duplicates_2(bd_d_1)#WAIT for 3 minutes
#Se calcula y aplica el umbral optimo que minimiza la presencia de lecturas faltantes
s2 <- compute_threshold_2(bd_d_2,allDates)
limit<-0.95
bad_cont <- unique(s2[s2$count<length(allDates)*limit,1])

bd_d_3 <- filter(bd_d_2, !ID_CONTRATO %in% bad_cont$ID_CONTRATO)
#Se adicionan las lecturas faltantes para cada contrato
bd_d_3 <- add_missing_2(bd_d_3,allDates)#WAIT for 5 minutes
s3 <- summarize(group_by(bd_d_3, ID_CONTRATO, COD_CANAL), count=n(), lowerDate=min(FECHA), upperDate=max(FECHA))

####################################################################################################################
# Agrupacion de las lecturas con base a diversos criterios, para luego realizar una clusterizacion por cada agrupacion
####################################################################################################################
bd_d_3 <- bd_d_3[order(bd_d_3$VALOR_LECTURA),]
bd_d_3$DIA <- ifelse(as.numeric(format(as.Date(bd_d_3$FECHA), "%w"))==0,7,as.numeric(format(as.Date(bd_d_3$FECHA), "%w")))
bd_d_3$MES <- as.numeric(format(as.Date(bd_d_3$FECHA), "%m"))
bd_d_3$SEMANA <- as.numeric(format(as.Date(bd_d_3$FECHA), "%W"))+1
#Se hallan y remueven las 15 lecturas con valores anomalos
out_v1 <- bd_d_3[bd_d_3$VALOR_LECTURA>= 4529848683 | bd_d_3$ID_CONTRATO==2940356,]
bd_d_3 <- bd_d_3[!bd_d_3$ID_CONTRATO %in% unique(out_v1$ID_CONTRATO),]
#Agrupacion de montos de consumo por dia de la semana
s_weekday <- summarize(group_by(bd_d_3, COD_CLI,ID_CONTRATO, COD_CANAL, DIA), count=n(), VALOR_LECTURA=sum(VALOR_LECTURA))
#Agrupacion de montos de consumo por mes del año
s_month <- summarize(group_by(bd_d_3, COD_CLI,ID_CONTRATO, COD_CANAL, MES), count=n(), VALOR_LECTURA=sum(VALOR_LECTURA))
#Agrupacion de montos de consumo por semana del año
s_weeknum <- summarize(group_by(bd_d_3, COD_CLI,ID_CONTRATO, COD_CANAL, SEMANA), count=n(), VALOR_LECTURA=sum(VALOR_LECTURA))

uni_cont<-unique(s_weeknum$ID_CONTRATO) #t<-cbind.data.frame(unique(s_weeknum[,1:2]),s_weekday_v2[,-1])
s_weekday_v2 <- as.data.frame(setNames(replicate(14,numeric(0)), c(sprintf("a%d", 1:7),sprintf("r%d", 1:7))),stringsAsFactors=FALSE)
s_month_v2 <- as.data.frame(setNames(replicate(22,numeric(0)), c(sprintf("a%d", 1:11),sprintf("r%d", 1:11))),stringsAsFactors=FALSE)
s_weeknum_v2 <- as.data.frame(setNames(replicate(98,numeric(0)), c(sprintf("a%d", 1:49),sprintf("r%d", 1:49))),stringsAsFactors=FALSE)

c = 1
while(c <= length(uni_cont)){
  t1 <- as.numeric(t(s_weekday[s_weekday$ID_CONTRATO==uni_cont[c],])[6,])
  t2 <- as.numeric(t(s_month[s_month$ID_CONTRATO==uni_cont[c],])[6,])
  t3 <- as.numeric(t(s_weeknum[s_weeknum$ID_CONTRATO==uni_cont[c],])[6,])
  s_weekday_v2[c,] <- t1
  s_month_v2[c,] <- t2
  s_weeknum_v2[c,] <- t3
  
  c<- c+1
}
s_weekday_v2<-cbind.data.frame(ID_CONTRATO=uni_cont,s_weekday_v2)
s_month_v2<-cbind.data.frame(ID_CONTRATO=uni_cont,s_month_v2)
s_weeknum_v2<-cbind.data.frame(ID_CONTRATO=uni_cont,s_weeknum_v2)
#Agrupacion de ratios de consumo (Reactiva/Activa) por dia de la semana
sr_weekday <- cbind.data.frame(ID_CONTRATO=uni_cont,do.call(data.frame,lapply((s_weekday_v2[9:15]/s_weekday_v2[2:8]), function(x) replace(x, !is.finite(x),0)))) #|x>1
#Agrupacion de ratios de consumo (Reactiva/Activa) por mes del año
sr_month <- cbind.data.frame(ID_CONTRATO=uni_cont,do.call(data.frame,lapply((s_month_v2[13:23]/s_month_v2[2:12]), function(x) replace(x, !is.finite(x),0)))) #|x>1
#Agrupacion de ratios de consumo (Reactiva/Activa) por semana del año
sr_weeknum <- cbind.data.frame(ID_CONTRATO=uni_cont,do.call(data.frame,lapply((s_weeknum_v2[51:99]/s_weeknum_v2[2:50]), function(x) replace(x, !is.finite(x),0)))) #|x>1
#Se hallan y remueven los contratos con valores anomalos (ratios > 5) en cada una de las agrupaciones
sr_weekday_out <- sr_weekday[rowMeans(sr_weekday[,-c(1,ncol(sr_weekday))])>5,] #NULL int(0)
sr_month_out <- sr_month[rowMeans(sr_month[,-c(1,ncol(sr_month))])>5,] #2855072 2911564
sr_weeknum_out <- sr_weeknum[rowMeans(sr_weeknum[,-c(1,ncol(sr_weeknum))])>5,]#2855072 2911564 2800580 2934954
sr_weekday <- sr_weekday[!sr_weekday$ID_CONTRATO %in% sr_weekday_out,]
sr_month <- sr_month[!sr_month$ID_CONTRATO %in% sr_month_out,]
sr_weeknum <- sr_weeknum[!sr_weeknum$ID_CONTRATO %in% sr_weeknum_out,]

####################################################################################################################
# Agrupacion con valores porcentuales del consumo y su normalizacion
####################################################################################################################
group_week_slots <- as.data.frame(t(mapply(cbind, ID_CONTRATO=uni_cont,rowSums(s_weekday_v2[,2:6]), rowSums(s_weekday_v2[7:8]),rowSums(s_weekday_v2[9:13]),rowSums(s_weekday_v2[14:15]))))
groupr_week_slots <- as.data.frame(t(mapply(cbind, ID_CONTRATO=uni_cont,rowMeans(sr_weekday[,2:6]), rowMeans(sr_weekday[7:8]))))

group_r_v1 <- cbind.data.frame(ID_CONTRATO=uni_cont,
                               sr_weekday[,-c(1,ncol(sr_weekday))],
                               sr_month[,-c(1,ncol(sr_month))],
                               Total_Ratio_AVG = rowMeans(sr_weekday[,-c(1,ncol(sr_weekday))]))
#groupr_week_slots[,-1])
out_r_v2<-group_r_v1[!is.finite(rowSums(group_r_v1[,-1])) | rowMeans(group_r_v1[,-1])>5,]
group_r_v1<-group_r_v1[!group_r_v1$ID_CONTRATO %in% out_r_v2$ID_CONTRATO,]
group_r_v2 <- as.data.frame(scale(group_r_v1[,-1], scale = T))
fith_2 <- hclust(dist(group_r_v2, method = "euclidean"), method='ward.D')
for(x in c(2:20)){
  print(data.frame(table(as.factor(cutree(fith_2, x))),Percentage=as.data.frame(prop.table(table(as.factor(cutree(fith_2, x)))))$Freq),row.names = F)
  # png(paste(dir_plots,"rate_hclust_dendo_k",x,'.png',sep = ''), width=8000, height=2000)
  # par(cex=1,font=1)
  # plot(fith_2, hang=-1, main="Dendrogram", label=group_r_v1[!group_r_v1$ID_CONTRATO %in% out_r_v2$ID_CONTRATO,1])
  # rect.hclust(fith_2, k=x, border="red")
  # dev.off()
}
group_r_v2$cluster_h <- as.factor(cutree(fith_2, 7))
wss_11 <- (nrow(group_r_v2[,-c(ncol(group_r_v2))])-1)*sum(apply(group_r_v2[,-c(ncol(group_r_v2))],2,var))
for (k in 2:20){
  km <- kmeans(group_r_v2[,-c(ncol(group_r_v2))],centers=k,iter.max = 100,algorithm="MacQueen")
  wss_11[k] <- sum(km$withinss)# algorithm=Lloyd,MacQueen
  print(data.frame(table(as.factor(km$cluster)),Percentage=as.data.frame(prop.table(table(as.factor(km$cluster))))$Freq),row.names = F)
}
# png(paste(dir_plots,"rate_kmeans_codo.png",sep = ''), width=1200, height=700)
# plot(1:20, wss_11, type="b", xlab="Numero de Clusters",ylab="Withinss")
# dev.off()
group_r_v2$cluster_1 <- as.factor(kmeans(group_r_v2[,-c(ncol(group_r_v2))], 7,iter.max = 100,algorithm="MacQueen")$cluster)

group_r_v2$cluster_h <- as.factor(clusters_v1[match(group_r_v1$ID_CONTRATO, clusters_v1$ID_CONTRATO),2])
group_r_v2$cluster_1 <- as.factor(clusters_v1[match(group_r_v1$ID_CONTRATO, clusters_v1$ID_CONTRATO),3])

####################################################################################################################
# Analisis de Componenetes Principales
####################################################################################################################
prc_rat <- princomp(group_r_v2[,-c(ncol(group_r_v2),ncol(group_r_v2)-1)], cor=TRUE, scores=TRUE)
# t1<- summary(prc_rat)  # t1$print.loadings<-TRUE
# write('------------------------------------------------------------------------------------------', file_report, append = F)
# write('--------------------------Analisis de Reduccion Multidimensional con PCA--------------------------', file_report, append = T)
# write('------------------------------------------------------------------------------------------', file_report, append = T)
# sink(file_report, append=TRUE, split=TRUE)
# print(t1)
# sink()
png(paste(dir_plots,"rate_pca_hclust.png",sep = ''), width=1200, height=800)
autoplot(prc_rat, data=group_r_v2, colour = 'cluster_h', loadings = F, loadings.label = F)+ggtitle("Grafico de Reduccion Multidimensional usando PCA y Clusterizacion Jerarquica\n")+
  labs(x="\nComponente 1",y="Componente 2\n")+theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
    axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()
png(paste(dir_plots,"rate_pca_kmeans.png",sep = ''), width=1200, height=800)
autoplot(prc_rat, data=group_r_v2, colour = 'cluster_1', loadings = F, loadings.label = F)+ggtitle("Grafico de Reduccion Multidimensional usando PCA y Clusterizacion con Kmeans\n")+
  labs(x="\nComponente 1",y="Componente 2\n")+theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
    axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()
####################################################################################################################
# Reduccion de dimensiones con tsn-e y Visualizacion http://distill.pub/2016/misread-tsne/
####################################################################################################################
#Rtsne v1 -> Plot in image
rtsne_v1 <- Rtsne(as.matrix(group_r_v2[,-c(ncol(group_r_v2)-1,ncol(group_r_v2))]), dims = 2, perplexity=30, pca=F, verbose=T, max_iter = 2000,check_duplicates=F, theta = 0.25)
t1 <- data.frame(x = rtsne_v1$Y[,1], y= rtsne_v1$Y[,2])
t1$cluster_h <- group_r_v2$cluster_h
t1$cluster_1 <- group_r_v2$cluster_1
png(paste(dir_plots,"rate_tsne_hclust.png",sep = ''), width=1200, height=800)
ggplot(t1, aes(x, y, colour = cluster_h))+geom_point()+ggtitle("Grafico de Reduccion Multidimensional usando t-SNE y Clusterizacion Jerarquica\n")+labs(x="\nComponente 1",y="Componente 2\n")+
  theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
        axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()
png(paste(dir_plots,"rate_tsne_kmeans.png",sep = ''), width=1200, height=800)
ggplot(t1, aes(x, y, colour = cluster_1))+geom_point()+ggtitle("Grafico de Reduccion Multidimensional usando t-SNE y Clusterizacion con Kmeans\n")+labs(x="\nComponente 1",y="Componente 2\n")+
  theme(plot.title = element_text(size = text_size_v1[1],hjust = 0.5),legend.title = element_text(size=text_size_v1[2]),legend.text = element_text(size = text_size_v1[3]), 
        axis.title = element_text(size=text_size_v1[4]), axis.text = element_text(size=text_size_v1[5]))
dev.off()

####################################################################################################################
# Contruccion y Generacion de Reportes
####################################################################################################################
group_v3 <- group_r_v2[,c(ncol(group_r_v2)-1,ncol(group_r_v2))]
group_v3$ID_CONTRATO <- group_r_v1$ID_CONTRATO
group_v3$COD_CLI <- as.factor(unique(bd_daily[,c(1,2)])[match(group_v3$ID_CONTRATO, unique(bd_daily[,c(1,2)])$ID_CONTRATO),1])
group_v3$client <-  with(clients_v1[match(group_v3$COD_CLI, clients_v1$COD_CLI),], paste(COD_CLI, DES_CLI,sep = ' - '))
group_v3$sector <- as.factor(unique(bd_daily[,c(2,13)])[match(group_v3$ID_CONTRATO, unique(bd_daily[,c(2,13)])$ID_CONTRATO),2])
group_v3$ID_CONTRATO <- factor(group_v3$ID_CONTRATO, levels = group_v3$ID_CONTRATO[order(group_v3$sector)])
group_v3$sector_full <- paste(group_v3$sector,as.factor(sectors_v1[match(group_v3$sector, sectors_v1$COD_ACTIVIDAD_ECO),2]),sep = " - ")

group_r_v2_s_kmeans <- data.frame()
group_r_v2_s_hclust <- data.frame()
for(k in 1:length(unique(group_r_v2$cluster_1))){
  cont <- group_r_v1[group_r_v2$cluster_1 == k,1]
  t1 <- data.frame(num=bd_d_3[bd_d_3$ID_CONTRATO %in% cont & bd_d_3$COD_CANAL=='REACTIVA',5]$VALOR_LECTURA/
                     bd_d_3[bd_d_3$ID_CONTRATO %in% cont & bd_d_3$COD_CANAL=='ACTIVA',5]$VALOR_LECTURA)
  t2 <- data.frame(num=s_month[s_month$ID_CONTRATO %in% cont & s_month$COD_CANAL=='REACTIVA',6]$VALOR_LECTURA/
                     s_month[s_month$ID_CONTRATO %in% cont & s_month$COD_CANAL=='ACTIVA',6]$VALOR_LECTURA)
  group_r_v2_s_kmeans <- rbind(group_r_v2_s_kmeans,data.frame(
    size=length(cont),
    AVG_DAILY=mean(t1[is.finite(t1$num),]),
    AVG_MONTHLY=mean(t2[is.finite(t2$num),]),
    AVG_GLOBAL=mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,20]),
    d1 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,2]),
    d2 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,3]),
    d3 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,4]),
    d4 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,5]),
    d5 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,6]),
    d6 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,7]),
    d7 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,8]),
    m1 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,9]),
    m2 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,10]),
    m3 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,11]),
    m4 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,12]),
    m5 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,13]),
    m6 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,14]),
    m7 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,15]),
    m8 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,16]),
    m9 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,17]),
    m10 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,18]),
    m11 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,19])
  ))}
for(k in 1:length(unique(group_r_v2$cluster_h))){
  cont <- group_r_v1[group_r_v2$cluster_h == k,1]
  t1 <- data.frame(num=bd_d_3[bd_d_3$ID_CONTRATO %in% cont & bd_d_3$COD_CANAL=='REACTIVA',5]$VALOR_LECTURA/
                     bd_d_3[bd_d_3$ID_CONTRATO %in% cont & bd_d_3$COD_CANAL=='ACTIVA',5]$VALOR_LECTURA)
  t2 <- data.frame(num=s_month[s_month$ID_CONTRATO %in% cont & s_month$COD_CANAL=='REACTIVA',6]$VALOR_LECTURA/
                     s_month[s_month$ID_CONTRATO %in% cont & s_month$COD_CANAL=='ACTIVA',6]$VALOR_LECTURA)
  group_r_v2_s_hclust <- rbind(group_r_v2_s_hclust,data.frame(
    size=length(cont),
    AVG_DAILY=mean(t1[is.finite(t1$num),]),
    AVG_MONTHLY=mean(t2[is.finite(t2$num),]),
    AVG_GLOBAL=mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,20]),
    d1 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,2]),
    d2 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,3]),
    d3 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,4]),
    d4 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,5]),
    d5 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,6]),
    d6 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,7]),
    d7 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,8]),
    m1 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,9]),
    m2 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,10]),
    m3 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,11]),
    m4 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,12]),
    m5 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,13]),
    m6 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,14]),
    m7 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,15]),
    m8 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,16]),
    m9 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,17]),
    m10 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,18]),
    m11 = mean(group_r_v1[group_r_v1$ID_CONTRATO %in% cont,19])
  ))}

group_r_v2_s_hclust <- cbind.data.frame(cluster=as.factor(1:nrow(group_r_v2_s_hclust)),group_r_v2_s_hclust)
group_r_v2_s_kmeans <- cbind.data.frame(cluster=as.factor(1:nrow(group_r_v2_s_kmeans)),group_r_v2_s_kmeans)
group_r_v1$ID_CONTRATO <- as.factor(group_r_v1$ID_CONTRATO)
colnames(group_r_v1)[2:8] <- paste('d',seq(1:7),sep = '')
colnames(group_r_v1)[9:19] <- paste('m',seq(1:11),sep = '')
colnames(group_r_v2)[1:7] <- colnames(group_r_v1)[2:8]
colnames(group_r_v2)[8:18] <- colnames(group_r_v1)[9:19]

t1 <- data.frame(group_r_v1,group_v3[,-c(3,4,6)])
options(scipen=999)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write('---------------------------------Clusterizacion Jerarquica---------------------------------', file_report, append = T)
write('------------------------------------------------------------------------------------------', file_report, append = T)
write.table(group_r_v2_s_hclust, file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
for(k in 1:length(unique(group_v3$cluster_h))){
  cont <- group_v3[group_v3$cluster_h == k,3]
  write(paste("---------------------------------Cluster k = ",k,'---------------------------------',sep = ''), file_report, append = T)
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
write.table(group_r_v2_s_kmeans, file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
for(k in 1:length(unique(group_v3$cluster_1))){
  cont <- group_v3[group_v3$cluster_1 == k,3]
  write(paste("---------------------------------Cluster k = ",k,'---------------------------------',sep = ''), file_report, append = T)
  s1 <- summarize(group_by(group_v3[group_v3$cluster_h == k,], sector_full), count=n())
  s2 <- summarize(group_by(group_v3[group_v3$cluster_h == k,], client), count=n())
  write("-----------------Sectores-----------------", file_report, append = T)
  write.table(s1[order(-s1$count),c(2,1)], file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
  write("-----------------Clientes-----------------", file_report, append = T)
  write.table(s2[order(-s2$count),c(2,1)], file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
  write("-----------------Tabla Completa-----------------", file_report, append = T)
  write.table(format(t1[t1$ID_CONTRATO %in% cont,], digits=2), file_report, append = T,col.names = T,row.names = F, quote = F,sep = '\t')
}

sizes <- c(5,2,NA,0.08,0.08,12)
#mean points size, data points size, tick y axis (total), tick y axis (percentage)
plot_dim <- c(2800,1500,2000,3000)
lab_days <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))
lab_months <-  months(x=as.Date(seq(length.out = 12,by = 31), origin="1950-01-01"))
lab_clust <- c(' (Usando Jerarquico)\n',' (Usando Kmeans)\n')

for(k in 1:length(unique(group_r_v2$cluster_h))){
  melt_v1 <- melt(group_r_v1[group_r_v2$cluster_h==as.character(k),], id.vars=c("ID_CONTRATO"))
  m1_days <- melt_v1[substring(melt_v1$variable, 1, 1)=='d',]
  m1_months <- melt_v1[substring(melt_v1$variable, 1, 1)=='m',]
  m1_total <- melt_v1[substring(melt_v1$variable, 1, 1)=='T',]
  m1_total <- m1_total[order(m1_total$value),]
  m1_total$row <- 1:nrow(m1_total)
  m1_days$variable <- as.numeric(substring(as.character(m1_days$variable), 2))
  m1_months$variable <- as.numeric(substring(as.character(m1_months$variable), 2))
  m1_total$value <- round(m1_total$value,2)
  m1_total$type<- ifelse(m1_total$value <= 0.5, "below", "above")
  
  melt_v2 <- melt(group_r_v1[group_r_v2$cluster_1==as.character(k),], id.vars=c("ID_CONTRATO"))
  m2_days <- melt_v2[substring(melt_v2$variable, 1, 1)=='d',]
  m2_months <- melt_v2[substring(melt_v2$variable, 1, 1)=='m',]
  m2_total <- melt_v2[substring(melt_v2$variable, 1, 1)=='T',]
  m2_total <- m2_total[order(m2_total$value),]
  m2_total$row <- 1:nrow(m2_total)
  m2_days$variable <- as.numeric(substring(as.character(m2_days$variable), 2))
  m2_months$variable <- as.numeric(substring(as.character(m2_months$variable), 2))
  m2_total$value <- round(m2_total$value,2)
  m2_total$type<- ifelse(m2_total$value <= 0.5, "below", "above")
  
  m1_ncol <- nrow(m1_total) %/% (plot_dim[2]/30) +if_else(nrow(m1_total) %% (plot_dim[2]/30) >0,1,0)
  m2_ncol <- nrow(m2_total) %/% (plot_dim[2]/30) +if_else(nrow(m2_total) %% (plot_dim[2]/30) >0,1,0)
  m1_scale <- -qt(c(0.025,0.05,0.1), df=nrow(m1_total)-1)*(sd(m1_total$value)/sqrt(nrow(m1_total)))
  m2_scale <- -qt(c(0.025,0.05,0.1), df=nrow(m2_total)-1)*(sd(m2_total$value)/sqrt(nrow(m2_total)))
  #geom_quasirandom(dodge.width=0.5)
  g1 <- ggplot(m1_days, aes(x=variable, y=value, colour=ID_CONTRATO)) + geom_quasirandom(size= sizes[2])+ggtitle(paste("Ratios de Consumo, Cluster k=",k,lab_clust[1],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]), legend.position="none",#legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_days)+
    scale_y_continuous(breaks = seq(min(m1_days$value,0), max(m1_days$value), by = round(max(m1_days$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m1_ncol))
  g2 <- ggplot(m1_months, aes(x=variable, y=value, colour=ID_CONTRATO)) + geom_quasirandom(size= sizes[2])+ggtitle(paste("Ratios de Consumo, Cluster k=",k,lab_clust[1],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]),legend.position="none",#legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_months)+
    scale_y_continuous(breaks = seq(min(m1_months$value,0), max(m1_months$value),  by = round(max(m1_months$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m1_ncol))
  g3 <- ggplot(m1_total, aes(x=row, y=value, label=value))+ggtitle(paste("Ratios de Consumo Promedio, Cluster k=",k,lab_clust[1],sep = ""))+labs(x='Contrato\n', y="\nRatio (Activa/Reactiva)")+
    theme(legend.position=c(0.85, 0.1),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(3, "cm"),legend.title = element_blank(),#element_text(size=text_size_v2[2]*2),
          legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_blank(), axis.text.x = element_text(size=text_size_v2[6]))+
    geom_quasirandom(stat='identity', aes(col=type), size=8)+scale_color_manual(name="Posibilidad de Multa",labels = c("Riesgo de Multa","A Salvo de Multa"),values = c("above"="#f8766d", "below"="#00ba38")) + 
    coord_flip()+scale_y_continuous(breaks = seq(min(m1_total$value,0), max(m1_total$value),  by = round(max(m1_total$value)*sizes[5],2)))+#geom_text(color="white", size=10)+scale_x_discrete(limits=m1_total$ID_CONTRATO)+
    annotate("rect", ymin = mean(m1_total$value)-m1_scale[1], ymax = mean(m1_total$value)+m1_scale[1], xmin = -Inf, xmax = Inf, fill = "gray60", alpha = 0.05)+
    annotate("rect", ymin = mean(m1_total$value)-m1_scale[2], ymax = mean(m1_total$value)+m1_scale[2], xmin = -Inf, xmax = Inf, fill = "gray40", alpha = 0.05)+
    annotate("rect", ymin = mean(m1_total$value)-m1_scale[3], ymax = mean(m1_total$value)+m1_scale[3], xmin = -Inf, xmax = Inf, fill = "gray20", alpha = 0.05)
  
  g4 <- ggplot(m2_days, aes(x=variable, y=value, colour=ID_CONTRATO)) + geom_quasirandom(size= sizes[2])+ggtitle(paste("Ratios de Consumo, Cluster k=",k,lab_clust[2],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]),legend.position="none",#legend.text =element_text(size = text_size_v2[3]), 
          axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_days)+
    scale_y_continuous(breaks = seq(min(m2_days$value,0), max(m2_days$value),  by = round(max(m2_days$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m2_ncol))
  g5 <- ggplot(m2_months, aes(x=variable, y=value, colour=ID_CONTRATO)) + geom_quasirandom(size= sizes[2])+ggtitle(paste("Ratios de Consumo, Cluster k=",k,lab_clust[2],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
    theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(1, "cm"),legend.title = element_text(size=text_size_v2[2]),legend.position="none",#legend.text =element_text(size = text_size_v2[3]),  
          axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
    stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_months)+
    scale_y_continuous(breaks = seq(min(m2_months$value,0), max(m2_months$value),  by = round(max(m2_months$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m2_ncol))
  g6 <- ggplot(m2_total, aes(x=row, y=value, label=value))+ggtitle(paste("Ratios de Consumo Promedio, Cluster k=",k,lab_clust[2],sep = ""))+labs(x='Contrato\n', y="\nRatio (Activa/Reactiva)")+
    theme(legend.position=c(0.85, 0.1),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(3, "cm"),legend.title = element_blank(),#element_text(size=text_size_v2[2]*2),
          legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_blank(), axis.text.x = element_text(size=text_size_v2[6]))+
    geom_quasirandom(stat='identity', aes(col=type), size=8)+scale_color_manual(name="Posibilidad de Multa",labels = c("Riesgo de Multa","A Salvo de Multa"),values = c("above"="#f8766d", "below"="#00ba38")) + 
    coord_flip()+scale_y_continuous(breaks = seq(min(m2_total$value,0), max(m2_total$value),  by = round(max(m2_total$value)*sizes[5],2)))+#geom_text(color="white", size=10)+scale_x_discrete(limits=m2_total$ID_CONTRATO)+
    annotate("rect", ymin = mean(m2_total$value)-m2_scale[1], ymax = mean(m2_total$value)+m2_scale[1], xmin = -Inf, xmax = Inf, fill = "gray60", alpha = 0.05)+
    annotate("rect", ymin = mean(m2_total$value)-m2_scale[2], ymax = mean(m2_total$value)+m2_scale[2], xmin = -Inf, xmax = Inf, fill = "gray40", alpha = 0.05)+
    annotate("rect", ymin = mean(m2_total$value)-m2_scale[3], ymax = mean(m2_total$value)+m2_scale[3], xmin = -Inf, xmax = Inf, fill = "gray20", alpha = 0.05)
  
  png(paste(dir_plots,"rate_hclust_k",k,'_daily.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g1)
  dev.off()
  png(paste(dir_plots,"rate_hclust_k",k,'_monthly.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g2)
  dev.off()
  png(paste(dir_plots,"rate_hclust_k",k,'_total.png',sep = ""), width=plot_dim[3], height=plot_dim[4])
  plot(g3)
  dev.off()
  png(paste(dir_plots,"rate_kmeans_k",k,'_daily.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g4)
  dev.off()
  png(paste(dir_plots,"rate_kmeans_k",k,'_monthly.png',sep = ""), width=plot_dim[1], height=plot_dim[2])
  plot(g5)
  dev.off()
  png(paste(dir_plots,"rate_kmeans_k",k,'_total.png',sep = ""), width=plot_dim[3], height=plot_dim[4])
  plot(g6)
  dev.off()
}

melt_v1 <- melt(group_r_v2_s_hclust, id.vars=c("cluster"))
m1_days <- melt_v1[melt_v1$variable %in% substring(lab_days, 1, 3),]
m1_months <- melt_v1[melt_v1$variable %in% substring(lab_months, 1, 3),]
m1_days$variable <- as.integer(factor(m1_days$variable, levels = substring(lab_days, 1, 3),ordered = T))
m1_months$variable <- as.integer(factor(m1_months$variable, levels = substring(lab_months, 1, 3),ordered = T))

melt_v2 <- melt(group_r_v2_s_kmeans, id.vars=c("cluster"))
m2_days <- melt_v2[melt_v2$variable %in% substring(lab_days, 1, 3),]
m2_months <- melt_v2[melt_v2$variable %in% substring(lab_months, 1, 3),]
m2_days$variable <- as.integer(factor(m2_days$variable, levels = substring(lab_days, 1, 3),ordered = T))
m2_months$variable <- as.integer(factor(m2_months$variable, levels = substring(lab_months, 1, 3),ordered = T))

m1_ncol <- length(unique(melt_v1$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v1$cluster)) %% (plot_dim[2]/30) >0,1,0)
m2_ncol <- length(unique(melt_v2$cluster)) %/% (plot_dim[2]/30) +if_else(length(unique(melt_v2$cluster)) %% (plot_dim[2]/30) >0,1,0)

g1 <- ggplot(m1_days, aes(x=variable, y=value, colour=cluster)) + geom_line(size= sizes[2])+ggtitle(paste("Panorama General de Ratios de Consumo",lab_clust[1],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2), legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m1_days$value,0), max(m1_days$value), by = round(max(m1_days$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m1_ncol))
g2 <- ggplot(m1_months, aes(x=variable, y=value, colour=cluster)) + geom_line(size= sizes[2])+ggtitle(paste("Panorama General de Ratios de Consumo",lab_clust[1],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2),legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m1_months$value,0), max(m1_months$value),  by = round(max(m1_months$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m1_ncol))
g3 <- ggplot(m2_days, aes(x=variable, y=value, colour=cluster)) + geom_line(size= sizes[2])+ggtitle(paste("Panorama General de Ratios de Consumo",lab_clust[2],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2),legend.text =element_text(size = text_size_v2[3]*2), 
        axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_days)+
  scale_y_continuous(breaks = seq(min(m2_days$value,0), max(m2_days$value),  by = round(max(m2_days$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m2_ncol))
g4 <- ggplot(m2_months, aes(x=variable, y=value, colour=cluster)) + geom_line(size= sizes[2])+ggtitle(paste("Panorama General de Ratios de Consumo",lab_clust[2],sep = ""))+labs(x='\nPeriodo',y="Ratio (Activa/Reactiva)\n")+
  theme(plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_text(size=text_size_v2[2]*2),legend.text =element_text(size = text_size_v2[3]*2),  
        axis.title = element_text(size=text_size_v2[4]), axis.text.y = element_text(size=text_size_v2[5]), axis.text.x = element_text(size=text_size_v2[6]))+
  stat_summary(fun.y = "median", geom = "point", color = "orange", size = sizes[1])+scale_x_discrete(limits=lab_months)+
  scale_y_continuous(breaks = seq(min(m2_months$value,0), max(m2_months$value),  by = round(max(m2_months$value)*sizes[4],2)))+geom_hline(yintercept=0.5,color, size = 2, colour="red", linetype="dashed")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, fill = "red", alpha = 0.1)+guides(colour=guide_legend(ncol=m2_ncol))


png(paste(dir_plots,"rate_global_hclust_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g1)
dev.off()
png(paste(dir_plots,"rate_global_hclust_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g2)
dev.off()
png(paste(dir_plots,"rate_global_kmeans_daily.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g3)
dev.off()
png(paste(dir_plots,"rate_global_kmeans_monthly.png",sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g4)
dev.off()

####################################################################################################################
# Mapeo con sectores y clientes
####################################################################################################################
clients_v1$full_name <- paste(clients_v1$COD_CLI,' - ',clients_v1$DES_CLI,sep='')
sectors_v1$full_name <- paste(sectors_v1$COD_ACTIVIDAD_ECO,' - ',sectors_v1$DES_ACTIVIDAD_ECO,sep='')
clients_v1 <- clients_v1[order(clients_v1$COD_CLI),]
sectors_v1 <- sectors_v1[order(sectors_v1$COD_ACTIVIDAD_ECO),]
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
    png(paste(dir_plots,"rate_hclust_heat_sectors_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
    plot(g1)
    dev.off()
  }
  if(nrow(m2_total)>0){
    g1 <- ggplot(m2_total, aes(x=1:nrow(m2_total), y=value)) + geom_bar(stat='identity', aes(fill=value),width=.5)+scale_y_continuous(breaks = round(seq(0, max(m2_total$value), by = max(m2_total$value)%/%20 + 1),1))+
      ggtitle(paste("Sectores mas Frecuentes, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Sectores economicos\n', y="\nFrecuencia dentro del cluster")+
      theme(legend.position=c(0.9, 0.2),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
            legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))+coord_flip()+scale_x_discrete(limits=substr(m2_total$rnames,1,50))#substr(m2_total$rnames,1,20)
    png(paste(dir_plots,"rate_kmeans_heat_sectors_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
    plot(g1)
    dev.off()
  }
  if(nrow(m3_total)>0){
    g1 <- ggplot(m3_total, aes(x=1:nrow(m3_total), y=value)) + geom_bar(stat='identity', aes(fill=value),width=.5)+scale_y_continuous(breaks = round(seq(0, max(m3_total$value), by = max(m3_total$value)%/%20 + 1),1))+
      ggtitle(paste("Clientes mas Frecuentes, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Clientes\n', y="\nFrecuencia dentro del cluster")+
      theme(legend.position=c(0.9, 0.2),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
            legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))+coord_flip()+scale_x_discrete(limits=substr(m3_total$rnames,1,50))#substr(m3_total$rnames,1,20)
    png(paste(dir_plots,"rate_hclust_heat_clients_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
    plot(g1)
    dev.off()
  }
  if(nrow(m4_total)>0){
    g1 <- ggplot(m4_total, aes(x=1:nrow(m4_total), y=value)) + geom_bar(stat='identity', aes(fill=value),width=.5)+scale_y_continuous(breaks = round(seq(0, max(m4_total$value), by = max(m4_total$value)%/%20 + 1),1))+
      ggtitle(paste("Clientes mas Frecuentes, Cluster ",k,lab_clust[1],sep = ""))+labs(x='Clientes\n', y="\nFrecuencia dentro del cluster")+
      theme(legend.position=c(0.9, 0.2),plot.title = element_text(size = text_size_v2[1],hjust = 0.5),legend.key.size = unit(2, "cm"),legend.title = element_blank(),axis.text.y = element_text(size=text_size_v2[5]),#element_text(size=text_size_v2[2]*2),
            legend.text = element_text(size = text_size_v2[3]*2),axis.title = element_text(size=text_size_v2[4]), axis.text.x = element_text(size=text_size_v2[6]))+coord_flip()+scale_x_discrete(limits=substr(m4_total$rnames,1,50))#substr(m4_total$rnames,1,20)
    png(paste(dir_plots,"rate_kmeans_heat_clients_k",k,'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
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

png(paste(dir_plots,"rate_global_hclust_heat_sectors",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g1)
dev.off()
png(paste(dir_plots,"rate_global_kmeans_heat_sectors",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g2)
dev.off()
png(paste(dir_plots,"rate_global_hclust_heat_clients",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g3)
dev.off()
png(paste(dir_plots,"rate_global_kmeans_heat_clients",'.png',sep = ''), width=plot_dim[1], height=plot_dim[2])
plot(g4)
dev.off()

#bd_hourly$COD_CLI <- bd_daily[match(bd_hourly$SUMI_SGC_NIC, bd_daily$ID_CONTRATO),1]
s_pre <-  as.data.frame(summarize(group_by(bd_daily, COD_CLI), count=length(unique(ID_CONTRATO)), contratos=paste(unique(ID_CONTRATO),collapse=" ")))
s_post <- as.data.frame(summarize(group_by(group_v3, COD_CLI), count=length(unique(ID_CONTRATO)), contratos=paste(unique(ID_CONTRATO),collapse=" ")))
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

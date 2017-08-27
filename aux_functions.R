#############################################################################################################
#Funcion que verifica la carga exitosa del dataset diario y acomoda los tipos de los datos al adecuado
#############################################################################################################
verify_input_data_2 <- function(bd_ori, bd_datos){
  checks = logical()
  checks[1]=identical(unique(is.na(bd_ori)[1,]),FALSE)
  
  # Hay que modificar el caracter separador en los valores de las lecturas de ',' a '.'
  bd_datos$VALOR_LECTURA <- gsub(",", ".", bd_datos$VALOR_LECTURA)
  # Hay valores vacios ?
  checks[2]=identical(unique(is.na(bd_datos$VALOR_LECTURA)),FALSE)
  # si no tenemos ningún TRUE, es porque no hay valores vacíos
  
  # Se vuelven numericos los valores de las lecturas del consumo
  bd_datos$VALOR_LECTURA<-as.numeric(as.character(bd_datos$VALOR_LECTURA))
  checks[3]=identical(unique(is.na(bd_datos$VALOR_LECTURA)),FALSE)
  # si no tenemos ningún TRUE, es porque no hay valores vacíos
  
  # Las lecturas hechas a medianoche carecen del componenete horario en el campo FECHA_HORA
  bd_datos$FECHA <- as.character(bd_datos$FECHA)
  Sys.setlocale("LC_TIME", "C")   #Se acomoda la configuracion local del ambiente
  bd_datos$FECHA <- as.Date(bd_datos$FECHA, format='%d/%m/%Y')
  checks[4]=identical(unique(is.na(bd_datos$FECHA)),FALSE) # No debe haber fechas vacías (en TRUE)
  
  c1 <- summarize(group_by(unique(select(bd_datos, COD_CLI, ID_CONTRATO)), ID_CONTRATO), num_clientes=n(),cod_clientes=paste(COD_CLI,collapse = " "))
  err_1 <- c1[c1$num_clientes>1,]
  checks[5] = nrow(err_1)==0
  
  if(nrow(err_1)>0){
    write.table(err_1, "data", sep = "\t", append = F,row.names = F)
    bd_datos<-bd_datos[!bd_datos$ID_CONTRATO %in% err_1$ID_CONTRATO,]
  }
  
  print(paste("Verificacion:",paste(checks,collapse = " ")))
  bd_datos
}

#############################################################################################################
#Funcion que verifica la carga exitosa del dataset horario y acomoda los tipos de los datos al adecuado
#############################################################################################################
verify_input_data_3 <- function(bd_ori, bd_datos){
  checks = logical()
  checks[1]=identical(unique(is.na(bd_ori)[1,]),FALSE)
  
  # Hay que modificar el caracter separador en los valores de las lecturas de ',' a '.'
  bd_datos$VALOR_LECTURA <- gsub(",", ".", bd_datos$VALOR_LECTURA)
  # Hay valores vacios ?
  checks[2]=identical(unique(is.na(bd_datos$VALOR_LECTURA)),FALSE)
  # si no tenemos ningún TRUE, es porque no hay valores vacíos
  
  # Se vuelven numericos los valores de las lecturas del consumo
  bd_datos$VALOR_LECTURA<-as.numeric(as.character(bd_datos$VALOR_LECTURA))
  checks[3]=identical(unique(is.na(bd_datos$VALOR_LECTURA)),FALSE)
  # si no tenemos ningún TRUE, es porque no hay valores vacíos
  
  #bd_datos$HORA <- ifelse(bd_datos$CONS_HORA == 24,0,bd_datos$CONS_HORA)
  bd_datos$FECHA_HORA <- paste(as.character(bd_datos$CONS_FECHA),sprintf("%02d:00",ifelse(bd_datos$CONS_HORA == 24,0,bd_datos$CONS_HORA)))
  bd_datos$FECHA_HORA <- as.character(bd_datos$FECHA_HORA)
  bd_datos$FECHA_HORA <- as.POSIXct(bd_datos$FECHA_HORA, format='%d/%m/%Y %H:%M')
  checks[4]=identical(unique(is.na(bd_datos$FECHA_HORA)),FALSE) # No debe haber fechas vacías (en TRUE)
  bd_datos$CONS_HORA <- NULL
  bd_datos$CONS_FECHA <- NULL
  
  c1 <- summarize(group_by(unique(bd_datos[,1:2]), ID_CONTRATO), num_clientes=n(),cod_clientes=paste(COD_CLI,collapse = " "))
  err_1 <- c1[c1$num_clientes>1,]
  checks[5] = nrow(err_1)==0
  if(nrow(err_1)>0){
    write.table(err_1, "data", sep = "\t", append = F,row.names = F, quote = F)
    bd_datos<-bd_datos[!bd_datos$ID_CONTRATO %in% err_1$ID_CONTRATO,]
  }
  print(paste("Verificacion:",paste(checks,collapse = " ")))
  bd_datos #bd_h_1 <<-
}

#############################################################################################################
#Funcion que remueve las lecturas duplicadas (Mismo contrato, canal y fecha) del dataset diario
#############################################################################################################
remove_duplicates_2 <- function(bd_datos){
  bd_datos$FECHA <- as.character(bd_datos$FECHA, format="%d/%m/%Y")
  g1 <- summarize(group_by(bd_datos, COD_CLI, ID_CONTRATO, COD_CANAL,FECHA),VALOR_LECTURA=sum(VALOR_LECTURA))
  g1$FECHA <- as.Date(g1$FECHA, format='%d/%m/%Y')
  g1
}

#############################################################################################################
#Funcion que remueve las lecturas duplicadas (Mismo contrato, canal y fecha) del dataset horario
#############################################################################################################
remove_duplicates_3 <- function(bd_datos){
  bd_datos$FECHA_HORA <- as.character(bd_datos$FECHA_HORA, format="%d/%m/%Y %H:%M:%S")
  g1 <- summarize(group_by(bd_datos, COD_CLI, ID_CONTRATO,CLASE_CLI,FECHA_HORA),VALOR_LECTURA=sum(VALOR_LECTURA))
  g1$FECHA_HORA <- as.POSIXct(g1$FECHA_HORA, format='%d/%m/%Y %H:%M:%S')
  g1
}

#############################################################################################################
#Funcion que añade las lecturas faltantes para cada contrato en el dataset diario. Cada nueva lectura se añade con un valor de 0
#############################################################################################################
add_missing_2 <- function(bd_datos,allDates){
  contratos <- sort(unique(bd_datos$ID_CONTRATO))#unique(bd_datos["ID_CONTRATO"])
  canales <-c("ACTIVA","REACTIVA") #unique(bd_datos["COD_CANAL"])
  nueva_lectura <- 0
  temp2 <- data.frame()
  c = 1
  while(c<=length(contratos)){
    contrato=as.integer(contratos[c])
    client <- unique(bd_datos[bd_datos$ID_CONTRATO==contrato,"COD_CLI"])[1]
    ca = 1
    while(ca<=length(canales)){
      canal=as.character(canales[ca])
      temp <- filter(bd_datos, ID_CONTRATO==contrato & COD_CANAL==canal)
      temp3<- data.frame(FECHA= allDates[!allDates %in% temp$FECHA]) 
      
      #temp <- temp %>% #complete(FECHA = seq.POSIXt(minDate, limitDate, by = 'hour'))
      #temp <- temp[is.na(temp$VALOR_LECTURA),]
      
      if(nrow(temp3)<1) break
      temp3$COD_CLI <- as.integer(client)
      temp3$ID_CONTRATO <- as.integer(contrato)
      temp3$COD_CANAL <- canal
      temp3$VALOR_LECTURA <- nueva_lectura
      temp3 <- temp3[,c(2,3,4,1,5)]
      
      temp2 <- rbind.data.frame(temp2,temp3)
      ca<-ca+1
    }
    c<-c+1
  }
  rbind.data.frame(bd_datos,temp2)
}

#############################################################################################################
#Funcion que añade las lecturas faltantes para cada contrato en el dataset horario. Cada nueva lectura se añade con un valor de 0
#############################################################################################################
add_missing_3 <- function(bd_datos,allDates){
  contratos <- as.character(sort(unique(bd_datos$ID_CONTRATO)))
  nueva_lectura <- 0
  temp2 <- data.frame()
  for(c in contratos){
    client <- unique(bd_datos[bd_datos$ID_CONTRATO==c,"COD_CLI"])[1]
    temp <- bd_datos[bd_datos$ID_CONTRATO==c,]
    temp3<- data.frame(FECHA_HORA= allDates[!allDates %in% temp$FECHA_HORA]) 
    
    if(nrow(temp3)<1) next
    temp3$COD_CLI <- as.integer(client)
    temp3$ID_CONTRATO <- c
    temp3$CLASE_CLI <- temp$CLASE_CLI[1]
    temp3$VALOR_LECTURA <- nueva_lectura
    temp3 <- temp3[,c(2,3,4,1,5)]
    temp2 <- rbind.data.frame(temp2,temp3)
  }
  rbind.data.frame(bd_datos,temp2)
}

#############################################################################################################
#Funcion que calcula el umbral optimo para el dataset diario, que permite conservar la mayoria de contratos, minimizando el numero de lecturas faltantes
#############################################################################################################
compute_threshold_2<- function(bd_datos,allDates){
  #Omitir los contratos con lecturas insuficientes
  s2 <- summarize(group_by(bd_datos, ID_CONTRATO, COD_CANAL), count=n(), minDate=min(FECHA), limitDate=max(FECHA))
  seq_1 <- append(seq(0.5, 0.95, by=0.05),seq(0.96, 1, by=0.01))
  frec_1 <- data.frame()
  for(sq in seq_1){
    uni_1 <- unique(s2[s2$count<length(allDates)*sq,1])
    if(nrow(uni_1)<1) next
    fil_1 <- filter(bd_datos, !ID_CONTRATO %in% uni_1$ID_CONTRATO)#c(1,3)
    frec_1 <- rbind(frec_1,c(sq,nrow(fil_1),round(nrow(fil_1)/nrow(bd_datos),3),length(unique(fil_1$ID_CONTRATO)),
                             round(length(unique(fil_1$ID_CONTRATO))/length(unique(bd_datos$ID_CONTRATO)),3)))
    }
  colnames(frec_1)<-c('limite','lecturas','l_%','contratos','c_%')
  #frec_1<-summarize(group_by(frec_1, lectures), limit=max(limit))[,c(2,1)]
  print(frec_1)
  limit <<- 0.95
  s2
}

#############################################################################################################
#Funcion que calcula el umbral optimo para el dataset horario, que permite conservar la mayoria de contratos, minimizando el numero de lecturas faltantes
#############################################################################################################
compute_threshold_3<- function(bd_datos,allDates){
  #Omitir los contratos con lecturas insuficientes
  s2 <- summarize(group_by(bd_datos, ID_CONTRATO), count=n(), minDate=min(FECHA_HORA), limitDate=max(FECHA_HORA))
  seq_1 <- append(seq(0.5, 0.95, by=0.05),seq(0.96, 1, by=0.01))
  frec_1 <- data.frame()
  for(sq in seq_1){
    uni_1 <- unique(s2[s2$count<length(allDates)*sq,1])
    if(nrow(uni_1)<1) next
    fil_1 <- filter(bd_datos, !ID_CONTRATO %in% uni_1$ID_CONTRATO)#c(1,3)
    frec_1 <- rbind(frec_1,c(sq,nrow(fil_1),round(nrow(fil_1)/nrow(bd_datos),3),length(unique(fil_1$ID_CONTRATO)),
                             round(length(unique(fil_1$ID_CONTRATO))/length(unique(bd_datos$ID_CONTRATO)),3)))
  }
  colnames(frec_1)<-c('limite','lecturas','l_%','contratos','c_%')
  #frec_1<-summarize(group_by(frec_1, lectures), limit=max(limit))
  # limit    lectures
  #  1.00    0.021   28465
  print(frec_1)
  limit <<- 0.95
  s2
}

#############################################################################################################
#Funcion que permite cargar todos los datasets horarios de los diferentes años
#############################################################################################################
load_multiple_datasets<- function(){
  files <- list.files("C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/dataset_hourly_full/")
  for (f in files){
    if (!exists("bd_hourly")){
      bd_hourly<-read.table(paste("C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/dataset_hourly_full/",f,sep =''), header = T, sep="\t")
    }
    else{
      bd_hourly<-rbind(bd_hourly, read.table(paste("C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/dataset_hourly_full/",f,sep =''), header = T, sep="\t"))
    }
  }
  bd_hourly
}
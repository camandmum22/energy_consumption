####################################################################################################################
# Script que permite ejecutar la prueba de concepto
####################################################################################################################
rm(list=ls())
data <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/"
code <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/[code]/"
plots <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/test_v1/"
report <- "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/test_v1/[hourly_v1].txt"
setwd(code)

source("prototype_v2.R")
main_research_line(data,code,plots,report)

# Lectura de los tiempos de ejecucion
t2-t1
t3-t2
t4-t3
t5-t4
t6-t5
t7-t6
t8-t7
t9-t8
t10-t9
t11-t10
t12-t11
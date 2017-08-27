####################################################################################################################
# Script que permite ejecutar la prueba de concepto desde Python
####################################################################################################################
import rpy2.robjects as robjects
import gc
r_setwd = robjects.r['setwd']
r_source = robjects.r['source']

robjects.r('memory.size()')
robjects.r('memory.limit()')
robjects.r('memory.limit(size=12200)')

robjects.r('rm(list=ls())')
data = "C:/Users/Camilo Andres/Desktop/Variety/i2t/[data]/"
code = "C:/Users/Camilo Andres/Desktop/Variety/i2t/[code]/"
plots = "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/test_v1/"
report = "C:/Users/Camilo Andres/Desktop/Variety/i2t/plots/test_v1/[hourly_v1].txt"
r_setwd(code)

r_source("prototype_v2.R")
r_main_research_line = robjects.r['main_research_line']
gc.collect()
r_main_research_line(data,code,plots,report)

# Consulta de los tiempos de ejecucion
for t in list(range(2, 13)):
    s = str('t'+str(t)+'-'+'t'+str(t-1))
    print(str(t)+' '+str(robjects.r(s)))

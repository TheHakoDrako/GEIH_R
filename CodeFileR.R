#------------------------------------------------


# Jeronimo Ramirez Mejia

# (GEIH 2022 - Enero, Febrero y Marzo)


#------------------------------------------------

# Limpieza de consola

cat("\f")

# Se crea directorio, luego se cargan las librerias y paquetes

rm(list = ls())
options("scipen" = 100, "digits" = 4)

setwd("Modelo")

library(data.table)
library(readr)
library(dplyr)
library(plyr)
library(carData)
library(car)
library(haven)
library(tidyverse)

# Asignacion de etiquetas para las bases de datos de la GEIH
#
# CG = Caracteristicas Generales...    ***
# DH = Datos de hogar y vivienda.      
# FT = Fuerza de trabajo.
# NO = No Ocupados.                    ***
# OC = Ocupados.                       ***
# OFT = Otras formas de trabajo.
# OI = Otros ingresos e impuestos.
#

# Se cargan las bases de datos del año 2022 (Enero, Febrero y Marzo).

# Mes de Enero (Union de datos) - Se Asigna E "Enero" al final de las variables
# Bases externas

DHE <- read_dta("DH.dta")
FTE <- read_dta("FT.dta")
OFTE <- read_dta("OFT.dta")
OIE <- read_dta("OI.dta")

# Bases que se cargan en el modelo

CGE <- read_dta("CG.dta")
NOE <- read_dta("NO.dta")
OCE <- read_dta("OC.dta")

DatosEnero <- list(CGE, NOE, OCE)
Enero <- Reduce(function(...) merge(..., by = c("DIRECTORIO","SECUENCIA_P","ORDEN","FEX_C18","AREA"), all.x = T, suffixes = c(" ","_E")), DatosEnero) %>% mutate(MES=1)

rm(CGE, NOE, OCE)

# Mes de Febrero (Union de datos) - Se Asigna F "Febrero" al final de las variables
# Bases externas

DHF <- read_dta("DH.dta")
FTF <- read_dta("FT.dta")
OFTF <- read_dta("OFT.dta")
OIF <- read_dta("OI.dta")

# Bases que se cargan

CGF <- read_dta("CG.dta")
NOF <- read_dta("NO.dta")
OCF <- read_dta("OC.dta")

DatosFebrero = list(CGF, NOF, OCF)
Febrero <- Reduce(function(...) merge(..., by = c("DIRECTORIO","SECUENCIA_P","ORDEN","FEX_C18","AREA"), all.x = T, suffixes = c(" ","_F")), DatosFebrero) %>% mutate(MES=2)

rm(CGF, NOF, OCF)

# Mes de Marzo (Union de datos) - Se Asigna M "Marzo" al final de las variables
# Bases externas

DHM <- read_dta("DH.dta")
FTM <- read_dta("FT.dta")
OFTM <- read_dta("OFT.dta")
OIM <- read_dta("OI.dta")

CGM <- read_dta("CG.dta")
NOM <- read_dta("NO.dta")
OCM <- read_dta("OC.dta")

DatosMarzo = list(CGM, NOM, OCM)
Marzo <- Reduce(function(...) merge(..., by = c("DIRECTORIO","SECUENCIA_P","ORDEN","FEX_C18","AREA"), all.x = T, suffixes = c(" ","_M")), DatosMarzo) %>% mutate(MES=3)

rm(CGM, NOM, OCM)
GEIH_En_Mz <- plyr::rbind.fill(Enero,Febrero,Marzo) %>% mutate(MES=as.numeric(.$MES,length=0)) %>% mutate(FEX_C18=as.numeric(.$FEX_C18,length=0))

# Algunas descripciones y otros calculos

pet = GEIH %>% subset(fuerza==1) %>% group_by(mes,periodo) %>% summarise(pet=sum(FEX_C18.x)) %>% 
  reshape2::dcast(.,formula = mes~periodo,value.var = 'pet')
colnames(pet)=c('mes','pet_2022')
ina = nacional %>% subset(inanctivo==1) %>% group_by(mes,periodo) %>% summarise(ina=sum(FEX_C18)) %>% 
  reshape2::dcast(.,formula = mes~periodo,value.var = 'ina')
colnames(ina)=c('mes','ina_2022')
des = nacional %>% subset(desocupado==1) %>% group_by(mes,periodo) %>% summarise(des=sum(FEX_C18.x)) %>% 
  reshape2::dcast(.,formula = mes~periodo,value.var = 'des')
colnames(des)=c('mes','des_2022')
desempleo = merge(x = pet,y = ina,by = 'mes',all.x=T) %>% 
  merge(x = .,y = des,by = 'mes',all.x=T)
desempleo = mutate(desempleo, pea_2022=(pet_2022-ina_2022))
desempleo = mutate(desempleo, un_2022=(des_2022/pea_2022)*100)

desempleo$mes_num = ifelse(desempleo$mes=="Enero",1,
                           ifelse(desempleo$mes=='Febre',2,
                                  ifelse(desempleo$mes=='Marzo',3,)))

desempleo = desempleo[order(desempleo$mes_num),]
desempleo = reshape2::melt(data = desempleo, id.vars=c("mes","mes_num"),value.name='desem') %>% subset(.,variable=="un_2022"|variable=='un_2022')
desempleo = mutate(desempleo, fecha = as.yearmon(ifelse(desempleo$variable=='un_2022',paste0('2022','-',desempleo$mes_num))))
poblacion = nacional %>% subset(.,is.na(dpto.x)==F) %>%  group_by(dpto.x,mes,periodo) %>% summarise(pob=sum(FEX_C18.x,na.rm=T)) %>% as.data.frame()


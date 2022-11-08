#------------------------------------------------


# Jeronimo Ramirez Mejia

# (GEIH 2022 - Enero, Febrero y Marzo)


#------------------------------------------------

# Limpieza de consola

cat("\f")

# Se crea directorio, luego se cargan las librerias y paquetes

rm(list = ls())
options("scipen" = 100, "digits" = 4)

setwd("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo")

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

DHE <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Enero/DTA/DH.dta")
FTE <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Enero/DTA/FT.dta")
OFTE <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Enero/DTA/OFT.dta")
OIE <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Enero/DTA/OI.dta")

# Bases que se cargan en el modelo

CGE <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Enero/DTA/CG.dta")
NOE <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Enero/DTA/NO.dta")
OCE <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Enero/DTA/OC.dta")

DatosEnero <- list(CGE, NOE, OCE)
Enero <- Reduce(function(...) merge(..., by = c("DIRECTORIO","SECUENCIA_P","ORDEN","FEX_C18","AREA"), all.x = T, suffixes = c(" ","_E")), DatosEnero) %>% mutate(MES=1)

rm(CGE, NOE, OCE)

# Mes de Febrero (Union de datos) - Se Asigna F "Febrero" al final de las variables
# Bases externas

DHF <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Febrero/DTA/DH.dta")
FTF <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Febrero/DTA/FT.dta")
OFTF <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Febrero/DTA/OFT.dta")
OIF <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Febrero/DTA/OI.dta")

# Bases que se cargan

CGF <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Febrero/DTA/CG.dta")
NOF <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Febrero/DTA/NO.dta")
OCF <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Febrero/DTA/OC.dta")

DatosFebrero = list(CGF, NOF, OCF)
Febrero <- Reduce(function(...) merge(..., by = c("DIRECTORIO","SECUENCIA_P","ORDEN","FEX_C18","AREA"), all.x = T, suffixes = c(" ","_F")), DatosFebrero) %>% mutate(MES=2)

rm(CGF, NOF, OCF)

# Mes de Marzo (Union de datos) - Se Asigna M "Marzo" al final de las variables
# Bases externas

DHM <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Marzo/DTA/DH.dta")
FTM <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Marzo/DTA/FT.dta")
OFTM <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Marzo/DTA/OFT.dta")
OIM <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Marzo/DTA/OI.dta")

# Bases que se cargan

CGM <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Marzo/DTA/CG.dta")
NOM <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Marzo/DTA/NO.dta")
OCM <- read_dta("D:/Documentos/Universidad/Universidad Del Quindío/Economía/Economía 8 semestre/CuentasNacionales/Desarrollos/Parcial2/Modelo/GEIH_PARCIAL/DatosMeses/2022/Marzo/DTA/OC.dta")

DatosMarzo = list(CGM, NOM, OCM)
Marzo <- Reduce(function(...) merge(..., by = c("DIRECTORIO","SECUENCIA_P","ORDEN","FEX_C18","AREA"), all.x = T, suffixes = c(" ","_M")), DatosMarzo) %>% mutate(MES=3)

rm(CGM, NOM, OCM)

# Union de las bases de datos (Enero, febrero y marzo) para tener una base de datos en conjunto

GEIH_En_Mz <- plyr::rbind.fill(Enero,Febrero,Marzo) %>% mutate(MES=as.numeric(.$MES,length=0)) %>% mutate(FEX_C18=as.numeric(.$FEX_C18,length=0))

# Aplico factor de expansion

GEIH_En_Mz$fet <- GEIH_En_Mz$FEX_C18/3
rm(DatosEnero, DatosFebrero, DatosMarzo)

# Se Filtra el departamento de Bolivar = 13

Bolivar <- filter(GEIH_En_Mz, AREA == 13)

# Calculos para el departamento de Bolivar (Trimestre)

# PTN = Poblacion Total Nacional
# PTB = Poblacion Total Bolivar

PTN <- xtabs(formula= FEX_C18 ~ MES, data = GEIH_En_Mz) %>% mean()
PTB <- xtabs(formula= FEX_C18 ~ MES, data = Bolivar) %>% mean()

# PETN = Poblacion en edad de trabajar Nacional
# PETB = Poblacion en edad de trabajar Bolivar

PETN <- GEIH_En_Mz %>% filter(P6040 >= 15) %>% xtabs(formula = FEX_C18 ~ MES, data=) %>% mean()
PETB <- Bolivar %>% filter(P6040 >= 15) %>% xtabs(formula = FEX_C18 ~ MES, data=) %>% mean()

# La fuerza de trabajo segun el manual de la GEIH, se divide en ocupados "OCI" y desocupados "DSI"

# OCIN = Ocupados Nacional
# OCIB = Ocupados Bolivar

OCIN <- GEIH_En_Mz %>% xtabs(formula= FEX_C18 ~ OCI + MES, data=) %>% mean()
OCIB <- Bolivar %>% xtabs(formula = FEX_C18 ~ OCI + MES, data=) %>% mean()

# DSIN = Desocupados Nacional
# DSIB = Desocupados Bolivar

DSIN <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ DSI + MES, data=) %>% mean()
DSIB <- Bolivar %>% xtabs(formula = FEX_C18 ~ DSI + MES, data=) %>% mean()

# Fuerza de Trabajo: es la población economicamente activa que trabaja o busca empleo
# FTN = Fuerza de trabajo Nacional (Suma de ocupados y desocupados en el trimestre)
# FTB = Fuerza de trabajo Bolivar (Suma de ocupados y desocupados en el trimestre)

FTN <- (OCIN + DSIN) 
FTB <- (OCIB + DSIB)

# FTTN = Poblacion Fuera de la Fuerza de Trabajo Nacional
# FTTB = Poblacion Fuera de la Fuerza de Trabajo Bolivar

FTTN <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ FFT + MES, data=) %>% mean()
FTTB <- Bolivar %>% xtabs(formula = FEX_C18 ~ FFT + MES, data=) %>% mean()

# Subocupados: Trabajadores que manifiestan mejora de ingresos y ademas gestionan un cambio laboral.
# SI = 1 (filtro de seleccion).
# P7150 = Hizo diligencias para cambio de trabajo. 
# P7160 = Si encontraron empleo nuevo, puede ejercerlo antes de un mes.
# P7090 = Deseo Manifestado a mejorar ingresos, QUIERE TRABAJAR MAS HORAS.
# P7140S1 = Cambio laboral para mejorar capacidades o formacion
# P7140S2 = Desea mejorar sus ingresos

# P7100 = Deseo de agregar mas horas adicionales a trabajar
# P7110 = Hizo diligencias para trabajar mas horas
# P7120 = Si resulta trabajo, esta disponible y dispuesto

# Subocupados por insuficiencia de horas (IH)

SUBOS_IH <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ (P7090 == 1) + (P7160 == 1) + MES, data=) %>% mean()
SUBOS_IHB <- Bolivar %>% filter(P7090 == 1, P7160 == 1) %>% xtabs(formula = FEX_C18 ~ P7090 + P7160 + MES, data=) %>% mean()

# Subocupados por competencias (C)

SUBOS_C <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ (P7140S1 == 1) + (P7150 == 1) + MES, data=) %>% mean()
SUBOS_CB <- Bolivar %>% filter(P7140S1 == 1, P7150 == 1) %>% xtabs(formula = FEX_C18 ~ P7140S1 + P7150 + MES, data=) %>% mean()

# Subocupados por ingresos (I)

SUBOS_I <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ (P7140S2 == 1) + P7090 + MES, data=) %>% mean()
SUBOS_IB <- Bolivar %>% filter(P7140S2 == 1, P7090 == 1) %>% xtabs(formula = FEX_C18 ~ P7140S2 + MES, data=) %>% mean()

# TSUBOSN = Subocupados Total Nacional
# TSUBOSB = Subocupados Total Bolivar

TSUBOSN = (SUBOS_IH + SUBOS_C + SUBOS_I) %>% mean()
TSUBOSB = (SUBOS_IHB + SUBOS_CB + SUBOS_IB)

# Fuerza de trabajo potencial: Busca trabajo, no está disponible; Disponible, no busca trabajo; No busca trabajo, ni está disponible, pero quiere un empleo

# FTPN = Fuerza de Trabajo Potencial Nacional
# FTPN = Fuerza de Trabajo Potencial Bolivar

FTPN_1 <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ (P744 == 1) + MES, data=) %>% mean()
FTPN_2 <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ (P744 == 2) + MES, data=) %>% mean()
FTPN_3 <- GEIH_En_Mz %>% xtabs(formula = FEX_C18 ~ P7250 + MES, data=) %>% mean()

FTPN = (FTPN_1 + FTPN_2 + FTPN_3)
FTPB <- Bolivar %>% filter (P7250 >= 52) %>% xtabs(formula = FEX_C18 ~ P744 + P7250 + MES, data=) %>% mean()

# INGRESOS POR SEXO 
# HOMBRES 
# INGREHOMBRENA = Ingreso Hombres Nacional
# INGREHOMBREDE = Ingreso Hombres Bolivar

INGREHOMBRENA <- GEIH_En_Mz %>% filter(P3039 == 1) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()
INGREHOMBREDE <- Bolivar %>% filter(P3039 == 1) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()

# MUJERES 
# INGREMUJERNA = Ingreso Mujer Nacional
# INGREMUJERDE = Ingreso Mujer Bolivar

INGREMUJERNA <- GEIH_En_Mz %>% filter(P3039 == 2) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()
INGREMUJERDE <- Bolivar %>% filter(P3039 == 2) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()

# HOMBRES TRANS 
# INGREHOMBRETRANSNA = Ingreso Hombre Trans Nacional
# INGREHOMBRETRANSDE = Ingreso Hombre Trans Bolivar

INGREHOMBRETRANSNA <- GEIH_En_Mz %>% filter(P3039 == 3) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()
INGREHOMBRETRANSDE <- Bolivar %>% filter(P3039 == 3) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()

# MUJERES TRANS
# INGREMUJERESTRANSNA = Ingreso Mujeres Trans Nacional
# INGREMUJERESTRANSDE = Ingreso Mujeres Trans Bolivar

INGREMUJERESTRANSNA <- GEIH_En_Mz %>% filter(P3039 == 4) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()
INGREMUJERESTRANSDE <- Bolivar %>% filter(P3039 == 4) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()

# OTRO
# INGREOTRONA = Ingreso Otro Nacional
# INGREOTRODE = Ingreso Otro Bolivar

INGREOTRONA <- GEIH_En_Mz %>% filter(P3039 == 5) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()
INGREOTRODE <- Bolivar %>% filter(P3039 == 5) %>% xtabs(formula = FEX_C18 ~ P6500 + P3039 + MES, data =) %>% mean()

# INGRESO PROMEDIO TOTAL
# INGTOTN = Ingreso Total Nacional
# INGTOTB = Ingreso Total Bolivar

INGTOTN = (INGREHOMBRENA + INGREMUJERNA + INGREHOMBRETRANSNA + INGREMUJERESTRANSNA + INGREOTRONA)
INGTOTB = (INGREHOMBREDE + INGREMUJERDE)

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


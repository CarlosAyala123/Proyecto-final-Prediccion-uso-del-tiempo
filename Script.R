#### Proyecto Final - machine Learning and Big Data ####

# Cargar las bases

rm(list=ls())
#Console
cat("\f")

#------------------------------------------------------------------------------
# Cargar paquetes
require(pacman)
p_load(tidyverse,dplyr,here,skimr,tidyr,gamlr,modelsummary,caret,
       rio,knitr, kableExtra, rstudioapi,tidymodels,janitor,MLmetrics,
       reshape,rattle,doParallel,mixgb, install = TRUE)

p_load(mltools, xgboost,
       mixgb, vctrs,
       mlr, spdep,
       install = T)


set.seed(666)



#------------------------------------------------------------------------------
# Cargar las bases de datos


path_code <- dirname(getActiveDocumentContext()$path)
setwd(path_code)
getwd()

a <- read.csv("Data/ENUT_C1/ENUT_C1.csv", header = T)

colnames(a) <- tolower(colnames(a))

skim(a)

a <- subset(a, select = -c(p4030s1a1, p4030s4a1, dia_ref, f_exp_viv)) ## dejar las variables a usar

names <- c(  vivienda = "directorio",tipo_vivienda = "p424" ) ## vercotr de nombres

a <- rename(a, names ) ## asignar nuevo nombre a las variables


#C_7
C_7<-read.csv("Data/ENUT_C7/ENUT_C7.csv", header = T)
colnames(C_7) <- tolower(colnames(C_7))
skim(C_7)
C_7 <- subset(C_7, select = c(directorio, p6340, secuencia_p, orden))
names <- c( vivienda = "directorio",  Intento_trabajo_u12m = "p6340", hogar = "secuencia_p", individuo = "orden"  ) ## vercotr de nombres
C_7 <- rename(C_7, names ) ## asignar nuevo nombre a las variables
sum(is.na(C_7$Intento_trabajo_u12m )) #No usar C_7

#C_7_1
C_7_1<-read.csv("Data/ENUT_C7_1/ENUT_C7_1.csv", header = T)
colnames(C_7_1) <- tolower(colnames(C_7_1))
skim(C_7_1)
C_7_1 <- subset(C_7_1, select = c(directorio, p6545a, secuencia_p, orden,p6545b,p6545c,p6545d,p1118,
                                  p1150, p1150s1, p1150s2)
                                   ) #solo hay cosas de subsidios, auxilios y cosas de tiempo en el trabajo en el día, mes anño de referencia.
names <- c( vivienda = "directorio", hogar = "secuencia_p", individuo = "orden", trabajo_dmar = "p1150", 
            horas = "p1150s1", minutos = "p1150s2"   )
C_7_1 <- rename(C_7_1, names )

C_7_1$trabajo_dmar[C_7_1$trabajo_dmar==2]<-0
C_7_1$p6545a[C_7_1$p6545a==2]<-0
C_7_1$p6545b[C_7_1$p6545b==2]<-0  
C_7_1$p6545c[C_7_1$p6545c==2]<-0 
C_7_1$p6545d[C_7_1$p6545d==2]<-0 
C_7_1$p1118[C_7_1$p1118==2]<-0 

sum(is.na(C_7_1$trabajo_dmar))#68240 NA
sum(is.na(C_7_1$p6545a)) #96000 NA
sum(is.na(C_7_1$p6545b)) #96000 NA
sum(is.na(C_7_1$p6545c)) #96000 NA
sum(is.na(C_7_1$p6545d)) #96000 NA
sum(is.na(C_7_1$p1118)) #No NA recibió en los últimos meses dinero por concepto de otros hogares

C_7_1$horas = C_7_1$horas * 60
C_7_1$total_m <- (C_7_1$horas+C_7_1$minutos)
C_7_1$total_m [C_7_1$trabajo_dmar==0]<-0 #consideramos que esos NA son 0 pues si no trabajo el timpo es 0
sum(is.na(C_7_1$total_m))

#C_8 sacamos el tiempo no remunerado del hogar
C_8<-read.csv("Data/ENUT_C8/ENUT_C8.csv", header = T)
colnames(C_8) <- tolower(colnames(C_8))
skim(C_8)
C_8 <- subset(C_8, select = c(directorio, secuencia_p, orden,
                              p1143s1,p1143s2,p1143s3,p1143s4,p1143s5, p1143s1a1,p1143s1a2,
                              p1143s2a1, p1143s2a2, p1143s3a1, p1143s3a2, p1143s4a1, p1143s4a2,
                              p1142s1, p1142s2,p1142s3, p1142s4,p1142s5,p1142s1a1,p1142s1a2,
                              p1142s2a1,p1142s2a2,p1142s3a1,p1142s3a2,p1142s4a1,p1142s4a2,
                              p1136s1,p1136s2,p1136s3,p1136s4,p1136s5,p1136s6,p1136s7,p1136s1a1,
                              p1136s1a2,p1136s2a1,p1136s2a2,p1136s3a1,p1136s3a2,p1136s4a1,
                              p1136s4a2,p1136s5a1,p1136s5a2,p1136s6a1,p1136s6a2,p1136s7a1,p1136s7a2,
                              p1141s1,p1141s2,p1141s3,p1141s4,p1141s5,p1141s1a1,p1141s1a2,
                              p1141s2a1,p1141s2a2,p1141s3a1,p1141s3a2,p1141s4a1,p1141s4a2,
                              p1140s1,p1140s2,p1140s3,p1140s4,p1140s5,p1140s6,p1140s7,p1140s8,
                              p1140s1a1, p1140s1a2, p1140s1a3, p1140s1a4, p1140s2a1, p1140s2a2,
                              p1140s2a3, p1140s2a4, p1140s3a1, p1140s3a2, p1140s4a1, p1140s4a2,
                              p1140s4a3, p1140s4a4,p1140s5a1, p1140s5a2, p1140s5a3,p1140s5a4,
                              p1140s6a1, p1140s6a2, p1140s7a1,p1140s7a2))
names <- c( vivienda = "directorio", hogar = "secuencia_p", individuo = "orden"  )
C_8 <- rename(C_8, names )

Alimentos = c( C_8$p1143s1,C_8$p1143s2,C_8$p1143s3,C_8$p1143s4,C_8$p1143s5)
for(i in 1:5){
  for (j in 1:126753){
    if(is.na(Alimentos[i][j])==T){
      Alimentos[i][j] = 0
    }
  }
 
  
   
}








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
       reshape,rattle,doParallel,mixgb, tictoc, install = TRUE)

p_load(mltools, xgboost,
       mixgb, vctrs,
       mlr, spdep,
       install = T)

p_load(tidyverse,rio,glue,
       hexbin,
       patchwork,vip, ## plot: 
       ggrepel, ## plot: geom_text_repel
       stringi,tidytext,stopwords, ## text-data
       tidymodels,finetune) 


set.seed(666)



#------------------------------------------------------------------------------
# Cargar las bases de datos


path_code <- dirname(getActiveDocumentContext()$path)
setwd(path_code)
getwd()



## Base 1

c_1 <- read.csv("Data/ENUT_C1/ENUT_C1.csv", header = T)

colnames(c_1) <- tolower(colnames(c_1))

skim(c_1)

c_1$clase[c_1$clase==2]<-0
c_1$p4030s1[c_1$p4030s1==2]<-0
c_1$p4030s2[c_1$p4030s2==2]<-0
c_1$p4030s3[c_1$p4030s3==2]<-0
c_1$p4030s4[c_1$p4030s4==2]<-0
c_1$p4030s5[c_1$p4030s5==2]<-0
c_1$p4030s6[c_1$p4030s6==2]<-0


c_1 <- subset(c_1, select = -c(p4030s1a1, p4030s4a1, f_exp_viv)) ## dejar las variables a usar

names <- c(directorio = "vivienda", p424 = "tipo_vivienda", p4030s1 = "electricidad", p4030s5 = "acueducto",
           p4030s3 = "alcantarillado", p4030s4 = "recoleccion_basuras", p4030s6 = "internet", p4030s2 = "gas",
           clase = "cabecera") ## vercotr de nombres

c_1 <- rename(c_1, names) ## asignar nuevo nombre a las variables

c_1$cabecera[c_1$cabecera==2]<-0

skim(c_1)

write.csv(c_1, file = "Data/c_1.csv", col.names = T)



##Base 2

c_1 <- read.csv("Data/ENUT_C2/ENUT_C2.csv", header = T)

colnames(c_1) <- tolower(colnames(c_1))

c_1$p1103s1[c_1$p1103s1==2]<-0
c_1$p1103s2[c_1$p1103s2==2]<-0
c_1$p1103s3[c_1$p1103s3==2]<-0
c_1$p1176s1[c_1$p1176s1==2]<-0
c_1$p1176s2[c_1$p1176s2==2]<-0
c_1$p1176s3[c_1$p1176s3==2]<-0
c_1$p1176s4[c_1$p1176s4==2]<-0
c_1$p1176s5[c_1$p1176s5==2]<-0
c_1$p1176s6[c_1$p1176s6==2]<-0
c_1$p1176s7[c_1$p1176s7==2]<-0
c_1$p1176s9[c_1$p1176s9==2]<-0
c_1$p1176s10[c_1$p1176s10==2]<-0
c_1$p1176s11[c_1$p1176s11==2]<-0
c_1$p1176s12[c_1$p1176s12==2]<-0
c_1$p5093[c_1$p5093==2]<-0

c_1$subsidio[c_1$p1103s1==1]<-1
c_1$subsidio[c_1$p1103s2==1]<-1
c_1$subsidio[c_1$p1103s3==1]<-1
c_1$subsidio[is.na(c_1$subsidio)==T]<-0


c_1 <- subset(c_1, select = c(directorio, secuencia_p, p5090, subsidio,
                              p1176s1, p1176s2, p1176s3, p1176s4, p1176s5,
                              p1176s6, p1176s7, p1176s9, p1176s10, p1176s11, p1176s12,
                              p5093)) ## dejar las variables a usar

names <- c(directorio = "vivienda", secuencia_p = "hogar", p5090 = "tipo_ocupacion", p1176s1 = "lavadora", 
           p1176s2 = "secadora", p1176s3 = "nevera", p1176s4 = "licuadora", p1176s5 = "plancha", 
           p1176s6 = "estufa", p1176s7 = "horno", p1176s9 = "lava_platos", p1176s10 = "aspiradora", 
           p1176s11 = "automovil", p1176s12 = "motocicleta", p5093 = "servicio_dom") ## vercotr de nombres

c_1 <- rename(c_1, names) ## asignar nuevo nombre a las variables

skim(c_1)

write.csv(c_1, file = "Data/c_2.csv", col.names = T)



##Base 3

c_1 <- read.csv("Data/ENUT_C3/ENUT_C3.csv", header = T)

colnames(c_1) <- tolower(colnames(c_1))

c_1$p6020[c_1$p6020==1]<-0
c_1$p6020[c_1$p6020==2]<-1
c_1$p1173_1[c_1$p1173_1==2]<-0
c_1$p1173_1[c_1$p1173_1==9]<-0
c_1$p1172[c_1$p1172==2]<-0
c_1$p5762[c_1$p5762==2]<-0
c_1$p5762[c_1$p5762==3]<-0
c_1$p5754[c_1$p5754==2]<-0
c_1$p5754[c_1$p5754==3]<-0

c_1<- c_1 %>% drop_na(p1173_1) %>% drop_na(p426) %>% drop_na(p1172) %>% drop_na(p5762) %>% drop_na(p5754)

c_1 <- subset(c_1, select = c(directorio, secuencia_p, orden, p6040, p6020, p1173, p1173_1, p426,
                              p1172, p5762, p5754)) ## dejar las variables a usar

names <- c(directorio = "vivienda", secuencia_p = "hogar", orden = "ind", p6040 = "edad", p6020 = "mujer",
           p1173 = "comunidad", p1173_1 = "campesino", p426 = "estado_civil", p1172 = "pareja",
           p5762 = "madre", p5754 = "padre") ## vercotr de nombres

c_1 <- rename(c_1, names) ## asignar nuevo nombre a las variables

skim(c_1)

write.csv(c_1, file = "Data/c_3.csv", col.names = T)



##Base 4

c_1 <- read.csv("Data/ENUT_C4/ENUT_C4.csv", header = T)

colnames(c_1) <- tolower(colnames(c_1))

c_1$p6090[c_1$p6090==9]<-0
c_1$p6090[c_1$p6090==2]<-0
c_1$p1166[c_1$p1166==2]<-0

vars <- c("p1166s1a1", "p1166s1a2")
c_1 <- mutate_at(c_1, vars, ~replace(., is.na(.), 0))

c_1$p1166s1a1<- c_1$p1166s1a1*60
c_1$min_at_medica<-c_1$p1166s1a1+c_1$p1166s1a2
c_1$enfermedad[c_1$p1170s1==1]<-1
c_1$enfermedad[c_1$p1170s2==1]<-1
c_1$enfermedad[c_1$p1170s3==1]<-1
c_1$enfermedad[c_1$p1170s4==1]<-1
c_1$enfermedad[c_1$p1170s5==1]<-1
c_1$enfermedad[c_1$p1170s6==1]<-1
c_1$enfermedad[c_1$p1170s7==1]<-1
c_1$enfermedad[c_1$p1170s8==1]<-1
c_1$enfermedad[c_1$p1170s9==1]<-1
c_1$enfermedad[is.na(c_1$enfermedad)==T]<-0


c_1 <- subset(c_1, select = c(directorio, secuencia_p, orden, p6090, enfermedad, p1166, 
                              min_at_medica)) ## dejar las variables a usar

names <- c(directorio = "vivienda", secuencia_p = "hogar", orden = "ind", p6090 = "salud", p1166 = "at_medica") ## vercotr de nombres

c_1 <- rename(c_1, names) ## asignar nuevo nombre a las variables

skim(c_1)

write.csv(c_1, file = "Data/c_4.csv", col.names = T)



##Base 5

c_1 <- read.csv("Data/ENUT_C5/ENUT_C5.csv", header = T)

colnames(c_1) <- tolower(colnames(c_1))

c_1$p1165[c_1$p1165==2]<-0
c_1$p1178s1[is.na(c_1$p1178s1)==T]<-0
c_1$p1178s1[c_1$p1178s1==2]<-0

vars <- c("p1162s1a1", "p1162s2a1", "p1162s3a1", "p1162s4a1", "p1162s5a1", "p1162s6a1", "p1178s1")

for (i in vars){
  c_1[,i]<- c_1[,i]*60
}

vars <- c(paste0("p1162s",1:6,"a1"), paste0("p1162s",1:6,"a2"))
c_1 <- mutate_at(c_1, vars, ~replace(., is.na(.), 0))


c_1$tiempo_esparcimiento_ninos <- c_1$p1162s1a1 + c_1$p1162s2a1 + c_1$p1162s3a1 + c_1$p1162s4a1 + c_1$p1162s5a1 + c_1$p1162s6a1 +
  c_1$p1162s1a2 + c_1$p1162s2a2 + c_1$p1162s3a2 + c_1$p1162s4a2 + c_1$p1162s5a2 + c_1$p1162s6a2

c_1 <- subset(c_1, select = c(directorio, secuencia_p, orden, p1165, p1178s1, 
                              tiempo_esparcimiento_ninos)) ## dejar las variables a usar

names <- c(directorio = "vivienda", secuencia_p = "hogar", orden = "ind", p1165 = "jardin", 
           p1178s1 = "tiempo_jardin") ## vercotr de nombres

c_1 <- rename(c_1, names) ## asignar nuevo nombre a las variables

skim(c_1)

write.csv(c_1, file = "Data/c_5.csv", col.names = T)



##Base 6

c_1 <- read.csv("Data/ENUT_C6/ENUT_C6.csv", header = T)

colnames(c_1) <- tolower(colnames(c_1))

c_1$p6160[c_1$p6160==2]<-0
c_1$p1160s1a1<- c_1$p1160s1a1*60
c_1$tiempo_estudio<-c_1$p1160s1a1+c_1$p1160s1a2
c_1$tiempo_estudio[is.na(c_1$tiempo_estudio)==T]<-0
c_1 <- c_1[!is.na(c_1$p6210),] #### quitar na

vars <- c("p1153s1a1", "p1153s2a1", "p1153s3a1", "p1153s4a1", "p1153s5a1", "p1153s6a1")

for (i in vars){
  c_1[,i]<- c_1[,i]*60
}

vars <- c(paste0("p1153s",1:6,"a1"), paste0("p1153s",1:6,"a2"))
c_1 <- mutate_at(c_1, vars, ~replace(., is.na(.), 0))

c_1$tiempo_esparcimiento <- c_1$p1153s1a1 + c_1$p1153s2a1 + c_1$p1153s3a1 + c_1$p1153s4a1 + c_1$p1153s5a1 + c_1$p1153s6a1 +
  c_1$p1153s1a2 + c_1$p1153s2a2 + c_1$p1153s3a2 + c_1$p1153s4a2 + c_1$p1153s5a2 + c_1$p1153s6a2

c_1 <- subset(c_1, select = c(directorio, secuencia_p, orden, p6160, tiempo_estudio, p6210, 
                              tiempo_esparcimiento)) ## dejar las variables a usar

names <- c(directorio = "vivienda", secuencia_p = "hogar", orden = "ind", p6160 = "leer_escribir",
           p6210 = "grado_aprobado") ## vercotr de nombres

c_1 <- rename(c_1, names) ## asignar nuevo nombre a las variables

skim(c_1)

write.csv(c_1, file = "Data/c_6.csv", col.names = T)



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
names <- c( vivienda = "directorio", hogar = "secuencia_p", ind = "orden"  )
C_8 <- rename(C_8, names )

Horas <- c( "p1143s1a1", "p1143s2a1","p1143s3a1","p1143s4a1","p1142s1a1", "p1142s2a1","p1142s3a1",
            "p1142s4a1", "p1136s1a1", "p1136s2a1", "p1136s3a1", "p1136s4a1", "p1136s5a1", "p1136s6a1", "p1136s7a1",
            "p1141s1a1","p1141s2a1", "p1141s3a1", "p1141s4a1", "p1140s1a1", "p1140s1a3", "p1140s2a1", "p1140s2a3",
            "p1140s3a1", "p1140s4a1", "p1140s4a3", "p1140s5a1", "p1140s5a3", "p1140s6a1", "p1140s7a1")

vars_hora <- c(paste0("p1143s",1:4,"a1"), paste0("p1142s",1:4,"a1"),paste0("p1136s",1:7,"a1"),paste0("p1141s",1:4,"a1")
               ,paste0("p1140s",1:7,"a1"),"p1140s1a3","p1140s2a3","p1140s4a3","p1140s5a3"  )

for (i in Horas){
  C_8[,i]<- C_8[,i]*60 
  
}

C_8 <- mutate_at(C_8, c(vars_hora), ~replace(., is.na(.), 0))

vars_min <- c(paste0("p1143s",1:4,"a2"), paste0("p1142s",1:4,"a2"),paste0("p1136s",1:7,"a2"),paste0("p1141s",1:4,"a2")
              ,paste0("p1140s",1:7,"a2"),"p1140s1a4","p1140s2a4","p1140s4a4","p1140s5a4"  )

C_8 <- mutate_at(C_8, c(vars_min), ~replace(., is.na(.), 0))

   
C_8$H_tot<- (C_8$p1143s1a1+C_8$p1143s2a1+C_8$p1143s3a1
              +C_8$p1143s4a1+C_8$p1142s1a1+C_8$p1142s2a1
                 +C_8$p1142s3a1+C_8$p1142s4a1+C_8$p1136s1a1+C_8$p1136s2a1+C_8$p1136s3a1+C_8$p1136s4a1+
                   C_8$p1136s5a1+C_8$p1136s6a1+C_8$p1136s7a1+C_8$p1141s1a1+C_8$p1141s2a1+C_8$p1141s3a1+
                   C_8$p1141s4a1+C_8$p1140s1a1+C_8$p1140s1a3+C_8$p1140s2a1+C_8$p1140s2a3+C_8$p1140s3a1+
                   C_8$p1140s4a1+C_8$p1140s4a3+C_8$p1140s5a1+C_8$p1140s5a3+C_8$p1140s6a1+C_8$p1140s7a1)

C_8$m <- (C_8$p1143s2a2+C_8$p1143s1a2+C_8$p1143s3a2+C_8$p1143s4a2+C_8$p1142s2a2+C_8$p1142s3a2+C_8$p1142s4a2+
            C_8$p1136s1a2+C_8$p1136s2a2+C_8$p1136s3a2+C_8$p1136s4a2+C_8$p1136s5a2+C_8$p1136s6a2+C_8$p1136s7a2+
            C_8$p1141s1a2+ C_8$p1141s2a2+C_8$p1141s3a2+C_8$p1141s4a2+ C_8$p1140s1a2+C_8$p1140s1a4+C_8$p1140s2a2+
            C_8$p1140s2a4+C_8$p1140s3a2+C_8$p1140s4a2+ C_8$p1140s4a4+C_8$p1140s5a2+C_8$p1140s5a4+ C_8$p1140s6a2+
            C_8$p1140s7a2)

C_8$Tiempo_labores_no_rem <-(C_8$H_tot+C_8$m)

C_8<-subset(C_8, select = c(vivienda,hogar,ind,Tiempo_labores_no_rem ))

write.csv(C_8,file = "Data/c_8.csv")



## merge de la base

filenames <- list.files("Data", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
names(ldf) <- substr(filenames, 6, 30)

a<-ldf$c_1.csv
b<-ldf$c_2.csv
c<-ldf$c_3.csv
d<-ldf$c_4.csv
e<-ldf$c_5.csv
f<-ldf$c_6.csv
g<-ldf$c_8.csv

a$X<-NULL
b$X<-NULL
c$X<-NULL
d$X<-NULL
e$X<-NULL
f$X<-NULL
g$X<-NULL


base <- merge(x = a, y = b, all=T)
base <- merge(x = base, y = c, all=T)
base <- merge(x = merge(x = merge(x = base, y = d, all=T), y = f, all=T), y = g, all=T)

colnames(base) <- tolower(colnames(base))

write.csv(base, file = "Data/base_final.csv")
base <- read.csv(file = "Data/base_final.csv", header = T)
base$X<-NULL

base <- base[!is.na(base$ind),] #### quitar na
base <- base[!is.na(base$tipo_vivienda),]
base <- base[!is.na(base$leer_escribir),]

skim(base)

write.csv(base, file = "Data/base_final_na.csv")


base <- read.csv(file = "Data/base_final_na.csv", header = T)
base$X<-NULL

table(base$mujer)

set.seed(666)

# Mujer

mujer <- subset(base, mujer == 1)
mujer$mujer<-NULL

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mujer))

## set the seed to make your partition reproducible
set.seed(10101)

train_ind <- sample(seq_len(nrow(mujer)), size = smp_size)

m_train <- mujer[train_ind, ]
m_test <- mujer[-train_ind, ]

# Hombre

hombre <- subset(base, mujer == 0)
hombre$mujer<-NULL

## 75% of the sample size
smp_size <- floor(0.75 * nrow(hombre))

## set the seed to make your partition reproducible
set.seed(10101)

train_ind <- sample(seq_len(nrow(hombre)), size = smp_size)

h_train <- hombre[train_ind, ]
h_test <- hombre[-train_ind, ]

rm(base, hombre, mujer, smp_size, train_ind, filenames, ldf, a,b,c,d,e,f,g)

## Implementación modelo XGBoost

## Mujer

# XGBoost

m_train_recipe <- recipe(formula= tiempo_labores_no_rem~ . , data=m_train) %>% ## En recip se detallan los pasos que se aplicarán a un conjunto de datos para prepararlo para el análisis de datos.
  update_role(c("vivienda", "hogar", "ind"), new_role = "id")
m_train_recipe

m_test_recipe <- recipe(formula= tiempo_labores_no_rem~ . , data=m_test) %>% ## En recip se detallan los pasos que se aplicarán a un conjunto de datos para prepararlo para el análisis de datos.
  update_role(c("vivienda", "hogar", "ind"), new_role = "id")
m_test_recipe

## set n-folds
db_folds <- vfold_cv(data=m_train, v=10 , strata=NULL)
db_folds

## set metrics
db_metrics <- metric_set(yardstick::rmse, yardstick::rsq, ccc) ## para categoricas, la última es la función de perdida. RMSE para regresion

## Boosted Tree Model Specification
xgb_spec <- boost_tree(trees = 1000,
                       tree_depth = tune(),
                       min_n = tune(),
                       mtry = tune(),
                       sample_size = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression") ## para regresión cambiar classification por regress
xgb_spec

## workflow
xgb_word_wf <- workflow(m_train_recipe, xgb_spec)

## tunne hiperparametros
xgb_grid <- grid_max_entropy(tree_depth(c(5L, 10L)),
                             min_n(c(10L, 40L)),
                             mtry(c(5L, 10L)), 
                             sample_prop(c(0.5, 1.0)), 
                             learn_rate(c(-2, -1)),
                             size = 20)
xgb_grid

## estimate model
tic()
xgb_word_rs <- tune_race_anova(object = xgb_word_wf,
                               resamples = db_folds,
                               grid = xgb_grid,
                               metrics = db_metrics,
                               control = control_race(verbose_elim = T))
toc()

saveRDS(xgb_word_rs, file = "Data/xgb_word_rs_mujer.rds")

##=== **3. Desempeño del modelo** ===##

xgb_word_rs <- import("Data/xgb_word_rs_mujer.rds")

## plot model
plot_race(xgb_word_rs) + labs(title = "Gráfico 1: Grafico de RMSE ",
                              y = "RMSE",
                              x = "Fold")

## best model
show_best(xgb_word_rs)

## xgboost model
xgb_last <- xgb_word_wf %>%
  finalize_workflow(select_best(xgb_word_rs, "rmse")) 

xgb_last

## predecir :)

xgb_word_wf_test <- workflow(m_test_recipe, xgb_spec)

xgb_last_m_test <- xgb_word_wf_test %>%
  finalize_workflow(select_best(xgb_word_rs, "rmse")) 
xgb_last_m_test

predictions_test <- collect_predictions(xgb_last_m_test)
predictions_t <- subset(predictions_test, select = c("price", ".pred"))

pred <- predictions_test[1:5000,]

id <- test$property_id

pred$property_id<-id

pred <- subset(pred, select=c("property_id", ".pred"))

colnames(pred)[2]="price"

write.csv(pred, file ="predictions_ayala_contreras_meneses.csv")





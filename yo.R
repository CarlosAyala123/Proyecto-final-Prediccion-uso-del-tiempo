#### Proyecto Final - machine Learning and Big Data ####

# Cargar las bases

rm(list=ls())

require(pacman)
p_load(skimr, reshape, tidyr)

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

### Juntar las bases


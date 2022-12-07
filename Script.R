#### Proyecto Final - machine Learning and Big Data ####

# Cargar las bases

rm(list=ls())

require(pacman)
p_load(skimr, reshape)

a <- read.csv("Data/ENUT_C1/ENUT_C1.csv", header = T)

colnames(a) <- tolower(colnames(a))

skim(a)

a <- subset(a, select = -c(p4030s1a1, p4030s4a1, dia_ref, f_exp_viv)) ## dejar las variables a usar

names <- c(directorio = "vivienda", p424 = "tipo_vivienda") ## vercotr de nombres

a <- rename(a, names) ## asignar nuevo nombre a las variables


#### Proyecto Final - machine Learning and Big Data ####

# Cargar las bases

require(pacman)
p_load(skimr, reshape)

a <- read.csv("Data/ENUT_C1/ENUT_C1.csv", header = T)

colnames(a) <- tolower(colnames(a))

skim(a)

b <- subset(a, select = -c(p4030s1a1, p4030s4a1, dia_ref, f_exp_viv))

names <- c(tipo_vivienda = "p424")




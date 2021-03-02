# Code 2: Análisis Informe Barreras movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, summarytools, sjPlot)

# 2. Load data  -------------------------------------------
## movid_i-19
## movid_i-19
load(file = "output/data/movid_impact.RData")

## OJO: Corrí toda la sintaxis de 01proc-data.R sobre movid_o para tener una base con todo

data <- movid_i
rm(movid_i, movid_o, movid_if8)
data <- data[data$a5=="Entrevistado(a)",]

## OJO2: Agregue base final (ver si quiere ocupar esta
## En mi sintaxis tengo opcion de recodificar sin tener que hacerlo nuevamente con fct_reverse
movid_i_proc <- readRDS(file = "output/data/movid_i_proc.RDS")
data <- movid_i_proc

# 3. Informe Cuidados  -------------------------------------------

# Lámina 1: ¿Qué es MOVID-19?


# Lámina 2: ¿Quienes somos MOVID-19?


# Lámina 3: MOVID-Impact: Metodología, dimensiones


# Lámina 4: ¿Cuánto nos cuidamos del COVID-19 en Chile?
## 1 Gráfico descriptivo de toque de queda y los 5 cuidados (quizás usar un grid de 3 x 2) 
## f4_toque, f7_wash", "f7_distance", "f7_social", "f7_mask", "f7_mask2"
## Cada gráfico con todas las categorías de respuesta y un NS/NR


# Lámina 5: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque no creemos que el COVID-19 sea un peligro real
## 1 Gráfico descriptivo de f6 con todas las categorías de respuesta y un NS/NR

# Lámina 6: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque no creemos que el COVID-19 sea un peligro real
## 1 Gráfico de probabilidades predichas de cumplir "siempre" con cada medida para peligro = 1 y peligro = 5 - 
## dejar variables de cuidado donde hay diferencias significativas


# Lámina 7: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque nos sentimos invencibles
## 1 Gráfico descriptivo de f5_1 con todas las categorías de respuesta y un NS/NR

# Lámina 8: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque nos sentimos invencibles
## 1 Gráfico de probabilidades predichas de cumplir "siempre" con cada medida para f5_1 = 1 y f5_1 = 5 - 
## dejar variables de cuidado donde hay diferencias significativas


# Lámina 9: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque no pasará nada si no seguimos las reglas
## 1 Gráfico descriptivo de f5_5 con todas las categorías de respuesta y un NS/NR

# Lámina 10: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque no pasará nada si no seguimos las reglas
## 1 Gráfico de probabilidades predichas de cumplir "siempre" con cada medida para f5_5 = 1 y f5_5 = 5 - 
## dejar variables de cuidado donde hay diferencias significativas


# Lámina 11: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque los demás tampoco lo hacen
## 1 Gráfico descriptivo de f8 con todas las categorías de respuesta y un NS/NR

# Lámina 12: ¿Por qué no nos cuidamos del COVID-19 en Chile? | Porque los demás tampoco lo hacen
## 1 Gráfico de probabilidades predichas de cumplir "siempre" con cada medida para f8 = 1 y f8 = 6 - 
## dejar variables de cuidado donde hay diferencias significativas


# Lámina 13: ¿Cómo promovemos el respeto a las medidas de cuidado COVID-19?
## Comunicación de riesgos
## Fiscalización real y comunicada a la ciudadanía
## Comunicar normas sociales (ejemplos de quienes cumplen, no de quienes no cumplen)
## ¿Propuestas más concretas?

# Lámina 14: Chao




# Cumplimiento toque de queda

data$cuidado_toque <- ifelse(data$f4_toque=="Si", 0, 
                             ifelse(data$f4_toque=="No", 1, NA))


# Cuidados 

data$cuidado_manos <- ifelse(data$f7_wash=="Siempre", 1, 
                             ifelse(data$f7_wash=="Casi siempre" | data$f7_wash=="Frecuentemente" | 
                                    data$f7_wash=="A veces" | data$f7_wash=="Casi nunca", 0, NA))

data$cuidado_dist <- ifelse(data$f7_distance=="Siempre", 1, 
                             ifelse(data$f7_distance=="Casi siempre" | data$f7_distance=="Frecuentemente" | 
                                      data$f7_distance=="A veces" | data$f7_distance=="Casi nunca", 0, NA))

data$cuidado_social <- ifelse(data$f7_social=="Siempre", 1, 
                             ifelse(data$f7_social=="Casi siempre" | data$f7_social=="Frecuentemente" | 
                                      data$f7_social=="A veces" | data$f7_social=="Casi nunca", 0, NA))

data$cuidado_masc <- ifelse(data$f7_mask=="Siempre", 1, 
                             ifelse(data$f7_mask=="Casi siempre" | data$f7_mask=="Frecuentemente" | 
                                      data$f7_mask=="A veces" | data$f7_mask=="Casi nunca", 0, NA))

data$cuidado_masc2 <- ifelse(data$f7_mask2=="Siempre", 1, 
                             ifelse(data$f7_mask2=="Casi siempre" | data$f7_mask2=="Frecuentemente" | 
                                      data$f7_mask2=="A veces" | data$f7_mask2=="Casi nunca", 0, NA))

# Barreras al cuidado
data$barrera_obedecer <- ifelse(data$f3_3==1, 5,
                                ifelse(data$f3_3==2, 4,
                                  ifelse(data$f3_3==3, 3,
                                         ifelse(data$f3_3==4, 2,
                                                ifelse(data$f3_3==5, 1, NA)))))

data$barrera_gob <- ifelse(data$f3_4==1, 5,
                           ifelse(data$f3_4==2, 4,
                                  ifelse(data$f3_4==3, 3,
                                         ifelse(data$f3_4==4, 2,
                                                ifelse(data$f3_4==5, 1, NA)))))

data$barrera_protect <- ifelse(data$f5_1==1, 5,
                                  ifelse(data$f5_1==2, 4,
                                         ifelse(data$f5_1==3, 3,
                                                ifelse(data$f5_1==4, 2,
                                                       ifelse(data$f5_1==5, 1, NA)))))


data$barrera_info <- ifelse(data$f5_2==1, 5,
                            ifelse(data$f5_2==2, 4,
                                   ifelse(data$f5_2==3, 3,
                                          ifelse(data$f5_2==4, 2,
                                                 ifelse(data$f5_2==5, 1, NA)))))


data$barrera_desmot <- ifelse(data$f5_3==1, 5,
                              ifelse(data$f5_3==2, 4,
                                     ifelse(data$f5_3==3, 3,
                                            ifelse(data$f5_3==4, 2,
                                                   ifelse(data$f5_3==5, 1, NA)))))


data$barrera_desmot2 <- ifelse(data$f5_4==1, 5,
                               ifelse(data$f5_4==2, 4,
                                      ifelse(data$f5_4==3, 3,
                                             ifelse(data$f5_4==4, 2,
                                                    ifelse(data$f5_4==5, 1, NA)))))


data$barrera_fisc <- ifelse(data$f5_5==1, 0,
                            ifelse(data$f5_5==2, 0,
                                   ifelse(data$f5_5==3, 0,
                                          ifelse(data$f5_5==4, 1,
                                                 ifelse(data$f5_5==5, 1, NA)))))

data$barrera_riesgo <- ifelse(data$f6=="Nada peligroso", 1,
                              ifelse(data$f6=="Algo peligroso", 2,
                                     ifelse(data$f6=="Bastante peligroso", 3,
                                            ifelse(data$f6=="Muy peligroso", 4,
                                                   ifelse(data$f6=="Extremadamente peligroso", 5, NA)))))

data$barrera_normas <- ifelse(data$f8==1, 5,
                             ifelse(data$f8==2, 4,
                                    ifelse(data$f8==3, 3,
                                           ifelse(data$f8==4, 2,
                                                  ifelse(data$f8==5, 1, NA)))))


# Modelos para predecir cuidados

lm1 <- glm(cuidado_toque ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
lm2 <- glm(cuidado_manos ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
lm3 <- glm(cuidado_dist ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
lm4 <- glm(cuidado_social ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
lm5 <- glm(cuidado_masc ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
lm6 <- glm(cuidado_masc2 ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")

tab_model(lm1, lm2, lm3, lm4, lm5, lm6)



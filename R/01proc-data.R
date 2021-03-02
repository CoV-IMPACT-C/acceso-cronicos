# Code 1: Proc movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_o <- haven::read_dta("input/data/20210125_base_movid_version_codificada.dta")
movid_i <- movid_o
x <- haven::read_dta("input/data/20210125_base_movid_version_codificada.dta")

# 3. Recodes -----------------------------------------------------
# Modulo A ----------------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("a"), sexo, edad), funs(as.numeric(.)))

# A_1 Identificacion ----------------------------------------------------------
movid_i$entrevistado <- ifelse(is.na(movid_i$entrevistado), 0, 1)

# A_2 Sexo -----------------------------------------------------------------
movid_i$sexo <- car::recode(movid_i$sexo, c("1='Hombre';2='Mujer'"), as.factor = T,
                            levels = c("Hombre", "Mujer"))
# A_3 Age -----------------------------------------------------------------
## Edad (numerica)
## Edad en tres categorías
movid_i$edad_3cat <- car::recode(movid_i$edad, c("0:39='18 a 39';40:64='40 a 64';65:hi='65 y más'"), as.factor = T,
                            levels = c("18 a 39", "40 a 64", "65 y más"))

table(movid_i$edad_3cat)

## Edad en cuatro categorías
movid_i$edad_cat <- car::recode(movid_i$edad, c("0:29='18 a 29';30:39='30 a 39';40:49='40 a 49';50:59='50 a 59';60:69='60 a 69';70:79='70 a 79';80:hi='80+'"), as.factor = T,
                                 levels = c("18 a 29", "30 a 39","40 a 49","50 a 59","60 a 69","70 a 79", "80+"))

table(movid_i$edad_cat)

## Edad adultos mayores
movid_i$edad_65 <- car::recode(movid_i$edad, c("0:64='No Adulto Mayor';65:hi='Adulto Mayor'"), as.factor = T,
                                levels = c("Adulto Mayor","No Adulto Mayor"))

table(movid_i$edad_65)

# A4 Relacion Jefe Hogar -------------------------------------------------
table(movid_i$a4)
movid_i$a4 <- car::recode(movid_i$a4, c("1='Jefe de Hogar';2='Cónyuge o pareja';3='Hijo(a)';4='Padre o madre';5='Suegro(a)';6='Yerno o nuera';7='Nieto(a)';8='Hermano(a)';9='Cuñado(a)';10='Otro Familiar';11='Otro no familiar';12='S. Doméstico P. Adentro'"), as.factor = T,
                                levels = c('Jefe de Hogar', 'Cónyuge o pareja','Hijo(a)','Padre o madre','Suegro(a)','Yerno o nuera', 'Nieto(a)', 'Hermano(a)', 'Cuñado(a)', 'Otro Familiar', 'Otro no familiar','S. Doméstico P. Adentro'))

table(movid_i$a4)

# A5 Relacion con entrevistado -------------------------------------------
table(movid_i$a5)
movid_i$a5 <- car::recode(movid_i$a5, c("1='Entrevistado(a)';2='Cónyuge o pareja';3='Hijo(a)';4='Padre o madre';5='Suegro(a)';6='Yerno o nuera';7='Nieto(a)';8='Hermano(a)';9='Cuñado(a)';10='Otro Familiar';11='Otro no familiar';12='S. Doméstico P. Adentro'"), as.factor = T,
                          levels = c('Entrevistado(a)', 'Cónyuge o pareja','Hijo(a)','Padre o madre','Suegro(a)','Yerno o nuera', 'Nieto(a)', 'Hermano(a)', 'Cuñado(a)', 'Otro Familiar', 'Otro no familiar','S. Doméstico P. Adentro'))

table(movid_i$a5)

# A6 Estado civil -------------------------------------------
table(movid_i$a6)
movid_i$a6 <- car::recode(movid_i$a6, c("1='Casado(a)';2='Conviviente o pareja';3='Anulado(a)';4='Separado(a)';5='Divorciado(a)';6='Viudo(a)';7='Soltero(a)'"), as.factor = T,
                          levels = c('Casado(a)', 'Conviviente o pareja','Anulado(a)','Separado(a)','Divorciado(a)','Viudo(a)', 'Soltero(a)'))

table(movid_i$a6)

# A8 Education ---------------------------------------------------------------
# A7 Asistir a educacion -------------------------------------------------------
table(movid_i$a7)
movid_i$a7 <- car::recode(movid_i$a7, c("1='Sí';2='No'"), as.factor = T,
                               levels = c("Sí","No"))

table(movid_i$a7)

# A8A Nivel mas alto alcanzado -------------------------------------------

# Nivel Educacion 3 categorias --------------------------------------------------
# 3 categories: High school or less, Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_3cat <- car::recode(movid_i$a8a, c("c(1,2,3,4,5,6)='Media o menos';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                                 levels = c("Media o menos", 'Técnica', "Profesional"))

table(movid_i$educ_3cat)

# Nivel Educacion 4 categorias --------------------------------------------------
# 4 categories: Primary or less, High school,Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_4cat <- car::recode(movid_i$a8a, c("c(1,2,3,4)='Básica o menos';c(5,6)='Media';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                                 levels = c("Básica o menos","Media", 'Técnica', "Profesional"))

table(movid_i$educ_4cat)


# Nivel educacional 9 cat -------------------------------------------------------
table(movid_i$a8a)
movid_i$a8a <- car::recode(movid_i$a8a, c("1='Nunca asistió';2='Preescolar';3='Especial (Diferencial)';4='Básica';5='Media Científico-Humanista';6='Media Técnica-Profesional';7='Superior Técnica (en CFT o I. Profesional)';8='Superior Universitaria (Pregrado)';9='Postgrado';99=NA"), as.factor = T,
                          levels = c('Nunca asistió', 'Preescolar','Especial (Diferencial)','Básica','Media Científico-Humanista','Media Técnica-Profesional', 'Superior Técnica (en CFT o I. Profesional)', 'Superior Universitaria (Pregrado)','Postgrado'))

table(movid_i$a8a)

# A8b Nivel mas alto alcanzado Ano -------------------------------------------
table(movid_i$a8b)
movid_i$a8a[movid_i$a8a == 99] <- NA
table(movid_i$a8a)


# A9 Duracion pregrado -------------------------------------------
table(movid_i$a9)
movid_i$a9[movid_i$a9 == 99] <- NA
table(movid_i$a9)


# Modulo B. Otras variables demograficas ----------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("b")), funs(as.numeric(.)))

# B1 Pueblo originario -------------------------------------------------------
table(movid_i$b1)
movid_i$b1 <- car::recode(movid_i$b1, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$b1)


# B2 Prevision salud  --------------------------------------------------------

# Prevision general (b2) --------------------------------------------------
table(movid_i$b2)
movid_i$b2 <- car::recode(movid_i$b2, c("1='FONASA';2='ISAPRE';3='Fuerzas Armadas y de Orden';4='Otro';5='Ninguna';c(8,9)=NA"), as.factor = T,
                                     levels = c("FONASA", "ISAPRE", 'Fuerzas Armadas y de Orden', 'Otro', 'Ninguna' ))

table(movid_i$b2)

# Prevision 2 categorias --------------------------------------------------
movid_i$prev_2categ <- car::recode(movid_i$b2, c("1='FONASA';2='ISAPRE';4='Otro';hi=NA"), as.factor = T,
                                   levels = c("FONASA", "ISAPRE", 'Otro'))

table(movid_i$prev_2categ)

# Prevision 4 categorias --------------------------------------------------
movid_i$prev_4categ <- car::recode(movid_i$b2, c("1='FONASA';2='ISAPRE';4='Otro';5='Ninguna';hi=NA"), as.factor = T,
                                   levels = c("FONASA", "ISAPRE", 'Otro', 'Ninguna'))

table(movid_i$prev_4categ)

# B3 Establecimiento última atencion --------------------------------------
table(movid_i$b3)
movid_i <- movid_i %>% mutate(b3_tipo = case_when(b3 %in% c(1:7,15) ~ "Público",
                                                  b3 %in% c(8,9,10,12,16) ~ "Privada",
                                                  b3 %in% c(11,13,14,17) ~ "Otro",
                                                  TRUE ~ NA_character_))

movid_i$b3 <- car::recode(movid_i$b3, c("1='Consultorio general';
                                        2='Posta rural';
                                        3='CRS o CDT';
                                        4='COSAM';
                                        5='SAPU';
                                        6='Posta';
                                        7 ='Hospital público o del SNSS';
                                        8='Consulta o centro médico privado';
                                        9='Clínica u hospital privado';
                                        10='Centro de salud mental privado';
                                        11= 'Establecimiento de las FF.AA. o del Orden';
                                        12='Servicio de urgencia de clínica u hospital privado';
                                        13='Mutual de Seguridad';
                                        14='Servicio médico de alumnos del lugar en que estudia';
                                        15='Telemedicina por consultorio u hospital público';
                                        16='Telemedicina por centro médico o clinica privada';
                                        17 ='Otro';
                                        c(98,99)=NA"), as.factor = T,
                          levels = c('Consultorio general',
                                     'Posta rural',
                                     'CRS o CDT',
                                     'COSAM',
                                     'SAPU',
                                     'Posta',
                                     'Hospital público o del SNSS',
                                     'Consulta o centro médico privado',
                                     'Clínica u hospital privado',
                                     'Centro de salud mental privado',
                                     'Servicio de urgencia de clínica u hospital privado',
                                     'Mutual de Seguridad',
                                     'Servicio médico de alumnos del lugar en que estudia',
                                     'Telemedicina por consultorio u hospital público',
                                     'Telemedicina por centro médico o clinica privada',
                                     'Otro'))

table(movid_i$b3)
table(movid_i$b3_tipo)

# Modulo C. Preexistencias ------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("c"), -c1_6_esp), funs(as.numeric(.)))

# C1_* Enfermedades cronicas ----------------------------------------------
# C1_1 Diabetes -----------------------------------------------------------
movid_i$c1_1 <- ifelse(is.na(movid_i$c1_1), 0, 1)
# C1_2 Hipertension -----------------------------------------------------------
movid_i$c1_2 <- ifelse(is.na(movid_i$c1_2), 0, 1)
# C1_3 Cardiovascular -----------------------------------------------------------
movid_i$c1_3 <- ifelse(is.na(movid_i$c1_3), 0, 1)
# C1_4 Respiratoria -----------------------------------------------------------
movid_i$c1_4 <- ifelse(is.na(movid_i$c1_4), 0, 1)
# C1_5 Mental -----------------------------------------------------------
movid_i$c1_5 <- ifelse(is.na(movid_i$c1_5), 0, 1)
# C1_6 Otra -----------------------------------------------------------
movid_i$c1_6 <- ifelse(is.na(movid_i$c1_6), 0, 1)
# C1_7 Sano -----------------------------------------------------------
movid_i$c1_7 <- ifelse(is.na(movid_i$c1_7), 0, 1)
# C1_9 NA -----------------------------------------------------------
movid_i$c1_8 <- ifelse(is.na(movid_i$c1_8), 0, 1)
movid_i$c1_9 <- ifelse(is.na(movid_i$c1_9), 0, 1)


# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies
## Health risk General
table(movid_i$c1_9)
movid_i <- movid_i %>% mutate(cronicos = case_when(c1_1 == 1 ~ 1,
                                                   c1_2 == 1 ~ 1,
                                                   c1_3 == 1 ~ 1,
                                                   c1_4 == 1 ~ 1,
                                                   c1_5 == 1 ~ 1,
                                                   c1_6_esp %in% c("artritis", "artritis reumatoide, fibromialgia") ~ 1,
                                                   c1_7 == 1 ~ 0,
                                                   c1_8 == 1 ~ NA_real_,
                                                   c1_9 == 1 ~ NA_real_)) %>%
  mutate(cronicos = if_else(cronicos == 1, "Sí", "No"))

table(movid_i$cronicos) ## Artritis

movid_i$cronicos <- ifelse(movid_i$c1_1==0 &
                             movid_i$c1_2==0 &
                             movid_i$c1_3==0 &
                             movid_i$c1_4==0 &
                             movid_i$c1_5==0 &
                             movid_i$c1_6_esp!= "artritis reumatoide, fibromialgia" &
                             movid_i$c1_6_esp!="artritis", "No",
                           movid_i$cronicos)

movid_i$cronicos <- ifelse(is.na(movid_i$cronicos) &
                             is.na(movid_i$c1_6_esp) &
                             movid_i$c1_6==1, "No",
                           movid_i$cronicos)



# C2 Sintomas Salud Mental ----------------------------------------------------------------

# C2_1 Nervioso -----------------------------------------------------------
table(movid_i$c2_1)
movid_i$c2_1 <- car::recode(movid_i$c2_1, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                                   levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))

table(movid_i$c2_1)
# C2_2 Angustia -----------------------------------------------------------
table(movid_i$c2_2)
movid_i$c2_2 <- car::recode(movid_i$c2_2, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                            levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))
table(movid_i$c2_2)

# C2_3 Deprimido -----------------------------------------------------------
table(movid_i$c2_3)
movid_i$c2_3 <- car::recode(movid_i$c2_3, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                            levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))
table(movid_i$c2_3)

# C2_4 Desmotivacion -----------------------------------------------------------
table(movid_i$c2_4)
movid_i$c2_4 <- car::recode(movid_i$c2_4, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                            levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))
table(movid_i$c2_4)


# Modulo D Acceso a Salud COVID-19 ----------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("d"), -d5_esp), funs(as.numeric(.)))
# D1 Contacto  -----------------------------------------------------------
table(movid_i$d1)
movid_i$d1 <- car::recode(movid_i$d1, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$d1)

# D2 Contacto2 ----------------------------------------------------------
table(movid_i$d2)
movid_i$d2 <- car::recode(movid_i$d2, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$d2)
# D3_* Sintomas -----------------------------------------------------------
# d3_snt_disnea -----------------------------------------------------------
table(movid_i$d3_1)
movid_i$d3_snt_disnea <- car::recode(movid_i$d3_1, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$d3_snt_disnea)

# d3_snt_tos -----------------------------------------------------------
table(movid_i$d3_2)
movid_i$d3_snt_tos <- car::recode(movid_i$d3_2, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_tos)
# d3_snt_fiebre -----------------------------------------------------------
table(movid_i$d3_3)
movid_i$d3_snt_fiebre <- car::recode(movid_i$d3_3, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_fiebre)

# d3_snt_mialgias -----------------------------------------------------------
table(movid_i$d3_4)
movid_i$d3_snt_mialgias <- car::recode(movid_i$d3_4, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_mialgias)
# d3_snt_odinofagia -----------------------------------------------------------
table(movid_i$d3_5)
movid_i$d3_snt_odinofagia <- car::recode(movid_i$d3_5, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_odinofagia)

# d3_snt_anosmia -----------------------------------------------------------
table(movid_i$d3_6)
movid_i$d3_snt_anosmia <- car::recode(movid_i$d3_6, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_anosmia)
# d3_snt_dol_torax -----------------------------------------------------------
table(movid_i$d3_7)
movid_i$d3_snt_dol_torax <- car::recode(movid_i$d3_7, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_dol_torax)
# d3_snt_cefalea -----------------------------------------------------------
table(movid_i$d3_8)
movid_i$d3_snt_cefalea <- car::recode(movid_i$d3_8, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_cefalea)

# d3_snt_diarrea -----------------------------------------------------------
table(movid_i$d3_9)
movid_i$d3_snt_diarrea <- car::recode(movid_i$d3_9, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_diarrea)

# d3_snt_disgeusia -----------------------------------------------------------
table(movid_i$d3_10)
movid_i$d3_snt_disgeusia <- car::recode(movid_i$d3_10, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                     levels = c("Sí","No"))
table(movid_i$d3_snt_disgeusia)
# d3_snt_null -----------------------------------------------------------
movid_i$d3_snt_null <- ifelse(movid_i$d3_1 == 2&movid_i$d3_2 == 2&movid_i$d3_3 == 2&movid_i$d3_4 == 2&movid_i$d3_5 == 2&
                              movid_i$d3_6 == 2& movid_i$d3_7 == 2&movid_i$d3_8 == 2&movid_i$d3_9 == 2& movid_i$d3_10 == 2,"Sí", "No")
# D4 Consulta -------------------------------------------------------------
table(movid_i$d4)
movid_i$d4 <- car::recode(movid_i$d4, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$d4)

# D5 No consulta ----------------------------------------------------------
table(movid_i$d5)
movid_i$d5 <- car::recode(movid_i$d5, c("1='Porque no le pareció importante';
                                        2='Por el costo económico';
                                        3='Porque no sabía dónde realizarlo';
                                        4='Porque tenía que esperar mucho tiempo';
                                        5='Porque está evaluando si los síntomas empeoran antes de consultar';
                                        6='Por miedo a contagiarse';
                                        7='Porque los síntomas son leves o habituales';
                                        8='Porque el sistema de salud está muy lleno';
                                        9='Otro. Especifique';
                                        c(88,99)=NA"), as.factor = T,
                            levels = c('Porque no le pareció importante',
                                       'Por el costo económico',
                                       'Porque no sabía dónde realizarlo',
                                       'Porque tenía que esperar mucho tiempo',
                                       'Porque está evaluando si los síntomas empeoran antes de consultar',
                                       'Por miedo a contagiarse',
                                       'Porque los síntomas son leves o habituales',
                                       'Porque el sistema de salud está muy lleno',
                                       'Otro. Especifique'))
table(movid_i$d5)

# D6 Diagnostico ----------------------------------------------------------
table(movid_i$d6)
movid_i$d6 <- car::recode(movid_i$d6, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$d6)
# D7 Licencia ----------------------------------------------------------------------
table(movid_i$d7)
movid_i$d7 <- car::recode(movid_i$d7, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$d7)

# D8 Hospitalizado ----------------------------------------------------------------------
table(movid_i$d8)
movid_i$d8 <- car::recode(movid_i$d8, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$d8)

# D9 Long COVID -----------------------------------------------------------
table(movid_i$d9)
movid_i$d9 <- car::recode(movid_i$d9, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$d9)

# Modulo E Acceso a Salud NO COVID-19 -------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("e"), -ends_with("_esp")), funs(as.numeric(.)))

# E1 Posponer Cronicos ----------------------------------------------------
movid_i$posponer <- ifelse(movid_i$e1_1 == 1|
                           movid_i$e1_2 == 1|
                           movid_i$e1_3 == 1|
                           movid_i$e1_4 == 1|
                           movid_i$e1_5 == 1|
                           movid_i$e1_6 == 1, 1,0)

movid_i$posponer <- ifelse(movid_i$posponer == 1 &
                           movid_i$cronicos == 'Sí',1,0)

movid_i$posponer <- car::recode(movid_i$posponer, c("1='Sí';0='No'", as.factor= T))
table(movid_i$posponer)

# E1_1 Pospone Diabetico -------------------------------------------------
table(movid_i$e1_1)
movid_i$e1_1 <- car::recode(movid_i$e1_1, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$e1_1)

# E1_2 Pospone Hipertenso -------------------------------------------------
table(movid_i$e1_2)
movid_i$e1_2 <- car::recode(movid_i$e1_2, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Sí","No"))
table(movid_i$e1_2)
# E1_3 Pospone Cardiovascular -------------------------------------------------
table(movid_i$e1_3)
movid_i$e1_3 <- car::recode(movid_i$e1_3, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Sí","No"))
table(movid_i$e1_3)
# E1_4 Pospone Respiratorio -------------------------------------------------
table(movid_i$e1_4)
movid_i$e1_4 <- car::recode(movid_i$e1_4, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Sí","No"))
table(movid_i$e1_4)
# E1_5 Pospone Mental -------------------------------------------------
table(movid_i$e1_5)
movid_i$e1_5 <- car::recode(movid_i$e1_5, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Sí","No"))
table(movid_i$e1_5)
# E1_6 Pospone Otra -------------------------------------------------
table(movid_i$e1_6)
movid_i$e1_6 <- car::recode(movid_i$e1_6, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Sí","No"))
table(movid_i$e1_6)

# E2 Razon principal no consulta ------------------------------------------

# E2_* Razon principal para cada condition  -------------------------------------------------
table(movid_i$e2_3)
movid_i <- movid_i %>%  mutate_at(c("e2_1", "e2_2","e2_3", "e2_4", "e2_5", "e2_6"), funs(car::recode(.,
                            c("1='Porque no le pareció importante controlarse';
                                        2='Por el costo económico';
                                        3='Porque no sabía dónde realizarlo';
                                        4='Porque tenía que esperar mucho tiempo';
                                        5='Por miedo a contagiarse de COVID-19';
                                        6='Porque el sistema de salud está muy lleno';
                                        7='Porque los síntomas son leves o habituales';
                                        8='No contó con transporte para trasladarse al lugar de atención';
                                        9='No tuvo con quien dejar a pesonas de su cuidado';
                                        10='Otro.Especifique';
                                        c(88,99)=NA"), as.factor = T,
                            levels = c('Porque no le pareció importante controlarse',
                                       'Por el costo económico',
                                       'Porque no sabía dónde realizarlo',
                                       'Porque tenía que esperar mucho tiempo',
                                       'Por miedo a contagiarse de COVID-19',
                                       'Porque el sistema de salud está muy lleno',
                                       'No contó con transporte para trasladarse al lugar de atención',
                                       'No tuvo con quien dejar a pesonas de su cuidado',
                                       'Otro.Especifique'))))
table(movid_i$e2_1)
table(movid_i$e2_6)
table(movid_i$e2_1_esp)
###  Despues hacer una que sume un acumulado por cronicos -----


# E3 Acceso a salud no relacionada a COVID  ----------------------------------------------------------------------
table(movid_i$e3)
movid_i$e3 <- car::recode(movid_i$e3, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Sí","No"))
table(movid_i$e3)

# E4 Consulta a salud no relacionada a COVID  ----------------------------------------------------------------------
table(movid_i$e4)
movid_i$e4 <- car::recode(movid_i$e4, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$e4)

# E5_* Atencion pospuesta -------------------------------------------------
# E5_1 e5_posp_consulta -------------------------------------------------
table(movid_i$e5_1)
movid_i$e5_posp_consulta <- car::recode(movid_i$e5_1, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$e5_posp_consulta)

# E5_2 e5_posp_dental -------------------------------------------------
table(movid_i$e5_2)
movid_i$e5_posp_dental <- car::recode(movid_i$e5_2, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_dental)

# E5_3 e5_posp_vacuna -------------------------------------------------
table(movid_i$e5_3)
movid_i$e5_posp_vacuna <- car::recode(movid_i$e5_3, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_vacuna)

# E5_4 e5_posp_examen -------------------------------------------------
table(movid_i$e5_4)
movid_i$e5_posp_examen <- car::recode(movid_i$e5_4, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_vacuna)

# E5_5 e5_posp_examenmuj -------------------------------------------------
table(movid_i$e5_5)
movid_i$e5_posp_examenmuj <- car::recode(movid_i$e5_5, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_examenmuj)

# E5_6 e5_posp_examenhom -------------------------------------------------
table(movid_i$e5_6)
movid_i$e5_posp_examenhom <- car::recode(movid_i$e5_6, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_examenhom)

# E5_7 e5_posp_insumos -------------------------------------------------
table(movid_i$e5_7)
movid_i$e5_posp_insumos <- car::recode(movid_i$e5_7, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_insumos)

# E5_8 e5_posp_cirugia -------------------------------------------------
table(movid_i$e5_8)
movid_i$e5_posp_cirugia <- car::recode(movid_i$e5_8, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_cirugia)

# E5_9 e5_posp_otra -------------------------------------------------
table(movid_i$e5_9)
movid_i$e5_posp_otra <- car::recode(movid_i$e5_9, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                                        levels = c("Sí","No"))
table(movid_i$e5_posp_otra)

# e5_posp_null ---------------------------------------------------------
movid_i$e5_posp_null <- ifelse(movid_i$e5_1 == 2&movid_i$e5_2 == 2&movid_i$e5_3 == 2&movid_i$e5_4 == 2&movid_i$e5_5 == 2&
                                movid_i$e5_6 == 2& movid_i$e5_7 == 2&movid_i$e5_8 == 2&movid_i$e5_9 == 2,"Sí", "No")


# E6 Razon para posponer -----------------------------------------
table(movid_i$e6)
movid_i$e6 <- car::recode(movid_i$e6,
                           c("1='Porque no le pareció importante';
                                        2='Por el costo económico';
                                        3='Porque no sabía dónde realizarlas';
                                        4='Porque tenía que esperar mucho tiempo';
                                        5='Porque está evaluando si los síntomas empeoran antes de consultar';
                                        6='Por miedo a contagiarse';
                                        7='Porque los síntomas son leves o habituales';
                                        8='Porque el sistema de salud está muy lleno';
                                        9='No contó con transporte para trasladarse al lugar de atención';
                                        10='No tuvo con quien dejar a pesonas de su cuidado';
                                        11='El consultorio, hospital o clínica le suspendió la hora y no le ha reagendado';
                                        12='Otra';
                                        c(88,99)=NA"), as.factor = T,
                           levels = c('Porque no le pareció importante',
                                      'Por el costo económico',
                                      'Porque no sabía dónde realizarlas',
                                      'Porque tenía que esperar mucho tiempo',
                                      'Porque está evaluando si los síntomas empeoran antes de consultar',
                                      'Por miedo a contagiarse',
                                      'Porque los síntomas son leves o habituales',
                                      'Porque el sistema de salud está muy lleno',
                                      'No contó con transporte para trasladarse al lugar de atención',
                                      'No tuvo con quien dejar a pesonas de su cuidado',
                                      'El consultorio, hospital o clínica le suspendió la hora y no le ha reagendado',
                                      'Otra'))

table(movid_i$e6)

# E7 Salud Autopercibida --------------------------------------------------
movid_i$e7 <- ifelse(movid_i$e7 %in% c(8,9), NA, movid_i$e7)
table(movid_i$e7)

# E8 Salud Autopercibida (Cambio) -----------------------------------------
table(movid_i$e8)
movid_i$e8 <- car::recode(movid_i$e8, c("1='Ha mejorado';2='Se ha mantenido igual';3='Ha empeorado'; c(8,9)=NA"), as.factor = T,
                                    levels = c('Ha mejorado','Se ha mantenido igual','Ha empeorado'))
table(movid_i$e8)

# E9 Acceso anticonceptivos -----------------------------------------
table(movid_i$e9)
movid_i$e9 <- car::recode(movid_i$e9, c("1='Sí';2='No';3='No utilizó métodos anticonceptivos'; c(8,9)=NA"), as.factor = T,
                          levels = c('Sí','No','No utilizó métodos anticonceptivos'))
table(movid_i$e9)

# E9 Dificultad Acceso anticonceptivos -----------------------------------------
table(movid_i$e10)
movid_i$e10 <- car::recode(movid_i$e10, c("1='No me han dado hora para acceder a la receta en mi centro de salud';
                                          2='No me han entregado los anticonceptivos en mi centro de salud';
                                          3='He optado por no salir para evitar contagios';
                                          4='No he encontrado anticonceptivos disponibles en la farmacia';
                                          5='No he tenido dinero suficiente';
                                          6='Otra razón';
                                          c(8,9)=NA"), as.factor = T,
                          levels = c('No me han dado hora para acceder a la receta en mi centro de salud',
                                     'No me han entregado los anticonceptivos en mi centro de salud',
                                     'He optado por no salir para evitar contagios',
                                     'No he encontrado anticonceptivos disponibles en la farmacia',
                                     'No he tenido dinero suficiente',
                                     'Otra razón'))
table(movid_i$e10)

# Modulo F. Acceso social a la pandemia ----------------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("f"), -ends_with("_esp")), funs(as.numeric(.)))

# F1. Reproduccion  ------------------------------------------------------------

# F1 Trabajo domestico ----------------------------------------------------
##Nivel
table(movid_i$f1)
movid_i$f1 <- car::recode(movid_i$f1, c("1='Ha bajado';2='Se ha mantenido igual';3='Ha subido';c(8,9)=NA"), as.factor = T,
                           levels = c("Ha bajado", "Se ha mantenido igual", "Ha subido"))
table(movid_i$f1)

## Horas
summary(movid_i$f1_horas)
movid_i$f1_horas <- as.numeric(movid_i$f1_horas)
# F2 Trabajo cuidados -----------------------------------------------------
## Nivel
table(movid_i$f2)
movid_i$f2 <- car::recode(movid_i$f2, c("1='Ha bajado';2='Se ha mantenido igual';3='Ha subido';c(8,9)=NA"), as.factor = T,
                           levels = c("Ha bajado", "Se ha mantenido igual", "Ha subido"))
table(movid_i$f2)

##Horas
summary(movid_i$f2_horas)
movid_i$f2_horas <- as.numeric(movid_i$f2_horas)

# F3. Percepciones y legitimidad  -----------------------------------------------
# F3_1 Percepcion desigualdad f3_desigualdad  ---------------------------------------------
table(movid_i$f3_1)
movid_i$f3_desigualdad <- car::recode(movid_i$f3_1, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                    levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))
table(movid_i$f3_desigualdad)

# F3_2 Intereses del gobierno f3_bienestar  ---------------------------------------------
table(movid_i$f3_2)
movid_i$f3_bienestar <- car::recode(movid_i$f3_2, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                      levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))
table(movid_i$f3_bienestar)
# F3_3 Legitimidad f3_obedecer ----------------------------------------------------
###Aunque a veces no estemos de acuerdo con las autoridades sanitarias y las medidas que se proponen, es nuestro deber seguir sus indicaciones al pie de la letra.
table(movid_i$f3_3)
movid_i$f3_obedecer <- car::recode(movid_i$f3_3, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                     levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))
table(movid_i$f3_obedecer)

# F3_4 Medidas gobierno f3_gob ---------------------------------------------------
table(movid_i$f3_4)
movid_i$f3_gob <- car::recode(movid_i$f3_4, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))
table(movid_i$f3_gob)
# F3_5 Castigo f3_castigo ---------------------------------------------------
table(movid_i$f3_5)
movid_i$f3_castigo <- car::recode(movid_i$f3_5, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                              levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))
table(movid_i$f3_castigo)

# F4. Accion colectiva y comunitaria ---------------------------------------------------
# F4_1. Protesta --------------------------------------------------
table(movid_i$f4_1)
movid_i$f4_protesta <- car::recode(movid_i$f4_1, c("1='Si';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Si", "No"))
table(movid_i$f4_protesta)
# F4_2. Ollas --------------------------------------------------
table(movid_i$f4_2)
movid_i$f4_ollas <- car::recode(movid_i$f4_2, c("1='Si';2='No'; c(8,9)=NA"), as.factor = T,
                                   levels = c("Si", "No"))
table(movid_i$f4_ollas)

# F4_3. Ayuda comunitaria  --------------------------------------------------
table(movid_i$f4_4)
movid_i$f4_comunitario <- car::recode(movid_i$f4_4, c("1='Si';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Si", "No"))
table(movid_i$f4_comunitario)

# F4_4. Toque queda --------------------------------------------------
table(movid_i$f4_4)
movid_i$f4_toque <- car::recode(movid_i$f4_4, c("1='Si';2='No'; c(8,9)=NA"), as.factor = T,
                                   levels = c("Si", "No"))
table(movid_i$f4_toque)

# F5. Fatiga pandemica -------------------------------------------------------------
# F5_1. Poder protegerse  (f5_protect) --------------------------------------------
### Puedo protegerme completamente del coronavirus si tomo las medidas de protección adecuadas.
table(movid_i$f5_1)
movid_i$f5_protect <- car::recode(movid_i$f5_1, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_protect)
# F5_2. Inform  (f5_inform) --------------------------------------------
### Me informo constantemente sobre los avances del coronavirus y sus efectos en Chile
table(movid_i$f5_2)
movid_i$f5_inform <- car::recode(movid_i$f5_2, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_inform)

# F5_3. Care motivation  (f5_motivacion) --------------------------------------------
### A medida que ha avanzado la crisis sanitaria, me siento cada vez más desmotivado para seguir las medidas de protección recomendadas (f5_3)
table(movid_i$f5_3)
movid_i$f5_motivacion <- car::recode(movid_i$f5_3, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_motivacion)

# F5_4. Dificult protect  (f5_protect) --------------------------------------------
### Resulta cada vez más difícil seguir las medidas de protección sin que esto afecte de manera negativa mi vida (f5_4)
table(movid_i$f5_4)
movid_i$f5_difprotect <- car::recode(movid_i$f5_4, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_difprotect)

# F5_5. Legal enforcement  (f5_legal) --------------------------------------------
### En Chile, si una persona sale sin permiso durante una cuarentena es muy poco probable que sea controlado y multado.
table(movid_i$f5_5)
movid_i$f5_legal <- car::recode(movid_i$f5_5, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                   levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_legal)

# F5_6. Willigness to protest  (f5_protesta) --------------------------------------------
### Incluso considerando la situación sanitaria, estoy dispuesto/a a participar en manifestaciones masivas en este momento.
movid_i$f5_protesta <- car::recode(movid_i$f5_6, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_protesta)


# F6. Riesgo percibido (f6) ----------------------------------------------------
### ¿Qué tan peligroso cree que es el coronavirus para usted y sus cercanos?
table(movid_i$f6)
movid_i$f6 <- car::recode(movid_i$f6, c("1='Nada peligroso';2='Algo peligroso';3='Bastante peligroso';4='Muy peligroso';5='Extremadamente peligroso';c(8,9)=NA"), as.factor = T,
                                 levels = c("Nada peligroso", "Algo peligroso", "Bastante peligroso","Muy peligroso", "Extremadamente peligroso"))

table(movid_i$f6)

## Niveles (bajo_riesgo y alto_riesgo) -------------------------------------------------------------

movid_i$alto_riesgo <- ifelse(movid_i$f6 == "Bastante peligroso" | movid_i$f6 == "Muy peligroso"| movid_i$f6 == "Extremadamente peligroso",1,0)
movid_i$bajo_riesgo <-  ifelse(movid_i$f6 == "Nada peligroso"     | movid_i$f6 == "Algo peligroso",1,0)

# F7.  Cumplimiento de medidas (f7_*)----------------------------------------------------------
### En la última semana, ¿con qué frecuencia ha realizado las siguientes acciones para protegerse del coronavirus? Ud. me debe decir si las ha realizado Casi nunca, A veces, Frecuentemente, Casi siempre o Siempre

# F7_1 Lavarse las manos (f7_wash) ----------------------------------------
table(movid_i$f7_1)

movid_i$f7_wash <- car::recode(movid_i$f7_1, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))
table(movid_i$f7_wash)
# F7_2 Distancia fisica (f7_distance) ----------------------------------------
table(movid_i$f7_2)
movid_i$f7_distance <- car::recode(movid_i$f7_2, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))
table(movid_i$f7_wash)
# F7_3 Evitar reuniones sociales (f7_social) ----------------------------------------
table(movid_i$f7_3)
movid_i$f7_social <- car::recode(movid_i$f7_3, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))
table(movid_i$f7_social)
# F7_4 Mascarilla al salir (f7_mask) ----------------------------------------
table(movid_i$f7_4)
movid_i$f7_mask <- car::recode(movid_i$f7_4, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))
table(movid_i$f7_mask)
# F7_5 Mascarilla en lugares cerrados (f7_mask2) ----------------------------------------
table(movid_i$f7_5)
movid_i$f7_mask2 <- car::recode(movid_i$f7_5, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

table(movid_i$f7_mask2)

## Alto cumplimiento (alto cumplimiento)  ---------------------------------------------------

movid_i <- movid_i %>% mutate(alto_cumplimiento = case_when(f7_wash %in%  c('Casi siempre', 'Siempre')~ 1,
                                               f7_social %in%  c('Casi siempre', 'Siempre')~ 1,
                                               f7_distance %in%   c('Casi siempre', 'Siempre')~ 1,
                                               f7_mask %in%  c('Casi siempre', 'Siempre')~ 1,
                                               f7_mask2 %in% c('Casi siempre', 'Siempre')~ 1,
                                               TRUE ~ 0))

table(movid_i$alto_cumplimiento)

# F8. Percepcion del cumplimiento de medidas (f8) ----------------------------------------------
### Pensando en distintas medidas de cuidado ante el coronavirus (quedarse en casa, usar     mascarilla, mantener distanciamiento social o lavarse las manos). ¿En qué medida diría Ud.     que su círculo cercano (personas que viven con Ud. o su familia cercana) cumple estas     recomendaciones?
table(movid_i$f8)
movid_i$f8 <- car::recode(movid_i$f8, c("1='Completamente';2='En gran medida';3='Bastante';4='Algo';5='Poco';6 ='Nada';c(8,9)=NA"), as.factor = T,
                              levels = c("Completamente", "En gran medida", "Bastante", "Algo", "Poco", "Nada"))

table(movid_i$f8) #es normas en movid

## Percepcion del cumplimiento de normas
movid_i$cumple_normas <- ifelse(movid_i$f8=="Completamente" | movid_i$f8=="En gran medida",1,0)
movid_i$nocumple_normas <- ifelse(movid_i$f8=="Nada" | movid_i$f8=="Poco" | movid_i$f8=="Algo",1,0)
table(movid_i$cumple_normas)

# Modulo G: Empleo  -----------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("g"), -ends_with("_esp")), funs(as.numeric(.)))

# Especiales modulo empleo  -----------------------------------------------------------------

# CAE  --------------------------------------------------------------------
movid_i <- movid_i %>% mutate(cae = case_when(g1 == 1 ~ "Ocupado",
                                              g1 == 2 & g5 == 1 ~ "Desocupado",
                                              g1 == 2 & g5 == 2 ~ "Inactivo"),
                              cae_covid = case_when(g1 == 1 ~ "Ocupado",
                                                    g1 == 2 & g3 %in% c(4,5,6,7,8,9) ~ "Ocupado ausente",
                                                    g1 == 2 & g3 %in% c(1,2,3) ~ "Ocupado ausente por pandemia",
                                                    g1 == 2 & g5 == 2 & g6b == 1 ~ "Desocupado por pandemia",
                                                    g1 == 2 & g5 == 2 & g6b == 2 ~ "Inactivo por pandemia"))

table(movid_i$cae)
table(movid_i$cae_covid)

### Desocupacion por pandemia (GB == 1 | G3 %in% c(1,2,3))
### Inactividad por pandemia (G6B == 2)
### Ocupados ausentes (G3 =/= 10)

##CISE (g10)

## Ingreso del hogar per capita (ingh_p) ------------------------------------
###g47 (ingresos = ingresos autonomos + transferencias)
### No excluí a los 7 casos que ganan 0
hogares <- movid_i %>% group_by(id_encuesta) %>% summarise(nhogar = n())
movid_i <- movid_i %>% mutate(g47 = if_else(g47 == 1, 1, NA_real_)) %>% 
  merge(hogares, by = "id_encuesta", all.x = T) %>% 
  mutate(ingh_p =  g47_monto/nhogar)

# Condicion de Actividad Economica (CAE) ----------------------------------------
# G1 Trabajar -------------------------------------------------------------
table(movid_i$g1)
movid_i$g1 <- car::recode(movid_i$g1, c("1='Sí'; 2='No'"), as.factor = T,
                          levels = c('Sí', 'No'))
table(movid_i$g1)
# G2 Verificacion empleo --------------------------------------------------
table(movid_i$g2)
movid_i$g2 <- car::recode(movid_i$g2, c("1='Sí'; 2='No'"), as.factor = T,
                          levels = c('Sí', 'No'))
table(movid_i$g2)


# G3 Razon desocupacion ------------------------------------------------------
table(movid_i$g3)
movid_i$g3 <- car::recode(movid_i$g3, c("1='Suspensión temporal contrato por COVID-19';
                                         2='Licencia médica por COVID-19';
                                         3='Permiso de empleador por cuarentena';
                                         4='Licencia médica por otra enfermedad';
                                         5='Pre o postnatal';
                                         6='Huelga';
                                         7='Enfermedad';
                                         8='Vacaciones';
                                         9='Otra. Especifique';
                                         10='Sin empleo'"), as.factor = T,
                               levels = c('Suspensión temporal contrato por COVID-19',
                                          'Licencia médica por COVID-19',
                                          'Permiso de empleador por cuarentena',
                                          'Licencia médica por otra enfermedad',
                                          'Pre o postnatal',
                                          'Huelga',
                                          'Enfermedad',
                                          'Vacaciones',
                                          'Otra. Especifique',
                                          'Sin empleo'))
table(movid_i$g3)

# G4 Ocupado alguna vez ---------------------------------------------------
table(movid_i$g4)
movid_i$g4 <- car::recode(movid_i$g4, c("1='Sí'; 2='No'"), as.factor = T,
                          levels = c('Sí', 'No'))
table(movid_i$g4)

# G5 Buscar trabajo remunerado o cuentapropia  ---------------------------------------------------
table(movid_i$g5)
movid_i$g5 <- car::recode(movid_i$g5, c("1='Sí'; 2='No'"), as.factor = T,
                          levels = c('Sí', 'No'))
table(movid_i$g5)

# G6 Razon inactividad  ---------------------------------------------------
table(movid_i$g6)
movid_i$g6 <- car::recode(movid_i$g6, c("1='Quehaceres del hogar';
                                        2='Cuidado de menores';
                                        3='Cuidado de adulto mayor o enfermo';
                                        4='Estudiante';
                                        5='Discapacitado o enfermo crónico';
                                        6='Jubilado o pensionado';
                                        7='Menor de edad o edad avanzada';
                                        8='Otra. Especifique'"), as.factor = T,
                          levels = c('Quehaceres del hogar',
                                     'Cuidado de menores',
                                     'Cuidado de adulto mayor o enfermo',
                                     'Estudiante',
                                     'Discapacitado o enfermo crónico',
                                     'Jubilado o pensionado',
                                     'Menor de edad o edad avanzada',
                                     'Otra. Especifique'))
table(movid_i$g6)

# G6b Buscar trabajo remunerado SIN PANDEMIA  ---------------------------------------------------
table(movid_i$g6b)
movid_i$g6b <- car::recode(movid_i$g6b, c("1='Sí'; 2='No'"), as.factor = T,
                          levels = c('Sí', 'No'))
table(movid_i$g6b)

# Situacion de Empleo (CISE) ----------------------------------------------
# Ocupados G8-G18 ---------------------------------------------------------
# G8 CIUO -----------------------------------------------------------------
#g8 no está

# G18 Lack income due COVID ---------------------------------------------------
# Autoemployers
table(movid_i$g18)
movid_i$g18 <- as.numeric(movid_i$g18)
movid_i$g18 <- car::recode(movid_i$g18, c("1='Ha bajado';2='Se ha mantenido igual';3='Ha subido';c(8,9)=NA"), as.factor = T,
                           levels = c("Ha bajado", "Se ha mantenido igual", "Ha subido"))
table(movid_i$g18)

# CESANTES G31  -------------------------------------------------------


# G49 Lack income todos ---------------------------------------------------
table(movid_i$g49)
movid_i$g49 <- as.numeric(movid_i$g49)
movid_i$g49 <- car::recode(movid_i$g49, c("1='Ha bajado';2='Se ha mantenido igual';3='Ha subido';c(8,9)=NA"), as.factor = T,
                                   levels = c("Ha bajado", "Se ha mantenido igual", "Ha subido"))
table(movid_i$g49)


# Modulo V. Vacuna --------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("v"), -ends_with("_esp")), funs(as.numeric(.)))
# V1 Propension a vacunarse -----------------------------------------------
table(movid_i$v1)
## Grados
movid_i$v1_alto <- ifelse(movid_i$v1 %in% c(1,2,3)&!is.na(movid_i$v1), "Alta probabilidad", "Baja probabilidad")
## Escala
movid_i$v1 <- car::recode(movid_i$v1, c("1='Me vacunaría sin dudas';
                                        2='Es muy probable que me vacune';
                                        3='Es bastante probable que me vacune';
                                        4='Es poco probable que me vacune';
                                        5='No me vacunaría por ningún motivo';
                                        c(8,9)=NA"), as.factor = T,
                           levels = c('Me vacunaría sin dudas',
                                      'Es muy probable que me vacune',
                                      'Es bastante probable que me vacune',
                                      'Es poco probable que me vacune',
                                      'No me vacunaría por ningún motivo'))
table(movid_i$v1)
table(movid_i$v1_alto)

# V2 Razon de poco y nada probable a vacunarse -----------------------------------------------
table(movid_i$v2_1)

# V2_1 Efectos secundarios (v2_efectos) -----------------------------------
## Me preocupa que la vacuna COVID-19 tenga efectos adversos para mi salud
table(movid_i$v2_1)
movid_i$v2_efectos <- ifelse(!is.na(movid_i$v2_1)&movid_i$v2_1 == 1, "Sí", "No")
table(movid_i$v2_efectos)

# V2_2 Desarrollo de vacuna (v2_efectos) -----------------------------------
## Me preocupa que el desarrollo rápido de la vacuna para el COVID-19 la haga menos segura
table(movid_i$v2_2)
movid_i$v2_desarrollo <- ifelse(!is.na(movid_i$v2_2)&movid_i$v2_2 == 2, "Sí", "No")
table(movid_i$v2_desarrollo)

# V2_3 Efectividad (v2_efectiva) -----------------------------------
## No creo que la vacuna sea realmente efectiva contra el COVID-19
table(movid_i$v2_3)
movid_i$v2_efectiva <- ifelse(!is.na(movid_i$v2_3)&movid_i$v2_3 == 3, "Sí", "No")
table(movid_i$v2_efectiva)

# V2_4 Contagiarse de COVID con vacuna (v2_contagio) -----------------------------------
## El riesgo que yo tengo de contraer COVID-19 es bajo
table(movid_i$v2_4)
movid_i$v2_contagio <- ifelse(!is.na(movid_i$v2_4)&movid_i$v2_4 == 4, "Sí", "No")
table(movid_i$v2_contagio)

# V2_5 Contra vacunas (v2_antivacuna) -----------------------------------
## Estoy en contra de las vacunas en general
table(movid_i$v2_5)
movid_i$v2_antivacuna <- ifelse(!is.na(movid_i$v2_5)&movid_i$v2_5 == 5, "Sí", "No")
table(movid_i$v2_antivacuna)

# V2_6 Otra (v2_otra) -----------------------------------
## Otra. Especifique
table(movid_i$v2_6)
movid_i$v2_otra <- ifelse(!is.na(movid_i$v2_6)&movid_i$v2_6 == 6, "Sí", "No")
table(movid_i$v2_otra)
table(movid_i$v2_6_esp) # recodificarlas

# 4.Merge data ------------------------------------------------------------
# 4.1 Select variables ----------------------------------------------------

## Para informe
movid_i_proc <- movid_i %>%
  filter(a5 == "Entrevistado(a)") %>%
  select(id_encuesta, orden, region, comuna, entrevistado, factor_expansion, #Identificacion
         sexo, starts_with("edad"), starts_with("educ"), #Sociodemografica
         starts_with("prev"), cronicos,
         cae, cae_covid, ingh_p, nhogar,
         a4:b3, #Otras sociodemograficas
         starts_with("c2_"), #Salud mental
         f1, f1_horas, f2, f2_horas, 192:208, f6, 209:216, f8, #Sociales
         v1, 221:227) #Vacunas

## Base final
movid_i <- movid_i %>% 
  filter(a5 == "Entrevistado(a)") %>% 
  select(id_encuesta, orden, region, comuna, entrevistado, factor_expansion, #Identificacion
         sexo, starts_with("edad"), starts_with("educ"), #Sociodemografica
         starts_with("prev"), cronicos,
         cae, cae_covid, ingh_p, nhogar,
         a4:b3, #Otras sociodemograficas
         starts_with("c1_"), #Tipos Cronicos
         starts_with("c2_"), #Salud mental
         d1,d2, starts_with("d3_snt"), d4,d5,d5_esp, d6,d7,d8, d9, posponer, #Acceso a Salud
         starts_with("e1_"),starts_with("e2_"), e3, e4, starts_with("e5_posp"), e6,e7,e8,e9,e10, #Acceso a salud no covid
         f1, f1_horas, f2, f2_horas, 192:208, f6, 209:216, f8, #Sociales
         g1:g50, # Empleo
         v1, 221:227) #Vacunas

## Base original
movid_o <- movid_o %>% filter(a5 == 1)

# 5. Save  -----------------------------------------------------------------
saveRDS(movid_i, file = "output/data/movid_i.RDS") # MOVID-IMPACT
saveRDS(movid_o, file = "output/data/movid_o.RDS") # MOVID-IMPACT-ORIGINAL
save(movid_i,movid_o, file = "output/data/movid_impact.RData")
saveRDS(movid_i, file = "output/data/movid_i_proc.RDS") #MOVID-IMPACT-INFORME


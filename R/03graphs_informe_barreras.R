# Code 3: Graficos Informe Barreras movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, srvyr, survey, ggsci)

# 1.1 Set ups -------------------------------------------------------------
library(ggplot2); theme_set(theme_classic(base_size = 12) + 
                              theme(legend.position = "none",
                                    text = element_text(size = 12, face = "bold"), strip.text.x = element_text(face = "bold")))
library(knitr);   options(knitr.kable.NA = 'No sabe/No responde')

library(survey);  options(survey.lonely.psu = "certainty")

# 2. Load data  -------------------------------------------
## movid_i-19
movid_i_proc <- readRDS(file = "output/data/movid_i_proc.RDS")
data <- movid_i_proc

# 3. Recodificaciones para informe ----------------------------------------
#Sociodemograficas
## prev_2categ

#cronicos
### Version sin factores
label <- c(
  c1_1= "Diabetes",
  c1_2= "Hipertensión arterial",
  c1_3= "Infarto cardiaco\n o ACV",
  c1_4= "Asma, EPOC\n, enfisema pulmonar",
  c1_5= "Depresión,\nTAG u otra",
  c1_6= "Otra",
  c1_7= "Ninguna")

#consulta
### Version sin factores

label2 <- c(
  e5_posp_consulta = "Consulta médica",
  e5_posp_examen = "Exámenes de laboratorio\no imágenes",
  e5_posp_insumos = "Retiro o compra\n medicamentos")



# 4. Informe Barreras y medidas cuidado  -------------------------------------------

# Figura 1 - Distribucion para cada cronico de posponer al menos una atencion -------------------------------------
data %>%
  pivot_longer(cols = c(e5_posp_consulta,e5_posp_examen, e5_posp_insumos),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = as_factor(valor)) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor == "Sí") %>% 
  mutate(variable = factor(variable,levels = c("e5_posp_consulta","e5_posp_examen", "e5_posp_insumos"))) %>% 
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.5)  + 
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(variable~., nrow = 1, labeller = labeller(variable = label2)) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Tipo de prestación que se pospone", title = "")  +
  scale_fill_jama(name = "", na.value = "grey50") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figura2.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)


# Figura 3 - Distribucion de cronicos que posponen segun tipo  ---------------------
data %>%
  pivot_longer(cols = c(starts_with("c1_"), -c(c1_8,c1_9,c1_6_esp)),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = as_factor(valor)) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor == 1) %>% 
  mutate(variable = factor(variable,levels = c("c1_1", "c1_2", "c1_3","c1_4","c1_5","c1_6", "c1_7"))) %>% 
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.5)  + 
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(variable~., nrow = 1, labeller = labeller(variable = label)) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje de personas con condiciones crónicas", title = "")  +
  scale_fill_jama(name = "", na.value = "grey50") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figura1.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)


# Figura 2 - Distribucion de cronicos que posponen segun prevision ---------------------
data %>%
  pivot_longer(cols = starts_with("c2_3"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Nunca", "Varios días", "Más de la mitad de los días","Casi todos los días", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor != "NS/NR") %>% 
  mutate(grado = if_else(valor %in% c("Muy en desacuerdo", "En desacuerdo"),"Bajo","Alto" )) %>% 
  ggplot(aes(x = valor, y = prop, fill = grado)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp),position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(c2_3 = "Durante las últimas dos semanas, se ha sentido\nbajoneado, deprimido, irritable o desesperanzado"))) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje de respuestas", title = "")  + scale_fill_jama(name = "", 
                                                                              na.value = "grey50") +
  guides(fill = F)


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure3.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Figura 3 - Proporcion de personas que posponen al menos una atencion --------
## Por que se la posponen, por miedo al contagio u otra razón.


# Lamina 4: ¿Cuánto nos cuidamos del COVID-19 en Chile? -------------------
# Figura 1 ---------------------------------------------------------------
## Indicacion:Descriptivo de toque de queda y los 5 cuidados (quizás usar un grid de 3 x 2) 
## Variable:f4_toque, f7_wash", "f7_distance", "f7_social", "f7_mask", "f7_mask2"
## Detalle: Cada gráfico con todas las categorías de respuesta y un NS/NR


# Figure 2 ----------------------------------------------------------------
## Style
pp_trbj %>% 
  ggplot(aes(y = predicted, x = x, ymin = conf.low, ymax = conf.high)) +
  geom_bar(aes(fill = x), stat = "identity", position = "dodge") +
  geom_errorbar(stat = "identity", position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(100*predicted, 0), "%")), fill = "transparent",
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) +
  scale_fill_jama(name = "") +
  facet_wrap(~dv) +
  scale_y_continuous(labels = percent_format(suffix = "")) +
  labs(x = "Trabaja", y = "Probabilidad estimada de cuidarse siempre") 

## Save plot
ggsave(plot = last_plot(), filename = "output/figures/figure2.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)


# Figure 3 ----------------------------------------------------------------
data %>%
  pivot_longer(cols = starts_with("c2_3"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Nunca", "Varios días", "Más de la mitad de los días","Casi todos los días", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor != "NS/NR") %>% 
  mutate(grado = if_else(valor %in% c("Muy en desacuerdo", "En desacuerdo"),"Bajo","Alto" )) %>% 
  ggplot(aes(x = valor, y = prop, fill = grado)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp),position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(c2_3 = "Durante las últimas dos semanas, se ha sentido\nbajoneado, deprimido, irritable o desesperanzado"))) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje de respuestas", title = "")  + scale_fill_jama(name = "", 
                                                       na.value = "grey50") +
  guides(fill = F)


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure3.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)



# Figure 4 ----------------------------------------------------------------
## Style
pp_dep %>% 
  ggplot(aes(y = predicted, x = x, ymin = conf.low, ymax = conf.high)) +
  geom_bar(aes(fill = x), stat = "identity", position = "dodge") +
  geom_errorbar(stat = "identity", position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(100*predicted, 0), "%")), fill = "transparent",
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) +
  scale_fill_jama(name = "") +
  facet_wrap(~dv) +
  scale_y_continuous(labels = percent_format(suffix = "")) +
  labs(x = "Síntomas depresivos", y = "Probabilidad estimada de cuidarse siempre (%)") 

## Save plot
ggsave(plot = last_plot(), filename = "output/figures/figure4.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)


# Figura 5 ----------------------------------------------------------------
## Indicacion: descriptivo de f6 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f6"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Nada peligroso", "Algo peligroso", "Bastante peligroso","Muy peligroso", "Extremadamente peligroso", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor != "NS/NR") %>% 
  mutate(grado = if_else(valor %in% c("Nada peligroso", "Algo peligroso"),"Bajo","Alto" )) %>% 
  ggplot(aes(x = valor, y = prop, fill = grado)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f6 = "Percepción de peligro del coronavirus para si mismo y cercanos"))) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje de respuestas", title = "")  + scale_fill_jama(name = "", 
                                    na.value = "grey50")

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure5.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Figura 6 ----------------------------------------------------------------
## Indicacion: Descriptivo de f5_1 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f5_protect"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Muy en desacuerdo", "En desacuerdo", "Indiferente","De acuerdo", "Muy de acuerdo", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor != "NS/NR") %>% 
  mutate(grado = if_else(valor %in% c("Muy en desacuerdo", "En desacuerdo"),"Bajo","Alto" )) %>% 
  ggplot(aes(x = valor, y = prop, fill = grado)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp),position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f5_protect = "Puedo protegerme completamente del coronavirus\nsi tomo las medidas de protección adecuadas"))) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje de respuestas", title = "")  + scale_fill_jama(name = "", 
                                                       na.value = "grey50") +
  guides(fill = F)

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure6.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)

# Figura 7 ----------------------------------------------------------------
## Indicacion: Probabilidades predichas de cumplir "siempre" con cada medida para peligro = 1 y peligro = 5 - 
## dejar variables de cuidado donde hay diferencias significativas
pp_rsg %>% 
  ggplot(aes(y = predicted, x = x, ymin = conf.low, ymax = conf.high)) +
  geom_bar(aes(fill = x), stat = "identity", position = "dodge") +
  geom_errorbar(stat = "identity", position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(100*predicted, 0), "%")), position = position_stack(vjust = .5),
             fill = "transparent",
             color = "white",
             size=4,
             fontface = "bold") +
  scale_fill_jama(name = "") +
  facet_wrap(~dv) +
  scale_y_continuous(labels = percent_format(suffix = "")) +
  labs(x = "Percepción de riesgo del coronavirus", y = "Probabilidad estimada de cuidarse siempre (%)")

ggsave(plot = last_plot(), filename = "output/figures/figure7.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)

# Figura 8 ----------------------------------------------------------------
## Indicacion: Probabilidades predichas de cumplir "siempre" con cada medida para f5_1 = 1 y f5_1 = 5 - 
## dejar variables de cuidado donde hay diferencias significativas

pp_prt %>% 
  ggplot(aes(y = predicted, x = x, ymin = conf.low, ymax = conf.high)) +
  geom_bar(aes(fill = x), stat = "identity", position = "dodge") +
  geom_errorbar(stat = "identity", position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(100*predicted, 0), "%")), position = position_stack(vjust = .5),
             fill = "transparent",
             color = "white",
             size=4,
             fontface = "bold") +
  scale_fill_jama(name = "") +
  facet_wrap(~dv) +
  scale_y_continuous(labels = percent_format(suffix = "")) +
  labs(x = "Percepción de control sobre el coronavirus", y = "Probabilidad estimada de cuidarse siempre (%)")

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure8.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Figura 9 ----------------------------------------------------------------
## Indicacion: descriptivo de f8 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f8"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Nada", "Poco", "Algo","Bastante", "En gran medida", "Completamente","NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor != "NS/NR") %>% 
  mutate(grado = if_else(valor %in% c("Nada", "Poco", "Algo"),"Bajo","Alto" )) %>% 
  ggplot(aes(x = valor, y = prop, fill = grado)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f8 = "Grado en que mis cercanos y yo cumplimos las medidas de cuidado"))) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje de respuestas", title = "")  + scale_fill_jama(name = "", 
                                                       na.value = "grey50") + 
  guides(fill = F)

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure9.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Figura 10 ----------------------------------------------------------------
## Indicacion: Probabilidades predichas de cumplir "siempre" con cada medida para f8 = 1 y f8 = 6 - 
## dejar variables de cuidado donde hay diferencias significativas

pp_nrm %>% 
  ggplot(aes(y = predicted, x = x, ymin = conf.low, ymax = conf.high)) +
  geom_bar(aes(fill = x), stat = "identity", position = "dodge") +
  geom_errorbar(stat = "identity", position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(100*predicted, 0), "%")), position = position_stack(vjust = .5),
             fill = "transparent",
             color = "white",
             size=4,
             fontface = "bold") +
  scale_fill_jama(name = "") +
  facet_wrap(~dv) +
  scale_y_continuous(labels = percent_format(suffix = "")) +
  labs(x = "Grado en que mis cercanos y yo cumplimos las medidas de cuidado", y = "Probabilidad estimada de cuidarse siempre (%)") +
  theme(legend.position = "none")


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure10.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Figura 11 ----------------------------------------------------------------
## Indicacion: Descriptivo de f5_5 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f5_legal"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Muy en desacuerdo", "En desacuerdo", "Indiferente","De acuerdo", "Muy de acuerdo", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor != "NS/NR") %>% 
  mutate(grado = if_else(valor %in% c("Muy en desacuerdo", "En desacuerdo"),"Bajo","Alto" )) %>% 
  ggplot(aes(x = valor, y = prop, fill = grado)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f5_legal = "En Chile, si una persona sale sin permiso durante una cuarentena es muy poco probable que sea controlado y multado"))) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje de respuestas", title = "")  + scale_fill_jama(name = "", 
                                                       na.value = "grey50") + 
  guides(fill = F)


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure11.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Figura 12 ----------------------------------------------------------------
## Indicacion: probabilidades predichas de cumplir "siempre" con cada medida para f5_5 = 1 y f5_5 = 5 - 
## dejar variables de cuidado donde hay diferencias significativas

pp_fsc %>% 
  ggplot(aes(y = predicted, x = x, ymin = conf.low, ymax = conf.high)) +
  geom_bar(aes(fill = x), stat = "identity", position = "dodge") +
  geom_errorbar(stat = "identity", position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(100*predicted, 0), "%")), position = position_stack(vjust = .5),
             fill = "transparent",
             color = "white",
             size=4,
             fontface = "bold") +
  scale_fill_jama(name = "") +
  facet_wrap(~dv) +
  scale_y_continuous(labels = percent_format(suffix = "")) +
  labs(x = "En Chile, si una persona sale sin permiso durante una cuarentena\nes muy poco probable que sea controlado y multado", y = "Probabilidad estimada de cuidarse siempre (%)") +
  theme(legend.position = "none")


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure12.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)



# Lamina 13 : ¿Cómo promovemos el respeto a las medidas de cuidado COVID-19? ---------------------------------------------------------------
## Comunicación de riesgos
## Fiscalización real y comunicada a la ciudadanía
## Comunicar normas sociales (ejemplos de quienes cumplen, no de quienes no cumplen)
## ¿Propuestas más concretas?


# Lamina 14: Cierre -------------------------------------------------------










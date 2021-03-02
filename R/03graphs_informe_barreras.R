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
load(file = "output/models/model-care.RData")
data <- movid_i_proc

# 3. Recodificaciones para informe ----------------------------------------
# Cumplimiento toque de queda
data <- data %>% mutate_at(vars(starts_with("f7_")),
                     funs(car::recode(.,'"Siempre" = "Si";
                                      c("Casi siempre","Frecuentemente","A veces","Casi nunca")="No"', as.factor = T,
                          levels = c('No', 'Si')))) %>% # El orden es donde 0 = No, 1 = Si
  mutate(f4_toque = if_else(f4_toque == "Si", "No", "Si")) %>% # Dar vuelta para entender como "Respeto del toque"
  mutate_at(vars(f4_toque, starts_with("f7_"), starts_with("f3_")),
                           funs(as_factor(.)))  %>%  # Como un factor para no perder etiquetas
  mutate_at(vars(starts_with("f3_"), starts_with("f5_"), f8), funs(forcats::fct_rev(.))) # Revertir  los factores de las barreras donde 1 es


## Sintesis
### 1. Cuidados (f7_*): 1 = No, 2 = Si
### 2. Barreras legitimidad (f3_*): 1 = Muy en desacuerdo a 5 = Muy de acuerdo (revertidas)
### 3. Barreras fatiga (f5_*): 1 = Muy en desacuerdo a 5 = Muy de acuerdo (revertidas)
### 4. Barreras percepcion riesgo (f6): 1 = Nada peligroso a 5 = Extremadamente peligroso
### 5. Barreras (f8): 1 = Nada a 5 = Completamente (revertidas)



# 4. Modelos para informe -------------------------------------------------
# Modelos para predecir cuidados

# lm1 <- glm(f4_toque ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm2 <- glm(f7_wash ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm3 <- glm(f7_dist ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm4 <- glm(f7_social ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm5 <- glm(f7_mask ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm6 <- glm(f7_mask2 ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# 
# tab_model(lm1, lm2, lm3, lm4, lm5, lm6)


# 5. Informe Barreras y medidas cuidado  -------------------------------------------

# Lamina 1: ¿Qué es MOVID-19? -------------------------------------------

# Lamina 2: ¿Quienes somos MOVID-19? -------------------------------------------

# Lamina 3: MOVID-Impact: Metodología, dimensiones -------------------------------------------


# Lamina 4: ¿Cuánto nos cuidamos del COVID-19 en Chile? -------------------
# Figura 1 ---------------------------------------------------------------
## Indicacion:Descriptivo de toque de queda y los 5 cuidados (quizás usar un grid de 3 x 2) 
## Variable:f4_toque, f7_wash", "f7_distance", "f7_social", "f7_mask", "f7_mask2"
## Detalle: Cada gráfico con todas las categorías de respuesta y un NS/NR

### Version sin factores
label <- c(
  f7_distance = "Distancia física\n de 2 metros",
  f7_social = "Evitar reuniones de\nmás de 10 personas",
  f7_wash = "Lavado de manos",
  f7_mask2 = "Uso mascarilla\nen lugares cerrados",
  f4_toque = "Respeto\ntoque de queda",
  f7_mask = "Uso mascarilla\nfuera del hogar")

data %>%
  pivot_longer(cols = c(starts_with("f7_"), f4_toque),
                      names_to = "variable",
                      values_to = "valor") %>% 
  group_by(variable, valor) %>% 
  summarise(n = n()) %>% 
  mutate (prop = round(n/ sum(n),4)*100) %>%
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.9) +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = label)) +
  geom_label(aes(label = paste0(prop, "%")),
            position = position_stack(vjust = .5),
            color="white", size= 4, fontface = "bold",
            show.legend = FALSE) + 
  labs(x = "", y = "%", title = "") + scale_fill_jama(name = "",
                                    na.value = "grey50")


# Version con factores ----------------------------------------------------

#Figure 1 ----------------------------------------------------------------

data %>%
  pivot_longer(cols = c(starts_with("f7_"), f4_toque),
                        names_to = "variable",
                        values_to = "valor") %>%
  mutate( valor = as.character(valor),
    valor = if_else(is.na(valor), "NS/NR", valor),
    valor = as_factor(valor)) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor == "Si") %>% 
  mutate(variable = factor(variable,levels = c("f7_distance", "f7_social", "f7_wash", "f7_mask2", "f4_toque", "f7_mask"))) %>% 
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.5)  + 
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(variable~., nrow = 1, labeller = labeller(variable = label)) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "Porcentaje que se cuida siempre", title = "")  +
  scale_fill_jama(name = "", na.value = "grey50") + 
  theme(axis.text.x = element_blank(),
    axis.ticks.x=element_blank())


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure1.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)

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










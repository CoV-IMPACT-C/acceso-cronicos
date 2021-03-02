# Code 6: Others movid_i-IMPACT ------------------------------------------------------
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
## movid_i-19
load(file = "output/data/movid_impact.RData")

# Modulo V. Vacuna --------------------------------------------------------
movid_i <- movid_o %>% mutate_at(vars(starts_with("v"), -ends_with("_esp")), funs(as.numeric(.)))
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
                                        c(8,9)='NS/NR'"), as.factor = T,
                          levels = c('Me vacunaría sin dudas',
                                     'Es muy probable que me vacune',
                                     'Es bastante probable que me vacune',
                                     'Es poco probable que me vacune',
                                     'No me vacunaría por ningún motivo',
                                     'NS/NR'))
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

data <- movid_i; remove(movid_i)

# 3. Graficos -----------------------------------------------------------------
data %>%
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(v1)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100))  %>% 
  ggplot(aes(x = v1, y = prop, fill = v1)) +
  geom_bar(stat = "identity", width = 0.5)  + 
  geom_errorbar(aes(x = v1, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(prop,1), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "", title = "", caption = "Figura 1. Disposición a la vacunación. Datos ponderados (n = 1.261)")  +
  scale_fill_jama(name = "", na.value = "grey50")

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/vaccine1.png",
       device = "png",dpi = "retina", units = "cm",
       width = 40,height = 15)


# Vaccine 2 ---------------------------------------------------------------
data %>%
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(v1_alto)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100))  %>% 
  ggplot(aes(x = v1_alto, y = prop, fill = v1_alto)) +
  geom_bar(stat = "identity", width = 0.5)  + 
  geom_errorbar(aes(x = v1_alto, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  geom_label(aes(label = paste0(round(prop,1), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "", title = "", caption = "Figura 2.Disposición a la vacunación. Datos ponderados (n = 1.261)")  +
  scale_fill_jama(name = "", na.value = "grey50")

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/vaccine2.png",
       device = "png",dpi = "retina", units = "cm",
       width = 30,height = 15)


# Vaccine 3 ---------------------------------------------------------------
label <- c(
  v2_antivacuna = "No creo en\n las vacunas",
  v2_contagio = "Miedo a\ncontagiarme",
  v2_desarrollo = "Desarrollo de la vacuna",
  v2_efectiva = "No creo\nen su efectividad",
  v2_efectos = "Miedo a efectos\nsecundarios",
  v2_otra = "Otra\nrazón")

data %>%
  pivot_longer(cols = c("v2_antivacuna", "v2_contagio", "v2_desarrollo", "v2_efectiva", "v2_efectos", "v2_otra"),
               names_to = "variable",
               values_to = "valor") %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  filter(valor == "Sí") %>% 
  mutate(variable = factor(variable,levels = c("v2_antivacuna", "v2_contagio", "v2_desarrollo", "v2_efectiva", "v2_efectos", "v2_otra"))) %>% 
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.5)  + 
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), position = "dodge", 
                width = .33, color = "#8D8680") +
  facet_wrap(variable~., nrow = 1, labeller = labeller(variable = label)) +
  geom_label(aes(label = paste0(round(prop,0), "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "%", title = "", caption = "Figura 3.Razones para no vacunarse entre quienes indicaron que era poco probable y nada vacunarse.\nDatos ponderados (n=1.261)")  +
  scale_fill_jama(name = "", na.value = "grey50") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/vaccine3.png",
       device = "png",dpi = "retina", units = "cm",
       width = 30,height = 15)

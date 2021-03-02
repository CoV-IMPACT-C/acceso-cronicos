# 1. Packages -----------------------------------------------------
if (!(require(pacman))) install.packages("pacman")
pacman::p_load(dplyr, 
               readr,
               lubridate,
               ggplot2)

# 2. Import data  -------------------------------------------
url_pandata <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv"
pandata <- read_csv(url(url_pandata))
pandata <- pandata %>% 
  mutate(week = isoweek(Fecha),
         week = case_when(
           week < 10 ~ week + 53,
           TRUE ~ week),
         survey = case_when(
           Fecha >= "2020-12-05" & Fecha <= "2020-12-22" ~ T,
           TRUE ~ F
         ))

# Plot
## Relevant dates
start <- as.Date("2020-12-05")
finish <- as.Date("2020-12-22")
presentation <- as.Date("2021-01-28")

## Create plot
pandata %>% 
  filter(!is.na(`Casos activos`)) %>% 
  ggplot(aes(x = Fecha, y = `Casos activos`)) +
  geom_rect(xmin = start, xmax = finish,
            ymin = -Inf, ymax = Inf, color = "transparent",
            fill = "grey60", alpha = 0.009) +
  geom_line(stat = "identity", color = "darkgrey") +
  geom_point(stat = "identity", aes(fill = survey, col = survey)) +
  scale_x_date(date_breaks = "3 week", date_labels = "%d-%m", 
               limits = c(min(pandata$Fecha), presentation)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".")) +
  theme_classic(base_size = 20) +
  geom_text(aes(x = start, y = 25e+3, label = "04-dic"), 
            size = 5, color = "grey32") +
  geom_text(aes(x = finish, y = 25e+3, label = "22-dic"),
                size = 5, color = "grey32") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank()) + scale_color_jama()

## Export plot
ggsave(filename = "output/figures/figure0.png", dpi = 500,
       width = 8, height = 4.5, scale = 1.8)

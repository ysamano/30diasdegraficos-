library(tidyverse)
library(countrycode)
library(ggrepel)


datos <- read_csv("dia_03/gdp-vs-happiness.csv") %>% 
  janitor::clean_names()

datos_select <- datos %>% 
  filter(year == "2017",
         !is.na(code)) %>% 
  mutate(continente = countrycode(code, origin = "iso3c", destination = "continent")) %>% 
  filter(!is.na(continente))


colores = c("#ffc50c", "#014a81", "#ec640f", "#be125f", "#393863")

p1 <- ggplot(datos_select, 
             aes(x = gdp_per_capita,
                 y = life_satisfaction,
                 colour = continente,
                 label = entity)) + 
  geom_point(aes(size = population),
             alpha = 0.8) +
  scale_size(range = c(1, 25)) +
  geom_text_repel(size = 1.8,
                  colour = "gray70",
                  segment.size = 0.2) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_color_manual(values = colores) +
  guides(size = FALSE) +
  labs(title = "Habitantes de países ricos están mas satisfechos ante la vida",
       subtitle = "El eje vertical muestra la satisfacción ante la vida en una escala de 0 a 10, donde 10 significa la satisfacción más alta.\nEl eje horizontal muestra el PIB per cápita ajustado por inflación y diferencias de precios entre países.",
       caption = "Fuente: Our World Data",
       x = "PIB per cápita (logaritmo)",
       y = "Satisfaccion ante la vida") +
  theme_ybn_b(base_size = 8,
              base_family = "Roboto Condensed Light") +
  theme(legend.position = "top",
        legend.title = element_blank())

ggsave("dia_03/02_burbujas3.png", height = 8.5, width = 11, units = "in", dpi = 300)

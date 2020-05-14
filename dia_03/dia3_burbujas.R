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

x11()
ggplot(datos_select, aes(gdp_per_capita, life_satisfaction, colour = continente, label = entity)) + 
  geom_point(aes(size = population), alpha = 0.8) +
  scale_size(range = c(1, 20)) +
  geom_text_repel(size = 1.2, colour = "gray70", segment.size = 0.2) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_color_manual(values = colores) +
  guides(size = FALSE) +
  labs(title = "Habitantes de países ricos están mas satisfechos ante la vida",
       subtitle = "El eje vertical muestra la satisfacción ante la vida en una escala de 0 a 10, donde 10 significa la satisfacción más alta.\nEl eje horizontal muestra el PIB per cápita ajustado por inflación y diferencias de precios entre países.",
       caption = "Fuente: Our World Data",
       x = "PIB per cápita (logaritmo)",
       y = "Satisfaccion ante la vida") +
  theme_minimal(base_size = 6, base_family = "Roboto Condensed Light") +
  theme(plot.background = element_rect(fill = "#1b1f2b", color = "#1b1f2b"),
        plot.title = element_text(family = "Roboto Condensed", colour = "white",
                                      size = 12,  margin = margin(t = 5, b= 3)),
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "white"),
        panel.grid = element_line(colour = "gray20", size = 0.1),
        axis.title = element_text(colour = "gray90", size = 5, hjust = 1),
        axis.text = element_text(colour = "gray90"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "white", face="bold"),
        legend.position = "top",
        legend.box = "horizontal")

ggsave("dia_03/02_burbujas.png", height = 5, width = 7, units = "in", dpi = 300)








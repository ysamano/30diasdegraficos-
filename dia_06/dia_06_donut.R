library(tidyverse)
library(ggtext)
extrafont::loadfonts("win")

muje_parl <- read_csv("dia_06/ConsultaIntegrada.csv")

mujer_tidy <- muje_parl %>% 
  pivot_longer(cols = 2:24, names_to = "anio", values_to = "porcent_muj") %>% 
  mutate(percent_hom = 100 - porcent_muj) %>% 
  pivot_longer(cols = 3:4, names_to = "grupo", values_to = "porcentaje") %>% 
  janitor::clean_names() %>% 
  filter(pais == "América Latina y el Caribe",
         anio %in% c("1997", "2000", "2005", "2010", "2015", "2019")) %>% 
  mutate(alpha = if_else(grupo == "porcent_muj", F, T))

labels <- mujer_tidy %>%
  filter(grupo == "porcent_muj") %>%
  mutate(anio_l = str_c("<span style='font-size:7pt; color:black'>", anio, "</span>"),
         porcentaje = str_c("<span style='font-size:18pt; color:black'>**", round(porcentaje), "%", "**</span>"),
         etiqueta = str_c(porcentaje, "<br>", anio_l))


p1 <- 
  ggplot(mujer_tidy, aes(x = 2, y = porcentaje, fill = grupo, alpha = alpha)) +
  geom_bar(stat = "identity") +
  geom_richtext(data = labels,
                aes(x = 0, y = 0, label = etiqueta),
                fill = NA,
                colour = NA) +
  scale_fill_manual(values = c("#212038", "#d6146b")) +
  scale_alpha_discrete(range = c(1, 0.8)) +
  xlim(0, 2.5) +
  facet_wrap( ~ anio, nrow = 2) +
  coord_polar(theta = "y", start = 0) +
  labs(title = "Mujeres en el poder en América Latina y el Caribe",
       subtitle = "Porcentaje de escaños ocupados por mujeres en los parlamentos",
       caption = "Fuente: Unión Interparlamentaria - Mujeres en el parlamento") +
  theme_void(base_size = 8, base_family = "Roboto Condensed Light") +
  theme(plot.background = element_rect(fill = "#fafafa",
                                       colour = "#fafafa"),
        plot.margin = margin(10, 7, 7, 7),
        plot.title = element_text(family = "Roboto Condensed",
                                  face = "bold",
                                  size = 16),
        strip.text = element_blank(),
        legend.position = "none")

ggsave("dia_06/06_dona.png", p1, height = 5.3, width = 7, units = "in", dpi = 300)


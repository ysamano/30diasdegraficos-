library(tidyverse)
extrafont::loadfonts("win")

pob_pobreza <- readxl::read_excel("dia_04/pob_pobreza.xlsx")

pobreza_tidy <- pob_pobreza %>% 
  pivot_longer(cols = 2:7, names_to = "anio", values_to = "porcentaje") %>% 
  mutate(anio = as.double(anio))

x11()
p1 <- 
  ggplot(pobreza_tidy, aes(x = anio)) + 
  geom_ribbon(aes(ymin = 0, ymax = porcentaje),
              fill = "#006765") +
  geom_vline(aes(xintercept = anio), 
             size = 0.1, 
             color = "#f2efe6") +
  facet_wrap( ~ entidad, scales = "free_x") +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2), labels = c("2008", "'10", "'12", "'14", "'16", "'18")) +
  labs(title = "Diez años de combate a la pobreza en México",
       subtitle = "Porcentaje de personas en situación de pobreza por entidad federativa",
       caption = "Fuente: CONEVAL",
       x = "Año",
       y = "Porcentaje") +
  theme_minimal(base_size = 7, base_family = "Roboto Condensed Light") +
  theme(axis.title = element_text(hjust = 1),
        panel.grid = element_line(colour = "#e6e0d0"),
        panel.grid.minor = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        plot.background = element_rect(fill = "#f2efe6",
                                       color = "#f2efe6"),
        plot.title = element_text(family = "Roboto Condensed",
                                  colour = "#0f0e14",
                                  size = 14,
                                  margin = margin(t = 5, b = 5)),
        plot.subtitle = element_text(size = 8,
                                     margin = margin(b = 10)),
        strip.text = element_text(face = "bold",
                                  size = 8))
  

ggsave("dia_04/04_facetas.png", p1, height = 7, width = 5.5, units = "in", dpi = 300)






library(tidyverse)

pob_pobreza <- readxl::read_excel("dia_04/pob_pobreza.xlsx")

pobreza_tidy <- pob_pobreza %>% 
  pivot_longer(cols = 2:7, names_to = "anio", values_to = "porcentaje") %>% 
  mutate(anio = as.double(anio))

p1 <- 
  ggplot(pobreza_tidy, aes(anio, porcentaje)) +
  geom_area(fill = "#243C5C") +
  geom_vline(aes(xintercept = anio), 
             size = 0.1, 
             color = "gray80") +
  facet_wrap( ~ entidad,
              scales = "free_x") +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2),
                     labels = c("2008", "'10", "'12", "'14", "'16", "'18")) +
  labs(title = "Diez años de combate a la pobreza en México",
       subtitle = "Porcentaje de personas en situación de pobreza por entidad federativa",
       caption = "Fuente: CONEVAL",
       x = "Año",
       y = "Porcentaje") +
  theme_ybn_w(base_size = 9,
              base_family = "Roboto Condensed Light",
              title_hjust = 0.5,
              subtitle_hjust = 0.5,
              subtitle_margin_b = 25,
              plot_margin = margin(25, 20, 20, 20)
              ) +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 8.5))

ggsave("dia_04/04_facetas2.png", p1, height = 11, width = 8.5, units = "in", dpi = 300)

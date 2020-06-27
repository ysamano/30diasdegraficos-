
library(tidyverse)
library(countrycode)

whr20 <- readxl::read_excel("dia_01/WHR20_Data.xls") %>% 
  janitor::clean_names()

datos_graf <- whr20 %>% 
  select(country_name, regional_indicator, ladder_score) %>% 
  filter(regional_indicator == "Latin America and Caribbean") %>% 
  mutate(puntaje = round(ladder_score, 4),
         pais = countrycode(country_name, origin = 'country.name', destination = "un.name.es"),
         pais = case_when(pais == "Venezuela (República Bolivariana de)" ~ "Venezuela",
                          pais == "Bolivia (Estado Plurinacional de)" ~ "Bolivia",
                          TRUE ~ pais))


p1 <- 
  ggplot(datos_graf,aes(ladder_score, fct_reorder(pais, ladder_score), label = puntaje)) +
  geom_col(width = 0.6, fill = "#27233A") +
  geom_text(nudge_x = 0.35,
            size = 3.5,
            colour = "#292b32",
            family = "Source Sans Pro") +
  scale_x_continuous(expand = c(0, 0.2)) +
  labs(title = "La Felicidad en\nAmérica Latina y el Caribe",
       subtitle = "Ranking de los países más felices",
       caption = "Fuente: World Happiness Report 2020 | Gráfica: @ysamano28") +
  theme_ybn_w(base_size = 12,
              title_size = 28,
              title_hjust = 0.5,
              subtitle_hjust = 0.5,
              subtitle_margin_b = 30,
              caption_margin_t = 40,
              plot_margin = margin(50, 40, 20, 40),
              axis_grid = F) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())


ggsave("dia_01/01_barra3.png", p1, height = 11, width = 8.5, units = "in", dpi = 300)

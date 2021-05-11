library(tidyverse)
library(countrycode)

whr20 <- readxl::read_excel("dia_01/WHR20_Data.xls") %>% 
  janitor::clean_names()

datos_graf <- whr20 %>% 
  select(country_name, regional_indicator, ladder_score) %>% 
  filter(regional_indicator == "Latin America and Caribbean") %>% 
  mutate(puntaje = round(ladder_score, 2),
         pais = countrycode(country_name, origin = 'country.name', destination = "un.name.es"),
         pais = case_when(pais == "Venezuela (República Bolivariana de)" ~ "Venezuela",
                          pais == "Bolivia (Estado Plurinacional de)" ~ "Bolivia",
                          TRUE ~ pais))

ggplot2::update_geom_defaults("text", list(family = "Source Sans Pro"))

p1 <- ggplot(datos_graf, aes(ladder_score, fct_reorder(pais, ladder_score))) +
  geom_col(width = 0.6, fill = "#27233A") +
  geom_text(aes(label = puntaje),
            nudge_x = 0.2,
            size = 4,
            colour = "#27233A") +
  geom_text(aes(x = 0.05, label = country_name),
            hjust = 0,
            size = 4,
            colour = "white") +
  scale_x_continuous(limits = c(0, 8), expand = c(0, 0)) +
  labs(title = "La Felicidad en América Latina y el Caribe",
       subtitle = "Ranking de los países más felices",
       caption = "Fuente: World Happiness Report 2020 | Gráfica: @ysamano28") +
  theme_void(base_family = "Source Sans Pro") +
  theme(plot.margin = margin(40, 60, 40, 60),
        plot.title.position = "plot",
        plot.title = element_text(family = "Lora SemiBold",
                                  size = 22), 
        plot.subtitle = element_text(size = 15, 
                                     margin = margin(t = 7, b = 10)),
        plot.caption = element_text(size = 11, 
                                    hjust = 0,
                                    margin = margin(t = 15))
        )

ggsave("dia_01/01_barra.png", p1, height = 11, width = 8.5, units = "in", type = "cairo")

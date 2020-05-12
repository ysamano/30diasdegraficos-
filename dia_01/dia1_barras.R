library(tidyverse)
library(readxl)
library(countrycode)
library(LaCroixColoR)
extrafont::loadfonts(device = "win")

whr20 <- read_excel("dia_01/WHR20_Data.xls") %>% 
  janitor::clean_names()


datos_tidy <- whr20 %>%
  select(country_name:ladder_score, contains("explained"), "dystopia_residual") %>% 
  pivot_longer(cols = -c(1:3), names_to = "variables", values_to = "valores") %>% 
  filter(regional_indicator == "Latin America and Caribbean") %>% 
  mutate(puntaje = round(ladder_score, 4),
         pais = countrycode(country_name, origin = 'country.name', destination = "un.name.es"),
         pais = case_when(pais == "Venezuela (República Bolivariana de)" ~ "Venezuela",
                          pais == "Bolivia (Estado Plurinacional de)" ~ "Bolivia",
                          TRUE ~ pais), 
         variables = str_remove_all(variables, "explained_by_"),
         variables = factor(variables, levels = c("dystopia_residual", "perceptions_of_corruption", "generosity", "freedom_to_make_life_choices",
                                                  "healthy_life_expectancy", "social_support", "log_gdp_per_capita")))


pal <- lacroix_palette("PassionFruit", n = 7, type = "continuous")

legendas <- c("Distopía (1.97) + residual", "Explicado por: Percepciones de corrupción", "Explicado por: Generosidad",
              "Explicado por: Libertad para tomar decisiones en la vida", "Explicado por: Esperanza de vida saludable",
              "Explicado por: Apoyo social", "Explicado por: PIB per cápita")

x11()
ggplot(datos_tidy, aes(valores, fct_reorder(pais, ladder_score))) +
  geom_col(aes(fill = variables), width = 0.5) +
  geom_text(aes(-0.5, pais, label = puntaje), size = 1.8, color = "gray90",
            family = "Roboto Condensed") +
  xlim(-0.5, 8) +
  labs(title = "¿Cuáles son los países más felices de \nAmérica Latina y el Caribe?",
       caption = "Fuente: World Happiness Report 2020",
       x = "Puntaje") + 
  scale_fill_manual(values = pal, guide = guide_legend(reverse = TRUE, ncol = 2), labels = legendas) +
  
  
  theme_minimal(base_family = "Roboto Condensed", base_size = 7) +
  theme(plot.background = element_rect(fill = "#252a32", color = "#252a32"),
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5, colour = "white", margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 5, colour = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(colour = "gray90", size = 5, hjust = 1),
        axis.text = element_text(colour = "gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 4, family = "Roboto Condensed Light", face = "bold", colour = "gray90"),
        legend.key.size = unit(.2, "cm"))

ggsave("dia_01/01_barras.png", height = 7, width = 5, units = "in", dpi = 300)



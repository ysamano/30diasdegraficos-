# Fuente: https://worldhappiness.report/

library(tidyverse)
library(countrycode)
library(LaCroixColoR)

whr20 <- readxl::read_excel("dia_01/WHR20_Data.xls") %>% 
  janitor::clean_names()


datos_tidy <- whr20 %>%
  select(-c(standard_error_of_ladder_score:ladder_score_in_dystopia)) %>% 
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

colores <- c("#CF266A", "#FBC84D", "#DE6C85", "#EC630F", "#008F77", "#4A4983", "#15448F")

legendas <- c("Distopía (1.97) + residual", "Percepciones de corrupción", "Generosidad",
              "Libertad para tomar decisiones en la vida", "Esperanza de vida saludable",
              "Apoyo social", "PIB per cápita")

p1 <- 
  ggplot(datos_tidy, aes(valores, fct_reorder(pais, ladder_score))) +
  geom_col(aes(fill = variables),
           width = 0.5) +
  geom_text(aes(-0.5, pais, label = puntaje), 
            size = 1.9, 
            color = "gray90",
            family = "Roboto Condensed") +
  xlim(-0.5, 8) +
  scale_fill_manual(values = colores, 
                    guide = guide_legend(reverse = TRUE, ncol = 3, title = "Explicado por:"), 
                    labels = legendas) +
  labs(title = "¿Cuáles son los países más felices de \nAmérica Latina y el Caribe?",
       caption = "Fuente: World Happiness Report 2020",
       x = "Puntaje") + 
  theme_ybn_b() +
  theme(axis.text.y = element_text(family = "Roboto Condensed"),
        axis.title.y = element_blank(),
        legend.key.size = unit(.3, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot")
  
ggsave("dia_01/01_barra.png", p1, height = 8, width = 5.5, units = "in", dpi = 300)



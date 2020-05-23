library(tidyverse)

rank_uni <- readxl::read_excel("dia_12/ranking_universidades.xlsx")

rank_uni <- rank_uni %>% 
  mutate(institucion = str_c(institucion, " (", locacion, ")"))


p1 <- 
  ggplot(rank_uni, aes(fct_reorder(institucion, puntaje), puntaje)) +
  geom_point(color = "#15448F", size = 3.2) +
  geom_segment(aes(y = 0,
                   x = institucion,
                   yend = puntaje, 
                   xend = institucion),
               color = "#15448F", size = 0.7) +
    geom_text(aes(y = puntaje + 7, label = puntaje), size = 2) +
  ylim(0, 110) +
  labs(title = "Chile tiene la mejor Universidad de América Latina",
       subtitle = "Las 20 mejores Universidades de América Latina en 2020",
       caption = "Fuente: QS World University Rankings Latin America Region 2020") +
  coord_flip() +
  theme_ybn() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 7, family = "Roboto Condensed"), 
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16, hjust = 1.3),
        plot.subtitle = element_text(size = 12, hjust = 2))


ggsave("dia_12/12_lollipop.png", p1, height = 8, width = 5.5, units = "in", dpi = 300)



library(tidyverse)

rank_uni <- readxl::read_excel("dia_12/ranking_universidades.xlsx")

rank_uni <- rank_uni %>% 
  mutate(institucion = str_c(institucion, " (", locacion, ")"))


p1 <- 
  ggplot(rank_uni, aes(fct_reorder(institucion, puntaje), puntaje)) +
  geom_point(color = "#15448f",
             size = 3.5) +
  geom_segment(aes(y = 0,
                   x = institucion,
                   yend = puntaje, 
                   xend = institucion),
               color = "#15448F",
               size = 1) +
    geom_text(aes(y = puntaje + 7,
                  label = puntaje),
              size = 3.5,
              family = "Roboto Condensed Light") +
  ylim(0, 110) +
  labs(title = "Chile tiene la mejor Universidad de América Latina",
       subtitle = "Las 20 mejores Universidades de América Latina en 2020",
       caption = "Fuente: QS World University Rankings Latin America Region 2020") +
  coord_flip() +
  theme_ybn_w(title_hjust = 0.5,
              subtitle_hjust = 0.5,
              axis_grid = F) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10,
                                   face = "bold"), 
        axis.text.x = element_blank())

ggsave(filename = "dia_12/12_lollipop.png",
       plot = p1, 
       type = "cairo",
       height = 11,
       width = 8.5,
       units = "in")


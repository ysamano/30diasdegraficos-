library(tidyverse)
library(ggridges)

peliculas <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")

peliculas <- peliculas %>% 
  mutate(genero = str_split(genero, pattern = ", ")) %>% 
  unnest()

p1 <- ggplot(peliculas) +
  geom_density_ridges(aes(x = puntaje, y = genero), 
                      scale = 3,
                      rel_min_height = 0.01,
                      alpha = 0.9,
                      size = 0.3,
                      color = "#E35526",
                      fill = "#E8724A") +
  labs(title = "¿Qué tipo de películas tienen en promedio mejores\ncalificaciones en IMDb?",
       caption = "Fuente: IMDb") +
  theme_ybn_w(base_family = "Roboto Condensed Light",
              title_hjust = 0.5,
              title_margin_b = 25) +
  theme(axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.y = element_blank())

ggsave(filename = "dia_07/07_ridges3.png",
       plot = p1, 
       type = "cairo",
       height = 11,
       width = 8.5,
       units = "in")

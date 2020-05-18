library(tidyverse)
library(ggridges)

peliculas <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")

peliculas <- peliculas %>% 
  mutate(genero = str_split(genero, pattern = ", ")) %>% 
  unnest()

p1 <- 
  peliculas %>% 
  ggplot() +
  geom_density_ridges(aes(x = puntaje, y = genero, fill = genero), 
                      scale = 3, rel_min_height = 0.01, alpha = 0.9, size = 0.3) +
  labs(title = "¿Qué tipo de películas tienen en promedio mejores\ncalificaciones en IMDb?",
       caption = "Fuente: IMDb") +
  scale_fill_cyclical(values = c("#006766", "#033f40")) +
  theme_ybn(colour_background = "#f2efe6", 
            base_colour = "#1e2831", 
            grid_colour = "gray80", 
            axis_text_colour = "gray10") +
  theme(axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank())

ggsave("dia_07/07_ridges.png", p1, height = 7, width = 5, units = "in", dpi = 300)

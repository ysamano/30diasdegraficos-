library(tidyverse)
library(patchwork)

millonarios <- read_csv("dia_16/millonarios.csv")


datos_select <- millonarios %>% 
  mutate(fortuna = fortuna/ 100) %>% 
  filter(rank < 6) %>% 
  select(nombre, fortuna)

#crear funcion para hacer coordenadas

crea_coord_wafle <- function(nombre, fortuna, columnas) {
  
  tibble(nombre = rep(nombre, fortuna),
         axis_x = rep(1:columnas, length = fortuna),
         axis_y = rep(1:100, each = columnas, length = fortuna) )
  
}

# creamos funcion para crear graficas

crear_waffle <- function(datos, axis_x, axis_y, titulo, tam_titulo = 10) {
  
  gg <- ggplot(datos, aes(axis_x, axis_y)) +
    geom_tile(colour = "#1b1f2b", fill = "#EE711E", size = 0.9) +
    labs(title = titulo) +
    coord_fixed() +
    scale_x_continuous(expand = c(0, 0)) +
    theme_void() +
    theme(plot.title = element_text(family = "Roboto Condensed",
                                    face = "bold",
                                    size = tam_titulo,
                                    colour = "white"))
  
  gg
  
}


datos_graf <- datos_select %>% 
  group_by(nombre) %>% 
  mutate(list(crea_coord_wafle(nombre, fortuna, columnas = 55))) %>% 
  unnest()

A <- datos_graf %>% 
  filter(nombre == "Jeff Bezos") %>% 
  crear_waffle(axis_x, axis_y, titulo = "Jeff Bezos")

B <- datos_graf %>% 
  filter(nombre == "Bill Gates") %>% 
  crear_waffle(axis_x, axis_y, titulo = "Bill Gates")

C <- datos_graf %>% 
  filter(nombre == "Bernard Arnault") %>% 
  crear_waffle(axis_x, axis_y, titulo = "Bernard Arnault")

D <- datos_graf %>% 
  filter(nombre == "Warren Buffett") %>% 
  crear_waffle(axis_x, axis_y, titulo = "Warren Buffett")

E <- datos_graf %>% 
  filter(nombre == "Larry Ellison") %>% 
  crear_waffle(axis_x, axis_y, titulo = "Larry Ellison")


p1 <- (A + B + C + D + E) +
  plot_layout(ncol = 1) +
  plot_annotation(title = "La fortuna de las cinco personas más ricas del mundo",
                  subtitle = "Cada recuadro representa $100,000,000 de dólares",
                  caption = "Fuente: Revista Forbes",
                  theme = theme_ybn(base_size = 10, plot_title_size = 18))

ggsave("dia_16/15_waffle.png", p1, height = 10, width = 5.4, units = "in", dpi = 300)




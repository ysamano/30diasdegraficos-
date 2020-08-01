library(tidyverse)
library(lubridate)

precios <- readxl::read_excel("dia_13/precios_petroleo_oro.xlsx")

precios_tidy <- precios %>% 
  pivot_longer(cols = 2:3, names_to = "comodity", values_to = "precio") %>% 
  filter(precio != "null") %>% 
  mutate(precio = as.double(precio),
         fecha = ymd(fecha))

colores <- c("#F7C147", "#151613")

p1 <- 
  ggplot(precios_tidy, aes(fecha, precio, colour = comodity)) +
  geom_area(aes(fill = comodity), alpha = 0.9) +
  facet_wrap( ~ comodity, nrow = 2, scales = "free") +
  labs(title = "Diez años de subidas y bajadas en el precio del Oro y el Petróleo",
       subtitle = "Evolución diaria del precio del oro y del petróleo, 2010-2020",
       caption = "Fuente: Yahoo Finance") +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 50)) + #50/365
  scale_y_continuous(labels =  scales::label_dollar(), expand = c(0, 1)) +
  theme_ybn_w()+
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

ggsave("dia_13/13_temporal2.png", p1, height = 8.5, width = 11, units = "in", type = "cairo")

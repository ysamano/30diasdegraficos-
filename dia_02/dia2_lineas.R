library(tidyverse)
library(ggtext)


esp_vida <- readxl::read_excel("dia_02/esperanza_vida.xlsx")

esperanza_tidy <- esp_vida %>% 
  pivot_longer(cols = 3:61, names_to = "anios", values_to = "valores") %>%
  mutate(anios = as.double(anios),
         colores = case_when(pais == "Camboya" ~ "Camboya",
                             pais == "Rwanda" ~ "Rwanda",
                             TRUE ~ "Otros"))


x11()
ggplot(esperanza_tidy, aes(anios, valores, group = pais)) +
  geom_line(size = 0.1, colour = "gray70") +
  geom_line(data = esperanza_tidy %>% filter(pais %in% c("Camboya", "Rwanda")),
            aes(anios, valores, group = pais,  colour = colores), size = 0.6) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(breaks = seq(0, 90, by = 20), limits = c(0, 90)) +
  scale_color_manual(values = c("#ffc50c", "#ec640f")) +
  labs(title = "El horror del genocidio en la esperanza de vida <br>de las mujeres de 
       <span style='color:#ffc50c'>**Camboya**</span> y <span style='color:#ec640f'>**Rwanda**</span>", 
       caption = "Fuente: Banco Mundial",
       x = "Año",
       y = "Esperanza de vida de mujeres en años") + 
  theme_minimal(base_size = 7, base_family = "Roboto Condensed Light") +
  theme(plot.background = element_rect(fill = "#1b1f2b", color = "#1b1f2b"),
        plot.title = element_markdown(family = "Roboto Condensed", colour = "white",
                                  size = 12,  margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 6, colour = "white"),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.major.y = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.y = element_line(colour = "gray30", size = 0.1),
        axis.title = element_text(colour = "gray90", hjust = 1),
        axis.text = element_text(colour = "gray90"),
        legend.position = "none")

ggsave("dia_02/02_lineas.png", height = 7, width = 5, units = "in", dpi = 400)







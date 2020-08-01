# Fuente: https://datos.bancomundial.org/indicador/SH.MMR.DTHS

library(tidyverse)
library(ggtext)

muertes <- readxl::read_excel("dia_09/muertes_maternas.xlsx", skip = 3)

niveles = c("NAC", "ECS", "MEA", "LCN", "EAS",  "SAS", "SSF")

muertes_mat <- muertes %>% 
  pivot_longer(cols = 5:64, names_to = "anio", values_to = "num_muertes") %>% 
  filter(!is.na(num_muertes)) %>% 
  janitor::clean_names() %>% 
  filter(country_code %in% niveles) %>% 
  mutate(anio = as.double(anio),
         country_code =factor(country_code, levels = niveles))

nota <- "Nota: La muerte materna es definida como la muerte de una mujer embarazada o dentro de los 
        42 días siguientes a la terminación del embarazo, independientemente de la duración y el 
        sitio del embarazo, debida a cualquier causa relacionada <br>o agravada con el embarazo mismo o su 
        atención pero no por causas accidentales o incidentales.
        <br>Fuente: Banco Mundial"

colores <- c("#79788a", "#905F4F", "#82A2BA", "#E8724A", "#699478", "#EBD991", "#56658C")


p1 <- ggplot(muertes_mat) +
  geom_area(aes(anio, num_muertes, fill = country_code)) +
  scale_y_continuous(labels = scales::comma,
                     n.breaks = 10,
                     expand = c(0.01, 0)) +
  scale_x_continuous(n.breaks = 10,
                     expand = c(0.01, 0)) +
  scale_fill_manual(values = colores,
                    guide = guide_legend(nrow = 1),
                    labels = c("América del Norte",
                               "Europa y Asia Central",
                               "Medio Oriente y Norte de África",
                               "América Latina y el Caribe",
                               "Asia Oriental y el Pacífico",
                               "Asia del Sur",
                               "África Subsahariana")) +
  labs(title = "Evolución de muertes maternas en el mundo",
       subtitle = "Número de muertes maternas por region, 2000-2017",
       caption = nota) +
  theme_ybn_w() +
  theme(axis.title = element_blank(),
        legend.position = "top",
        legend.key.size = unit(0.35, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        plot.caption = element_markdown(size = 8,
                                        hjust = 0))
ggsave(filename = "dia_09/09_area.png",
       plot = p1, 
       type = "cairo",
       height = 8.5,
       width = 11,
       units = "in")

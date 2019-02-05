library(readr)
library(tsibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)

tasa <- read_csv("tasa_interes_prestamos_personales/data/tasa_interes_prestamos_personales.csv") %>% 
  mutate(key = "BCRA", #required for tsibble
         fecha = dmy(Fecha),
         valor = Valor / 100) %>% 
  select(key, fecha, valor) %>%
  as_tsibble(key=id(key), index=fecha) %>% 
  index_by(year_week = yearweek(fecha)) %>% 
  summarise(tasa_semanal = mean(valor))

ggplot(tasa) +
  geom_line(aes(x=year_week, y=tasa_semanal), color="steelblue")  +
  transition_reveal(year_week) +
  labs(title="Evolucion Tasa de Interes Prestamos Personales",
       subtitle="Según datos BCRA.",
       caption = "Elaborado por @pdelboca en base a datos del BCRA." ,
       x = "Fecha", y = "% Tasa Interes") +
  scale_y_continuous(breaks=seq(0, 0.7, 0.1), labels = scales::percent, minor_breaks = NULL, limits = c(0,0.7)) +
  scale_x_date(breaks = "1 year", date_labels ="%Y", minor_breaks = NULL) +
  theme_minimal()

anim_save("tasa_interes_prestamos_personales/tasa_de_interes_prestamos_personales.gif")
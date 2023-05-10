library(readr)
library(tidyverse)
library(geobr)
library(ggplot2)
library(sf)

#Mapa ----
municipios <- read_municipality(code_muni = "all", year = 2018)
municipios <- readRDS(file = "municipios.rds")


df <- read_csv("base.csv") %>% 
  rename("code_muni" = "id_municipio") %>% 
  mutate(incluso = ifelse(distancia < 150, 1, 0),
         grupo = ifelse((tratamento == 1 & incluso == 1), "Tratamento", 
                        ifelse((tratamento == 0 & incluso == 1), "Controle", "Não incluso")))

data <- left_join(municipios, df, by = "code_muni")


gg <- ggplot() +
  geom_sf(data = data, aes(fill = grupo), color = "#B3DBCF", lwd = .001) +
  scale_fill_manual(values = c("Tratamento" = "#5C5B60",
                                "Controle"="#D28673",
                                "Não incluso"= "#B3DBCF"))+
  theme_void()
gg
ggsave("mapa.png", gg, dpi = 600)

#RDD

df <- read_csv("base_abstencao.csv") %>% 
  mutate(distancia = ifelse(tratamento == 1, -distancia, distancia)) %>% 
  filter(ano == 2018)

ggplot(df, aes(x = distancia, y = abstencao, size = populacao)) +
  geom_point(alpha = .1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(c(-150,150))


#Tendências ----


df <- read_csv("base_abstencao.csv") %>% 
  filter(incluso == 1) %>% 
  select(id_municipio, ano, turno, tratamento, abstencao) %>% 
  group_by(ano, turno, tratamento) %>%
  summarise(media = mean(abstencao),
            n = n(),
            sd = sd(abstencao)) %>% 
    mutate(ci = qt(.995, n) * sd / sqrt(n),
    turno = ifelse(turno == 1, "Primeiro Turno", "Segundo Turno"),
         tratamento= ifelse(tratamento == 1, "Tratamento", "Controle"))


gg <- ggplot(df, aes(x = ano, y = media, color = tratamento)) +
  facet_wrap("turno") +
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width=.5) +
  geom_line(lwd = .8)+
  geom_point(size = 2)+
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018, 2022)) +
  labs(color = "Grupo", x = "Ano", y = "Abstenção", 
       title = "Abstenção para cada grupo ao longo das eleições") +
  theme(text = element_text(family = "A")) +
  scale_color_manual(values = c("Tratamento" = "#5C5B60",
                                "Controle"="#D28673")) +
  theme_bw()
  
gg

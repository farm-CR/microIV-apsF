library(sf)
library(geobr)
library(tidyverse)

states <- read_state(code_state = "all", showProgress = TRUE)
# states_hd <- read_state(code_state = "all", showProgress = TRUE, simplified = FALSE)
save(states, file = "states.RData")
# save(states_hd, file = "states_hd.RData")

muni <- read_municipality(code_muni = "all", showProgress = TRUE)
# muni_hd <- read_municipality(code_muni = "all", showProgress = TRUE, simplified = FALSE)
save(muni, file = "muni.RData")
# save(muni_hd, file = "muni_hd.RData")

load("states.RData")
load("muni.RData")

states_perim <- st_cast(states, "MULTILINESTRING")

pr_ms <- st_intersection(states_perim %>% filter(abbrev_state %in% c("PR", "MS")))
sp_ms <- st_intersection(states_perim %>% filter(abbrev_state %in% c("SP", "MS")))
mg_ms <- st_intersection(states_perim %>% filter(abbrev_state %in% c("MG", "MS")))
go_ms <- st_intersection(states_perim %>% filter(abbrev_state %in% c("GO", "MS")))
go_mt <- st_intersection(states_perim %>% filter(abbrev_state %in% c("GO", "MT")))
to_mt <- st_intersection(states_perim %>% filter(abbrev_state %in% c("TO", "MT")))
pa_mt <- st_intersection(states_perim %>% filter(abbrev_state %in% c("PA", "MT")))
pa_am <- st_intersection(states_perim %>% filter(abbrev_state %in% c("PA", "AM")))
pa_rr <- st_intersection(states_perim %>% filter(abbrev_state %in% c("PA", "RR")))

boundary <- pr_ms %>%
  bind_rows(sp_ms, mg_ms, go_ms, go_mt, to_mt, pa_mt, pa_am, pa_rr) %>%
  filter(n.overlaps != 1)
boundary <- boundary$geom %>%
  st_combine(.) %>%
  st_as_sf(.) %>%
  st_transform(., crs = 4326) %>% 
  st_cast(., "MULTIPOINT")
save(boundary, file = "boundary.RData")

### MAPAS

boundary <- pr_ms %>%
  bind_rows(sp_ms, mg_ms, go_ms, go_mt, to_mt) %>%
  filter(n.overlaps != 1)
boundary <- boundary$geom %>%
  st_combine(.) %>%
  st_as_sf(.) %>%
  st_transform(., crs = 4326) %>% 
  st_cast(., "MULTIPOINT")

df <- read_csv("data/data.csv") %>% 
  left_join(read_csv("data/municipios.csv")) %>% 
  filter(fuso_horario == "America/Sao_Paulo" | fuso_horario == "America/Porto_Velho") %>% 
  mutate(tratamento = ifelse(fuso_horario == "America/Porto_Velho", 1, 0))
df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
df_dist <- df %>% 
  filter(ano == 2022 & turno == 1) %>% 
  mutate(distancia = as.numeric(st_distance(., boundary)),
         distancia = ifelse(tratamento, distancia * -1, distancia))
df <- df %>% 
  st_join(df_dist %>% select(id_municipio, distancia))

states_mapa <- states %>% 
  filter(abbrev_state %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO")) %>% 
  st_transform(., crs = 4326)
muni_mapa <- df_dist %>% 
  filter(uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO"),
         abs(distancia) <= 250000) %>% 
  st_transform(., crs = 4326)

ggplot() +
  geom_sf(data = states_mapa) +
  geom_sf(data = muni_mapa) +
  geom_sf(data = boundary, colour="#1B8546", size = 1.5) +
  ggthemes::theme_map()
ggsave("plots/boundary.png", dpi = 600, width = 20, height = 20, units = c("cm"))


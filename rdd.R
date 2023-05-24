library(sf)
library(tidyverse)
library(SpatialRDD)
library(rdrobust)

load("states.RData")
load("boundary.RData")

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
  st_join(df_dist %>% select(id_municipio, distancia)) %>% 
  mutate(abstencao = log(abstencao))
  # filter(populacao >= 5000)

df.rdd <- df %>% 
  filter(ano == 2022, turno == 1,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO"))

rdbwselect(df.rdd$abstencao, df.rdd$distancia, c = 0) %>% summary()
bandwidth <- 250000

###########################################
#                                         #
#       GRDD anual (22, 18, 14, 10)       #
#                                         #
###########################################

df.rdd <- df %>% 
  filter(ano == 2022, turno == 1, abs(distancia) < bandwidth,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO"))

rdrobust(df.rdd$abstencao, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$abstencao, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "abstencao", x.label = "distance to border")

boundary_points <- discretise_border(boundary)
boundary_points <- boundary_points[seq(1, nrow(boundary_points), round(nrow(boundary_points) / 750, 0)), ]
boundary_points$id <- 1:nrow(boundary_points)

fit <- spatialrd(y = "abstencao", data = df.rdd, cutoff.points = boundary_points,
                 treated = "tratamento", minobs = 10, spatial.object = TRUE)
plotspatialrd(fit)

############################## 
#          PLACEBOS          #
##############################

### 2018
df.rdd <- df %>% 
  filter(ano == 2018, turno == 1, abs(distancia) < 200000,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO"))

rdrobust(df.rdd$abstencao, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$abstencao, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "abstencao", x.label = "distance to border")

fit <- spatialrd(y = "abstencao", data = df.rdd, cutoff.points = boundary_points,
                 treated = "tratamento", minobs = 10, spatial.object = TRUE)
plotspatialrd(fit)

### 2014
df.rdd <- df %>% 
  filter(ano == 2014, turno == 1, abs(distancia) < 200000,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO"))

rdrobust(df.rdd$abstencao, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$abstencao, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "abstencao", x.label = "distance to border")

fit <- spatialrd(y = "abstencao", data = df.rdd, cutoff.points = boundary_points,
                 treated = "tratamento", minobs = 10, spatial.object = TRUE)
plotspatialrd(fit, map = TRUE)

### 2010
df.rdd <- df %>% 
  filter(ano == 2010, turno == 1, abs(distancia) < 200000,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO"))

rdrobust(df.rdd$abstencao, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$abstencao, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "abstencao", x.label = "distance to border")

fit <- spatialrd(y = "abstencao", data = df.rdd, cutoff.points = boundary_points,
                 treated = "tratamento", minobs = 10, spatial.object = TRUE)
plotspatialrd(fit)


##########################################
#                                        #
#      GRDD delta                        #
#      (18-22, 14-18, 10-14, 14-22)      #
#                                        #
##########################################

# 255571.081
df.rdd <- df %>% 
  filter(ano %in% c(2018, 2022), turno == 1,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO")) %>% 
  arrange(id_municipio.x, ano) %>% 
  mutate(delta = abstencao - lag(abstencao)) %>% 
  filter(ano == 2022)

### RESULTADO PRINCIPAL NAIVE
rdrobust(df.rdd$delta, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$delta, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "Delta da Abstencao", x.label = "Distancia")$rdplot +
  labs(title = "RDD Naive") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )
ggsave("plots/naive_250_18-22.png", bg = "transparent", dpi = 600,
       units = c("cm"), width = 20, height = 14)

### RESULTADO PRINCIPAL GEOGRAFICO
boundary_points <- discretise_border(boundary)
boundary_points <- boundary_points[seq(1, nrow(boundary_points), round(nrow(boundary_points) / 750, 0)), ]
boundary_points$id <- 1:nrow(boundary_points)

fit.delta <- spatialrd(y = "delta", data = df.rdd, cutoff.points = boundary_points,
                       treated = "tratamento", minobs = 10, spatial.object = TRUE,
                       bwfix_m = bandwidth)
plotspatialrd(fit.delta)
  

############################## 
#          PLACEBOS          #
##############################

### 2014 - 2018
df.rdd <- df %>% 
  filter(ano %in% c(2014, 2018), turno == 1,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO")) %>% 
  arrange(id_municipio.x, ano) %>% 
  mutate(delta = abstencao - lag(abstencao)) %>% 
  filter(ano == 2018)

rdrobust(df.rdd$delta, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$delta, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "Delta da Abstencao", x.label = "Dist?ncia")

fit.14_18 <- spatialrd(y = "delta", data = df.rdd, cutoff.points = boundary_points,
                       treated = "tratamento", minobs = 10, spatial.object = TRUE,
                       bwfix_m = bandwidth)
plotspatialrd(fit.14_18)

### 2010 - 2014
df.rdd <- df %>% 
  filter(ano %in% c(2010, 2014), turno == 1,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO")) %>% 
  arrange(id_municipio.x, ano) %>% 
  mutate(delta = abstencao - lag(abstencao)) %>% 
  filter(ano == 2014)

rdrobust(df.rdd$delta, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$delta, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "Delta da Abstencao", x.label = "Dist?ncia")

fit.10_14 <- spatialrd(y = "delta", data = df.rdd, cutoff.points = boundary_points,
                       treated = "tratamento", minobs = 10, spatial.object = TRUE,
                       bwfix_m = bandwidth)
plotspatialrd(fit.10_14)

### 2014 - 2022
df.rdd <- df %>% 
  filter(ano %in% c(2014, 2022), turno == 1,
         uf %in% c("MS", "MT", "PR", "SP", "MG", "GO", "TO")) %>% 
  arrange(id_municipio.x, ano) %>% 
  mutate(delta = abstencao - lag(abstencao)) %>% 
  filter(ano == 2022)

rdrobust(df.rdd$delta, df.rdd$distancia, c = 0) %>% summary()
rdplot(df.rdd$delta, df.rdd$distancia, c = 0, ci = 95, 
       kernel = "triangular", y.label = "Delta da Abstencao", x.label = "Dist?ncia")

fit.14_22 <- spatialrd(y = "delta", data = df.rdd, cutoff.points = boundary_points,
                       treated = "tratamento", minobs = 10, spatial.object = TRUE,
                       bwfix_m = 255571.081)
plotspatialrd(fit.14_22)

### GRAFICOS
fit.delta %>% 
  mutate(rob_sig = p_Conv <= 0.05) %>% 
  ggplot(aes(Point)) +
  geom_errorbar(aes(ymin = CI_Conv_l, ymax = CI_Conv_u), color = "#05732F", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  geom_point(aes(Point, Estimate, color = rob_sig), size = .7) +
  scale_color_manual(values = c("#DB2B39", "#22577A")) +
  xlab("Ponto da Fronteira") +
  ylab("Estimativa do Ponto") +
  labs(title = "RDD Geográfico - Diferença do log abstenção 2018-2022") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), #transparent legend panel
    legend.position = "none"
  )
ggsave("plots/delta_250_18-22.png", bg = "transparent", dpi = 600,
       units = c("cm"), width = 22, height = 14)

fit.14_18 %>% 
  mutate(rob_sig = p_Conv <= 0.05) %>% 
  ggplot(aes(Point)) +
  geom_errorbar(aes(ymin = CI_Conv_l, ymax = CI_Conv_u), color = "#05732F", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  geom_point(aes(Point, Estimate, color = rob_sig), size = .7) +
  scale_color_manual(values = c("#DB2B39", "#22577A")) +
  xlab("Ponto da Fronteira") +
  ylab("Estimativa do Ponto") +
  labs(title = "RDD Geográfico - Diferença do log abstenção 2014-2018") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), #transparent legend panel
    legend.position = "none"
  )
ggsave("plots/delta_250_14-18.png", bg = "transparent", dpi = 600,
       units = c("cm"), width = 18, height = 7)

fit.10_14 %>% 
  mutate(rob_sig = p_Conv <= 0.05) %>% 
  ggplot(aes(Point)) +
  geom_errorbar(aes(ymin = CI_Conv_l, ymax = CI_Conv_u), color = "#05732F", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  geom_point(aes(Point, Estimate, color = rob_sig), size = .7) +
  scale_color_manual(values = c("#DB2B39", "#22577A")) +
  xlab("Ponto da Fronteira") +
  ylab("Estimativa do Ponto") +
  labs(title = "RDD Geográfico - Diferença do log abstenção 2010-2014") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), #transparent legend panel
    legend.position = "none"
  )
ggsave("plots/delta_250_10-14.png", bg = "transparent", dpi = 600,
       units = c("cm"), width = 18, height = 7)

fit.14_22 %>% 
  mutate(rob_sig = p_Conv <= 0.05) %>% 
  ggplot(aes(Point)) +
  geom_errorbar(aes(ymin = CI_Conv_l, ymax = CI_Conv_u), color = "#05732F", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  geom_point(aes(Point, Estimate, color = rob_sig), size = .7) +
  scale_color_manual(values = c("#DB2B39", "#22577A")) +
  xlab("Ponto da Fronteira") +
  ylab("Estimativa do Ponto") +
  labs(title = "RDD Geográfico - Diferença do log abstenção 2014-2022") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), #transparent legend panel
    legend.position = "none"
  )
ggsave("plots/delta_250_14-22.png", bg = "transparent", dpi = 600,
       units = c("cm"), width = 22, height = 14)


# df.rdd$segment <- border_segment(df.rdd, boundary_points, 10)
#
# feols(log(abstencao) ~ 
#         log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
#         log(pib_governo) + log(eleitores_secao) + i(ano, tratamento, ref = 2018) | segment + ano, 
#       data = df.rdd) %>% summary()
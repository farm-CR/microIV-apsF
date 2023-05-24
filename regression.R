library(tidyverse)
library(fixest)
library(modelsummary)
library(MatchIt)
library(haven)
library(estimatr)
library(rdrobust)
library(rddensity)
library(rdd)


df <- read_csv("data.csv") 
df <- df %>% 
  mutate(incluso = ifelse(distancia < 150, 1, 0)) %>% 
  filter(incluso == 1)

#Teste placebo----
modelo_placebo <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + i(ano, tratamento, ref= 2006) | id_municipio + ano

modelsummary(list(
  feols(modelo_placebo, data = df %>% filter(turno == 1 & ano != 2022)),
  feols(modelo_placebo, data = df %>% filter(turno == 2 & ano != 2022))
), stars = TRUE)

#Propensity score matching ----
df.psm <- read_csv("psm.csv") %>% 
  left_join(df %>% filter(ano == 2022) %>% select(id_municipio, tratamento, turno)) %>% 
  left_join(df %>% filter(ano == 2018) %>% select(id_municipio, abstencao, turno) %>% rename("abstencao_2018" = "abstencao")) %>% 
  left_join(df %>% filter(ano <2022) %>% group_by(id_municipio) %>% 
              summarize(abstencao = mean(abstencao), competitividade = mean(competitividade), pib_pc = mean(pib_pc),
                        beneficiados = mean(beneficiados), pib_governo = mean(pib_governo),
                        eleitores_secao = mean(eleitores_secao), tratamento = max(tratamento))) %>% 
  mutate(populacao_urbana = populacao_urbana/populacao) %>% 
  drop_na()

modelo_psm <- tratamento ~ razao_dependencia + taxa_envelhecimento + expectativa_anos_estudo + 
  taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + taxa_agua_encanada + log(populacao) + populacao_urbana + abstencao_2018 +
  log(abstencao) + log(competitividade) + log(pib_pc) + log(beneficiados) + pib_governo + eleitores_secao

#Estima??o do propensity
summary(glm(modelo_psm, data = df.psm, family = binomial(link = 'logit')))

modelo_psm <- tratamento ~ taxa_envelhecimento + taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + log(populacao) + populacao_urbana + log(abstencao) + log(pib_pc) + log(beneficiados) + eleitores_secao + abstencao_2018

match.1t <- matchit(
  modelo_psm,
  data=df.psm %>% filter(turno == 1), link="probit", replace = T) 
match.2t <- matchit(
  modelo_psm,
  data=df.psm %>% filter(turno == 2), link="probit", replace = T) 

#Resultado do balanceamento
plot(match.1t,type="density",interactive=FALSE)
plot(match.2t,type="density",interactive=FALSE)

df.1t <- match.1t %>%  
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 1)) %>% 
  select(id_municipio) %>% 
  left_join(df %>% filter(turno == 1))
df.2t <- match.2t %>%  
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 2)) %>% 
  select(id_municipio) %>% 
  left_join(df %>% filter(turno == 2))

bind_rows(df.1t, df.2t) %>% 
  write_csv("output/data_psm.csv")

#Teste placebo depois do PSM
modelsummary(list(
  feols(modelo_placebo, data = df.1t %>% filter(ano != 2022)),
  feols(modelo_placebo, data = df.2t %>% filter(ano != 2022))
))


#Estima??o do DD----
modelo <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + tratamento : (ano == 2022) | id_municipio + ano

modelo <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + i(ano, tratamento, ref = 2018) | id_municipio + ano

feols.1t <- feols(modelo, data = df.1t %>% filter(ano == 2018 | ano == 2022))
feols.2t <- feols(modelo, data = df.2t)


modelsummary(list(
  feols.1t,
  feols.2t
), stars = T)
summary(feols.1t)
summary(feols.2t)

iplot(feols.1t <- feols(modelo, data = df.1t %>% filter(ano == 2018 | ano == 2022)))





df <- read_csv("data.csv") 

df_r1 <- df %>% 
  filter(distancia < 150, turno == 2) %>% 
  mutate(distancia = ifelse(tratamento, distancia * -1, distancia))

df_r1 %>% 
  group_by(ano, tratamento) %>% 
  summarise(abs = mean(abstencao)) %>% 
  ggplot(aes(ano, abs, color = tratamento)) +
  # facet_wrap(~tratamento) +
  geom_line()




df_r2 <- df %>% 
  filter(distancia < 150, turno == 1, ano %in% c(2018, 2022)) %>% 
  mutate(distancia = ifelse(tratamento, distancia * -1, distancia)) %>% 
  arrange(id_municipio) %>% 
  mutate(delta = abstencao - lag(abstencao)) %>% 
  filter(ano == 2022)

fit <- lm_robust(delta ~ distancia * tratamento, 
                 data = df_r2)
summary(fit)

modelplot(fit)


ggplot(df_r2, aes(distancia, delta, color = tratamento)) +
  geom_point() +
  geom_hline(yintercept = 0)


rdr <- rdrobust(y = df_r2$delta,
                x = df_r2$distancia)
summary(rdr)

DCdensity(df_r2$distancia)

density <- rddensity(df_r2$distancia)
rdplotdensity(density, df_r2$distancia)

rdplot(y = df_r2$delta, x = df_r2$distancia, ci = 95, kernel = "triangular")






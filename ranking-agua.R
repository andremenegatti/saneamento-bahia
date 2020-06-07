library(magrittr)
library(tidyverse)
library(sp)
library(sf)
source('helpers.R')
theme_set(custom_theme())

# Loading datasets ------------------------------------------------------------
polygons_municipios_bahia <- readRDS('data/polygons_municipios_bahia.rds')
snis <- readRDS('data/snis-bahia.rds')

# Data wrangling --------------------------------------------------------------
snis_agua <- snis %>% filter(tipo_servico != 'Esgotos')

snis_agua <- snis_agua %>% 
  right_join(polygons_municipios_bahia %>% st_drop_geometry(),
             by = 'codigo_municipio') %>% 
  mutate(
    nat_jur_simplified2 = case_when(
      sigla_prestador == 'EMBASA' ~ 'EMBASA',
      natureza_juridica == 'Autarquia' ~ 'Autarquia municipal',
      natureza_juridica == 'Administração pública direta' ~ 'Prefeitura municipal',
      natureza_juridica == 'Empresa pública' ~ 'Empresa pública municipal',
      natureza_juridica == 'Sem dados' ~ 'Sem dados',
      is.na(natureza_juridica) ~ 'Sem dados',
      municipio == 'Itabuna' ~ 'Empresa pública municipal')
  )

snis_agua <- snis_agua %>% 
  mutate(nat_jur_simplified2 = 
           ifelse(agua_embasa_e_prefeitura,
                  'EMBASA + Prefeitura', nat_jur_simplified2)) %>% 
  mutate(nat_jur_simplified = case_when(
    str_detect(nat_jur_simplified2, 'EMBASA') ~ 'EMBASA',
    str_detect(nat_jur_simplified2, 'municipal') ~ 'Adm. pública municipal'
  ) %>% fct_relevel('EMBASA', 'Adm. pública municipal'))

# Ranking ---------------------------------------------------------------------
snis_ranking <- snis_agua %>% 
  select(municipio,
         tipo_prestador = nat_jur_simplified,
         populacao = pop2017,
         atendimento = in055_indice_de_atendimento_total_de_agua,
         tarifa_agua = in005_tarifa_media_de_agua,
         perdas = in013_indice_de_perdas_faturamento,
         inv_per_capita = inv_per_capita,
         desempenho = in012_indicador_de_desempenho_financeiro
         ) %>% 
  drop_na() %>% 
  mutate(perdas = ifelse(perdas > 0, perdas, 0)) %>% # <----
  mutate(
    tarifa_agua_norm = (max(tarifa_agua) - tarifa_agua) / 
      (max(tarifa_agua) - min(tarifa_agua)) * 100,
    inv_per_capita_norm = (inv_per_capita - min(inv_per_capita)) /
      (max(inv_per_capita) - min(inv_per_capita)) * 100,
    perdas_norm = (perdas - min(perdas)) /
      (max(perdas) - min(perdas)) * 100,
    desempenho_norm = (desempenho - min(desempenho)) / 
      (max(desempenho) - min(desempenho)) * 100,
    # Score as average of normalized indicators
    score = (atendimento + tarifa_agua_norm + inv_per_capita_norm +
      perdas_norm + desempenho_norm) / 5
  ) %>% 
  mutate(ranking = dense_rank(desc(score))) %>% 
  arrange(ranking)

# Table with top 10 and worst 10 cities ---------------------------------------
highlights_agua <- snis_ranking %>% 
  select(ranking, municipio, score,
         tipo_prestador, populacao, atendimento,
         tarifa_agua, perdas, inv_per_capita , desempenho) %>% 
  slice(c(1:10, (nrow(snis_ranking) - 9):(nrow(snis_ranking)))) %>% 
  mutate_at(.vars = vars(inv_per_capita, desempenho),
            .funs = partial(round, digits = 2)) %T>% 
  write.csv(file = 'data/highlights_agua.csv') ; highlights_agua

# Plotting score distribution -------------------------------------------------
salvador <- snis_ranking %>% filter(municipio == 'Salvador') %>% pull(score)
height_salvador <- 25
mediana <- snis_ranking$score %>% median()
height_mediana <- 10
# juazeiro <- snis_ranking %>% filter(municipio == 'Juazeiro') %>% pull(score)

hist_ranking_agua <- snis_ranking %>% 
  ggplot() +
  geom_histogram(aes(x = score, fill = tipo_prestador), bins = 30) +
  geom_path(
    data = tibble(x = rep(salvador, 2),y = c(0, height_salvador)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = salvador, y = height_salvador, color = 'gray15', fill = 'gray97',
    label = str_c('Salvador:\n', round(salvador, 1)) %>% 
      str_replace('\\.', ','),
    family = 'serif', size = 3.5
  ) +
  geom_path(
    data = tibble(x = rep(mediana, 2),y = c(0, height_mediana)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = mediana, y = height_mediana, color = 'gray15', fill = 'gray97',
    label = str_c('Mediana:\n', round(mediana, 1)) %>% 
      str_replace('\\.', ','),
    family = 'serif', size = 3.5
  ) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.17, .90)) +
  scale_fill_manual(values = c('#fed976', '#ef3b2c')) +
  scale_x_continuous(breaks = seq(20, 70, by = 10)) +
  coord_cartesian(xlim = c(20, 70)) +
  labs(
    x = 'Média dos indicadores normalizados (0 a 100)',
    y = 'Número de municípios',
    title = 'Qualidade dos serviços de fornecimento de água',
    subtitle = 'Distribuição das notas dos municípios baianos'
  ) ; hist_ranking_agua

# Saving
ggsave(plot = hist_ranking_agua, width = 6, height = 5,
       filename = 'plots/histogram-ranking-agua.png')

# Average by provider type
snis_ranking %>% 
  group_by(tipo_prestador) %>% 
  summarise(media_ponderada = weighted.mean(score, w = populacao),
            media_simples = mean(score)) %>% 
  ungroup()
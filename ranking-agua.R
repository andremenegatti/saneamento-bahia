library(tidyverse)
source('helpers/custom_plot_theme.R')
theme_set(custom_theme())

# Loading datasets ------------------------------------------------------------
snis_agua <- readRDS('data/snis-agua-clean.rds')

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
  mutate(
    perdas_trunc = ifelse(perdas > 0, perdas, 0),
    perdas_trunc_norm = (max(perdas_trunc) - perdas_trunc) /
      (max(perdas_trunc) - min(perdas_trunc)) * 100,
    perdas_norm = (max(perdas) - perdas) /
      (max(perdas) - min(perdas)) * 100,
    tarifa_agua_norm = (max(tarifa_agua) - tarifa_agua) / 
      (max(tarifa_agua) - min(tarifa_agua)) * 100,
    inv_per_capita_norm = (inv_per_capita - min(inv_per_capita)) /
      (max(inv_per_capita) - min(inv_per_capita)) * 100,
    desempenho_norm = (desempenho - min(desempenho)) / 
      (max(desempenho) - min(desempenho)) * 100,
    # Score as average of normalized indicators
    score = (atendimento + tarifa_agua_norm + inv_per_capita_norm +
               perdas_norm + desempenho_norm) / 5,
    score_perdas_trunc = (atendimento + tarifa_agua_norm + inv_per_capita_norm +
                perdas_trunc_norm + desempenho_norm) / 5
  ) %>% 
  mutate(
    ranking = dense_rank(desc(score)),
    ranking_perdas_trunc = dense_rank(desc(score_perdas_trunc))
    ) %>% 
  arrange(ranking)

# Saving results
snis_ranking %>% 
  select(municipio, ranking, #ranking_perdas_trunc,
         score, #score_perdas_trunc,
         tipo_prestador, populacao,
         desempenho, desempenho_norm, inv_per_capita, inv_per_capita_norm,
         tarifa_agua, tarifa_agua_norm, perdas, perdas_norm, perdas_trunc_norm) %>% 
  write_excel_csv2('data/ranking/ranking-agua-383municipios.csv')

# Table with top 10 and worst 10 cities ---------------------------------------
highlights_agua <- snis_ranking %>% 
  select(ranking, municipio, score,
         tipo_prestador, populacao, atendimento,
         tarifa_agua, perdas, inv_per_capita , desempenho) %>% 
  slice(c(1:10, (nrow(snis_ranking) - 9):(nrow(snis_ranking)))) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = partial(round, digits = 2)) ; highlights_agua

# Persisting highlights (csv and rds files)
write_excel_csv2(highlights_agua, 'data/ranking/highlights-agua.csv')
saveRDS(highlights_agua, 'data/ranking/highlights-agua.rds')

# Plotting score distribution -------------------------------------------------
salvador <- snis_ranking %>% filter(municipio == 'Salvador') %>% pull(score)
height_salvador <- 10
mediana <- snis_ranking$score %>% median()
height_mediana <- 20

hist_ranking_agua <- snis_ranking %>% 
  ggplot() +
  geom_histogram(aes(x = score, fill = tipo_prestador), bins = 30) +
  geom_path(
    data = tibble(x = rep(mediana, 2),y = c(0, height_mediana)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = mediana, y = height_mediana, color = 'gray15', fill = 'gray97',
    label = str_c('Mediana:\n', round(mediana, 1)) %>% 
      str_replace('\\.', ','),
    family = 'serif', size = 3
  ) +
  geom_path(
    data = tibble(x = rep(salvador, 2),y = c(0, height_salvador)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = salvador, y = height_salvador, color = 'gray15', fill = 'gray97',
    label = str_c('Salvador:\n', round(salvador, 1)) %>% 
      str_replace('\\.', ','),
    family = 'serif', size = 3
  ) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.17, .90)) +
  scale_fill_manual(values = c('#fed976', '#ef3b2c')) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  # coord_cartesian(xlim = c(20, 70)) +
  labs(
    x = 'Média dos indicadores normalizados (0 a 100)',
    y = 'Número de municípios',
    title = 'Qualidade dos serviços de fornecimento de água',
    subtitle = 'Distribuição das notas dos municípios baianos'
  )

hist_ranking_agua

# Saving
ggsave(plot = hist_ranking_agua, width = 6, height = 5,
       filename = 'plots/ranking/histogram-ranking-agua.png')

# Average by provider type ----------------------------------------------------
avg_by_provider_type <- snis_ranking %>% 
  group_by(tipo_prestador) %>% 
  summarise(avg_pop = mean(populacao),
            median_pop = median(populacao),
            media_simples = mean(score),
            media_ponderada = weighted.mean(score, w = populacao),
            mediana = median(score)) %>% 
  ungroup() ; avg_by_provider_type

# Saving
write_excel_csv2(avg_by_provider_type,
                 'data/ranking/score-agua-avg-provider-type.csv')


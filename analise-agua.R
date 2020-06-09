library(magrittr)
library(tidyverse)
library(sp)
library(sf)
source('helpers.R')
theme_set(custom_theme())

# Loading datasets ------------------------------------------------------------
polygons_municipios_bahia <- readRDS('data/polygons_municipios_bahia.rds')
snis_agua <- readRDS('data/snis-agua-clean.rds')

# Mapa: tipo de prestador -----------------------------------------------------
mapa_agua_tipo <- snis_agua %>% 
  mutate(nat_jur_simplified2 = 
           fct_relevel(nat_jur_simplified2, 'EMBASA', 'EMBASA + Prefeitura',
                       'Prefeitura municipal', 'Autarquia municipal',
                       'Empresa pública municipal')) %>% 
  rename(`Tipo de prestador` = nat_jur_simplified2) %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style("beaver") +
  tm_fill(
    'Tipo de prestador',
    palette = c('#fed976', '#225ea8',  '#41ab5d',  '#fb6a4a', '#dd1c77'),
    textNA = "Sem dados",
    alpha = 1,
    id = "municipio_clean"
  ) +
  tm_layout(main.title = 'Tipo do prestador de serviços - Água') +
  custom_map_settings ; mapa_agua_tipo

# Saving
tmap_save(mapa_agua_tipo, height = 6, width = 6,
          filename = 'plots/agua/mapa-tipo-prestador-agua.png')

# Tabela de frequencia: tipo de prestador  ------------------------------------
snis_agua %>% 
  count(nat_jur_simplified2, .drop = FALSE) %>% 
  arrange(desc(n)) %>% 
  mutate(nat_jur_simplified2 = ifelse(is.na(nat_jur_simplified2),
                                      'Sem dados', nat_jur_simplified2)) %>% 
  rename(tipo_prestador = nat_jur_simplified2) %T>% print() %>% 
  write_csv('data/nat_jur_prestadores.csv')

# Histograma: indice atendimento de agua --------------------------------------
mediana <- snis_agua$in055_indice_de_atendimento_total_de_agua %>% 
  median(na.rm = TRUE)
mediana_height <- 37

hist_atend_agua <- snis_agua %>% 
  ggplot() +
  geom_histogram(aes(x = in055_indice_de_atendimento_total_de_agua,
                     fill = nat_jur_simplified),
                 bins = 30) +
  geom_path(
    data = tibble(x = rep(mediana, 2),y = c(0, mediana_height)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = mediana, y = mediana_height, color = 'gray15', fill = 'gray97',
    label = str_c('Mediana:\n', round(mediana, 1)) %>% 
      str_replace('\\.', ','),
    family = 'serif', size = 3
  ) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.175, .9)) +
  scale_fill_manual(values = c('#fed976', '#ef3b2c')) +
  labs(
    x = 'Índice de atendimento total de água',
    y = 'Número de municípios',
    title = 'Distribuição do índice de atendimento de água'
  ) ; hist_atend_agua

# Saving
ggsave(plot = hist_atend_agua, width = 6, height = 5,
       filename = 'plots/agua/histogram-indice-atendimento-agua.png')

# Mapa: índice de atendimento -------------------------------------------------
mapa_atendimento_agua <- snis_agua %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de atendimento` =
           in055_indice_de_atendimento_total_de_agua) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Índice de atendimento',
    # style = 'fixed',
    # breaks = c(6, 50, 65, 75, 90, 100),
    # palette = c('#fee0d2', '#c6dbef', '#6baed6',
    #             '#4292c6',  '#08519c', "#08306B"),
    palette = 'Blues',
    style = 'quantile',
    n = 5,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Índice de atendimento total de água') +
  custom_map_settings ; mapa_atendimento_agua

# Saving
tmap_save(mapa_atendimento_agua, height = 6, width = 6,
          filename = 'plots/agua/mapa-atendimento-agua.png')

# Mapa: tarifa de água --------------------------------------------------------
mapa_tarifa_agua <- snis_agua %>% 
  add_geometry_municipios() %>% 
  rename(`Tarifa média (R$/m3)` =
           in005_tarifa_media_de_agua) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(fun = function(x) formatC(x, digits = 2,
                                     big.mark = '.', decimal.mark = ','),
           text.separator = " a ")
  ) +
  tm_fill(
    'Tarifa média (R$/m3)',
    palette = 'Blues',
    style = 'quantile',
    n = 5,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Tarifa média do fornecimento de água') +
  custom_map_settings ; mapa_tarifa_agua

# Saving
tmap_save(mapa_tarifa_agua,  height = 6, width = 6,
          filename = 'plots/agua/mapa-tarifa-media-agua.png')

snis_agua %>% 
  select(municipio_clean, tipo_servico, sigla_prestador,
         agua_embasa_e_prefeitura, in005_tarifa_media_de_agua) %>% 
  arrange(desc(in005_tarifa_media_de_agua)) %>% head(20)

# Mapa: investimento per capita -----------------------------------------------
mapa_investimento_per_capita <- snis_agua %>% 
  rename(`Inv. per capita (R$)` = inv_per_capita) %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(fun = function(x) formatC(x, big.mark = '.', decimal.mark = ','),
           text.separator = " a ")
  ) +
  tm_fill(
    'Inv. per capita (R$)',
    style = 'fixed',
    # palette = c('#f7fbff', '#c6dbef', '#6baed6',
    #             '#4292c6',  '#08519c', "#08306B"),
    # breaks = c(0, 5, 25, 50, 150, 500, 2300),
    palette = c('#f7fbff', '#c6dbef',
                '#4292c6',  '#08519c', "#08306B"),
    breaks = c(0, 5, 50, 150, 500, 2300),
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Investimento per capita - Água e Esgoto') +
  custom_map_settings ; mapa_investimento_per_capita

# Saving
tmap_save(mapa_investimento_per_capita, width = 6, height = 6,
          filename = 'plots/mapa-investimento-total-per-capita.png')

# Mapa: indicador de desempenho financeiro ------------------------------------
mapa_desempenho_financeiro <- snis_agua %>% 
  rename(`Ind. Desemp. Financeiro` =
           in012_indicador_de_desempenho_financeiro) %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(fun = function(x) round(x) %>% 
             str_replace('\\.', ','),
           text.separator = " a ")
  ) +
  tm_fill(
    'Ind. Desemp. Financeiro',
    palette = 'Blues',
    style = 'quantile',
    n = 5,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Indicador de desempenho financeiro') + 
  custom_map_settings ; mapa_desempenho_financeiro

# Saving
tmap_save(mapa_desempenho_financeiro, width = 6, height = 6,
          filename = 'plots/mapa-desempenho-financeiro.png')

# Boxplot: atendimento água ---------------------------------------------------
# Data wrangling
snis_boxplot_atendimento <- snis_agua %>% 
  filter(!is.na(nat_jur_simplified)) %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified))


summary_table_atendimento <- snis_boxplot_atendimento %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = median(in055_indice_de_atendimento_total_de_agua,
                             na.rm = TRUE)) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_atendimento_agua <- ggplot(snis_boxplot_atendimento) +
  geom_flat_violin(aes(x = 1,
                       y = in055_indice_de_atendimento_total_de_agua,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = 1,
                   y = in055_indice_de_atendimento_total_de_agua),
               width = 0.1, outlier.alpha = 0.3) +
  geom_text(data = summary_table_atendimento,
            aes(x = 1,
                y = Mediana, label = Label),
            size = 3, nudge_x = 0.25, family = 'serif') +
  scale_fill_manual(values = c('#fed976', '#ef3b2c')) +
  scale_x_discrete(labels = c('EMBASA', 'Adm. púb. municipal')) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(
    y = 'Percentual da população atendida',
    title = 'Distribuição dos índices municip. de atendimento (água)',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) +
  facet_wrap(~ nat_jur_simplified, nrow = 1); boxplot_atendimento_agua

# Saving
ggsave(plot = boxplot_atendimento_agua, width = 5, height = 4.5,
       filename = 'plots/agua/boxplot-atendimento-agua.png')

# Boxplot: tarifa água --------------------------------------------------------
# Data wrangling
snis_boxplot_tarifa <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified))

summary_table_tarifa <- snis_boxplot_tarifa %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = median(in005_tarifa_media_de_agua,
                             na.rm = TRUE)) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_tarifa_agua <- ggplot(snis_boxplot_tarifa) +
  geom_flat_violin(aes(x = 1,
                       y = in005_tarifa_media_de_agua,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = 1,
                   y = in005_tarifa_media_de_agua),
               width = 0.1, outlier.alpha = 0.3) +
  geom_text(data = summary_table_tarifa,
            aes(x = 1,
                y = Mediana, label = Label),
            size = 3, nudge_x = 0.17, family = 'serif') +
  scale_fill_manual(values = c('#fed976', '#ef3b2c')) +
  scale_x_discrete(labels = c('EMBASA', 'Adm. púb. municipal')) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(
    y = 'Tarifa média da água (R$/m3)',
    title = 'Distribuição das médias municipais da tarifa de água',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) +
  facet_wrap(~ nat_jur_simplified,
             nrow = 1, scales = 'free_y'); boxplot_tarifa_agua

# Saving
ggsave(plot = boxplot_tarifa_agua, width = 5, height = 4.5,
       filename = 'plots/agua/boxplot-tarifa-agua.png')


# Scatterplot: tarifa vs. atendimento: single plot ----------------------------
scatterplot_tarifa_atendimento <- snis_agua %>% 
  filter(!is.na(nat_jur_simplified)) %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot() +
  geom_point(aes(x = in055_indice_de_atendimento_total_de_agua,
                 y = in005_tarifa_media_de_agua,
                 size = pop, col = nat_jur_simplified),
             alpha = 0.6) +
  geom_smooth(aes(x = in055_indice_de_atendimento_total_de_agua,
                  y = in005_tarifa_media_de_agua),
              method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.3) +
  scale_color_manual(values = c('#fed976', '#ef3b2c')) +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  guides(size = FALSE) +
  labs(
    x = 'Índice de atendimento de água',
    y = 'Tarifa média de água',
    title = 'Relação entre tarifa e índice de atendimento',
    subtitle = 'Fornecimento de água nos municípios baianos',
    caption =
      'Fonte: SNIS (2018)\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) ; scatterplot_tarifa_atendimento

ggsave(plot = scatterplot_tarifa_atendimento, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-tarifa-vs-atendimento-agua.png')

# Scatterplot: tarifa vs. PIB - single plot  ----------------------------------
scatterplot_tarifa_pib <- snis_agua %>% 
  filter(!is.na(nat_jur_simplified)) %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib2017, y = in005_tarifa_media_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.6) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.3) +
  scale_x_log10(breaks = c(5e+4, 1e+5, 1e+6, 1e+7, 5e+7),
                labels = c('50mi', '100mi', '1bi', '10bi', '50bi')) +
  scale_color_manual(values = c('#fed976', '#ef3b2c')) +
  labs(
    x = 'PIB em reais (escala logarítmica)',
    y = 'Tarifa média de água',
    title = 'Relação entre tarifa e PIB',
    subtitle = 'Fornecimento de água nos municípios baianos',
    caption =
      'Fonte: dados tarifários do SNIS (2018); dados de PIB do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE)  +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()); scatterplot_tarifa_pib

# Saving
ggsave(plot = scatterplot_tarifa_pib, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-tarifa-agua-vs-pib.png')

# Scatterplot: tarifa vs. PIB per capita - single plot  -----------------------
scatterplot_tarifa_pib_per_capita <- snis_agua %>% 
  filter(!is.na(nat_jur_simplified)) %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib_per_capita2017, y = in005_tarifa_media_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.5) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.3) +
  scale_x_log10(breaks = c(5, 25, 50, 100, 250),
                labels = c('5 mil', '25 mil', '50 mil', '100 mil', '250 mil')) +
  scale_color_manual(values = c('#fed976', '#ef3b2c')) +
  labs(
    x = 'PIB per capita em reais (escala logarítmica)',
    y = 'Tarifa média de água',
    title = 'Relação entre tarifa e PIB per capita',
    subtitle = 'Fornecimento de água nos municípios baianos',
    caption =
      'Fonte: dados tarifários do SNIS (2018); dados de PIB e população do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE)  +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()); scatterplot_tarifa_pib_per_capita

# Saving
ggsave(plot = scatterplot_tarifa_pib_per_capita, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-tarifa-agua-vs-pib-per-capita.png')


# Scatterplot: atendimento vs PIB - single plot -------------------------------
scatterplot_atendimento_pib <- snis_agua %>% 
  filter(!is.na(nat_jur_simplified)) %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib2017, y = in055_indice_de_atendimento_total_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.5) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.3) +
  scale_x_log10(breaks = c(5e+4, 1e+5, 1e+6, 1e+7, 5e+7),
                labels = c('50mi', '100mi', '1bi', '10bi', '50bi')) +
  coord_cartesian(ylim = c(min(snis_agua$in055_indice_de_atendimento_total_de_agua, na.rm = TRUE), 100)) +
  scale_color_manual(values = c('#fed976', '#fb6a4a', '#225ea8')) +
  labs(
    x = 'PIB em reais (escala logarítmica)',
    y = 'Índice de atendimento',
    title = 'Relação entre índice de atendimento de água e PIB',
    subtitle = 'Fornecimento de água nos municípios baianos',
    caption =
      'Fonte: dados de atendimento do SNIS (2018); dados de PIB do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE) +
  theme(panel.grid = element_blank(), legend.title = element_blank(),
        legend.position = 'bottom') ; scatterplot_atendimento_pib

ggsave(plot = scatterplot_atendimento_pib, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-atendimento-agua-vs-pib.png')

# Scatterplot: atendimento vs PIB per capita - single plot --------------------
scatterplot_atendimento_pib_per_capita <- snis_agua %>% 
  filter(!is.na(nat_jur_simplified)) %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib_per_capita2017, y = in055_indice_de_atendimento_total_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.5) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.3) +
  scale_x_log10(breaks = c(5, 25, 50, 100, 250),
                labels = c('5 mil', '25 mil', '50 mil', '100 mil', '250 mil')) +
  scale_color_manual(values = c('#fed976', '#ef3b2c')) +
  coord_cartesian(ylim = c(min(snis_agua$in055_indice_de_atendimento_total_de_agua, na.rm = TRUE), 100)) +
  labs(
    x = 'PIB per capita em reais (escala logarítmica)',
    y = 'Índice de atendimento',
    title = 'Relação entre índice de atendimento de água e PIB per capita',
    subtitle = 'Fornecimento de água nos municípios baianos',
    caption =
      'Fonte: dados de atendimento do SNIS (2018); dados de PIB e população do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE) +
  theme(panel.grid = element_blank(), legend.title = element_blank(),
        legend.position = 'bottom') ; scatterplot_atendimento_pib_per_capita

ggsave(plot = scatterplot_atendimento_pib_per_capita, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-atendimento-agua-vs-pib-per-capita.png')


# Histograma: indice perdas ---------------------------------------------------
perda_mediana <- snis_agua$in013_indice_de_perdas_faturamento %>%
  median(na.rm = TRUE)
mediana_height <- 35

hist_perdas_agua <- snis_agua %>% 
  ggplot() +
  geom_histogram(aes(x = in013_indice_de_perdas_faturamento,
                     fill = nat_jur_simplified),
                 bins = 20) +
  geom_path(
    data = tibble(x = rep(perda_mediana, 2), y = c(0, mediana_height)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = perda_mediana, y = mediana_height, color = 'gray15', fill = 'gray98',
    label = str_c('Mediana:\n', round(perda_mediana, 1)) %>% 
      str_replace('\\.', ','),
    family = 'serif', size = 3
  ) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.82, .9)) +
  scale_fill_manual(values = c('#fed976', '#ef3b2c')) +
  scale_x_continuous(breaks = seq(-20, 100, 20)) +
  labs(
    x = 'Índice de perdas de faturamento',
    y = 'Número de municípios',
    title = 'Distribuição do índice de perdas de faturamento de água'
  ) ; hist_perdas_agua

# Saving
ggsave(plot = hist_perdas_agua, width = 6, height = 5,
       filename = 'plots/agua/histogram-indice-perdas-faturamento.png')


# Mapa: índice de perdas ------------------------------------------------------
mapa_perdas_agua <- snis_agua %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de perdas` =
           in013_indice_de_perdas_faturamento) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Índice de perdas',
    style = 'quantile',
    palette = 'Blues',
    n = 5,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Índice de perdas de faturamento (água)') +
  custom_map_settings ; mapa_perdas_agua

# Saving
tmap_save(mapa_perdas_agua, height = 6, width = 6,
          filename = 'plots/agua/mapa-perdas-faturamento.png')

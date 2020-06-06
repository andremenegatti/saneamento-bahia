library(magrittr)
library(tidyverse)
library(sp)
library(sf)
source('helpers.R')
theme_set(custom_theme())

# Loading datasets ------------------------------------------------------------
polygons_municipios_bahia <- readRDS('data/polygons_municipios_bahia.rds')
snis <- readRDS('data/snis-bahia.rds')

snis_esgoto <- snis %>% filter(tipo_servico != 'Água') 

# Checking missingness --------------------------------------------------------
library(naniar)
library(visdat)

snis_esgoto %>%
  right_join(polygons_municipios_bahia %>% st_drop_geometry(),
             by = 'codigo_municipio') %>% 
  select(municipio_clean,
         tarifa_esgoto = in006_tarifa_media_de_esgoto,
         coleta = in015_indice_de_coleta_de_esgoto,
         tratamento = in046_indice_de_esgoto_tratado_referido_a_agua_consumida) %>% 
  arrange(tarifa_esgoto, coleta) %>% 
  vis_miss()

# Data wrangling: type of provider --------------------------------------------
snis_esgoto2 <- snis_esgoto %>% 
  filter(!(municipio == 'Araci' & sigla_prestador == 'PMA')) %>% 
  right_join(polygons_municipios_bahia %>% st_drop_geometry(),
             by = 'codigo_municipio') %>% 
  mutate(
    nat_jur_simplified2 = case_when(
      municipio == 'Araci' ~ 'EMBASA + Prefeitura',
      sigla_prestador == 'EMBASA' ~ 'EMBASA',
      natureza_juridica == 'Autarquia' ~ 'Autarquia municipal',
      natureza_juridica == 'Administração pública direta' ~ 'Prefeitura municipal',
      natureza_juridica == 'Empresa pública' ~ 'Empresa pública municipal',
      natureza_juridica == 'Sem dados' ~ 'Sem dados',
      is.na(natureza_juridica) ~ 'Sem dados',
      municipio == 'Itabuna' ~ 'Empresa pública municipal',
      is.na(municipio) ~ 'Sem dados'
    )
  ) %>% 
  mutate(nat_jur_simplified = case_when(
    str_detect(nat_jur_simplified2, 'EMBASA') ~ 'EMBASA',
    str_detect(nat_jur_simplified2, 'municipal') ~ 'Adm. pública municipal',
    is.na(municipio) ~ 'Sem dados'
  ) %>% fct_relevel('EMBASA', 'Adm. pública municipal', 'Sem dados'))

# Mapa: natureza jurídica -----------------------------------------------------
mapa_esgoto_tipo <- snis_esgoto2 %>% 
  filter(nat_jur_simplified2 != 'Sem dados') %>% 
  mutate(nat_jur_simplified2 = fct_drop(nat_jur_simplified2) %>% 
           fct_relevel('EMBASA', 'EMBASA + Prefeitura', 'Prefeitura municipal',
                       'Autarquia municipal', 'Empresa pública municipal')) %>% 
  rename(`Tipo de prestador` = nat_jur_simplified2) %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style("beaver") +
  tm_fill(
    'Tipo de prestador',
    palette = c('#fed976', '#225ea8',  '#41ab5d',  '#fb6a4a', '#dd1c77'),
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados"
  ) +
  tm_layout(main.title = 
              'Tipo de prestador de serviços - Esgoto') +
  custom_map_settings ; mapa_esgoto_tipo

# Saving
tmap_save(mapa_esgoto_tipo, height = 6, width = 6,
          filename = 'plots/esgoto/mapa-tipo-prestador-esgoto.png')


# Tabela de frequencia: tipo de prestador  ------------------------------------
snis_esgoto2 %>% 
  count(nat_jur_simplified2, .drop = FALSE) %>% 
  arrange(desc(n)) %>% 
  mutate(nat_jur_simplified2 = ifelse(is.na(nat_jur_simplified2),
                                      'Sem dados', nat_jur_simplified2)) %>% 
  rename(tipo_prestador = nat_jur_simplified2) %T>% print() %>% 
  write_csv('data/nat_jur_prestadores_esgoto.csv')


# Mapa: índice de coleta de esgoto --------------------------------------------
mapa_coleta_esgoto <- snis_esgoto2 %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de coleta` =
           in015_indice_de_coleta_de_esgoto) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Índice de coleta',
    palette = 'Blues',
    style = 'quantile',
    n = 4,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Índice de coleta de esgoto') +
  custom_map_settings ; mapa_coleta_esgoto

# Saving
tmap_save(mapa_coleta_esgoto, height = 6, width = 6,
          filename = 'plots/esgoto/mapa-coleta-esgoto.png')

# Barplot: indice de coleta de esgoto --------------------------------------
coleta_binned <- snis_esgoto2 %>% 
  select(nat_jur_simplified,
         coleta = in015_indice_de_coleta_de_esgoto) %>% 
  filter(!is.na(coleta)) %>% 
  mutate(coleta_bins = cut(coleta,
                           breaks = c(1.8, 25, 50, 75, 100),
                           labels = c('1,8% a 25%', '25% a 50%',
                                      '50% a 75%', '75% a 100%')))

coleta_labels <- coleta_binned %>% 
  count(nat_jur_simplified, coleta_bins) %>% 
  arrange(coleta_bins, desc(nat_jur_simplified)) %>% 
  mutate(half = n / 2,
         meio = ifelse(nat_jur_simplified != 'EMBASA',
                       half, lag(n) + half))

barplot_coleta_esgoto <- coleta_binned %>% 
  ggplot() +
  geom_bar(aes(x = coleta_bins,
                     fill = nat_jur_simplified)) +
  geom_text(data = coleta_labels,
             mapping = aes(x = coleta_bins,
                           y = meio, label = n),
            family = 'serif') +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.83, .92)) +
  scale_fill_manual(values = c('#fed976', '#fb6a4a')) +
  labs(
    x = 'Índice de coleta de esgoto',
    y = 'Número de municípios',
    title = 'Distribuição do índice de coleta de esgoto'
  ) ; barplot_coleta_esgoto

# Saving
ggsave(plot = barplot_coleta_esgoto, width = 6, height = 6,
       filename = 'plots/esgoto/barplot-indice-coleta-esgoto.png')

# Mapa: índice de tratamento de esgoto ----------------------------------------
mapa_tratamento_esgoto <-  snis_esgoto2 %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de tratamento` =
           in046_indice_de_esgoto_tratado_referido_a_agua_consumida) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Índice de tratamento',
    palette = 'Blues',
    style = 'quantile',
    n = 4,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Índice de tratamento de esgoto') +
  custom_map_settings ; mapa_tratamento_esgoto

# Saving
tmap_save(mapa_tratamento_esgoto, height = 6, width = 6,
          filename = 'plots/esgoto/mapa-tratamento-esgoto.png')

# Barplot: indice de tratamento de esgoto --------------------------------------
trat_binned <- snis_esgoto2 %>% 
  select(nat_jur_simplified,
         trat = in046_indice_de_esgoto_tratado_referido_a_agua_consumida) %>% 
  filter(!is.na(trat)) %>% 
  mutate(trat_bins = cut(trat,
                         breaks = c(0, 25, 50, 75, 100),
                         labels = c('0% a 25%', '25% a 50%',
                                    '50% a 75%', '75% a 100%'),
                         include.lowest = TRUE))

trat_labels <- trat_binned %>% 
  count(nat_jur_simplified, trat_bins) %>% 
  arrange(trat_bins, desc(nat_jur_simplified)) %>% 
  mutate(half = n / 2,
         meio = ifelse(nat_jur_simplified != 'EMBASA',
                       half, lag(n) + half))

barplot_trat_esgoto <- trat_binned %>% 
  ggplot() +
  geom_bar(aes(x = trat_bins,
               fill = nat_jur_simplified)) +
  geom_text(data = trat_labels,
            mapping = aes(x = trat_bins,
                          y = meio, label = n),
            family = 'serif') +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.83, .92)) +
  scale_fill_manual(values = c('#fed976', '#fb6a4a')) +
  labs(
    x = 'Índice de tratamento de esgoto',
    y = 'Número de municípios',
    title = 'Distribuição do índice de tratamento de esgoto'
  ) ; barplot_trat_esgoto

# Saving
ggsave(plot = barplot_trat_esgoto, width = 6, height = 6,
       filename = 'plots/esgoto/barplot-indice-tratamento-esgoto.png')

# Mapa: tarifa de esgoto ------------------------------------------------------
mapa_tarifa_esgoto <- snis_esgoto %>% 
  add_geometry_municipios() %>% 
  rename(`Tarifa média (R$/m3)` =
           in006_tarifa_media_de_esgoto) %>% 
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
              'Tarifa média - Esgoto') +
  custom_map_settings ; mapa_tarifa_esgoto

# Saving
tmap_save(mapa_tarifa_esgoto,  height = 6, width = 6,
          filename = 'plots/esgoto/mapa-tarifa-media-esgoto.png')
library(tidyverse)
library(sp)
library(sf)

# Loading datasets ------------------------------------------------------------
snis <- readRDS('data/snis-bahia.rds')
polygons_municipios_bahia <- readRDS('data/polygons_municipios_bahia.rds')

# Data wrangling: type of provider --------------------------------------------
snis_esgoto <- snis %>%
  filter(tipo_servico != 'Água') %>% 
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

# Checking missingness --------------------------------------------------------
snis_esgoto %>%
  select(municipio_clean,
         tarifa_esgoto = in006_tarifa_media_de_esgoto,
         coleta = in015_indice_de_coleta_de_esgoto,
         tratamento = in046_indice_de_esgoto_tratado_referido_a_agua_consumida) %>% 
  arrange(tarifa_esgoto, coleta) %>% 
  naniar::vis_miss()

# Persisting dataframe --------------------------------------------------------
saveRDS(snis_esgoto, 'data/snis-esgoto-clean.rds')

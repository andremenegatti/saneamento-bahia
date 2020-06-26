library(tidyverse)
library(sp)
library(sf)

# Loading datasets ------------------------------------------------------------
snis <- readRDS('data/snis-bahia.rds')
polygons_municipios_bahia <- readRDS('data/polygons_municipios_bahia.rds')

# Data wrangling: type of provider  -------------------------------------------
snis_agua <- snis %>% 
  filter(tipo_servico != 'Esgotos') %>% 
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

# Checking missingness --------------------------------------------------------
snis_agua %>%
  select(municipio_clean,
         tarifa_agua = in005_tarifa_media_de_agua,
         tarifa_esgoto = in006_tarifa_media_de_esgoto,
         atendimento = in055_indice_de_atendimento_total_de_agua,
         coleta = in015_indice_de_coleta_de_esgoto,
         tratamento = in046_indice_de_esgoto_tratado_referido_a_agua_consumida,
         desempenho_financeiro = in012_indicador_de_desempenho_financeiro,
         perdas = in013_indice_de_perdas_faturamento,
         inv_per_capita) %>% 
  arrange(tarifa_esgoto, coleta, atendimento, tarifa_agua) %>% 
  naniar::vis_miss()

# Persisting dataframe --------------------------------------------------------
saveRDS(snis_agua, 'data/snis-agua-clean.rds')
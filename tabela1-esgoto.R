library(tidyverse)

# Loading datasets ------------------------------------------------------------
snis_esgoto <- readRDS('data/snis-esgoto-clean_v2.rds')

snis_esgoto %>% filter(municipio_clean == 'ARACI') %>% select(sigla_prestador,
                                                              ag001_populacao_total_atendida_com_abastecimento_de_agua,
                                                              ag026_populacao_urbana_atendida_com_abastecimento_de_agua,
                                                              es001_populacao_total_atendida_com_esgotamento_sanitario,
                                                              es026_populacao_urbana_atendida_com_esgotamento_sanitario)

# snis_esgoto <- snis_esgoto %>% 
#   mutate(es001_populacao_total_atendida_com_esgotamento_sanitario =
#            ifelse(municipio_clean == 'ARACI', 17456, es001_populacao_total_atendida_com_esgotamento_sanitario),
#          sigla_prestador = ifelse(municipio_clean == 'ARACI', 'PM', sigla_prestador))

snis_agua <- readRDS('data/snis-agua-clean_v2.rds')
  

mun_atendidos_agua <- snis_agua %>% 
  select(municipio_clean, sigla_prestador, pop, pop_urbana, ag001 = ag001_populacao_total_atendida_com_abastecimento_de_agua,
         ag026 = ag026_populacao_urbana_atendida_com_abastecimento_de_agua) %>% 
  drop_na()


snis_esgoto <- mun_atendidos_agua %>% 
  select(municipio_clean, sigla_prestador, pop, pop_urbana) %>% 
  left_join(snis_esgoto %>% select(-pop, -pop_urbana, -sigla_prestador), by = 'municipio_clean')

snis_esgoto <- snis_esgoto %>% 
  mutate(es001_populacao_total_atendida_com_esgotamento_sanitario = 
           ifelse(is.na(es001_populacao_total_atendida_com_esgotamento_sanitario),
                  0, es001_populacao_total_atendida_com_esgotamento_sanitario),
         es026_populacao_urbana_atendida_com_esgotamento_sanitario =
           ifelse(is.na(es026_populacao_urbana_atendida_com_esgotamento_sanitario),
                  0, es026_populacao_urbana_atendida_com_esgotamento_sanitario))


# snis_esgoto <- snis_esgoto %>% 
#   mutate(sigla_prestador = ifelse(is.na(sigla_prestador), 'Missing', sigla_prestador))


# EMBASA, mais de 200 mil hab -------------------------------------------------
municipios_grandes_esgoto <- snis_esgoto %>% 
  filter(sigla_prestador == 'EMBASA') %>% 
  filter(municipio_clean %in% c('SALVADOR', 'FEIRA DE SANTANA',
                                'VITORIA DA CONQUISTA', 'CAMACARI')) %>%
  mutate(municipio_clean = fct_reorder(.f = municipio_clean, .x = -pop)) %>% 
  arrange(municipio_clean) %>% 
  select(municipio_clean,
         atend_esgoto_total = in056_indice_de_atendimento_total_de_esgoto_referido_aos_municipios_atendidos_com_agua,
         atend_esgoto_urb = in024_indice_de_atendimento_urbano_de_esgoto_referido_aos_municipios_atendidos_com_agua,
         coleta_esgoto = in015_indice_de_coleta_de_esgoto) ; municipios_grandes_esgoto

# municipios_grandes_esgoto %>% 
#   write_csv2('data/atualizacao-tabelas/esgoto_embasa_200mil.csv')

# EMBASA, outros municipios, grupos por tamanho -------------------------------
grupos_pop_esgoto <- snis_esgoto %>% 
  filter(sigla_prestador == 'EMBASA') %>% 
  mutate(pop_group = case_when(
    pop < 20e+3 ~ 'Até 20 mil habitantes',
    pop < 50e+3 ~ 'De 20 mil a 50 mil habitantes',
    pop < 100e+3 ~ 'De 50 mil a 100 mil habitantes',
    pop < 200e+3 ~ 'De 100 mil a 200 mil habitantes',
    TRUE ~ 'Mais de 200 mil habitantes'
  ) %>% fct_reorder(-pop)) %>% 
  select(municipio_clean, pop_group, pop, pop_urbana,
         es001 = es001_populacao_total_atendida_com_esgotamento_sanitario,
         es026 = es026_populacao_urbana_atendida_com_esgotamento_sanitario)

grupos_pop_agregado_esgoto <- grupos_pop_esgoto %>% 
  group_by(pop_group) %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    es001 = sum(es001),
    es026 = sum(es026),
    atend_esgoto_total = es001 / pop * 100,
    atend_esgoto_urb = es026 / pop_urb * 100,
    n = n()
  ) ; grupos_pop_agregado_esgoto

# grupos_pop_agregado_esgoto %>% 
#   select(pop_group, es001, es026, n) %>% 
#   left_join(grupos_pop_agregado_agua %>%
#               select(pop_group, pop, pop_urb),
#             by = 'pop_group') %>% 
#   mutate(
#     atend_esgoto_total = es001 / pop * 100,
#     atend_esgoto_urb = es026 / pop_urb * 100)

# grupos_pop_agregado_esgoto %>%
#   write_csv2('data/atualizacao-tabelas/embasa-agregado-esgoto.csv')

# EMBASA, agregado ------------------------------------------------------------
agregado_embasa_esgoto <- snis_esgoto %>% 
  filter(sigla_prestador == 'EMBASA') %>% 
  select(municipio_clean, pop, pop_urbana,
         es001 = es001_populacao_total_atendida_com_esgotamento_sanitario,
         es026 = es026_populacao_urbana_atendida_com_esgotamento_sanitario) %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    es001 = sum(es001),
    es026 = sum(es026),
    atend_esgoto_total = es001 / pop,
    atend_esgoto_urb = es026 / pop_urb,
    n = n()
  ) ; agregado_embasa_esgoto

agregado_embasa %>% 
  write_csv2('data/atualizacao-tabelas/embasa-agregado-esgoto2.csv')

# Não atendidos pela EMBASA, agregado -----------------------------------------
snis_outros_esgoto <- snis_esgoto %>% 
  filter(sigla_prestador != 'EMBASA') %>% 
  select(municipio_clean, pop, pop_urbana,
         es001 = es001_populacao_total_atendida_com_esgotamento_sanitario,
         es026 = es026_populacao_urbana_atendida_com_esgotamento_sanitario)

agregado_outros_esgoto <- snis_outros_esgoto %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    es001 = sum(es001),
    es026 = sum(es026),
    atend_esgoto_total = es001 / pop * 100,
    atend_esgoto_urb = es026 / pop_urb * 100,
    n = n()
  ) ; agregado_outros_esgoto


agregado_outros %>% write_csv2('data/atualizacao-tabelas/outros-agregado-esgoto.csv')

# Agregado Bahia (todos os prestadores) ---------------------------------------
snis_esgoto %>% 
  select(pop, pop_urbana,
         es001 = es001_populacao_total_atendida_com_esgotamento_sanitario,
         es026 = es026_populacao_urbana_atendida_com_esgotamento_sanitario) %>% 
  filter(!is.na(es001)) %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    es001 = sum(es001),
    es026 = sum(es026),
    atend_agua_total = es001 / pop,
    atend_agua_urb = es026 / pop_urb,
    n = n()
  )

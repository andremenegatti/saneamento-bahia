library(tidyverse)

# Loading datasets ------------------------------------------------------------
snis_agua <- readRDS('data/snis-agua-clean_v2.rds')

mun_2agua <- 
  c('BARRA DA ESTIVA', 'IRARA', 'ITACARE',
    'MACAUBAS', 'NOVO HORIZONTE', 'TABOCAS DO BREJO VELHO')

snis_agua <- snis_agua %>% 
  filter(!municipio_clean %in% mun_2agua)

# EMBASA, mais de 200 mil hab -------------------------------------------------
municipios_grandes_agua <- snis_agua %>% 
  filter(sigla_prestador == 'EMBASA') %>% 
  filter(municipio_clean %in% c('SALVADOR', 'FEIRA DE SANTANA',
                                'VITORIA DA CONQUISTA', 'CAMACARI')) %>% 
  mutate(municipio_clean = fct_reorder(.f = municipio_clean, .x = -pop)) %>% 
  arrange(municipio_clean) %>% 
  select(
    municipio_clean,
    atend_agua_total = in055_indice_de_atendimento_total_de_agua,
    atend_agua_urb = in023_indice_de_atendimento_urbano_de_agua
    ) ; municipios_grandes_agua

# municipios_grandes_agua %>% 
#   write_csv2('data/atualizacao-tabelas/agua_embasa_200mil.csv')

# EMBASA, outros municipios, grupos por tamanho -------------------------------
grupos_pop_agua <- snis_agua %>% 
  filter(sigla_prestador == 'EMBASA') %>% 
  mutate(pop_group = case_when(
    pop < 20e+3 ~ 'Até 20 mil habitantes',
    pop < 50e+3 ~ 'De 20 mil a 50 mil habitantes',
    pop < 100e+3 ~ 'De 50 mil a 100 mil habitantes',
    pop < 200e+3 ~ 'De 100 mil a 200 mil habitantes',
    TRUE ~ 'Mais de 200 mil habitantes'
  ) %>% fct_reorder(-pop)) %>% 
  select(municipio_clean, pop_group, pop, pop_urbana,
         ag001 = ag001_populacao_total_atendida_com_abastecimento_de_agua,
         ag026 = ag026_populacao_urbana_atendida_com_abastecimento_de_agua)

grupos_pop_agregado_agua <- grupos_pop_agua %>% 
  group_by(pop_group) %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    ag001 = sum(ag001),
    ag026 = sum(ag026),
    atend_agua_total = ag001 / pop,
    atend_agua_urb = ag026 / pop_urb,
    n = n()
    ) ; grupos_pop_agregado_agua

# grupos_pop_agregado_agua %>% 
#   write_csv2('data/atualizacao-tabelas/embasa-agregado-agua.csv')

# EMBASA, agregado ------------------------------------------------------------
agregado_embasa_agua <- snis_agua %>% 
  filter(sigla_prestador == 'EMBASA') %>% 
  select(municipio_clean, pop, pop_urbana,
         ag001 = ag001_populacao_total_atendida_com_abastecimento_de_agua,
         ag026 = ag026_populacao_urbana_atendida_com_abastecimento_de_agua) %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    ag001 = sum(ag001),
    ag026 = sum(ag026),
    atend_agua_total = ag001 / pop,
    atend_agua_urb = ag026 / pop_urb,
    n = n()
  ) ; agregado_embasa_agua

# agregado_embasa_agua %>% 
#   write_csv2('data/atualizacao-tabelas/embasa-agregado-agua2.csv')

# Não atendidos pela EMBASA, agregado -----------------------------------------
snis_outros_agua <- snis_agua %>% 
  filter(sigla_prestador != 'EMBASA') %>% 
  select(municipio_clean, pop, pop_urbana,
         ag001 = ag001_populacao_total_atendida_com_abastecimento_de_agua,
         ag026 = ag026_populacao_urbana_atendida_com_abastecimento_de_agua)

agregado_outros_agua <- snis_outros_agua %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    ag001 = sum(ag001),
    ag026 = sum(ag026),
    atend_agua_total = ag001 / pop * 100,
    atend_agua_urb = ag026 / pop_urb * 100,
    n = n()
  ) ; agregado_outros_agua

# agregado_outros_agua %>%
#   write_csv2('data/atualizacao-tabelas/outros-agregado-agua.csv')

# Agregado Bahia (todos os prestadores) ---------------------------------------
snis_agua %>% 
  select(pop, pop_urbana,
         ag001 = ag001_populacao_total_atendida_com_abastecimento_de_agua,
         ag026 = ag026_populacao_urbana_atendida_com_abastecimento_de_agua) %>% 
  filter(!is.na(ag001)) %>% 
  summarise(
    pop = sum(pop),
    pop_urb = sum(pop_urbana),
    ag001 = sum(ag001),
    ag026 = sum(ag026),
    atend_agua_total = ag001 / pop,
    atend_agua_urb = ag026 / pop_urb,
    n = n()
  )

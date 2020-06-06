library(tidyverse)

# Reading full dataset --------------------------------------------------------
snis <- readxl::read_excel('data/snis-bahia-desagregado-2018.xlsx',
                           range = 'A1:HD439')
colnames(snis) <- tolower(colnames(snis))

# Keeping relevant features only ----------------------------------------------
snis2 <- snis %>% 
  select(
    codigo_municipio, municipio, prestador, sigla_prestador, tipo_servico,
    natureza_juridica,
    pop = pop_tot_populacao_total_do_municipio_do_ano_de_referencia,
    pop_urbana = pop_urb_populacao_urbana_do_municipio_do_ano_de_referencia,
    starts_with('ag001_'), starts_with('ag026_'),
    starts_with('es001_'), starts_with('fn_017_'),
    # Investimentos prestador de serviços
    starts_with('fn023'), starts_with('fn024'),
    starts_with('fn025'), starts_with('fn033'),
    # Investimentos município
    starts_with('fn042'), starts_with('fn043'),
    starts_with('fn044'), starts_with('fn048'),
    # Investimentos estado
    starts_with('fn052'), starts_with('fn053'),
    starts_with('fn054'), starts_with('fn058'),
    # Índices
    starts_with('in003'), contains('_tarifa_media_'),
    starts_with('in012'), starts_with('in013'),
    starts_with('in015'), starts_with('in016'), starts_with('in022'),
    starts_with('in024'), starts_with('in046'), starts_with('in047'),
    starts_with('in055'), starts_with('in056')
  )

# New feature: simplified version of 'natureza_jurica' ------------------------
snis3 <- snis2 %>% 
  mutate(natureza_juridica = case_when(
    is.na(natureza_juridica) ~ 'Sem dados',
    str_detect(natureza_juridica,'economia') ~ 
      'Soc. de Econ. Mista com Adm. Pública',
    TRUE ~ natureza_juridica
  )) %>% 
  mutate(nat_jur_simplified = case_when(
    natureza_juridica %in% c('Administração pública direta',
                             'Autarquia',
                             'Empresa pública') ~ 'Administração pública',
    TRUE ~ natureza_juridica
  ) %>% fct_relevel('Soc. de Econ. Mista com Adm. Pública',
                    'Administração pública'))

# Joining population and GDP data ---------------------------------------------
snis4 <- snis3 %>% 
  left_join(
    readxl::read_excel('data/pib-municipios-2017.xlsx') %>% 
      mutate(codigo_municipio = as.numeric(str_extract(codigo, '\\d{6}'))) %>%
      select(codigo_municipio, pib2017 = pib) %>% 
      mutate(codigo_municipio = as.integer(codigo_municipio)),
    by = 'codigo_municipio'
    ) %>% 
  left_join(
    readxl::read_excel('data/estimativa-populacao-municipios-2017.xlsx') %>%
      mutate(codigo_municipio = as.numeric(str_extract(Codmun7, '\\d{6}'))) %>% 
      select(codigo_municipio, pop2017) %>% 
      mutate(codigo_municipio = as.integer(codigo_municipio)),
    by = 'codigo_municipio'
    ) %>% 
  mutate(pib_per_capita2017 = pib2017 / pop2017) # In thousands of BRL

# Dealing with special cases: cities with two service providers ---------------

# Cities with more than one company providing water OR sewage services
mun_2prestadores <- snis4 %>% count(municipio) %>% 
  filter(n > 1) %>% pull(municipio)

# Saving for visual inspection/manual cleaning
snis4 %>% 
  filter(municipio %in% mun_2prestadores) %>% 
  select(municipio, tipo_servico, prestador,
         contains('tarifa'), contains('atendimento_total_de_agua'),
         contains('coleta'),
         in046_indice_de_esgoto_tratado_referido_a_agua_consumida,
         contains('desempenho_financeiro'),
         contains('investimentos_totais_')) %>% 
  arrange(municipio, tipo_servico) %>% 
  write_csv('data/snis_2prestadores.csv')

# The following cities have more than one water provider
mun_2agua <- 
  c('Barra da Estiva', 'Irará', 'Itacaré',
    'Macaúbas', 'Novo Horizonte', 'Tabocas do Brejo Velho')

# All the others have 1 provider for water, and 1 for sewage
mun_1agua_1esgoto <- setdiff(mun_2prestadores, mun_2agua)

# Total investment: sum of investment from all providers
snis_2prestadores <- snis4 %>% 
  mutate(agua_embasa_e_prefeitura = FALSE) %>% 
  filter(municipio %in% mun_2prestadores) %>%
  group_by(municipio) %>% # Grouping by city
  mutate(
    # Total investment is the sum of values indicated for each provider
    fn033_investimentos_totais_realizados_pelo_prestador_de_servicos =
      sum(fn033_investimentos_totais_realizados_pelo_prestador_de_servicos,
          na.rm = TRUE),
    fn048_investimentos_totais_realizados_pelo_municipio =
      sum(fn048_investimentos_totais_realizados_pelo_municipio,
          na.rm = TRUE),
    fn058_investimentos_totais_realizados_pelo_estado =
      sum(fn058_investimentos_totais_realizados_pelo_estado,
          na.rm = TRUE),
    in012_indicador_de_desempenho_financeiro = 
      mean(in012_indicador_de_desempenho_financeiro,
           na.rm = TRUE)
  ) %>% 
  ungroup()

# Missing sewage data in cities that have 1 provider for water and 1 for sewage
# (Visual inspection of the data showed that values are indeed missing or inconsistent)
snis_1agua_1esgoto <- snis_2prestadores %>% 
  filter(municipio %in% mun_1agua_1esgoto) %>% 
  mutate_at(
    .vars =
      vars(in004_tarifa_media_praticada,
           in006_tarifa_media_de_esgoto,
           in015_indice_de_coleta_de_esgoto,
           in016_indice_de_tratamento_de_esgoto,
           in024_indice_de_atendimento_urbano_de_esgoto_referido_aos_municipios_atendidos_com_agua,
           in046_indice_de_esgoto_tratado_referido_a_agua_consumida,
           in047_indice_de_atendimento_urbano_de_esgoto_referido_aos_municipios_atendidos_com_esgoto),
    .funs = function(x) return(NA_real_)
    )

# Cities with 2 water providers: aggregating results
snis_2agua <- snis_2prestadores %>% 
  filter(municipio %in% mun_2agua) %>% 
  group_by(municipio) %>% 
  # Perdas: média ponderada pelo atendimento (apenas 1 missing: Novo Horizonte)
  mutate(
    in013_indice_de_perdas_faturamento = 
      weighted.mean(in013_indice_de_perdas_faturamento,
                    w = in055_indice_de_atendimento_total_de_agua),
    in005_tarifa_media_de_agua = 
      weighted.mean(in005_tarifa_media_de_agua, 
                    w = in055_indice_de_atendimento_total_de_agua,
                    na.rm = TRUE),
    in004_tarifa_media_praticada = 
      weighted.mean(in004_tarifa_media_praticada,
                    w = in055_indice_de_atendimento_total_de_agua,
                    na.rm = TRUE)
    ) %>% 
  # Atendimento: soma dos atendimentos
  mutate(in055_indice_de_atendimento_total_de_agua = 
           sum(in055_indice_de_atendimento_total_de_agua)) %>% 
  # Novo horizonte: soma maior do que 100%; tratada como NA
  mutate(in055_indice_de_atendimento_total_de_agua = 
           ifelse(in055_indice_de_atendimento_total_de_agua > 100,
                  NA_real_, in055_indice_de_atendimento_total_de_agua)) %>% 
  # Leaving only lines corresponding to EMBASA # <------
  mutate(agua_embasa_e_prefeitura = TRUE) %>% 
  filter(sigla_prestador == 'EMBASA')

# Updating lines in the original dataframe
snis5 <- snis4 %>% 
  filter(!municipio %in% mun_2prestadores) %>% 
  mutate(agua_embasa_e_prefeitura = FALSE) %>% 
  bind_rows(snis_1agua_1esgoto) %>% 
  bind_rows(snis_2agua)

# Per capita investment -------------------------------------------------------
# Total investment / population
# Total investment = provider + municipality + state investment
# If any of such investments is missing, we cannot compute total investment
snis_inv <- snis5 %>%
  rename(inv_prest =
           fn033_investimentos_totais_realizados_pelo_prestador_de_servicos,
         inv_est = fn058_investimentos_totais_realizados_pelo_estado,
         inv_mun = fn048_investimentos_totais_realizados_pelo_municipio) %>%
  mutate(na_inv_prest = is.na(inv_prest),
         na_inv_est = is.na(inv_est),
         na_inv_mun = is.na(inv_mun)) %>%
  mutate_at(.vars = vars(inv_prest, inv_est, inv_mun),
            .funs = function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(inv_total =
           ifelse(na_inv_prest & na_inv_est & na_inv_mun,
                  NA_real_,
                  inv_prest + inv_est + inv_mun)) %>%
  mutate(inv_per_capita = inv_total / pop) %>% 
  select(codigo_municipio, municipio, tipo_servico, contains('inv'))

# Adding per capita investment feature
snis6 <- snis5 %>%
  left_join(snis_inv %>% select(codigo_municipio, tipo_servico, inv_per_capita),
            by = c('codigo_municipio', 'tipo_servico'))

saveRDS(snis6, 'data/snis-bahia.rds')

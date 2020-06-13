library(tidyverse)
source('helpers/custom_plot_theme.R')
theme_set(custom_theme())

# Reading dataset -------------------------------------------------------------
snis <- readxl::read_excel(
  'data/empresas-delegacao.xlsx',
  sheet = 1, range = 'A1:AD46')

# Data wrangling --------------------------------------------------------------
# Selecting and renaming variables
delegacao <- snis %>% 
  select(
    ano = `Ano de Referência`, codigo_prestador = `Código do Prestador`,
    prestador = Prestador, sigla_prestador = `Sigla do Prestador`,
    agua_atendidos = `G05A - Quantidade total de municípios atendidos com abastecimento de água`,
    esgoto_atendidos =  `G05B - Quantidade total de municípios atendidos com esgotamento sanitário`,
    agua_delegacao_em_vigor = `GE001 - Quantidade de municípios atendidos com abastecimento de água com delegação em vigor`,
    agua_delegacao_vencida = `GE002 - Quantidade de municípios atendidos com abastecimento de água com delegação vencida`,
    agua_sem_delegacao = `GE003 - Quantidade de municípios atendidos com abastecimento de água sem delegação`,
    esgoto_delegacao_em_vigor = `GE014 - Quantidade de municípios atendidos com esgotamento sanitário com delegação em vigor`,
    esgoto_delegacao_vencida =  `GE015 - Quantidade de municípios atendidos com esgotamento sanitário com delegação vencida` ,
    esgoto_sem_delegacao = `GE016 - Quantidade de municípios atendidos com esgotamento sanitário sem delegação`
    )

# Splitting into separate, long dataframes (water and sewage) -----------------
delegacao_agua <- delegacao %>% 
  select(ano, codigo_prestador, sigla_prestador, contains('agua')) %>% 
  pivot_longer(cols = contains('agua'), names_to = 'situacao',
               values_to = 'numero_municipios_agua', names_prefix = 'agua_')

delegacao_esgoto <- delegacao %>% 
  select(ano, codigo_prestador, sigla_prestador, contains('esgoto')) %>% 
  pivot_longer(cols = contains('esgoto'), names_to = 'situacao',
               values_to = 'numero_municipios_esgoto', names_prefix = 'esgoto_')

# Joining datasets and computing shares ---------------------------------------
delegacao_long <- delegacao_agua %>% 
  inner_join(
    delegacao_esgoto,
    by = c('ano', 'codigo_prestador', 'sigla_prestador', 'situacao')
    ) %>% 
  group_by(ano, codigo_prestador, sigla_prestador) %>% 
  mutate(
    share_atendidos_agua = 
      numero_municipios_agua / first(numero_municipios_agua) * 100,
    share_atendidos_esgoto = 
      numero_municipios_esgoto / first(numero_municipios_esgoto) * 100
    ) %>% 
  ungroup() %>% 
  mutate(situacao = case_when(
    situacao == 'atendidos' ~ 'Atendidos',
    situacao == 'delegacao_vencida' ~ 'Delegação vencida',
    situacao == 'delegacao_em_vigor' ~ 'Delegação em vigor',
    situacao == 'sem_delegacao' ~ 'Sem delegação'
  ))

# Removing rows corresponding to the total number of cities -------------------
delegacao_long_status <- delegacao_long %>% 
  filter(situacao != 'Atendidos')

# Area plot: agua -------------------------------------------------------------
delegacao_long_status %>% 
  ggplot() +
  geom_area(
    aes(x = ano, y = numero_municipios_agua,
        fill = situacao)
  ) +
  scale_fill_manual(
    values = c("#377EB8", "#E41A1C", "#4DAF4A"),
    name = 'Situação'
    ) +
  theme(panel.grid = element_blank(), legend.position = c(.77, .125)) +
  labs(
    x = 'Ano',
    y = 'Número de municípios atendidos (água)',
    title = 'Número de municípios atendidos com água',
    subtitle = 'Evolução conforme situação da delegação - Prestadores selecionados'
  ) +
  facet_wrap(~ sigla_prestador, ncol = 2,
             scales = 'free_y')

# Area plot: esgoto -------------------------------------------------------------
delegacao_long_status %>% 
  ggplot() +
  geom_area(
    aes(x = ano, y = numero_municipios_esgoto,
        fill = situacao)
  ) +
  scale_fill_manual(
    values = c("#377EB8", "#E41A1C", "#4DAF4A"),
    name = 'Situação'
  ) +
  theme(panel.grid = element_blank(), legend.position = c(.77, .125)) +
  labs(
    x = 'Ano',
    y = 'Número de municípios atendidos (esgoto)',
    title = 'Número de municípios atendidos com esgoto',
    subtitle = 'Evolução conforme situação da delegação - Prestadores selecionados'
  ) +
  facet_wrap(~ sigla_prestador, ncol = 2,
             scales = 'free_y')
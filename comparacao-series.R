library(tidyverse)
source('helpers/custom_plot_theme.R')
theme_set(custom_theme())

# Reading dataset -------------------------------------------------------------
snis <- readxl::read_excel(
  'data/snis-agregado-embasa-e-outras-serie-temporal.xlsx',
  sheet = 1, range = 'A1:AQ46')

# Data wrangling --------------------------------------------------------------
snis_clean <- snis %>% 
  rename(
    sigla_prestador = `Sigla do Prestador`,
    pop_atendida_agua = `AG001 - População total atendida com abastecimento de água` ,
    pop_atendida_esgoto = `ES001 - População total atendida com esgotamento sanitário`,
    investimento_agua_prestador = `FN023 - Investimento realizado em abastecimento de água pelo prestador de serviços`,
    investimento_esgoto_prestador = `FN024 - Investimento realizado em esgotamento sanitário pelo prestador de serviços`,
    investimento_total_prestador = `FN033 - Investimentos totais realizados pelo prestador de serviços`,
    investimento_agua_mun = `FN042 - Investimento realizado em abastecimento de água pelo(s) município(s)`,
    investimento_esgoto_mun = `FN043 - Investimento realizado em esgotamento sanitário pelo(s) município(s)`,
    investimento_total_mun = `FN048 - Investimentos totais realizados pelo(s) município(s)`,
    investimento_agua_estado = `FN052 - Investimento realizado em abastecimento de água pelo estado`,
    investimento_esgoto_estado = `FN053 - Investimento realizado em esgotamento sanitário pelo estado`,
    investimento_total_estado = `FN058 - Investimentos totais realizados pelo estado`,
    indice_produtividade = `IN048 - Índice de produtividade: empregados próprios por 1000 ligações de água + esgoto`
  ) %>% 
  mutate_at(
    .vars = vars(starts_with('investimento_')),
    .funs = ~ ifelse(is.na(.), 0, .) 
    ) %>% 
  mutate(
    investimentos_totais_agua = investimento_agua_prestador + investimento_agua_mun + investimento_agua_estado,
    investimentos_totais_esgoto = investimento_esgoto_prestador + investimento_esgoto_mun + investimento_esgoto_estado,
    investimentos_totais = investimento_total_prestador + investimento_total_mun + investimento_total_estado,
    inv_totais_agua_per_capita = investimentos_totais_agua /  pop_atendida_agua,
    inv_prestador_agua_per_capita = investimento_agua_prestador / pop_atendida_agua,
    inv_totais_esgoto_per_capita = investimentos_totais_esgoto / pop_atendida_esgoto,
    inv_prestador_esgoto_per_capita = investimento_esgoto_prestador / pop_atendida_esgoto
  ) %>% 
  mutate(sigla_prestador = sigla_prestador %>% 
           fct_relevel('EMBASA', 'CAGECE', 'COMPESA', 'COPASA', 'SANEPAR'))

# Custom function to draw lineplots -------------------------------------------
draw_lineplot <-  function(df, x, y, col,
                           xlab = NA, ylab = NA,
                           col_lab = 'Prestador', title = NA,
                           subtitle = 'Comparação entre empresas selecionadas'){
  
  if (is.na(xlab)) xlab <- deparse(substitute(x))
  if (is.na(ylab)) ylab <- deparse(substitute(y))
  if (is.na(col_lab)) col_lab <- deparse(substitute(col))
  if (is.na(title)) title <- str_c(ylab)
  
  x <- enquo(x)
  y = enquo(y)
  col = enquo(col)
  
  ggplot(df) +
    geom_line(
      aes(x = !!x, y = !!y, col = !!col),
      size = 1, alpha = .6
    ) +
    theme(panel.grid = element_blank()) +
    scale_color_brewer(palette = 'Set1', name = col_lab) +
    labs(x = xlab, y = ylab,title = title, subtitle = subtitle)
}

# Plotting --------------------------------------------------------------------
lineplot_inv_bruto <- 
  draw_lineplot(snis_clean, `Ano de Referência`,
                y = investimentos_totais / 1e+6,
                col = sigla_prestador,
                ylab = 'Investimento total (milhões de reais)',
                title = 'Evolução do investimento total')

ggsave(plot = lineplot_inv_bruto, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-investimento-total.png')

# Investimento água
lineplot_inv_agua <- 
draw_lineplot(snis_clean, `Ano de Referência`,
              y = inv_prestador_agua_per_capita,
              col = sigla_prestador,
              ylab = 'Investimento per capita em abastecimento de água',
              title = 'Evolução do investimento per capita em abastecimento de água')

ggsave(plot = lineplot_inv_agua, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-inv-per-capita-agua-empresas-selecionadas.png')

# Investimento esgoto
lineplot_inv_esgoto <- 
  draw_lineplot(df = snis_clean, x = `Ano de Referência`,
                y = inv_prestador_esgoto_per_capita,
                col = sigla_prestador,
                ylab = 'Investimento per capita em esgotamento sanitário',
                title = 'Evolução do investimento per capita em esgotamento sanitário')

ggsave(plot = lineplot_inv_esgoto, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-inv-per-capita-esgoto-empresas-selecionadas.png')

# Índice de produtividade
lineplot_produtividade <- 
  draw_lineplot(df = snis_clean, x = `Ano de Referência`,
              y = indice_produtividade,
              col = sigla_prestador,
              ylab = 'Índice de produtividade',
              title = 'Evolução do índice de produtividade (empregados/1000 ligações)')

ggsave(plot = lineplot_produtividade, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-indice-produtividade-empresas-selecionadas.png')

# Quantidade de empregados
lineplot_total_empregados <- 
  draw_lineplot(df = snis_clean, x = `Ano de Referência`,
                y = `FN026 - Quantidade total de empregados próprios`,
                col = sigla_prestador,
                ylab = 'Número de empregados próprios',
                title = 'Evolução do número de empregados próprios')

ggsave(plot = lineplot_total_empregados, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-empregados-proprios-empresas-selecionadas.png')

# Indicador de desempenho financeiro
lineplot_desempenho <- 
  draw_lineplot(df = snis_clean, x = `Ano de Referência`,
                y = `IN012 - Indicador de desempenho financeiro`,
                col = sigla_prestador,
                ylab = 'Indicador de desempenho financeiro',
                title = 'Evolução do indicador de desempenho financeiro')

ggsave(plot = lineplot_total_empregados, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-desempenho-financeiro-empresas-selecionadas.png')

# Coleta de esgoto
lineplot_coleta <- 
  draw_lineplot(df = snis_clean, x = `Ano de Referência`,
                y = `IN015 - Índice de coleta de esgoto`,
                col = sigla_prestador,
                ylab = 'Índice de coleta de esgoto',
                title = 'Evolução do índice de coleta de esgoto')

ggsave(plot = lineplot_coleta, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-coleta-empresas-selecionadas.png')

# Tratamento de esgoto
lineplot_tratamento <- 
  draw_lineplot(df = snis_clean, x = `Ano de Referência`,
                y = `IN046 - Índice de esgoto tratado referido à água consumida`,
                col = sigla_prestador,
                ylab = 'Índice de tratamento de esgoto (relativo à água consumida)',
                title = 'Evolução do índice de tratamento de esgoto')

ggsave(plot = lineplot_tratamento, width = 7, height = 5,
       filename = 'plots/comparacao-series-embasa/evolucao-tratamento-empresas-selecionadas.png')

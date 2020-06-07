library(tidyverse)
library(reshape2)
source('helpers.R')
theme_set(custom_theme())

# Loading and filtering dataset -----------------------------------------------
snis <- readRDS('data/snis-bahia.rds')

snis_esgoto <- snis %>% filter(tipo_servico != 'Água') %>% 
  filter(!(municipio == 'Araci' & sigla_prestador == 'PMA'))

# Subsetting: only relevant columns, with shorter names -----------------------
snis_subset <- snis_esgoto %>% 
  select(municipio,
         pib_per_capita = pib_per_capita2017,
         tarifa_media_agua = in005_tarifa_media_de_agua,
         tarifa_media_esgoto = in006_tarifa_media_de_esgoto,
         perdas_agua = in013_indice_de_perdas_faturamento,
         atendimento_agua = in055_indice_de_atendimento_total_de_agua,
         coleta_esgoto = in015_indice_de_coleta_de_esgoto,
         tratamento_esgoto = in046_indice_de_esgoto_tratado_referido_a_agua_consumida,
         consumo_medio_per_capita_agua = in022_consumo_medio_percapita_de_agua,
         investimento_per_capita = inv_per_capita,
         desempenho_financeiro = in012_indicador_de_desempenho_financeiro
  )

dim(snis_subset)

# Computing the correlation matrix --------------------------------------------
snis_complete_variables <- snis_subset %>% 
  select(-municipio) %>% 
  drop_na()

dim(snis_complete_variables)

corr_matrix <- snis_complete_variables %>% 
  cor() %>% 
  round(2)

# Function to reorder matrix based on clustering
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}

# Reordering for a more informative plot
corr_matrix = reorder_cormat(corr_matrix)

# Melting
melted_cormat <- melt(corr_matrix, na.rm=TRUE)

# Function for redefining factor labels for better text on plot labels
pretty_labels <- function(x) {
  str_replace(x, 'investimento_per_capita', 'Invest. per capita') %>% 
    str_replace('tarifa_media_esgoto', 'Tarifa média (esgoto)') %>% 
    str_replace('tarifa_media$', 'Tarifa média (total)') %>% 
    str_replace('tarifa_media_agua', 'Tarifa média (água)') %>% 
    str_replace('coleta_esgoto', 'Coleta (esgoto)') %>% 
    str_replace('tratamento_esgoto', 'Tratamento (esgoto)') %>% 
    str_replace('consumo_medio_per_capita_agua', 'Cons. per capita (água)') %>% 
    str_replace('atendimento_agua' ,'Atendimento (água)') %>% 
    str_replace('desempenho_financeiro', 'Desemp. financeiro') %>% 
    str_replace('perdas_agua', 'Perdas de faturamento') %>% 
    str_replace('pib_per_capita', 'PIB per capita')
}

# Redefining factor labels for pretty names
melted_cormat_pretty <- melted_cormat %>% 
  mutate(Var1 = fct_relabel(Var1, .fun = ~pretty_labels(.)),
         Var2 = fct_relabel(Var2, .fun = ~pretty_labels(.)))

# Plotting
corr_plot <- 
  ggplot(data = melted_cormat_pretty,
         aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = 'white') +
  geom_text(
    aes(x = Var2, y = Var1,
        label = formatC(value, big.mark = '.', decimal.mark = ',')),
    color = "gray10", size = 3.25, family = 'serif'
  ) +
  geom_text(
    data = melted_cormat_pretty %>% filter(value >= .9),
    aes(x = Var2, y = Var1,
        label = formatC(value, big.mark = '.', decimal.mark = ',')),
    color = "gray90", size = 3.25, family = 'serif'
  ) +
  scale_fill_gradient2(
    high = '#053061', low  = '#67000d', #'#b2182b',
    mid = 'white',
    midpoint = 0, limit = c(-1, 1), space = 'Lab',
    name = "Coeficiente\nde correlação",
    label = function(x) formatC(x, big.mark = '.', decimal.mark = ',')
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    text = element_text(family = 'serif'),
    axis.title = element_blank(),
    axis.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid = element_blank(),
    legend.position = 'top',
    legend.direction = 'horizontal'
  ) +
  guides(fill = guide_colorbar(
    barwidth = 7, barheight = 1,
    title.position = 'top', title.hjust = 0.5
  )) ; corr_plot

# Saving
ggsave('plots/esgoto/corr-plot-esgoto.png', corr_plot,
       width = 6, height = 6)
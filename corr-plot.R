library(tidyverse)
library(reshape2)
library(cagedExplorer)

# Loading and filtering dataset -----------------------------------------------
snis <- readRDS('snis-2018-clean.rds')

snis_agua <- snis %>% 
  filter(!(municipio_clean %in% 
             c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA') &
             tipo_servico == 'Esgotos'))

snis_esgoto <- snis %>% 
  filter(!(municipio_clean %in% 
             c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA') &
             tipo_servico == 'Água'))

# Custom map theme and settings -----------------------------------------------
theme_set(custom_theme())

# Subsetting: only relevant columns, with shorter names -----------------------
# Water and 'overall' features
snis_subset_agua <- snis_agua %>% 
  select(municipio_clean,
         tarifa_media = in004_tarifa_media_praticada,
         tarifa_media_agua = in005_tarifa_media_de_agua,
         perdas_agua = in013_indice_de_perdas_faturamento,
         atendimento_agua = in055_indice_de_atendimento_total_de_agua,
         consumo_medio_per_capita_agua = in022_consumo_medio_percapita_de_agua,
         investimento_per_capita = inv_per_capita,
         desempenho_financeiro = in012_indicador_de_desempenho_financeiro
  )

# Sewage-related features
snis_subset_esgoto <- snis_esgoto %>% 
  select(municipio_clean,
         tarifa_media_esgoto = in006_tarifa_media_de_esgoto,
         coleta_esgoto = in015_indice_de_coleta_de_esgoto,
         tratamento_esgoto = in016_indice_de_tratamento_de_esgoto,
  )

# Concatenating partial datasets
snis_subset <- snis_subset_agua %>% 
  left_join(snis_subset_esgoto, by = 'municipio_clean')

# Computing the correlation matrix --------------------------------------------
corr_matrix <- snis_subset %>% 
  select(-municipio_clean) %>% 
  drop_na() %>% 
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
    str_replace('coleta_esgoto', 'Coleta de esgoto') %>% 
    str_replace('tratamento_esgoto', 'Trat. de esgoto') %>% 
    str_replace('consumo_medio_per_capita_agua', 'Cons. de água per capita') %>% 
    str_replace('atendimento_agua' ,'Atend. de água') %>% 
    str_replace('desempenho_financeiro', 'Desemp. financeiro') %>% 
    str_replace('perdas_agua', 'Perdas (faturamento)')
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
  scale_fill_gradient2(
    low = '#053061', high  = '#b2182b', mid = 'white',
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
ggsave('plots/corr-plot.png', corr_plot, width = 6, height = 6)

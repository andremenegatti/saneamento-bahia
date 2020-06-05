library(tidyverse)
library(sp)
library(sf)
source('helpers/substituir_caracteres_especiais.R')

mapa_brasil <- readRDS('data/mapa_brasil.rds') %>% 
  st_as_sf()

bahia_polygons <- mapa_brasil %>% 
  filter(UF == 29) %>% 
  mutate(
    codigo_municipio = str_extract(CD_GEOCMU, '\\d{6}') %>% as.numeric(),
    municipio_clean = substituir_caracteres_especiais(NM_MUNICIP)
    ) %>% 
  select(codigo_municipio, municipio_clean, geometry)

saveRDS(bahia_polygons, 'data/polygons_municipios_bahia.rds')
substituir_caracteres_especiais <- function(x) {
  toupper(x) %>% 
    str_replace_all('Á', 'A') %>% 
    str_replace_all('Ó', 'O') %>% 
    str_replace_all('É', 'E') %>% 
    str_replace_all('Ã', 'A') %>% 
    str_replace_all('Õ', 'O') %>% 
    str_replace_all('Ç', 'C') %>% 
    str_replace_all('Í', 'I') %>% 
    str_replace_all('À', 'A') %>% 
    str_replace_all('Ê', 'E') %>% 
    str_replace_all('Ê', 'O') %>% 
    str_replace_all('Ú', 'U')
}
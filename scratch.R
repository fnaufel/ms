# 
# 
# # Primeira tentativa ------------------------------------------------------
# 
# source('packages.R')
# 
# nome <- 'Dados/resultados.html'
# 
# lido <- read_html(nome, encoding = 'UTF-8')
#   
# linhas <- lido %>% 
#     html_nodes('body > table > tbody > tr:not([bgcolor])')
# 
# ver_linha <- function(linha) {
#   
#   cols <- linha %>% 
#     html_nodes('td') %>% 
#     html_text(trim = TRUE)
#   
#   cols
# 
# }
# 
# tr <- map(linhas, ver_linha)
# 
# df <- bind_rows(
#   tr %>% 
#   map(as_tibble_row, .name_repair = 'unique')
# )
# 
# 
# # Tentando com o novo arquivo html ----------------------------------------
# 
# source('packages.R')
# 
# nome <- 'Dados/d_megasc.htm'
# 
# lido <- read_html(nome, encoding = 'UTF-8')
# 
# df <- lido %>% 
#   rvest::html_table(
#     trim = TRUE,
#     fill = TRUE,
#     dec = ','
#   )
# 
# df[[1]] %>% view()
# 
# 
# # Com funções -------------------------------------------------------------
# 
# source('packages.R')
# source('R/ler_resultados.R')
# 
# nome <- 'Dados/d_megasc.htm'
# df <- ler_resultados(nome)
# 
# df %>% view()


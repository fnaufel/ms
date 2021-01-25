##' Processar tabela com caption 'Quantidade de prêmios a receber acertando'
##' no url http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena
##'
##' @title
##' @param 
##' @return tibble
##' @author fnaufel
##' @export
ler_premios <- function() {
  
  endereco <- 'http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena'

  lido <- read_html(endereco, encoding = 'UTF-8')
  
  premios <- lido %>% 
    html_nodes('table') %>% 
    `[[`(2) %>%
    html_table() %>% 
    as_tibble(.name_repair = 'unique') %>% 
    rename(
      jogados = 1,
      senas_6 = 2,
      quinas_6 = 3,
      quadras_6 = 4,
      quinas_5 = 5,
      quadras_5 = 6,
      quadras_4 = 7
    ) %>% 
    # Tirar cabeçalho extra
    slice(-1) %>% 
    pivot_longer(
      cols = !jogados,
      names_to = c('acertos'),
      names_pattern = '[[:alnum:]]+_([[:alnum:]]+)'
    ) %>% 
    nest(temp = value) %>% 
    mutate(
      jogados = as.numeric(jogados),
      acertos = as.numeric(acertos),
      temp = map2(acertos, temp, f)
    ) %>% 
    unnest(temp)
  
}


f <- function(acertos, sqq) {
  
  senas <- quinas <- quadras <- 0
  
  if (acertos == 6) {
    senas <- sqq$value[1]
    quinas <- sqq$value[2]
    quadras <- sqq$value[3]
  }
  
  if (acertos == 5) {
    quinas <- sqq$value[1]
    quadras <- sqq$value[2]
  }

  if (acertos == 4) {
    quadras <- sqq$value[1]
  }
  
  tibble(
    senas   = as.numeric(senas),
    quinas  = as.numeric(quinas),
    quadras = as.numeric(quadras)
  )
  
}

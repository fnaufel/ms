##' Ler e limpar arquivo html local, baixado de http://www1.caixa.gov.br/loterias/_arquivos/loterias/D_mgsasc.zip
##'
##' @param nome Nome do arquivo html a ser lido
##'
##' @return Uma tibble com o conteúdo da tabela contida no html
##' 
##' @author fnaufel
##' 
##' @export
##' 
ler_resultados <- function(nome) {

  lido <- read_html(nome, encoding = 'UTF-8')
  
  lista <- lido %>% 
    rvest::html_table(
      trim = TRUE,
      fill = TRUE,
      dec = ','
    )

  df <- lista[[1]] %>% 
    clean_names() %>% 
    select(-cidade, -uf) %>% 
    distinct()
  
  df %>%
    transmute(
      num = as.numeric(concurso),
      data = lubridate::dmy(data_sorteio),
      d1 = as.numeric(x1_dezena),
      d2 = as.numeric(x2a_dezena),
      d3 = as.numeric(x3a_dezena),
      d4 = as.numeric(x4a_dezena),
      d5 = as.numeric(x5a_dezena),
      d6 = as.numeric(x6a_dezena),
      arrec = valor(arrecadacao_total),
      g_sena = ganhadores_sena,
      r_sena = valor(rateio_sena),  
      g_quina = ganhadores_quina,
      r_quina = valor(rateio_quina),  
      g_quadra = ganhadores_quadra,
      r_quadra = valor(rateio_quadra),
      acumulou = as.logical(acumulado == 'SIM'),
      valor_ac = valor(valor_acumulado),
      est_prox = valor(estimativa_premio),
      ac_virada = valor(acumulado_mega_da_virada)
    )

}


#' Eliminar vírgulas de milhar e substituir ponto decimal por vírgula
#'
#' @param x String
#'
#' @return String sem vírgulas e com ponto como separador decimal
#' 
#' @export
#'
valor <- function(x) {
  
  x %>% 
    str_replace_all(
      stringr::fixed('.'),
      ''
    ) %>% 
    str_replace(
      ',(\\d\\d)',
      '.\\1'
    ) %>% 
    as.numeric()
  
}

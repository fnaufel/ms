##' Ler e limpar arquivo html local, baixado de http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena
##'
##' O html está tão bagunçado que não consigo usar \code{read_table}.
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
  
  linhas <- lido %>% 
    html_nodes('body > table > tbody > tr:not([bgcolor])')
  
  linhas_vetores <- linhas %>% 
    map(extrair_dados_linha)

  df <- bind_rows(
    linhas_vetores %>% 
      map(as_tibble_row)
  )
  
  df %>% 
    clean_names() %>% 
    mutate(
      concurso = as.numeric(concurso),
      data_do_sorteio = lubridate::dmy(data_do_sorteio),
      across(starts_with('coluna'), as.numeric),
      across(starts_with('ganhadores'), as.numeric),
      acumulado = as.factor(acumulado),
      sorteio_especial = as.factor(sorteio_especial),
      across(starts_with('rateio'), valor),
      across(starts_with('valor'), valor),
      estimativa_para_o_proximo_concurso = valor(estimativa_para_o_proximo_concurso)
    )
  
}


#' Processar uma linha da tabela
#' 
#' @param linha 
#'
#' @return Um vetor de strings, cada elemento correspondendo a uma célula
#' 
#' @export
#'
extrair_dados_linha <- function(linha) {
  
  cols <- linha %>% 
    html_nodes('td') %>% 
    html_text(trim = TRUE)

  if (cols[16] != '') {
    cols <- cols[-(16:18)]
  } else {
    cols <- cols[-16]
  }
    
  cols <- cols[1:21]
  names(cols) <- c(
    'Concurso',
    'Local',
    'Data do Sorteio',
    'Coluna 1',
    'Coluna 2',
    'Coluna 3',
    'Coluna 4',
    'Coluna 5',
    'Coluna 6',
    'Ganhadores Faixa 1',
    'Ganhadores Faixa 2',
    'Ganhadores Faixa 3',
    'Rateio Faixa 1',
    'Rateio Faixa 2',
    'Rateio Faixa 3',
    'Valor Arrecadado',
    'Estimativa para o próximo concurso',
    'Valor Acumulado Próximo Concurso',
    'Acumulado',
    'Sorteio Especial',
    'Observação'
  )
  
  cols
  
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
      fixed('.'),
      ''
    ) %>% 
    str_replace(
      ',(\\d\\d)',
      '.\\1'
    ) %>% 
    as.numeric()
  
}

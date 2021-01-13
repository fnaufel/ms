##' Criar novo df contendo, para cada concurso, as 60 dezenas 
##' em ordem decrescente de frquência acumulada
##'
##' @param df_vetor
##' @return df_raras
##' @author fnaufel
##' @export
criar_df_raras <- function(df_vetor) {

  df_vetor %>% 
    
    # resultado é list column, cada linha um vetor de 6 inteiros
    select(num, resultado) %>% 
    
    # criar coluna freqs com vetor de 60 posições por concurso 
    # (0 = dezena não saiu, 1 = saiu)
    rowwise() %>% 
    transmute(
      num = num,
      freqs = list(tabular(resultado))
    ) %>% 
    ungroup() %>%
    
    # transformar freqs em 60 colunas
    unnest_wider(freqs) %>%
    
    # transformar cada coluna em frequências acumuladas
    transmute(
      num = num,
      across(d1:d60, cumsum)
    ) %>% 
    
    # transformar d1:d60 em uma list column, cada linha um vetor de 60 posições
    rowwise() %>% 
    mutate(
      num = num,
      freqs = list(c_across(d1:d60)),
      .keep = 'unused'
    ) %>% 
    ungroup() %>% 
    
    # transformar freqs acumuladas nas dezenas correspondentes, em ordem crescente
    mutate(
      dezenas = map(freqs, order),
      .keep = 'unused'
    ) %>% 
    
    # Adicionar 1 ao número do concurso, para jogar no concurso n + 1 as dezenas 
    # mais raras até o concurso n 
    mutate(num = num + 1)
  
}


#' Transforma vetor de 6 posições em um vetor de 60 posições, 
#' com 0 para cada dezena que não está no vetor original
#' e 1 para cada dezena que está no vetor original
#'
#' @param resultado Vetor com 6 inteiros, cada um entre 1 e 60, inclusive
#'
#' @return vetor de 60 posições
#' @export
#'
tabular <- function(resultado) {
  
  # A princípio, nenhuma dezena saiu
  vetor <- rep(0, 60)
  
  # Nomes das posições, para facilitar unnest_wider depois
  names(vetor) <- paste0('d', 1:60)
  
  # Dezenas que saíram 
  vetor[resultado] <- 1
  
  vetor
  
}

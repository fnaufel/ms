##' Computa quantidade de acertos em cada concurso
##'
##' @param df_vetor Data frame com resultados
##' @param dezenas_a_jogar Data frame com 2 colunas: num e dezenas (vetor)
##' @return Data frame com uma linha por concurso, com coluna n_acertos
##' @author fnaufel
##' @export
computar_acertos <- function(
  df_vetor, 
  dezenas_a_jogar
) {

  df_vetor %>% 
    select(num, resultado) %>% 
    inner_join(dezenas_a_jogar, by = 'num') %>% 
    rowwise() %>% 
    mutate(
      n_acertos = sum(dezenas %in% resultado),
      .keep = 'unused'
    ) %>% 
    ungroup()

}
